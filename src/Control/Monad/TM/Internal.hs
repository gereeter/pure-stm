{-# LANGUAGE ExistentialQuantification, DoAndIfThenElse #-}

module Control.Monad.TM.Internal where

import qualified Data.Map as M
import qualified Data.Set as S
import Data.TLRef
import Data.Maybe (fromJust, catMaybes)
import Control.Exception
import Control.Monad
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.State.Strict
import Data.IORef
import Control.Concurrent
import Control.Applicative
import System.IO.Unsafe
import GHC.Exts (Any) -- TODO: figure out what to do if not GHC
import Unsafe.Coerce

-- Use thread-local counts to reduce contention
{-# NOINLINE uniques #-}
uniques :: TLRef Int
uniques = unsafePerformIO (newTLRef 0)

type Timestamp = Int -- N.B. These are Lamport timestamps
data TVarId = TVarId ThreadId Int deriving (Eq, Ord)
data TVar a = TVar TVarId (MVar ()) (IORef Timestamp) (IORef a) (IORef [MVar ()])

instance Eq (TVar a) where
    TVar tvarId1 _ _ _ _ == TVar tvarId2 _ _ _ _ = tvarId1 == tvarId2

-- TODO: Is this a good idea?
instance Ord (TVar a) where
    TVar tvarId1 _ _ _ _ `compare` TVar tvarId2 _ _ _ _ = tvarId1 `compare` tvarId2

data ATVar = forall a . ATVar (TVar a)

instance Eq ATVar where
    ATVar (TVar tvarId1 _ _ _ _) == ATVar (TVar tvarId2 _ _ _ _) = tvarId1 == tvarId2

instance Ord ATVar where
    ATVar (TVar tvarId1 _ _ _ _) `compare` ATVar (TVar tvarId2 _ _ _ _) = tvarId1 `compare` tvarId2

data Result a = Abort | Retry | Good !a

data TRec = TRec
        { curTime  :: !Timestamp
        , topTime  :: !(Maybe Timestamp)
        , writeSet :: !(S.Set ATVar)
        , curSet   :: !(M.Map ATVar Timestamp)
        , cache    :: !(M.Map ATVar Any)}

newtype TM a = TM {unTM :: StateT TRec IO (Result a)}

instance Monad TM where
    return x = TM $ return (Good x)
    TM mx >>= f = TM $ do
        x <- mx
        case x of
            Good val -> unTM (f val)
            Abort    -> return Abort
            Retry    -> return Retry

instance MonadPlus TM where
    mzero = TM $ return Retry
    TM left `mplus` TM right = TM $ do
        leftRet <- left
        case leftRet of
            Retry -> right
            _     -> return leftRet

modifyCurTime :: (Timestamp -> Timestamp) -> StateT TRec IO ()
modifyCurTime f = modify $ \rec -> rec {curTime = f (curTime rec)}

modifyTopTime :: (Maybe Timestamp -> Maybe Timestamp) -> StateT TRec IO ()
modifyTopTime f = modify $ \rec -> rec {topTime = f (topTime rec)}

modifyWriteSet :: (S.Set ATVar -> S.Set ATVar) -> StateT TRec IO ()
modifyWriteSet f = modify $ \rec -> rec {writeSet = f (writeSet rec)}

modifyCurSet :: (M.Map ATVar Timestamp -> M.Map ATVar Timestamp) -> StateT TRec IO ()
modifyCurSet f = modify $ \rec -> rec {curSet = f (curSet rec)}

modifyCache :: (M.Map ATVar Any -> M.Map ATVar Any) -> StateT TRec IO ()
modifyCache f = modify $ \rec -> rec {cache = f (cache rec)}

retry :: TM a
retry = mzero

orElse :: TM a -> TM a -> TM a
orElse = mplus

unsafeIOToTM :: IO a -> TM a
unsafeIOToTM act = TM $ Good <$> lift act

-- TODO: It feels like I need to grab a lock here.
-- FIXME: This breaks retry - we want to detect if unread vars change
unsafeUnreadTVar :: TVar a -> TM ()
unsafeUnreadTVar tvar = do
    validateTVar tvar
    TM $ do
        writeSet' <- writeSet <$> get
        if S.null writeSet'
        then do
            modifyCurSet (M.delete (ATVar tvar))
            modifyCache (M.delete (ATVar tvar))
        else return ()
        return (Good ())

newTVar :: a -> TM (TVar a)
newTVar val = TM $ do
    tvar <- lift (newTVarIO val)
    modifyCache (M.insert (ATVar tvar) (unsafeCoerce val))
    return (Good tvar)

newTVarIO :: a -> IO (TVar a)
newTVarIO val = do
    uniq <- readTLRef uniques
    writeTLRef uniques (uniq + 1)
    tid <- myThreadId
    let tvarId = TVarId tid uniq
    lock <- newMVar ()
    timeRef <- newIORef 0 -- this will be overwritten on commit
    valRef <- newIORef val -- this will be overwritten on commit
    waitLocksRef <- newIORef []
    return (TVar tvarId lock timeRef valRef waitLocksRef)

readTVar :: TVar a -> TM a
readTVar tvar@(TVar _ lock timeRef valRef _) = TM $ do
    cache' <- cache <$> get
    case M.lookup (ATVar tvar) cache' of
        Just val -> return $ Good $ unsafeCoerce val
        Nothing  -> do
            (time, val) <- lift $ withMVar lock $ \() -> (,) <$> readIORef timeRef <*> readIORef valRef
            modifyCurSet (M.insert (ATVar tvar) time)
            modifyCache (M.insert (ATVar tvar) (unsafeCoerce val))
            curTime' <- curTime <$> get
            when (time > curTime') $ modifyCurTime (const time)
            topTimeM <- topTime <$> get
            let success = time <= curTime' || maybe True (> curTime' + 1) topTimeM
            return (if success then Good val else Abort)
    
-- TODO: exception safety
readTVarIO :: TVar a -> IO a
readTVarIO (TVar _ _ _ ref _) = readIORef ref

writeTVar :: TVar a -> a -> TM ()
writeTVar tvar val = do
    _ <- readTVar tvar
    TM $ do
        modifyCache (M.insert (ATVar tvar) (unsafeCoerce val))
        modifyWriteSet (S.insert (ATVar tvar))
        return (Good ())

-- TODO: exception safety
writeTVarIO :: TVar a -> a -> IO ()
writeTVarIO (TVar _ lock timeRef valRef _) val = withMVar lock $ \() -> do
    modifyIORef' timeRef (+1)
    writeIORef valRef val

validateTVar :: TVar a -> TM ()
validateTVar tvar@(TVar _ _ timeRef _ _) = TM $ do
    newTime <- lift $ readIORef timeRef
    curSet' <- curSet <$> get
    case M.lookup (ATVar tvar) curSet' of
        Nothing -> return (Good ())
        Just oldTime | oldTime == newTime -> return (Good ())
                     | otherwise -> do
                         modifyCurSet (M.delete (ATVar tvar))
                         curTime' <- curTime <$> get
                         if curTime' + 1 < newTime
                         then do
                            modifyTopTime (Just . maybe newTime (min newTime))
                            return (Good ())
                         else return Abort

-- TODO: exception safety
atomically :: TM a -> IO a
atomically (TM act) = mask_ loop where
    loop = do
        let rec = TRec 0 Nothing S.empty M.empty M.empty
        (maybeRet, rec') <- runStateT act rec
        case maybeRet of
            Abort -> loop
            Retry -> do
                waitLock <- newEmptyMVar
                immediateAbort <- or <$>
                    (forM (M.keys (cache rec')) $
                    \atvar@(ATVar (TVar _ lock timeRef _ waitLocksRef)) ->
                    withMVar lock $ \() -> do
                        time <- readIORef timeRef
                        if time == curSet rec' M.! atvar
                        then modifyIORef waitLocksRef (waitLock :) >> return False
                        else return True
                    )
                if immediateAbort
                then loop
                else takeMVar waitLock >> loop
            Good ret -> do
                
                forM_ (M.keys (cache rec')) $ \(ATVar (TVar _ lock _ _ _)) -> takeMVar lock
                
                times <- (forM (M.toList (curSet rec')) $
                    \(ATVar (TVar _ _ timeRef _ _), recTime) -> do
                        time <- readIORef timeRef
                        if time /= recTime
                        then return (Just time)
                        else return Nothing)
                let prunedTimes = catMaybes (topTime rec' : times)
                let readSuccess = if null prunedTimes
                                  then True
                                  else curTime rec' + 1 < minimum prunedTimes
                writeSuccess <- and <$>
                    (forM (S.toList (writeSet rec')) $
                    \atvar@(ATVar (TVar _ _ timeRef _ _)) -> do
                        time <- readIORef timeRef
                        return (time == curSet rec' M.! atvar))
                let success = readSuccess && writeSuccess
                if success
                then forM_ (S.toList (writeSet rec')) $ \atvar@(ATVar (TVar _ _ timeRef valRef waitLocksRef)) -> do
                    let val = fromJust $ M.lookup atvar (cache rec')
                    writeIORef timeRef (curTime rec' + 1)
                    writeIORef valRef (unsafeCoerce val)
                    waitLocks <- readIORef waitLocksRef
                    forM_ waitLocks $ \waitLock -> tryPutMVar waitLock ()
                    writeIORef waitLocksRef []
                else return () -- abort
                
                forM_ (M.keys (cache rec')) $ \(ATVar (TVar _ lock _ _ _)) -> putMVar lock ()
                
                if success
                then return ret
                else loop -- abort
