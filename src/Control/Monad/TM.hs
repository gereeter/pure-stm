{-# LANGUAGE ExistentialQuantification, DoAndIfThenElse #-}

module Control.Monad.TM where

import qualified Data.Map as M
import qualified Data.Set as S
import Data.TLRef
import Data.Maybe (fromJust, catMaybes)
import Control.Exception
import Control.Monad
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

newtype TM a = TM {unTM ::
        IORef Timestamp -> -- the timestamp of the transaction
        IORef (Maybe Timestamp) -> -- the timestamp bound
        IORef (S.Set ATVar) -> -- the write set
        IORef (M.Map ATVar Timestamp) -> -- the set of TVars that were current last time we checked
        IORef (M.Map ATVar Any) -> -- the read/write cache
        IO (Maybe a)
    }

instance Monad TM where
    return x = TM $ \_ _ _ _ _ -> return (Just x)
    TM mx >>= f = TM $ \curTimeRef topTimeRef writeSetRef curSetRef cacheRef -> do
        x <- mx curTimeRef topTimeRef writeSetRef curSetRef cacheRef
        case x of
            Nothing -> return Nothing
            Just val -> unTM (f val) curTimeRef topTimeRef writeSetRef curSetRef cacheRef

instance MonadPlus TM where
    mzero = TM $ \_ _ _ _ _ -> return Nothing
    TM left `mplus` TM right = TM $ \curTimeRef topTimeRef writeSetRef curSetRef cacheRef -> do
        leftRet <- left curTimeRef topTimeRef writeSetRef curSetRef cacheRef
        case leftRet of
            Nothing -> right curTimeRef topTimeRef writeSetRef curSetRef cacheRef
            Just ret -> return (Just ret)

retry :: TM a
retry = mzero

orElse :: TM a -> TM a -> TM a
orElse = mplus

unsafeIOToTM :: IO a -> TM a
unsafeIOToTM act = TM $ \_ _ _ _ _ -> Just <$> act

newTVar :: a -> TM (TVar a)
newTVar val = TM $ \_ _ _ _ cacheRef -> do
    tvar <- newTVarIO val
    modifyIORef' cacheRef (M.insert (ATVar tvar) (unsafeCoerce val))
    return (Just tvar)

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
readTVar tvar@(TVar _ lock timeRef valRef _) = TM $ \curTimeRef topTimeRef _ curSetRef cacheRef -> do
    cache <- readIORef cacheRef
    if M.member (ATVar tvar) cache
    then return (Just $ unsafeCoerce $ cache M.! ATVar tvar)
    else do
        (time, val) <- withMVar lock $ \() -> (,) <$> readIORef timeRef <*> readIORef valRef -- TODO: do I really need the lock?
        modifyIORef' curSetRef (M.insert (ATVar tvar) time)
        modifyIORef' cacheRef (M.insert (ATVar tvar) (unsafeCoerce val))
        curTime <- readIORef curTimeRef
        success <- if time > curTime
            then do
                topTimeM <- readIORef topTimeRef
                case topTimeM of
                    Nothing -> writeIORef curTimeRef time >> return True
                    Just topTime | topTime > curTime -> writeIORef curTimeRef time >> return True
                                 | otherwise         -> return False
            else return True
        return (if success then Just val else Nothing)

-- TODO: exception safety
readTVarIO :: TVar a -> IO a
readTVarIO (TVar _ _ _ ref _) = readIORef ref

writeTVar :: TVar a -> a -> TM ()
writeTVar tvar val = do
    _ <- readTVar tvar
    TM $ \_ _ writeSetRef _ cacheRef -> do
        modifyIORef' cacheRef (M.insert (ATVar tvar) (unsafeCoerce val))
        modifyIORef' writeSetRef (S.insert (ATVar tvar))
        return (Just ())

-- TODO: exception safety
writeTVarIO :: TVar a -> a -> IO ()
writeTVarIO (TVar _ lock timeRef valRef _) val = withMVar lock $ \() -> do
    modifyIORef' timeRef (+1)
    writeIORef valRef val

validateTVar :: TVar a -> TM ()
validateTVar tvar@(TVar _ _ timeRef _ _) = TM $ \curTimeRef topTimeRef _ curSetRef _ -> do
    newTime <- readIORef timeRef
    curSet <- readIORef curSetRef
    case M.lookup (ATVar tvar) curSet of
        Nothing -> return (Just ())
        Just oldTime | oldTime == newTime -> return (Just ())
                     | otherwise -> do
                         writeIORef curSetRef (M.delete (ATVar tvar) curSet)
                         curTime <- readIORef curTimeRef
                         if curTime < newTime
                         then do
                            topTimeM <- readIORef topTimeRef
                            case topTimeM of
                                Nothing -> writeIORef topTimeRef (Just newTime)
                                Just topTime -> writeIORef topTimeRef (Just (min topTime newTime))
                            return (Just ())
                         else return Nothing

-- TODO: exception safety
atomically :: TM a -> IO a
atomically (TM act) = mask_ loop where
    loop = do
        curTimeRef <- newIORef 0
        topTimeRef <- newIORef Nothing
        writeSetRef <- newIORef S.empty
        curSetRef <- newIORef M.empty
        cacheRef <- newIORef M.empty
        maybeRet <- act curTimeRef topTimeRef writeSetRef curSetRef cacheRef
        curTime <- readIORef curTimeRef
        topTimeM <- readIORef topTimeRef
        writeSet <- readIORef writeSetRef
        curSet <- readIORef curSetRef
        cache <- readIORef cacheRef
        case maybeRet of
            Nothing -> do
                waitLock <- newEmptyMVar
                immediateAbort <- or <$>
                    (forM (M.keys cache) $
                    \atvar@(ATVar (TVar _ lock timeRef _ waitLocksRef)) ->
                    withMVar lock $ \() -> do
                        time <- readIORef timeRef
                        if time == curSet M.! atvar
                        then modifyIORef waitLocksRef (waitLock :) >> return False
                        else return True
                    )
                if immediateAbort
                then loop
                else takeMVar waitLock >> loop
            Just ret -> do
                
                forM_ (M.keys cache) $ \(ATVar (TVar _ lock _ _ _)) -> takeMVar lock
                
                times <- catMaybes <$>
                    (forM (M.toList curSet) $
                    \(ATVar (TVar _ _ timeRef _ _), recTime) -> do
                        time <- readIORef timeRef
                        if time /= recTime
                        then return (Just time)
                        else return Nothing)
                let topTime = case topTimeM of
                        Nothing -> if null times then Nothing else Just (minimum times)
                        Just tt -> if null times then Just tt else Just (min tt (minimum times))
                let readSuccess = case topTime of
                        Nothing -> True
                        Just tt -> curTime + 1 < tt
                writeSuccess <- and <$>
                    (forM (S.toList writeSet) $
                    \atvar@(ATVar (TVar _ _ timeRef _ _)) -> do
                        time <- readIORef timeRef
                        return (time == curSet M.! atvar))
                let success = readSuccess && writeSuccess
                if success
                then forM_ (S.toList writeSet) $ \atvar@(ATVar (TVar _ _ timeRef valRef waitLocksRef)) -> do
                    let val = fromJust $ M.lookup atvar cache
                    writeIORef timeRef (curTime + 1)
                    writeIORef valRef (unsafeCoerce val)
                    waitLocks <- readIORef waitLocksRef
                    forM_ waitLocks $ \waitLock -> tryPutMVar waitLock ()
                    writeIORef waitLocksRef []
                else putStrLn "abort" -- abort
                
                forM_ (M.keys cache) $ \(ATVar (TVar _ lock _ _ _)) -> putMVar lock ()
                
                if success
                then return ret
                else loop -- abort


test1 :: IO ()
test1 = do
    var1 <- atomically (newTVar 0) :: IO (TVar Int)
    var2 <- atomically (newTVar 0) :: IO (TVar Int)
    mvar1 <- newEmptyMVar
    mvar2 <- newEmptyMVar
    _ <- forkIO $ atomically (do 
        val <- readTVar var1
        unsafeIOToTM yield
        writeTVar var2 (val + 1)) >> putMVar mvar1 ()
    _ <- forkIO $ atomically (do 
        val <- readTVar var2
        unsafeIOToTM yield
        writeTVar var1 (val + 1)) >> putMVar mvar2 ()
    takeMVar mvar1
    takeMVar mvar2
    ret1 <- atomically (readTVar var1)
    ret2 <- atomically (readTVar var2)
    if ((ret1 == 1 && ret2 == 2) || (ret1 == 2 && ret2 == 1))
    then return ()
    else putStrLn "test1 failed" >> print ret1 >> print ret2
{-
test2 :: Int -> IO ()
test2 n = do
    locks <- replicateM n (atomically (newTVar Nothing))
    let locksArray = listArray (0, n-1) locks
    forM_ [1..n-1] $ \i -> forkIO $ atomically $ do
        mval <- readTVar (locksArray ! (i - 1))
        case mval of
            Nothing -> retry
            Just val -> writeTVar (locksArray ! i) (Just val)
    atomically $ writeTVar (head locks) (Just 3 :: Maybe Int)
    success <- atomically $ do
        mval <- readTVar (locksArray ! (n - 1))
        case mval of
            Nothing -> retry
            Just val -> return (val == 3)
    if success
    then return ()
    else putStrLn "test2 failed"
-}
test3 :: IO ()
test3 = do
    var <- atomically (newTVar 0) :: IO (TVar Int)
    mvar1 <- newEmptyMVar
    mvar2 <- newEmptyMVar
    _ <- forkIO $ atomically (do 
        val <- readTVar var
        unsafeIOToTM $ threadDelay 1000000
        writeTVar var (val + 1)) >> putMVar mvar1 ()
    _ <- forkIO $ threadDelay 500000 >> atomically (do
        val <- readTVar var
        writeTVar var (val + 1)) >> putMVar mvar2 ()
    takeMVar mvar1
    takeMVar mvar2
    ret <- atomically (readTVar var)
    if ret == 2
    then return ()
    else putStrLn "test3 failed"
