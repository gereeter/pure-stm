{-# LANGUAGE DoAndIfThenElse #-}

import Data.Array.IArray
import Control.Monad.TM
import Control.Monad.TM.Unsafe
import Control.Concurrent
import Control.Monad

-- TODO: Use a real testing framework

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

test2 :: Int -> IO ()
test2 n = do
    locks <- replicateM n (atomically (newTVar Nothing))
    let locksArray = listArray (0, n-1) locks :: Array Int (TVar (Maybe Int))
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

main = test1 >> test2 10000 >> test3
