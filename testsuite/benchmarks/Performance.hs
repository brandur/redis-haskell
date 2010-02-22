module Main where

import Control.Concurrent
import Control.Exception ( finally )
import Data.Time.Clock   ( diffUTCTime, getCurrentTime )
import Database.Redis
import System.IO.Unsafe  ( unsafePerformIO )
import Test.Util
import qualified Data.Text as T

-- ---------------------------------------------------------------------------
-- Main
-- 

main :: IO ()
main = do
    setPerfTest

-- ---------------------------------------------------------------------------
-- Performance
-- 

setPerfTest :: IO ()
setPerfTest = withTestConn $ \_ -> do
    let setJob n = withRedisConn' $ \r -> do
        _ <- select r "13"
        mapM_ (\i -> set r i i) $
            map (T.pack . show . (*n)) (take 200 [(0 :: Int)..])
    t0 <- getCurrentTime
    mapM_ (\n -> forkChild $ setJob n) $ take 200 [(0 :: Int)..]
    waitForChildren
    t1 <- getCurrentTime
    putStrLn $ "40000 sets took " ++ show (diffUTCTime t1 t0)

-- ---------------------------------------------------------------------------
-- Helpers
-- 

children :: MVar [MVar ()]
children = unsafePerformIO (newMVar [])

waitForChildren :: IO ()
waitForChildren = do
    cs <- takeMVar children
    case cs of 
        [] -> return ()
        m:ms -> do
            putMVar children ms
            takeMVar m
            waitForChildren

forkChild :: IO () -> IO ThreadId
forkChild io = do
    mvar <- newEmptyMVar
    childs <- takeMVar children
    putMVar children (mvar:childs)
    forkIO (io `finally` putMVar mvar ())

