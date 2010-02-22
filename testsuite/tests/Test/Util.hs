module Test.Util where 

import Control.Exception ( finally )
import Database.Redis
import Test.HUnit

-- ---------------------------------------------------------------------------
-- Utilities
-- 

-- | Prints the given string with pretty bars above and below it.
putFancyStr :: String -> IO ()
putFancyStr str = putStr $ "\n" ++ bar ++ str ++ "\n" ++ bar ++ "\n"
    where bar = take (length str) (repeat '-') ++ "\n"

-- | Shortcut for a test case with a Redis connection established with 
-- @withTestConn@.
testWithConn :: String -> (Server -> IO ()) -> Test
testWithConn testDescription testCase = 
    TestLabel testDescription $ TestCase $ withTestConn testCase

-- | Establishes a Redis connection, selects a safe testing database (13 is 
-- assumed safe), then runs some action. On success or failure, the selected 
-- database is cleared after the action is run.
withTestConn :: (Server -> IO ()) -> IO ()
withTestConn action = withRedisConn' $ \r -> do
    select' r >> flush r >> action r `finally` flush r
    where flush r   = do res <- flushdb r
                         assertBool "flush test database" res
          select' r = do res <- select r $ iToParam 13
                         assertBool "select database 13" res

