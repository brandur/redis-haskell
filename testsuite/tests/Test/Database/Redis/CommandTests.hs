module Test.Database.Redis.CommandTests (
    commandTests
) where

import Database.Redis
import Test.HUnit
import Test.Util

-- ---------------------------------------------------------------------------
-- Tests
-- 

commandTests :: Test
commandTests = TestLabel "command" $ 
    TestList [ -- * Connection
               testPing, 
               -- * String
               testSet, testGet, testGetset, testMget, testSetnx, testMset, 
               testMsetnx, testIncr, testIncrby, testDecr, testDecrby
             ]

-- ---------------------------------------------------------------------------
-- Connection
-- 

-- @todo: is an AUTH test possible?

testPing :: Test
testPing = testWithConn "ping" $ \r -> do
    res <- ping r
    assertEqual "test that ping returned pong" (RedisString "PONG") res

-- ---------------------------------------------------------------------------
-- String
-- 

testGet :: Test
testGet = testWithConn "get" $ \r -> do
    _ <- set r "mykey" "secret"
    res <- get r "mykey"
    assertEqual "test that get gets set value" (RedisString "secret") res

testSet :: Test
testSet = testWithConn "set" $ \r -> do
    res <- set r "mykey" "secret"
    assertBool "test successful set" res

testGetset :: Test
testGetset = testWithConn "getset" $ \r -> do
    _ <- set r "mykey" "secret"
    res <- getset r "mykey" "new"
    assertEqual "test getset returns old value" (RedisString "secret") res
    res' <- get r "mykey"
    assertEqual "test getset set new value" (RedisString "new") res'

testMget :: Test
testMget = testWithConn "mget" $ \r -> do
    _ <- set r "key1" "val1"
    _ <- set r "key2" "val2"
    _ <- set r "key3" "val3"
    res <- mget r ["key1", "key2", "key3"]
    assertEqual "test mget returns all values" 
                (RedisMulti [ RedisString "val1"
                            , RedisString "val2"
                            , RedisString "val3" ]) 
                res

testSetnx :: Test
testSetnx = testWithConn "setnx" $ \r -> do
    res <- setnx r "mykey" "secret"
    assertBool "test set when key does not exist" res
    res' <- setnx r "mykey" "secret"
    assertBool "test failed set when key already exists" $ not res'

testMset :: Test
testMset = testWithConn "mset" $ \r -> do
    mset r [ ("key1", "val1")
           , ("key2", "val2")
           , ("key3", "val3") ]
    res <- mget r ["key1", "key2", "key3"]
    assertEqual "test mset set all values"
                (RedisMulti [ RedisString "val1"
                            , RedisString "val2"
                            , RedisString "val3" ]) 
                res

testMsetnx :: Test
testMsetnx = testWithConn "mset" $ \r -> do
    res <- msetnx r [ ("key1", "val1")
                    , ("key2", "val2")
                    , ("key3", "val3") ]
    assertBool "test msetnx returned success" res
    res' <- mget r ["key1", "key2", "key3"]
    assertEqual "test msetnx set all values"
                (RedisMulti [ RedisString "val1"
                            , RedisString "val2"
                            , RedisString "val3" ]) 
                res'
    res'' <- msetnx r [ ("key3", "val3") 
                      , ("key4", "val4") ]
    assertBool "test msetnx failed when at least one key exists" $ not res''
    res''' <- exists r "key4"
    assertBool "test msetnx sets no keys on failure" $ not res'''

testIncr :: Test
testIncr = testWithConn "incr" $ \r -> do
    res <- incr r "counter"
    assertEqual "test incr sets key that didn't exist" (RedisInteger 1) res
    res' <- incr r "counter"
    assertEqual "test increment" (RedisInteger 2) res'

testIncrby :: Test
testIncrby = testWithConn "incrby" $ \r -> do
    res <- incrby r "counter" (iToParam 7)
    assertEqual "test incrby sets key that didn't exist" (RedisInteger 7) res
    res' <- incrby r "counter" (iToParam 2)
    assertEqual "test increment by" (RedisInteger 9) res'

testDecr :: Test
testDecr = testWithConn "decr" $ \r -> do
    _ <- set r "counter" (iToParam 10)
    res <- decr r "counter"
    assertEqual "test decrement" (RedisInteger 9) res

testDecrby :: Test
testDecrby = testWithConn "decrby" $ \r -> do
    _ <- set r "counter" (iToParam 10)
    res <- decrby r "counter" (iToParam 7)
    assertEqual "test decrement by" (RedisInteger 3) res

