module Main where 

import Test.Database.Redis.CommandTests
import Test.HUnit  ( Counts(..), Test(..), runTestTT )
import Test.Util
import System.Exit ( exitFailure, exitSuccess )

-- ---------------------------------------------------------------------------
-- All Tests
-- 

tests :: Test
tests = TestList [ commandTests ]

-- ---------------------------------------------------------------------------
-- Main
-- 

main :: IO ()
main = do
    putFancyStr "Running redis-haskell test suite ..."
    Counts { errors = e, failures = f } <- runTestTT tests
    if f < 1 && e < 1 then putFancyStr "Test suite successful!" >> exitSuccess
                      else putFancyStr "Test suite FAILED!"     >> exitFailure

