module HTrade.Test.Utils where

import Control.Applicative ((<$>))
import Data.Word (Word16)
import System.Exit (exitSuccess, exitFailure)

-- | Test port used to establish backend services during testing.
testPort :: Word16
testPort = 8888

-- | Run all the provided test cases and return with success or failure.
checkTestCases :: [(String, IO Bool)] -> IO ()
checkTestCases tests = do
  success <- and <$> mapM printAndRun tests
  if success then exitSuccess else exitFailure

  where
  printAndRun (desc, test) = printTestCase desc >> test
  printTestCase name = putStrLn $ "[*] Test case: " ++ name
