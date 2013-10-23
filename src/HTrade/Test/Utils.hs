--------------------------------------------------------------------
-- |
-- Module: HTrade.Test.Utils
--
-- Common functionality and settings used in test cases.

module HTrade.Test.Utils where

import           Control.Applicative             ((<$>))
import           Control.Concurrent              (threadDelay)
import qualified Control.Concurrent.Async        as C
import           Control.Monad                   (forM, guard, when)
import           Control.Monad.Trans             (lift, liftIO)
import           Control.Monad.Trans.Maybe       (runMaybeT)
import qualified Data.Maybe                      as MB
import           Data.Word                       (Word16)
import           System.Exit                     (exitSuccess, exitFailure)
import qualified Test.QuickCheck                 as Q

import qualified HTrade.Backend.ProxyLayer       as PL
import qualified HTrade.Proxy.Proxy              as P

-- | Test port used to establish backend services during testing.
testPort :: Word16
testPort = 8888

-- | Number of quickcheck test runs performed by default.
quickcheckTestRuns :: Int
quickcheckTestRuns = 200

-- | Run all the provided test cases and return with success or failure.
checkTestCases :: [(String, IO Bool)] -> IO ()
checkTestCases tests = do
  success <- and <$> mapM printAndRun tests
  if success then exitSuccess else exitFailure

  where
  printAndRun (desc, test) = printTestCase desc >> test
  printTestCase name = putStrLn $ "[*] Test case: " ++ name

-- | Evaluate a monadic property using QuickCheck with resonable parameters.
withQuickCheck :: Q.Testable prop => prop -> IO Bool
withQuickCheck f = do
  let args = Q.stdArgs { Q.maxSuccess = quickcheckTestRuns }
  res <- Q.quickCheckWithResult args f
  case res of
    Q.Success{} -> return True
    _ -> return False

-- | Execute an action with the given pool size over a cluster.
--   Will setup a backend node, proxies, and verify connectivity,
--   before executing the provided action, and finally terminating.
withLayers poolSize f = PL.withLayer testPort . fmap MB.isJust . runMaybeT $ do
  -- establish proxy nodes
  pool <- liftIO $ forM [1..poolSize] $
    \_ -> C.async (P.runWorker "127.0.0.1" testPort)
  liftIO . putStrLn $
    "[*] Pool with " ++ show testPort ++ " proxy nodes established."

  -- wait for full connectivity
  let waitLoop = do
      nodes <- lift PL.connectedNodes
      when (nodes /= poolSize) $ liftIO (threadDelay $ 500 * 10^(3 :: Int)) >> waitLoop

  waitLoop
  lift PL.ready >>= guard

  liftIO . putStrLn $
    "[*] Pool considered fully connected."

  -- evaluate the given action
  _ <- f

  -- make sure that all nodes are still alive and shutdown
  status <- liftIO $ allAlive pool
  guard $ all MB.isNothing status
  liftIO $ mapM_ C.cancel pool

  liftIO . putStrLn $
    "[*] Pool disconnected."
  where
  allAlive = mapM C.poll
