{-# Language FlexibleContexts #-}

--------------------------------------------------------------------
-- |
-- Module: HTrade.Test.Utils
--
-- Common functionality and settings used in test cases.

module HTrade.Test.Utils where

import           Control.Applicative             ((<$>))
import           Control.Concurrent              (threadDelay)
import qualified Control.Concurrent.Async        as C
import qualified Control.Error                   as E
import           Control.Monad                   (forever, forM, guard, void, when)
import           Control.Monad.Base              (MonadBase)
import           Control.Monad.Trans             (lift, liftIO, MonadIO)
import           Control.Monad.Trans.Maybe       (runMaybeT)
import qualified Data.Maybe                      as MB
import           Data.Monoid                     ((<>))
import           Data.Word                       (Word16)
import           System.Exit                     (exitSuccess, exitFailure)
import qualified System.Directory                as D
import           System.IO                       (hClose, hPutStr)
import           System.IO.Temp                  (openTempFile)
import qualified Test.QuickCheck                 as Q

import qualified HTrade.Backend.ProxyLayer       as PL
import qualified HTrade.Proxy.Proxy              as P
import qualified HTrade.Backend.Types            as T

-- | Test port used to establish backend services during testing.
testPort :: Word16
testPort = 8888

-- | Number of quickcheck test runs performed by default.
quickcheckTestRuns :: Int
quickcheckTestRuns = 200

-- | Run all the provided test cases and return with success or failure.
checkTestCases
  :: [(String, IO Bool)]
  -> IO ()
checkTestCases tests = do
  success <- and <$> mapM printAndRun tests
  if success then exitSuccess else exitFailure

  where
  printAndRun (desc, test) = printTestCase desc >> test
  printTestCase name = putStrLn $ "[*] Test case: " ++ name

-- | Evaluate a monadic property using QuickCheck with resonable parameters.
withQuickCheck
  :: Q.Testable prop
  => prop
  -> IO Bool
withQuickCheck f = do
  let args = Q.stdArgs { Q.maxSuccess = quickcheckTestRuns }
  res <- Q.quickCheckWithResult args f
  case res of
    Q.Success{} -> return True
    _ -> return False

-- | Execute an action with the given pool size over a cluster.
--   Will setup a backend node, proxies, and verify connectivity,
--   before executing the provided action, and finally terminating.
withLayers
  :: (Monad m, MonadBase IO m, MonadIO m)
  => Int
  -> E.MaybeT (PL.MProxyT m) t
  -> m Bool
withLayers poolSize f = PL.withLayer testPort . fmap MB.isJust . runMaybeT $ do
  -- establish proxy nodes
  pool <- liftIO $ forM [1..poolSize] $
    \_ -> C.async (forever . void . E.runEitherT . E.syncIO $ P.runWorker "127.0.0.1" testPort)
  liftIO . putStrLn $
    "[*] Pool with " ++ show poolSize ++ " proxy nodes established."

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
  liftIO $ putStrLn "[*] Action fully evaluted, shutting down.."

  -- make sure that all nodes are still alive and shutdown
  status <- liftIO $ allAlive pool
  guard $ all MB.isNothing status
  liftIO $ mapM_ C.cancel pool

  liftIO . putStrLn $
    "[*] Pool disconnected."
  where
  allAlive = mapM C.poll

-- | Serialize and write a market configuration to a temporary file.
--   A temporary directory is created containing the chosen filename.
writeTempConf
  :: FilePath
  -> FilePath
  -> T.MarketConfiguration
  -> IO ()
writeTempConf confDir filePattern conf = do
  exists <- D.doesDirectoryExist confDir
  when exists $ D.removeDirectoryRecursive confDir
  D.createDirectory confDir
  (_, handle) <- openTempFile confDir filePattern
  hPutStr handle (showConfiguration conf) >> hClose handle

-- | Serialize a configuration to the configurator-format being used.
showConfiguration
  :: T.MarketConfiguration
  -> String
showConfiguration conf = unlines . map showRow $
  [
    ("name"       , s $ T._marketName $ T._marketIdentifier conf),
    ("currency"   , s $ T._currency $ T._marketIdentifier conf),
    ("host"       , s $ T._marketHost conf),
    ("path"       , s $ T._marketPath conf),
    ("trades"     , s $ T._marketTrades conf),
    ("orderbook"  , s $ T._marketOrders conf),
    ("interval"   , s $ T._marketInterval conf)
  ]
  where
  s :: Show a => a -> String
  s = show
  showRow (key, val) = key <> " = " <> val
