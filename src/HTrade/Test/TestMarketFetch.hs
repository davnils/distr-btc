{-# LANGUAGE OverloadedStrings #-}

--------------------------------------------------------------------
-- |
-- Module: HTrade.Test.TestMarketFetch
--
-- Functional tests covering the MarketFetch module in the backend
-- but also verfication of the whole backend-proxy construction.
-- Executes a web server and loads a simple configuration, thereby
-- simulating an entire market.

module Main where

import           Control.Applicative             ((<$>))
import qualified Control.Concurrent.Async        as C
import qualified Control.Concurrent.STM          as STM
import qualified Control.Error                   as E
import           Control.Monad.Trans             (liftIO)
import qualified Data.ByteString.Char8           as B
import           Data.Monoid                     ((<>), mempty)
import           GHC.Conc                        (threadDelay)
import qualified Pipes.Concurrent                as P
import qualified Snap.Core                       as SNAP
import qualified Snap.Http.Server                as SNAP
import qualified System.Directory                as D

import           HTrade.Backend.Configuration
import           HTrade.Backend.Types
import           HTrade.Shared.Utils
import           HTrade.Test.Utils

-- | HTTP state carried around in handlers.
data HTTPState
  = HTTPState
  {
     _orderBookCount :: Int,                           -- ^ Number of requests to order book.
     _tradesCount :: Int,                              -- ^ Number of requests to trades.
     _response :: P.Input (B.ByteString, B.ByteString) -- ^ Channel used to read latest orders/trades.
  }

-- | HTTP state bundled in a shared variable.
--   Needed in order to communicate with HTTP route handlers.
type SharedState = STM.TVar HTTPState

-- | HTTP port to listen on.
httpPort :: Int
httpPort = 9999

-- | Market configuration used by all test cases.
testConf :: MarketConfiguration
testConf = MarketConfiguration
      (MarketIdentifier "testMarket" "sek")
      ("http://127.0.0.1:" <> B.pack (show httpPort))
      ""
      "trades.json"
      "orders.json"
      1000000

-- TODO: Add management of C* (separate keyspace, parameterize all existing functions over KS)

-- | Run a monadic action after initializing the snap backend simulating a market.
--   The action can communicate with route handlers through a shared variable.
withSnap
  :: (SharedState -> IO a)
  -> IO a
withSnap action = do
  (_, contentIn) <- P.spawn $ P.Latest (emptyOrderBook, emptyTrades)
  shared <- STM.newTVarIO (HTTPState 0 0 contentIn)
  let routes = SNAP.route [(_marketOrders testConf, orderBookHandler shared),
                           (_marketTrades testConf, tradesHandler shared)]

  -- launch HTTP thread
  httpThread <- C.async $ SNAP.httpServe config routes
  threadDelay 500000

  -- evaluate action
  result <- action shared

  -- cleanup
  C.cancel httpThread
  return result

  where
  config = SNAP.setAccessLog SNAP.ConfigNoLog
         $ SNAP.setErrorLog SNAP.ConfigNoLog
         $ SNAP.setPort httpPort
         $ SNAP.setBind "127.0.0.1" mempty
  emptyOrderBook = "{\"asks\": [], \"bids\": []}"
  emptyTrades = "[]"
  orderBookHandler = genericHandler (\s -> s { _orderBookCount = _orderBookCount s + 1 }) fst
  tradesHandler = genericHandler (\s -> s { _tradesCount = _tradesCount s + 1 }) snd

-- | Generic HTTP handler used by both routes, parameterized over path and relevant actions.
genericHandler
  :: (HTTPState -> HTTPState)
  -> ((B.ByteString, B.ByteString) -> B.ByteString)
  -> SharedState
  -> SNAP.Snap ()
genericHandler onRequest getResponse state = do
  -- pattern matching valid by construction
  Just s <- liftIO . STM.atomically $ do
    STM.modifyTVar' state onRequest
    chan <- _response <$> STM.readTVar state
    P.recv chan
  SNAP.writeBS $ getResponse s

-- | Verify that there are reasonable many HTTP queries to order book and trades.
--   Basically initializes all functionality and inspects counters after a given time slot.
verifyProbes :: IO Bool
verifyProbes  = withSnap $ withLayers poolSize . buildRet . withConfiguration . tester
  where
  poolSize = 10^(1 :: Int) :: Int
  tester state = do
    -- write configuration to disk and load
    liftIO $ writeTempConf confDir filePattern testConf
    loadConfigurations confDir >>= liftIO . print
    liftIO . threadDelay . fromIntegral $ seconds 1

    -- wait for a given time period
    liftIO $ putStrLn "[*] Waiting for samples"
    liftIO . threadDelay $ probes * fromIntegral (_marketInterval testConf)

    liftIO $ putStrLn "[*] Sampling done"
    stats <- liftIO $ STM.readTVarIO state

    -- cleanup
    liftIO $ D.removeDirectoryRecursive confDir

    -- check the number of requests
    let orderCount = _orderBookCount stats
        tradeCount = _tradesCount stats

    liftIO . putStrLn $ "[*] Order book count: " <> show orderCount <>
                        ", trade count: " <> show tradeCount
    return $ (_orderBookCount stats >= probes - 1) && (_tradesCount stats >= probes - 1)

  probes = 10
  confDir = "/tmp/test_configurations/"
  filePattern = "verify-probes-test.conf"
  buildRet = E.MaybeT . fmap convJust
  convJust True = Just ()
  convJust False = Nothing

-- | Entry point executing all tests related to market fetching funtionality.
main :: IO ()
main = checkTestCases [
  ("verifyProbes", verifyProbes)
  ]
