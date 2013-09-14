{-# Language FlexibleContexts #-}

module HTrade.Backend.MarketFetch where

import qualified Control.Concurrent.Async.Lifted as C
import Control.Monad hiding (mapM_)
import Control.Monad.Base
import Control.Monad.Trans
import Control.Monad.Trans.Control
import qualified Control.Monad.Trans.State       as S
import Control.Proxy.Concurrent
import Data.Foldable (mapM_)
import Prelude hiding (mapM_)

import qualified HTrade.Backend.ProxyLayer       as PL
import HTrade.Backend.Types
import HTrade.Shared.Types
import HTrade.Shared.Utils

-- | Control message sent to a market channel by some external entity.
data ControlMessage
  = LoadConfiguration MarketConfiguration
  | Shutdown

-- | Internal state used by market workers in order to keep track of
--   the thread and configuration, if yet loaded.
data MarketState m
  = MarketState
    {
      _cancelWorker :: Maybe (PL.MProxyT m ()),
      _configuration :: Maybe MarketConfiguration
    }

-- | Default market timeout used to define when a market is unreachable. 
defaultMarketTimeout :: MicroSeconds
defaultMarketTimeout = seconds 5

-- TODO: Check relation between query-timeout and marketreq-timeout

-- | Worker function executed in a separate thread.
--   Takes as input a configuration and will repeatedly poll the market
--   with the provided settings, and handle the possible outcomes.
worker
  :: MonadBase IO m
  => MarketConfiguration
  -> PL.MProxyT m ()
worker conf = forever $ do
  res <- PL.query Nothing (Just defaultMarketTimeout) marketReq
  case res of
    Nothing -> delay $ seconds 1
    Just reply -> parseReply reply >> delay (_marketInterval conf)

  where
  marketReq = MarketRequest
        (_marketHost conf)
        (_marketPath conf)
        (_marketTrades conf)
        (_marketOrders conf)
        defaultMarketTimeout

  parseReply (MarketReply Nothing)      = marketDisconnect conf
  parseReply (MarketReply (Just reply)) = handleReply conf reply
  parseReply _      = return () -- TODO: Investigate type-level reply difference

-- | Function executed when a market disconnect has been detected.
marketDisconnect
  :: MonadBase IO m
  => MarketConfiguration
  -> PL.MProxyT m ()
marketDisconnect _ = liftBase $ putStrLn "[MarketFetch] market considered disconnected"

-- | Function executed when a market reply has been received.
--
--   TODO: Should run an arbitrary monadic action on the details package
--         Could also supply an action which is executed when market-disconnect is detected.
handleReply
  :: MonadBase IO m
  => MarketConfiguration
  -> MarketReplyDetails
  -> PL.MProxyT m ()
handleReply market reply = liftBase $ putStrLn
  "--------------------\n[MarketFetch] Received reply" >> print market >> print reply

-- | Separate thread which corresponds to a single market.
--   Handles updated configurations and other external requests.
marketThread
  :: (MonadBase IO m, MonadBaseControl IO m)
  => Output ControlMessage
  -> PL.MProxyT m ()
marketThread messageQueue = void $
  S.runStateT threadLoop $ MarketState Nothing Nothing
  where
  threadLoop = liftBase (atomically $ recv messageQueue) >>= mapM_ handleMessage

  handleMessage (LoadConfiguration conf) = do
    terminateWorker
    thread <- lift . C.async $ worker conf
    S.put $ MarketState (Just $ C.cancel thread) (Just conf)
    threadLoop

  handleMessage Shutdown = terminateWorker
 
  terminateWorker = S.get >>= mapM_ lift . _cancelWorker
