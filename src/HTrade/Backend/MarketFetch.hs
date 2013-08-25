{-# Language FlexibleContexts #-}

module HTrade.Backend.MarketFetch where

import qualified Control.Concurrent.Async.Lifted as C
import Control.Monad hiding (mapM_)
import Control.Monad.Base
import Control.Monad.Trans
import Control.Monad.Trans.Control
import qualified Control.Monad.Trans.State   as S
import Control.Proxy.Concurrent
import qualified Data.ByteString.Char8       as B
import Data.Foldable (mapM_)
import Prelude hiding (mapM_)

import qualified HTrade.Backend.ProxyLayer   as PL
import HTrade.Backend.Types
import HTrade.Shared.Types
import HTrade.Shared.Utils

data ControlMessage
  = LoadConfiguration MarketConfiguration
  | Shutdown

data MarketState m
  = MarketState
    {
      _cancelWorker :: Maybe (PL.MProxyT m ()),
      _configuration :: Maybe MarketConfiguration
    }

defaultMarketTimeout :: MicroSeconds
defaultMarketTimeout = seconds 5

-- TODO: Check relation between query-timeout and marketreq-timeout

-- | TODO
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

-- | TODO
marketDisconnect
  :: MonadBase IO m
  => MarketConfiguration
  -> PL.MProxyT m ()
marketDisconnect _ = return ()

-- | TODO
-- TODO: Should run an arbitrary monadic action on the details package
--       Could also supply an action which is executed when market-disconnect is detected.
handleReply
  :: MonadBase IO m
  => MarketConfiguration
  -> MarketReplyDetails
  -> PL.MProxyT m ()
handleReply _ _ = return ()

-- | TODO
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
