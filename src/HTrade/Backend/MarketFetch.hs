{-# Language GADTs #-}

module HTrade.Backend.MarketFetch where

import qualified Control.Concurrent.Async.Lifted as C
import Control.Monad
import Control.Monad.Base
import Control.Monad.Trans
import qualified Control.Monad.Trans.State   as S
import Control.Proxy.Concurrent
import qualified Data.ByteString.Char8       as B

import qualified HTrade.Backend.ProxyLayer   as PL
import HTrade.Backend.Types
import HTrade.Shared.Types
import HTrade.Shared.Utils

data ControlMessage
  = LoadConfiguration MarketConfiguration
  | Shutdown

data MarketState mt where
  MarketState
    ::
    {
      _threadID :: Maybe (PL.MProxy ()),
      _configuration :: Maybe MarketConfiguration
    }
    -> MarketState mt

defaultMarketTimeout :: MicroSeconds
defaultMarketTimeout = seconds 5

-- TODO: Check relation between query-timeout and marketreq-timeout

-- | TODO
worker
  :: MarketConfiguration
  -> PL.MProxy ()
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
  :: MarketConfiguration
  -> PL.MProxy ()
marketDisconnect _ = return ()

-- | TODO
-- TODO: Should run an arbitrary monadic action on the details package
--       Could also supply an action which is executed when market-disconnect is detected.
handleReply
  :: MarketConfiguration
  -> MarketReplyDetails
  -> PL.MProxy ()
handleReply _ _ = return ()

-- | TODO
marketThread
  :: Output ControlMessage
  -> PL.MProxy ()
marketThread messageQueue = void $
  S.runStateT threadLoop $ MarketState Nothing Nothing
  where
  threadLoop = do
    msg <- liftBase . atomically $ recv messageQueue
    case msg of
      Nothing -> return ()
      Just msg' -> handleMessage msg'

  handleMessage (LoadConfiguration conf) = do
    terminateWorker
    thread <- lift . C.async $ worker conf
    S.put $ MarketState (Just $ C.cancel thread) (Just conf)
    threadLoop

  handleMessage Shutdown = terminateWorker
 
  terminateWorker = do
    (MarketState thread _) <- S.get
    case thread of
      Nothing -> return ()
      Just t  -> lift t
