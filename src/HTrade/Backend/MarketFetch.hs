{-# Language
  DataKinds,
  FlexibleContexts,
  OverloadedStrings
  #-}

module HTrade.Backend.MarketFetch where

import qualified Control.Concurrent.Async.Lifted as C
import qualified Control.Error                   as E
import Control.Monad hiding (mapM_)
import Control.Monad.Base
import Control.Monad.Trans
import Control.Monad.Trans.Control
import qualified Control.Monad.Trans.State       as S
import qualified Data.Aeson                      as A
import qualified Data.ByteString.Lazy.Char8      as BL
import Data.Foldable (mapM_)
import Data.Int (Int64)
import Data.List (partition)
import Data.Maybe (fromMaybe)
import Data.Monoid ((<>))
import Data.Time.Clock (getCurrentTime, utctDay, utctDayTime)
import Data.UUID.V4 (nextRandom)
import qualified Database.Cassandra.CQL          as DB
import qualified Pipes.Concurrent                as P
import Prelude hiding (mapM_)

import qualified HTrade.Backend.ProxyLayer       as PL
import qualified HTrade.Backend.Storage          as DB
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

-- | Number of trades stored clustered on a single row in Cassandra.
tradeGroupSize :: Int64
tradeGroupSize = 10000

-- TODO: Check relation between query-timeout and marketreq-timeout

-- | Worker function executed in a separate thread.
--   Takes as input a configuration and will repeatedly poll the market
--   with the provided settings, and handle the possible outcomes.
worker
  :: MonadBase IO m
  => MarketConfiguration
  -> DB.Pool
  -> PL.MProxyT m ()
worker conf pool = forever $ do
  lastTrade <- fmap defaultZero . liftBase . DB.runCas pool $
    DB.executeRow DB.QUORUM lastTradeQuery market
  res <- PL.query Nothing (Just defaultMarketTimeout) (marketReq lastTrade)
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
  market = _marketName $ _marketIdentifier conf
  lastTradeQuery = DB.query "select trade from market_last_trade where market=?"
  defaultZero = fromMaybe (0 :: TradeID)

  -- TODO: Investigate type-level reply difference
  parseReply (MarketReply Nothing)      = marketDisconnect conf
  parseReply (MarketReply (Just reply)) = handleReply pool conf reply
  parseReply _      = return ()

-- | Function executed when a market disconnect has been detected.
marketDisconnect
  :: MonadBase IO m
  => MarketConfiguration
  -> PL.MProxyT m ()
marketDisconnect _ = liftBase $ putStrLn "[MarketFetch] market considered disconnected"

-- | Function executed when a market reply has been received.
handleReply
  :: MonadBase IO m
  => DB.Pool
  -> MarketConfiguration
  -> MarketReplyDetails
  -> PL.MProxyT m ()
handleReply pool market reply = liftBase . (>>= checkError) . E.runEitherT $ do
  uuid <- lift nextRandom
  now <- lift getCurrentTime

  -- Parse order book and trades from the provided JSON format.
  parsedOrders <- decodeItem "Failed to parse orderbook" $ _orderBook reply
  parsedTrades<- decodeItem "Failed to parse trades" $ _trades reply

  -- Raw payload and order book insertion.
  let rawInsert = (
        uuid,
        _marketName $ _marketIdentifier market,
        now,
        DB.Blob $ _orderBook reply,
        DB.Blob $ _trades reply,
        fromIntegral (_responseTime reply) :: Int)

      orderInsert = (
        _marketName $ _marketIdentifier market,
        now { utctDayTime = 0 },
        now,
        _asks parsedOrders,
        _bids parsedOrders
        )

  -- Run all Cassandra actions, including inserting all new trades.
  E.fmapLT show . E.syncIO . DB.runCas pool $ do
    write rawQuery rawInsert
    write orderQuery orderInsert 
    processTrades (_marketIdentifier market) parsedTrades

  where
  rawQuery = "insert into " <> DB.marketRawTable <>
             " (id, market, retrieved, orderbook, trades, elapsed) values (?, ?, ?, ?, ?, ?)"

  orderQuery = "insert into " <> DB.marketOrderBookTable <>
               " (market, day, retrieved, asks, bids) values (?, ?, ?, ?, ?)"

  write query = DB.executeWrite DB.QUORUM (DB.query query)

  decodeItem msg = E.noteT msg . E.hoistMaybe . A.decode' . BL.fromStrict

  checkError (Left err) = print err
  checkError _ = return ()

processTrades
  :: DB.MonadCassandra m
  => MarketIdentifier
  -> Trades
  -> m ()
processTrades _ (Trades []) = return ()
processTrades market (Trades trades) = do
  -- Extract and partition w.r.t day of first trade
  let getDay = utctDay . _time
      processDay = getDay $ head trades
      processDayZero = (_time $ head trades) { utctDayTime = 0 }
      (current, future) = partition (\t -> getDay t == processDay) trades
      lastTrade = last trades

  -- Fetch and calculate new (first, last) trade identifiers for the day
  res <- DB.executeRow DB.QUORUM (DB.query firstLastFetchQuery) (_marketName market, processDayZero)
  let first' = fromMaybe (_transactionID $ head trades) res
      last'  = _transactionID lastTrade

  _ <- DB.executeWrite DB.QUORUM (DB.query updateFirstLastQuery)
    (_marketName market, processDayZero, first', last')

  -- Write all of the trades at the lowest consistency level
  forM_ current $ \t -> do
    let group     = mod (_transactionID t) tradeGroupSize
        insertion = (_marketName market, group, _transactionID t, _time t, _price t, _amount t)
    DB.executeWrite DB.ONE (DB.query tradeInsertQuery) insertion

  -- Update ID of latest trade
  _ <- DB.executeWrite DB.QUORUM (DB.query updateLastTradeQuery)
    (_marketName market, last', _time lastTrade)

  -- Process all remaining days
  processTrades market (Trades future)

  where
  firstLastFetchQuery  = "select first from " <> DB.marketTradesDaySchema <> " where market=? and day=?"
  updateFirstLastQuery = "insert into "       <> DB.marketTradesDaySchema <> " values (?, ?, ?, ?)"
  updateLastTradeQuery = "insert into "       <> DB.marketLastTradeTable  <> " values (?, ?, ?)"
  tradeInsertQuery     = "insert into "       <> DB.marketTradesTable     <> " values (?, ?, ?, ?, ?, ?)"

-- | Separate thread which corresponds to a single market.
--   Handles updated configurations and other external requests.
marketThread
  :: (MonadBase IO m, MonadBaseControl IO m)
  => P.Input ControlMessage
  -> PL.MProxyT m ()
marketThread messageQueue = do
  pool <- liftBase $ DB.newPool
    [(DB.cassandraHost, DB.cassandraPort)]
    DB.marketKeyspace

  void $ S.evalStateT (threadLoop pool) $ MarketState Nothing Nothing
  where
  threadLoop pool =
    liftBase (P.atomically $ P.recv messageQueue) >>= mapM_ (handleMessage pool)

  handleMessage pool (LoadConfiguration conf) = do
    terminateWorker
    thread <- lift . C.async $ worker conf pool
    S.put $ MarketState (Just $ C.cancel thread) (Just conf)
    threadLoop pool

  handleMessage _ Shutdown = terminateWorker
 
  terminateWorker = S.get >>= mapM_ lift . _cancelWorker
