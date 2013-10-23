{-# Language
  NoMonomorphismRestriction,
  OverloadedStrings #-}

--------------------------------------------------------------------
-- |
-- Module: HTrade.Proxy.Proxy
--
-- Repeatedly connectes to the backend service.
-- Accepts incoming requests, evaluates the request, and transmits
-- a response to the backend layer within some specified timeout.

module HTrade.Proxy.Proxy where

import           Control.Applicative             ((<$>))
import qualified Control.Exception               as E
import           Control.Monad                   (forever, join, void)
import           Control.Monad.State             (get, lift, modify, runStateT, StateT)
import qualified Data.ByteString.Char8           as B
import           Data.List                       (intersperse)
import           Data.Monoid                     ((<>))
import qualified Data.Monoid.Statistics          as MS
import qualified Data.Monoid.Statistics.Numeric  as MSN
import qualified Data.Set                        as S
import qualified Data.Time                       as T
import           Data.Word                       (Word16)
import qualified Network.Http.Client             as H
import           Network.Socket                  (HostName, Socket)
import           Pipes                           ((>->))
import qualified Pipes                           as P
import qualified Pipes.Prelude                   as P
import qualified Pipes.Binary                    as P
import qualified Pipes.Network.TCP               as P
import           System.Timeout                  (timeout)

import           HTrade.Shared.Types
import           HTrade.Shared.Utils

-- | State maintainted between evaluated queries.
type ProxyStatistics = S.Set Double

-- | Initial empty state configuration.
defaultStats :: ProxyStatistics
defaultStats = S.empty

-- | Monad used by the worker thread.
type MWorker = StateT ProxyStatistics IO

-- | Version of the proxy software running.
proxyVersion :: ProxyVersion
proxyVersion  = (0, 1)

-- | Precision of timer measurements.
timerPrecision :: Int
timerPrecision = 10^(6 :: Int)

-- | GET parameter identifying the latest retrieved trade.
tradeIDParam :: B.ByteString
tradeIDParam = "since"

-- | While not killed by an asynchronous exception, repeatedly connect to backend.
--   Any internal failures (such as lost backend connection) will result in a
--   new connection being established.
run
  :: HostName
  -> Word16
  -> IO ()
run host port = forever $
  blockExceptions $ runWorker host port >> return Nothing

-- | Connect to backend using the provided details and launch pipeline.
runWorker
  :: HostName
  -> Word16
  -> IO ()
runWorker host port = void . P.connect host (show port) $ \(socket, _) -> 
  runStateT (requestThread socket) defaultStats

-- | Initiate pipeline connected to the given socket.
--   This function will either terminate, or throw an exception, on failure.
requestThread
  :: Socket
  -> MWorker ()
requestThread socket = void . P.runEffect $ worker
  where
  worker = 
        readPacket socket          -- read request from backend
    >-> P.mapM handleRequest       -- generate reply packet
    >-> writePacket socket         -- transmit reply to backend

  readPacket sock =
        P.decodeMany (P.fromSocket sock 4096)
    >-> P.map snd

  writePacket sock =
        P.for P.cat P.encode
    >-> P.toSocket sock

-- | Handle request from backend and generate a reply packet.
handleRequest
  :: ProxyRequest
  -> MWorker ProxyResponse
handleRequest (MarketRequest site path trade order timeout' tradeID) = do
  let nonEmpty = filter (/= B.empty)
      prependPath suffix = ("/" <>) . B.concat . intersperse "/" $ nonEmpty [path, suffix]

  market <- lift $ getMarket
    site
    (prependPath trade)
    (prependPath order <> "?" <> tradeIDParam <> "=" <> B.pack (show tradeID))
    timeout'

  case market of
    Nothing -> return $ MarketReply market
    Just m  -> do
      modify $ \s -> S.insert (scaleSample $ _responseTime m) s
      return $ MarketReply market
  where
  scaleSample sample = (realToFrac sample / realToFrac timerPrecision)

handleRequest StatusRequest = buildStatusReply

handleRequest (ReloadRequest performUpgrade reboot) = do
  -- TODO: Implement upgrade and reboot actions
  return AcknowledgementReply

-- | Fetch market data from the given site and path, while respecting the given
--   timeout value. Will return Nothing if the timeout was reached or a
--   non 200-class HTTP response was retrieved, or if some misc connection
--   error occured.
getMarket
  :: B.ByteString
  -> B.ByteString
  -> B.ByteString
  -> MicroSeconds
  -> IO (Maybe MarketReplyDetails)
getMarket site tradePath orderPath allowance = do
  preConnectTime <- T.getCurrentTime
  blockExceptions . checkTimeout . withConn $ \conn -> do
    trades <- retrieve conn tradePath
    orders <- retrieve conn orderPath
    totalTime <- (`T.diffUTCTime` preConnectTime) <$> T.getCurrentTime

    return . Just $ MarketReplyDetails
      (round $ realToFrac timerPrecision * totalTime)
      trades
      orders
  where
  checkTimeout = fmap join . timeout (fromIntegral allowance)

  withConn = E.bracket
    (H.establishConnection site)
    H.closeConnection

  retrieve conn path = do
    req <- H.buildRequest $ do
      H.http H.GET path
      H.setAccept "*/*"

    H.sendRequest conn req H.emptyBody
    H.receiveResponse conn H.concatHandler' 

-- | Build a status reply containing runtime statistics.
buildStatusReply :: MWorker ProxyResponse
buildStatusReply = do
  samples <- get
  let responseMean   = MSN.calcMean   $ (MS.evalStatistic samples :: MSN.Mean)
      responseStddev = MSN.calcStddev $ (MS.evalStatistic samples :: MSN.Variance)

  -- TODO: Implement CPU and mem load
  return $! StatusReply
    proxyVersion 
    0.0
    0.0
    responseMean
    responseStddev
