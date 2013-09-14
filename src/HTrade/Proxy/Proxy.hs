{-# Language NoMonomorphismRestriction, OverloadedStrings #-}

module HTrade.Proxy.Proxy where

import Control.Applicative ((<$>))
import qualified Control.Exception           as E
import Control.Proxy
import Control.Proxy.Binary
import Control.Proxy.Safe
import Control.Proxy.Trans.State (runStateK)
import qualified Control.Proxy.TCP           as N
import Control.Monad.State
import qualified Data.ByteString.Char8       as B
import Data.Monoid ((<>))
import qualified Data.Monoid.Statistics      as MS
import qualified Data.Monoid.Statistics.Numeric         as MSN
import qualified Data.Set                    as S
import qualified Data.Time                   as T
import Data.Word (Word16)
import qualified Network.Http.Client         as H
import Network.Socket (HostName, Socket)
import System.Timeout (timeout)

import HTrade.Shared.Types
import HTrade.Shared.Utils

type ProxyStatistics = S.Set Double
defaultStats :: ProxyStatistics
defaultStats = S.empty

type MWorker = StateT ProxyStatistics IO

proxyVersion :: ProxyVersion
proxyVersion  = (0, 1)

timerPrecision :: Int
timerPrecision = 10^(6 :: Int)

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
runWorker host port = N.connect host (show port) $ \(socket, _) -> do
  void $ runStateT (requestThread socket) defaultStats

-- | Initiate pipeline connected to the given socket.
--   This function will either terminate, or throw an exception, on failure.
requestThread
  :: Socket
  -> MWorker ()
requestThread socket = void . runProxy . runStateK [] . runEitherK $ worker
  where
  worker = 
       hoist lift . readPacket socket          -- read request from backend
    >-> liftP . hoist lift . execD (putStrLn "[proxy] readPacket done")
    >-> liftP . mapMD handleRequest             -- generate reply packet
    >-> liftP . hoist lift . writePacket socket -- transmit reply to backend

  -- TODO: Verify if this implementation of readPacket actually is correct
  --       Might be the root cause behind limited packet propagation
  readPacket sock =
        N.socketReadS 4096 sock
    >-> execD (putStrLn "[proxy] socketReadS returned")
    >-> useD (\cont -> putStrLn $ "[proxy] socketReadS resulted in: " <> (show $ B.length cont))
    >-> mapD Just
    >-> decodeD

  writePacket sock =
        encodeD
    >-> N.socketWriteD sock

-- | Handle request from backend and generate a reply packet.
handleRequest
  :: ProxyRequest
  -> MWorker ProxyResponse

handleRequest (MarketRequest site path trade order timeout') = do
  let prependPath = (<> "/" <> path)
  market <- lift $ getMarket
    site
    (prependPath trade)
    (prependPath order)
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
  putStrLn "[proxy] runnning getMarket upon request"
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
      H.http H.GET (site <> path)
      H.setAccept "text/html"

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
