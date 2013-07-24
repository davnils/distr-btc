{-# Language NoMonomorphismRestriction, OverloadedStrings #-}

module Main where

import Control.Applicative ((<$>))
import Control.Error (hush)
import qualified Control.Exception           as E
import Control.Proxy
import Control.Proxy.Binary
import Control.Proxy.Safe
import Control.Proxy.Trans.Either (runEitherK)
import Control.Proxy.Trans.State (runStateK)
import qualified Control.Proxy.TCP           as N
import Control.Monad.State
import Data.Binary (Binary)
import qualified Data.ByteString.Char8       as B
import Data.Monoid ((<>))
import qualified Data.Monoid.Statistics      as MS
import qualified Data.Monoid.Statistics.Numeric         as MSN
import qualified Data.Set                    as S
import qualified Data.Time                   as T
import qualified Network.Http.Client         as H
import Network.Socket (HostName, ServiceName, Socket)
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
timerPrecision = 10^6

-- | While not killed by an asynchronous exception, repeatedly connect to backend.
--   Any internal failures (such as lost backend connection) will result in a
--   new connection being established.
run
  :: HostName
  -> ServiceName
  -> IO ()
run host port = forever $
  blockExceptions $ runWorker host port >> return Nothing

-- | Connect to backend using the provided details and launch pipeline.
runWorker
  :: HostName
  -> ServiceName
  -> IO ()
runWorker host port = N.connect host port $ \(socket, _) -> do
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
    >-> liftP . useD handleRequest              -- generate reply packet
    >-> liftP . hoist lift . writePacket socket -- transmit reply to backend

  readPacket socket =
        N.socketReadS 4096 socket
    >-> mapD Just
    >-> decodeD

  writePacket socket =
        encodeD
    >-> N.socketWriteD socket

-- | Handle request from backend and generate a reply packet.
handleRequest
  :: ProxyRequest
  -> MWorker ProxyResponse

handleRequest (MarketRequest site path trade order timeout) = do
  let prependPath = (<> "/" <> path)
  market <- lift $ getMarket
    site
    (prependPath trade)
    (prependPath order)
    timeout

  case market of
    Nothing -> return $ MarketReply False market
    Just m  -> do
      modify $ \s -> S.insert (scaleSample $ _response_time m) s
      return $ MarketReply True market
  where
  scaleSample sample = (realToFrac sample / realToFrac timerPrecision)

handleRequest StatusRequest = buildStatusReply

-- TODO: Implement and add acknowledgement packet, can be generic "ReplyAcknowledged".
handleRequest (ReloadRequest performUpgrade reboot) = undefined

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
  checkTimeout = fmap join . timeout allowance

  withConn = E.bracket
    (H.establishConnection site)
    H.closeConnection

  retrieve conn path = do
    req <- H.buildRequest $ do
      H.http H.GET (site <> path)
      H.setAccept "text/html"

    H.sendRequest conn req H.emptyBody
    H.receiveResponse conn H.concatHandler' 

-- TODO: Implement CPU and mem load
-- | Build a status reply containing runtime statistics.
buildStatusReply :: MWorker ProxyResponse
buildStatusReply = do
  samples <- get
  let responseMean   = MSN.calcMean   $ (MS.evalStatistic samples :: MSN.Mean)
      responseStddev = MSN.calcStddev $ (MS.evalStatistic samples :: MSN.Variance)

  return $ StatusReply
    proxyVersion 
    undefined
    undefined
    responseMean
    responseStddev

main :: IO ()
main = run defaultHost defaultPort
  where
  defaultHost = "127.0.0.1"
  defaultPort = "1234"
