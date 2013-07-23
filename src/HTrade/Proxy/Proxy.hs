{-# Language OverloadedStrings #-}

module Main where

import Control.Applicative ((<$>))
import Control.Error (hush)
import qualified Control.Concurrent.Async    as A
import qualified Control.Exception           as E
import Control.Proxy
import Control.Proxy.Binary
import Control.Proxy.Safe
import qualified Control.Proxy.TCP           as N
import Control.Monad.State
import Data.Binary (Binary)
import qualified Data.ByteString.Char8       as B
import Data.Monoid ((<>))
import qualified Data.Set                    as S
import qualified Data.Time                   as T
import qualified Network.Http.Client         as H
import Network.Socket (HostName, ServiceName, Socket)
import System.Timeout (timeout)

import HTrade.Shared.Types
import HTrade.Shared.Utils

type ProxyStatistics = S.Set ResponseTime
defaultStats :: ProxyStatistics
defaultStats = S.empty

type MWorker = StateT ProxyStatistics IO

run
  :: HostName
  -> ServiceName
  -> IO ()
run host port = N.connect host port $ \(socket, _) -> do
  void $ runStateT (requestThread socket) defaultStats

{- readPacket socket =
      N.socketReadS 4096 socket
  >-> liftP . (mapD Just >-> decodeD)

writePacket
  :: (Binary x, Proxy p)
  => Socket
  -> ()
  -> p () x () B.ByteString MWorker ()
writePacket socket =
      encodeD
  >-> ignoreProxy . runEitherP . N.socketWriteD socket -}

requestThread
  :: Socket
  -> MWorker ()
{- requestThread socket = runProxy workerPipeline
  where
  workerPipeline =
        readPacket socket
    >-> useD handleRequest
    >-> writePacket socket -}

requestThread socket = runProxy workerPipeline
  where
  workerPipeline =
        fromListS [StatusRequest, StatusRequest]
    >-> useD handleRequest
    >-> hoist lift . printD

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
    _       -> return $ MarketReply True market

handleRequest StatusRequest = buildStatusReply

handleRequest (ReloadRequest performUpgrade reboot) = undefined

tryAny :: IO a -> IO (Either SomeException a)
tryAny action = A.withAsync action A.waitCatch

-- | Hoist synchronous exceptions into Maybe and let all other pass.
blockExceptions :: IO (Maybe a) -> IO (Maybe a)
blockExceptions = fmap filterExp . tryAny
  where
  filterExp (Right result) = result
  filterExp (Left e) = case (E.fromException e :: Maybe E.AsyncException) of
    Nothing -> Nothing  -- synchronous exception occured, register.
    Just _ -> E.throw e -- asynchronous exception occured, re-throw.

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

    return . Just $
      MarketReplyDetails (round $ 10^6 * totalTime) trades orders
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

buildStatusReply :: MWorker ProxyResponse
buildStatusReply = undefined

main :: IO ()
main = run defaultHost defaultPort
  where
  defaultHost = "127.0.0.1"
  defaultPort = "1234"
