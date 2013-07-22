module Main where

import Control.Proxy
import Control.Proxy.TCP
import Control.Monad.State

import HTrade.Shared.Types
import HTrade.Shared.Utils

type ProxyStatistics = (S.Set ResponseTime)
defaultStats :: ProxyStatistics
defaultStats = S.empty

type MWorker = StateT ProxStatistics IO

defaultHost = "127.0.0.1"
defaultPort = "1234"

setupConnection
  :: B.ByteString
  -> B.ByteString
  -> IO ()
setupConnection = connect defaultHost defaultPort $ \(socket, _) -> do
  void $ runStateT (requestThread socket) defaultStats

requestThread
  :: N.Socket
  -> MWorker ()
requestThread socket = runProxy workerPipeline
  where
  workerPipeline =
        readPacket socket
    >-> handleRequest
    >-> writePacket socket

handleRequest
  :: ProxyRequest
  -> MWorker ProxyResponse
handleRequest (MarketRequest site path trade order timeout)
  = lift $ getMarket (site <> "/" <> path) trade order timeout
handleRequest StatusRequest
  = buildStatusReply
handleRequest ReloadRequest (performUpgrade, reboot)
  = undefined

getMarket
  :: B.ByteString
  -> B.ByteString
  -> B.ByteString
  -> MarketTimeout
  -> IO (Maybe MarketReplyDetails)
getMarket path tradePath orderPath = undefined

main :: IO ()
main = putStrLn "This is the proxy module running"
