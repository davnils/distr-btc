{-# Language ConstraintKinds, RankNTypes, TypeFamilies #-}

module ProxyLayer where

import qualified Data.ByteString.Char8       as B
import Control.Applicative
import qualified Control.Concurrent.Async    as C
import Control.Concurrent (threadDelay)
import Control.Concurrent.STM
import Control.Monad
import Control.Monad.Trans
import Control.Proxy
import Control.Proxy.Trans.State
import Control.Proxy.Binary
import Control.Proxy.Concurrent
import Control.Proxy.Safe
import qualified Control.Proxy.TCP           as N
import qualified Control.Monad.Reader        as R
import qualified Data.Map                    as M
import qualified Data.Set                    as S
import Data.Binary (Binary)
import Data.Maybe (fromMaybe)
import Data.Word (Word16)
import Network.Socket (Socket)
import Network.Socket.Internal (SockAddr(..))
import System.Random (randomIO)

import Types
import Utils

readyLimit :: Int
readyLimit = 1

defaultTimeout :: MarketTimeout
defaultTimeout = 1000000

retryDelay :: MarketTimeout
retryDelay = 1000

proxyTimeout :: Int
proxyTimeout = 300000

withLayer
  :: MProxyT mt mb
  => mt a
  -> mb a
withLayer routine = do
  threadState <- liftIO $ newTVarIO M.empty
  liftIO . C.async $ listener threadState
  R.runReaderT routine threadState

  -- kill all the threads(?) can also message Nothing to all pipes and wait

  where
  listener threadState =
    -- start accepting connections
    N.serve N.HostAny "1111" $ \(socket, addr) -> do
      -- serve each proxy node in a separate thread
      (localInput, localOutput) <- liftIO $ spawn Unbounded
      atomically $ do
        modifyTVar threadState $ M.insert (addr, 0) localInput

      -- handle requests until completion
      -- return $ runProxy $ runEitherK $ runEitherK $ evalStateK [] $
       -- liftP . liftP (recvS localOutput >-> terminateD >-> mapD fst >-> writePacket socket)
        -- >-> readPacket
        -- >-> buildResponse

        -- needs to split pipeline into two separate parts:
        -- (1) transmit chain
        -- (2) recieve chain
        -- these are also enabled by a separate runProxy call
        -- sort of like :
        -- (writeSocket, data) <- recv
        -- runProxy $ 
        -- runProxy $

      return undefined

query
  :: MProxyT mt mb
  => Maybe WorkerIdentifier
  -> Maybe MarketTimeout
  -> ProxyRequest
  -> mt ProxyResponse
query addr' timeout' req = do
  let timeout = fromMaybe defaultTimeout timeout'
  let getWorker = fromMaybe sampleNode $ return . Just <$> addr'

  -- iterate until a response is retrieved
  result <- forM [1..] $ \_ -> do
    worker <- getWorker
    case worker of
      Nothing -> liftIO (threadDelay retryDelay) >> return Nothing
      Just addr -> runQuery addr

  return $ fromMaybe (error "Inconsistent pool") (msum result)

  where
  runQuery :: MProxyT mt mb => WorkerIdentifier -> mt (Maybe ProxyResponse)
  runQuery addr = do
    thread <- R.ask >>= liftIO . atomically . liftM (M.lookup addr) . readTVar
    (localInput, localOutput) <- liftIO $ spawn Unbounded
    onJust thread $ \remote -> liftIO . atomically $ do
        send remote $ Just (req, localInput)
        liftM join $ recv localOutput

ready
  :: MProxyT mt mb
  => mt Bool
ready = R.ask >>= \threadState -> liftIO . atomically $
  (> readyLimit) . M.size <$> readTVar threadState

removeNode
  :: MProxyT mt mb
  => WorkerIdentifier
  -> mt ()
removeNode addr = R.ask >>= \threadState -> liftIO . atomically $ do
  entry <- M.lookup addr <$> readTVar threadState
  modifyTVar threadState $ M.delete addr
  case entry of
    Just chan -> void $ send chan Nothing
    Nothing -> return ()

-- | Maps a function over the set of connected proxies.
--   The proxy identifier might not exist when the function executes since
--   there is snapshot taken of the set in the beginning.
--   This also implies that additional proxies may be added during execution.
mapLayer
  :: MProxyT mt mb
  => (WorkerIdentifier -> mt a)
  -> mt (M.Map WorkerIdentifier a)
mapLayer f = do
  threadMap <- R.ask >>= liftIO . atomically . readTVar
  let addrs = M.keys threadMap
  mapped <- mapM f addrs
  return . M.fromList $ zip addrs mapped

sampleNode
  :: MProxyT mt mb
  => mt (Maybe WorkerIdentifier)
sampleNode = R.ask >>= liftIO . atomically . readTVar >>= \workers -> do
  rand <- liftIO randomIO
  let workerSize = M.size workers
  return $ case workerSize of
    0 -> Nothing
    _ -> Just . fst $ M.elemAt (rand `mod` workerSize) workers
