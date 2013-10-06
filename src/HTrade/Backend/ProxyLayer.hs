{-# Language
  FlexibleContexts,
  FlexibleInstances,
  GeneralizedNewtypeDeriving,
  MultiParamTypeClasses,
  Rank2Types,
  TypeFamilies,
  UndecidableInstances #-}

module HTrade.Backend.ProxyLayer (
  MProxyT,
  withLayer,
  query,
  ready,
  connectedNodes,
  removeNode,
  mapLayer,
  sampleNode
) where

import Data.Monoid ((<>))
import Control.Applicative
import qualified Control.Concurrent.Async    as C
import Control.Concurrent (threadDelay)
import Control.Concurrent.STM
import Control.Monad
import Control.Monad.Base
import Control.Monad.Trans
import Control.Monad.Trans.Control
import qualified Control.Monad.Reader        as R
import qualified Data.Map                    as M
import Data.Maybe (fromMaybe)
import Data.Word (Word16)
import Pipes ((>->))
import qualified Pipes                       as P
import qualified Pipes.Binary                as P
import qualified Pipes.Concurrent            as P
import qualified Pipes.Prelude               as P
import qualified Pipes.Network.TCP           as P hiding (recv, send)
import System.Random (randomRIO)
import System.Timeout (timeout)

import HTrade.Backend.Types
import HTrade.Shared.Types
import HTrade.Shared.Utils

-- | Type of a worker thread stored in the globally shared thread map.
type WorkerThread = P.Output (Maybe (ProxyRequest, WorkerThreadQueryState))

-- | Type of state used in pipeline.
type WorkerThreadQueryState = P.Output (Maybe ProxyResponse)

-- | Internal state used in monad transformer, stores a globally shared map.
type InternalState = TVar (M.Map WorkerIdentifier WorkerThread)

-- | Monad transformer giving access to the proxy layer.
newtype MProxyT m a
 = MProxyT
 {
   runProxyMT :: R.ReaderT InternalState m a
 }
 deriving (
   Applicative,
   Functor,
   Monad,
   MonadIO,
   MonadTrans,
   R.MonadReader InternalState
 )

instance MonadBase m m' => MonadBase m (MProxyT m') where
  liftBase = liftBaseDefault

instance MonadTransControl MProxyT where
  newtype StT MProxyT a = StProxy {unProxy :: StT (R.ReaderT InternalState) a}
  liftWith = defaultLiftWith MProxyT runProxyMT StProxy
  restoreT = defaultRestoreT MProxyT unProxy

instance MonadBaseControl b m => MonadBaseControl b (MProxyT m) where
  newtype StM (MProxyT m) a = StMT {unStMT :: ComposeSt MProxyT m a}
  liftBaseWith = defaultLiftBaseWith StMT
  restoreM     = defaultRestoreM   unStMT

-- | Minimum number of nodes required before allowing any proxy layer queries.
readyLimit :: Int
readyLimit = 1

-- | Default timeout used by proxy layer queries, when not otherwise specified.
defaultTimeout :: MicroSeconds
defaultTimeout = 1000000

-- | Proxy timeout value which defines internal allowance between backend
--   and proxy layer. Any requests surpassing this limit will be redirected
--   to another proxy node or considered unavailable.
proxyTimeout :: MicroSeconds
proxyTimeout = 10000000

-- | Internal delay after backend has been established.
listenDelay :: MicroSeconds
listenDelay = 500000

-- | Maximum number of proxy queries before result is considered unavailable.
maxProbes :: Int
maxProbes = 10

-- | Main entry point when interfacing with the proxy layer.
--   Accepts an computation to be evaluated over the proxy transformer.
--   Initially a thread pool is created and incoming connections are
--   accepted from the proxy layer. Each node is associated with a pipeline
--   which transfers data between queries and the proxy node.
--
--   Typically this function will wrap almost all computation.
withLayer
  :: MonadBase IO m
  => Word16
  -> MProxyT m a
  -> m a
withLayer port routine = do
  threadState <- liftBase $ newTVarIO M.empty
  listenID <- liftBase . C.async $ listener threadState

  liftBase . threadDelay $ fromIntegral listenDelay
  result <- R.runReaderT (runProxyMT routine) threadState

  -- cleanup (every connection is closed through 'serve')
  -- an alternative way is to ~ map (send Nothing) threadState
  liftBase $ C.cancel listenID
  return result

  where
  listener threadState =
    -- start accepting connections
    P.serve P.HostAny (show port) $ \(socket, addr) -> do
      -- serve each proxy node in a separate thread

      let threadID = addr
      (outputTransmit, inputTransmit) <- P.spawn P.Single
      (outputReceive, inputReceive) <- P.spawn P.Single
      atomically $
        modifyTVar' threadState $ M.insert threadID (outputTransmit <> outputReceive)

      threadTransmit <- C.async . tryAny . P.runEffect $ 
            P.fromInput inputTransmit
        >-> terminateD
        >-> P.map fst
        >-> P.for P.cat P.encode
        >-> P.toSocket socket

      threadReceive <- C.async . tryAny . P.runEffect $ 
            P.decodeMany (P.fromSocket socket 4096)
        >-> P.map snd
        >-> saveResponse inputReceive

      _ <- C.waitAnyCatchCancel [void threadTransmit, void threadReceive]

      -- cleanup thread pool upon termination
      atomically $
        modifyTVar' threadState $ M.delete threadID
      P.performGC

saveResponse
  :: MonadIO m
  => P.Input (Maybe (ProxyRequest, WorkerThreadQueryState))
  -> P.Consumer ProxyResponse m b
saveResponse input = forever $ do
  reply <- P.await
  output <- liftM join . liftIO . atomically . P.recv $ input
  case output of
    Nothing -> return ()
    Just (_, output') -> do
      void . liftIO . atomically $ check <$> P.send output' (Just reply)

-- | Execute a query on the proxy layer.
--   If no proxy node is given then a random sample is taken from the pool.
--
--   A non-existant node or an empty pool will result in an error value.
query
  :: MonadBase IO m
  => Maybe WorkerIdentifier
  -> Maybe MicroSeconds
  -> ProxyRequest
  -> MProxyT m (Maybe ProxyResponse)
query addr' timeout' req = do
  let timeout'' = fromMaybe defaultTimeout timeout' -- TODO: USE!
  let getWorker = fromMaybe sampleNode $ return . Just <$> addr'

  -- iterate until a response is retrieved or the limit is reached
  sendProbe getWorker maxProbes

  where
  -- retrieve target proxy node and run query, may fail with Nothing
  sendProbe _ 0 = return Nothing
  sendProbe getWorker limit = do
    result <- getWorker >>= flip onJust runQuery
    case result of
      Nothing    -> sendProbe getWorker (limit - 1)
      res        -> return res

  -- run a query on the given address, may fail with Nothing
  runQuery addr = do
    thread <- R.ask >>= liftBase . atomically . liftM (M.lookup addr) . readTVar
    (localInput, localOutput) <- liftBase $ P.spawn P.Single
    onJust thread $ \remote -> liftBase $ do
      void . atomically $ check <$> P.send remote (Just (req, localInput))
      atomically . liftM join $ P.recv localOutput

-- | Check if the number of connected proxy nodes have reached the critical limit.
--   It is suitable to call this function repeatedely until success during
--   initialization.
ready
  :: MonadBase IO m
  => MProxyT m Bool
ready = fmap (>= readyLimit) connectedNodes

-- | Retrieve the number of connected proxy nodes, based on a snapshot taken internally.
connectedNodes
  :: MonadBase IO m
  => MProxyT m Int
connectedNodes = R.ask >>= fmap M.size . liftBase . readTVarIO

-- | Remove the specified node from the proxy layer.
removeNode
  :: MonadBase IO m
  => WorkerIdentifier
  -> MProxyT m ()
removeNode addr = R.ask >>= \threadState -> liftBase . atomically $ do
  entry <- M.lookup addr <$> readTVar threadState
  modifyTVar' threadState $ M.delete addr
  case entry of
    Just chan -> always $ P.send chan Nothing
    Nothing -> return ()

-- | Maps a function over the set of connected proxies.
--   The proxy identifier might not exist when the function executes since
--   there is a snapshot taken of the set during initialization.
--   This also implies that additional proxies may be added during execution.
mapLayer
  :: MonadBase IO m
  => (WorkerIdentifier -> MProxyT m a)
  -> MProxyT m (M.Map WorkerIdentifier a)
mapLayer f = do
  threadMap <- R.ask >>= liftBase . atomically . readTVar
  let addrs = M.keys threadMap
  mapped <- mapM f addrs
  return . M.fromList $ zip addrs mapped

-- | Retrieve a randomly sampled node from the worker pool.
sampleNode
  :: MonadBase IO m
  => MProxyT m (Maybe WorkerIdentifier)
sampleNode = R.ask >>= liftBase . atomically . readTVar >>= \workers -> do
  let workerSize = M.size workers
  case workerSize of
    0 -> return Nothing
    _ -> do
      rand <- liftBase $ randomRIO (0, workerSize - 1)
      return . Just . fst $ M.elemAt rand workers
