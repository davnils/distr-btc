{-# Language FlexibleContexts, FlexibleInstances, GeneralizedNewtypeDeriving, MultiParamTypeClasses, Rank2Types, TypeFamilies, UndecidableInstances #-}

-- TODO: Check necessity of language pragmas

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

import qualified Data.Binary                 as BI
import qualified Data.ByteString.Char8       as B
import qualified Data.ByteString.Lazy.Char8  as BL
import Control.Applicative
import qualified Control.Concurrent.Async    as C
import Control.Concurrent (threadDelay)
import Control.Concurrent.STM
import Control.Monad
import Control.Monad.Base
import Control.Monad.Trans
import Control.Monad.Trans.Control
import qualified Control.Monad.Reader        as R
import qualified Control.Monad.State         as S
import qualified Data.Binary as DB --TODO: needed?
import qualified Data.Map                    as M
import Data.Binary (Binary)
import Data.Maybe (fromMaybe)
import Data.Word (Word16)
import qualified Network.Simple.TCP          as NS
import Network.Socket (Socket)
import Pipes ((>->))
import qualified Pipes                       as P
import qualified Pipes.Binary                as P
import qualified Pipes.Concurrent            as P
import qualified Pipes.Prelude               as P
import qualified Pipes.Network.TCP           as N
import System.Random (randomIO)
import System.Timeout (timeout)

import HTrade.Backend.Types
import HTrade.Shared.Types
import HTrade.Shared.Utils

-- | Type of a worker thread stored in the globally shared thread map.
type WorkerThread = P.Input (Maybe (ProxyRequest, WorkerThreadQueryState))

-- | Type of state used in pipeline.
type WorkerThreadQueryState = P.Input (Maybe ProxyResponse)

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
    N.serve N.HostAny (show port) $ \(socket, addr) -> do
      -- serve each proxy node in a separate thread

      liftBase $ putStrLn "accepted new proxy connection"

      let threadID = addr
      (localInput, localOutput) <- P.spawn P.Single
      atomically $
        modifyTVar' threadState $ M.insert threadID localInput

      -- execute pipeline which:
      -- (1) reads a local request
      -- (2) submits the corresponding request packet
      -- (2) retrieves the result and notifies the original query

      void . tryAny . P.runEffect $ 
          P.fromInput localOutput
        >-> terminateD
        >-> saveInputD
        >-> P.mapM (\cont -> putStrLn ("encoded " ++ (show $ BL.length $ DB.encode cont) ++ " bytes") >> return cont)
        >-> P.encode
        >-> P.mapM (\cont -> putStrLn ("Transmitting " ++ (show $ B.length cont) ++ " bytes") >> return cont)
        >-> liftIO . N.toSocket socket
        >-> getPacketD socket
        >-> saveResponseD

      -- cleanup thread pool upon termination
      atomically $
        modifyTVar' threadState $ M.delete threadID
      P.performGC

      liftBase $ putStrLn "removed proxy connection"

{- saveInputD
  :: (Monad m, Proxy p)
  => a
  -> PS.StateP s p () (q, s) a q m r -}
saveInputD _ = do
  (req, input) <- P.await
  S.put input
  P.yield req >>= saveInputD

{- getPacketD
  :: (Binary b, MonadIO m, Proxy p)
  => Socket
  -> b'
  -> p () x b' b m r -}
getPacketD socket = go socket
  where
  go sock _ = do
    _ <- P.await
    liftIO (retrievePacket B.empty) >>= P.yield >>= go sock

  retrievePacket acc = do
    Just rx <- fmap join . timeout (fromIntegral proxyTimeout) $ NS.recv socket 4096
    let total = B.append acc rx
    case BI.decodeOrFail (BL.fromChunks [total]) of
      Right (_, _, obj) -> return obj
      Left _ -> retrievePacket total

{- saveResponseD
  :: (MonadIO m, Proxy p)
  => a
  -> PS.StateP WorkerThreadQueryState p () ProxyResponse a ProxyResponse m r -}
saveResponseD _ = do
  reply <- P.await
  output <- S.get
  void . liftIO . atomically $ check <$> P.send output (Just reply)
  P.yield reply >>= saveResponseD

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
  liftBase $ putStrLn "Running query"
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
    liftBase $ putStrLn "Running runQuery"
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
--   Note: Not statistically uniform since it uses (`mod` poolSize).
sampleNode
  :: MonadBase IO m
  => MProxyT m (Maybe WorkerIdentifier)
sampleNode = R.ask >>= liftBase . atomically . readTVar >>= \workers -> do
  rand <- liftBase randomIO
  let workerSize = M.size workers
  return $ case workerSize of
    0 -> Nothing
    _ -> Just . fst $ M.elemAt (rand `mod` workerSize) workers
