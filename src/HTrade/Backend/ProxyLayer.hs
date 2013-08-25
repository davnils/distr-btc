{-# Language FlexibleContexts, FlexibleInstances, GeneralizedNewtypeDeriving, MultiParamTypeClasses, Rank2Types, TypeFamilies, UndecidableInstances #-}

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
import Control.Proxy
import qualified Control.Proxy.Trans.State   as PS
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
import qualified Network.Simple.TCP          as NS
import Network.Socket (Socket)
import System.Random (randomIO)
import System.Timeout (timeout)

import HTrade.Backend.Types
import HTrade.Shared.Types
import HTrade.Shared.Utils

type WorkerThread = Input (Maybe (ProxyRequest, WorkerThreadQueryState))
type WorkerThreadQueryState = Input (Maybe ProxyResponse)

type InternalState = TVar (M.Map WorkerIdentifier WorkerThread)
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

readyLimit :: Int
readyLimit = 1

defaultTimeout :: MicroSeconds
defaultTimeout = 1000000

proxyTimeout :: MicroSeconds
proxyTimeout = 10000000

listenDelay :: MicroSeconds
listenDelay = 500000

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
  threadState <- liftBase . atomically $ newTVar M.empty
  listenID <- liftBase . C.async $ listener threadState

  liftBase $ threadDelay listenDelay
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

      let threadID = addr
      (localInput, localOutput) <- spawn Single
      atomically $
        modifyTVar' threadState $ M.insert threadID localInput

      -- execute pipeline which:
      -- (1) reads a local request
      -- (2) submits the corresponding request packet
      -- (2) retrieves the result and notifies the original query

      tryAny . runProxy . runEitherK . PS.runStateK undefined $ 
            recvS localOutput
        >-> terminateD
        >-> saveInputD
        >-> encodeD
        >-> liftP . N.socketWriteTimeoutD proxyTimeout socket
        >-> getPacketD socket
        >-> saveResponseD

      -- cleanup thread pool upon termination
      atomically $
        modifyTVar' threadState $ M.delete threadID
      performGC

saveInputD
  :: (Monad m, Proxy p)
  => a
  -> PS.StateP s p () (q, s) a q m r
saveInputD _ = do
  (req, input) <- request ()
  PS.put input
  respond req >>= saveInputD

getPacketD
  :: (Binary b, MonadIO m, Proxy p)
  => Socket
  -> b'
  -> p () x b' b m r
getPacketD socket = runIdentityK (go socket)
  where
  go socket _ = do
    _ <- request ()
    liftIO (retrievePacket B.empty) >>= respond >>= go socket

  retrievePacket acc = do
    Just rx <- fmap join . timeout proxyTimeout $ NS.recv socket 4096
    let total = B.append acc rx
    case BI.decodeOrFail (BL.fromChunks [total]) of
      Right (_, _, obj) -> return obj
      Left _ -> retrievePacket total

saveResponseD
  :: (MonadIO m, Proxy p)
  => a
  -> PS.StateP WorkerThreadQueryState p () ProxyResponse a ProxyResponse m r
saveResponseD _ = do
  reply <- request ()
  output <- PS.get
  liftIO . atomically $ send output $ Just reply
  respond reply >>= saveResponseD

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
  let timeout = fromMaybe defaultTimeout timeout' -- TODO: USE!
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
    (localInput, localOutput) <- liftBase $ spawn Single
    onJust thread $ \remote -> liftBase $ do
      atomically . send remote $ Just (req, localInput)
      atomically . liftM join $ recv localOutput

-- | Check if the number of connected proxy nodes have reached the critical limit.
--   It is suitable to call this function repeatedely until success during
--   initialization.
ready
  :: MonadBase IO m
  => MProxyT m Bool
ready = fmap (> readyLimit) connectedNodes

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
    Just chan -> always $ send chan Nothing
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
