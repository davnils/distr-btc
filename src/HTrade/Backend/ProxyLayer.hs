{-# Language
  FlexibleContexts,
  FlexibleInstances,
  GeneralizedNewtypeDeriving,
  MultiParamTypeClasses,
  Rank2Types,
  TypeFamilies,
  UndecidableInstances #-}

--------------------------------------------------------------------
-- |
-- Module: HTrade.Backend.ProxyLayer
--
-- Module providing interaction with connected proxy nodes.
-- Starts listening for external connections upon initialization.
-- The proxy layer can then be queried and monitored through
-- high-level functions which operate over the 'MProxyT' monad.

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

import           Data.Monoid                     ((<>))
import           Control.Applicative             ((<$>), Applicative)
import qualified Control.Concurrent.Async        as C
import           Control.Concurrent              (threadDelay)
import qualified Control.Concurrent.STM          as STM
import qualified Control.Error                   as E
import           Control.Monad                   (forever, join, liftM, void, when)
import           Control.Monad.Base              (liftBase, liftBaseDefault, MonadBase)
import           Control.Monad.Trans             (liftIO, MonadIO, MonadTrans)
import qualified Control.Monad.Trans.Control     as TC
import qualified Control.Monad.Reader            as R
import qualified Data.Map                        as M
import           Data.Maybe                      (fromMaybe)
import           Data.Word                       (Word16)
import           Pipes                           ((>->))
import qualified Pipes                           as P
import qualified Pipes.Binary                    as P
import qualified Pipes.Concurrent                as P
import qualified Pipes.Prelude                   as P
import qualified Pipes.Network.TCP               as P hiding (recv, send)
import           System.Random                   (randomRIO)
import           System.Timeout                  (timeout)

import           HTrade.Backend.Types
import           HTrade.Shared.Types
import           HTrade.Shared.Utils

-- | Type of a worker thread stored in the globally shared thread map.
type WorkerThread = P.Output (Maybe (ProxyRequest, WorkerThreadQueryState))

-- | Type of state used in pipeline.
type WorkerThreadQueryState = P.Output (Maybe ProxyResponse)

-- | Internal state used in monad transformer, stored a globally shared map.
type InternalState = STM.TVar (M.Map WorkerIdentifier WorkerThread)

-- | Monad transformer giving access to the proxy layer.
newtype MProxyT m a
 = MProxyT
 {
   runProxyMT :: R.ReaderT InternalState m a -- ^ State carrying access to all proxies.
 }
 deriving (
   Applicative,
   Functor,
   Monad,
   MonadIO,
   MonadTrans,
   R.MonadReader InternalState
 )

-- | MonadBase instance required to lift IO actions.
instance MonadBase m m' => MonadBase m (MProxyT m') where
  liftBase = liftBaseDefault

-- | MonadTransControl instance required to integrate with the lifted async package.
instance TC.MonadTransControl MProxyT where
  newtype StT MProxyT a = StProxy {unProxy :: TC.StT (R.ReaderT InternalState) a}
  liftWith = TC.defaultLiftWith MProxyT runProxyMT StProxy
  restoreT = TC.defaultRestoreT MProxyT unProxy

-- | MonadBaseControl instance required to integrate with the lifted async package.
instance TC.MonadBaseControl b m => TC.MonadBaseControl b (MProxyT m) where
  newtype StM (MProxyT m) a = StMT {unStMT :: TC.ComposeSt MProxyT m a}
  liftBaseWith = TC.defaultLiftBaseWith StMT
  restoreM     = TC.defaultRestoreM   unStMT

-- | Minimum number of nodes required before allowing any proxy layer queries.
readyLimit :: Int
readyLimit = 1

-- | Proxy timeout value which defines internal allowance between backend
--   and proxy layer. Any requests surpassing this limit will be redirected
--   to another proxy node or considered unavailable.
proxyTimeout :: MicroSeconds
proxyTimeout = seconds 15

-- | Internal delay after backend has been established.
listenDelay :: MicroSeconds
listenDelay = 500000

-- | Maximum number of proxy queries before result is considered unavailable.
maxProbes :: Int
maxProbes = 4

-- | Main entry point when interfacing with the proxy layer.
--   Accepts a computation to be evaluated over the proxy transformer.
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
  threadState <- liftBase $ STM.newTVarIO M.empty
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
      STM.atomically $
        STM.modifyTVar' threadState $ M.insert threadID (outputTransmit <> outputReceive)

      -- launch a pipeline to transmit outgoing packets
      threadTransmit <- C.async . tryAny . P.runEffect $ 
            P.fromInput inputTransmit
        >-> terminateD
        >-> P.map fst
        >-> P.for P.cat P.encode
        >-> P.toSocket socket

      -- launch a pipeline to receive replies
      threadReceive <- C.async . tryAny . P.runEffect $ 
            P.decodeMany (P.fromSocket socket 4096)
        >-> P.map snd
        >-> saveResponse inputReceive

      -- terminate after either pipeline dies
      _ <- C.waitAnyCatchCancel [void threadTransmit, void threadReceive]

      -- cleanup thread pool upon termination
      STM.atomically $
        STM.modifyTVar' threadState $ M.delete threadID
      P.performGC

-- | Save the response to a mailbox read from the provided mailbox.
saveResponse
  :: MonadIO m
  => P.Input (Maybe (ProxyRequest, WorkerThreadQueryState))
  -> P.Consumer ProxyResponse m b
saveResponse input = forever $ do
  reply <- P.await
  output <- liftM join . liftIO . STM.atomically . P.recv $ input
  case output of
    Nothing -> return ()
    Just (_, output') ->
      liftIO . STM.atomically $ P.send output' (Just reply) >>= STM.check

-- | Execute a query on the proxy layer.
--   If no proxy node is given then a random sample is taken from the pool.
--
--   A non-existant node or an empty pool will result in an error value.
query
  :: MonadBase IO m
  => Maybe WorkerIdentifier
  -> Maybe Int
  -> ProxyRequest
  -> MProxyT m (Maybe ProxyResponse)
query addr probes req = do
  let getWorker = maybe sampleNode (return . Just) addr
  -- iterate until a response is retrieved or the limit is reached
  sendProbe getWorker $ fromMaybe maxProbes probes

  where
  -- retrieve target proxy node and run query, may fail with Nothing
  sendProbe _ 0 = return Nothing
  sendProbe getWorker limit = do
    result <- getWorker >>= flip onJust runQuery
    case result of
      Nothing    -> sendProbe getWorker (limit - 1)
      res        -> return res

  -- run a query on the given address, may fail with Nothing
  lift' = liftBase . E.runMaybeT . E.hushT . E.syncIO
  runQuery addr' = R.ask >>= \threadVar -> fmap join . lift' $ do
    thread <- STM.atomically . liftM (M.lookup addr') $ STM.readTVar threadVar
    (localInput, localOutput) <- P.spawn P.Single

    -- transmit request and await reply if the address was found
    onJust thread $ \remote -> do
      res <- timeout (fromIntegral proxyTimeout) $ do
        STM.atomically $ P.send remote (Just (req, localInput)) >>= STM.check
        STM.atomically $ join <$> P.recv localOutput

      -- GC mailbox if proxy node didn't respond
      when (E.isNothing res) P.performGC

      return $ join res

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
connectedNodes = R.ask >>= fmap M.size . liftBase . STM.readTVarIO

-- | Remove the specified node from the proxy layer.
removeNode
  :: MonadBase IO m
  => WorkerIdentifier
  -> MProxyT m ()
removeNode addr = R.ask >>= \threadState -> liftBase . STM.atomically $ do
  entry <- M.lookup addr <$> STM.readTVar threadState
  STM.modifyTVar' threadState $ M.delete addr
  case entry of
    Just chan -> P.send chan Nothing >>= STM.check
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
  threadMap <- R.ask >>= liftBase . STM.atomically . STM.readTVar
  let addrs = M.keys threadMap
  mapped <- mapM f addrs
  return . M.fromList $ zip addrs mapped

-- | Retrieve a randomly sampled node from the worker pool.
sampleNode
  :: MonadBase IO m
  => MProxyT m (Maybe WorkerIdentifier)
sampleNode = R.ask >>= liftBase . STM.atomically . STM.readTVar >>= \workers -> do
  let workerSize = M.size workers
  case workerSize of
    0 -> return Nothing
    _ -> do
      rand <- liftBase $ randomRIO (0, workerSize - 1)
      return . Just . fst $ M.elemAt rand workers
