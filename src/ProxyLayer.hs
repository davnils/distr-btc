{-# Language ConstraintKinds, TypeFamilies, RankNTypes #-}

module ProxyLayer where

import Control.Applicative
import qualified Control.Concurrent.Async    as C
import Control.Concurrent.STM
import Control.Monad
import Control.Monad.Trans
import Control.Proxy
import Control.Proxy.Concurrent
import qualified Control.Proxy.TCP           as N
import qualified Control.Monad.Reader        as R
import qualified Data.Map                    as M
import qualified Data.Set                    as S
import Data.Word (Word16)
import Network.Socket.Internal (SockAddr(..))

data ProxyRequest
 = Blargh
 | Blargh2

data ProxyResponse
 = Blargha
 | Blargha2

type MProxyT mt mb = (MonadIO mb, mt ~ R.ReaderT (TVar (M.Map WorkerIdentifier WorkerThread)) mb)
type WorkerIdentifier = (SockAddr, Word16)
type WorkerThread = Input (Maybe (ProxyRequest, Input (Maybe ProxyResponse)))

readyLimit :: Int
readyLimit = 1

withLayer
  :: MProxyT mt mb
  => mt a
  -> mb a
withLayer routine = do
  threadState <- liftIO $ newTVarIO M.empty
  liftIO . C.async $ listener threadState
  R.runReaderT routine threadState

  -- kill all the threads(?)

  where
  listener = undefined

-- todo: replace
onJust
  :: Monad m
  => Maybe a
  -> (a -> m (Maybe b))
  -> m (Maybe b)
onJust Nothing _ = return Nothing
onJust (Just val) f = f val

query
  :: MProxyT mt mb
  => Maybe WorkerIdentifier
  -> Maybe N.Timeout
  -> ProxyRequest
  -> mt ProxyResponse
query addr' timeout' req =  undefined
  -- call runQuery over infinite list until (1) Just _ or (2) timeout

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
