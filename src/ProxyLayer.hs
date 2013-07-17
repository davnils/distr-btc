{-# Language ConstraintKinds, TypeFamilies, RankNTypes #-}

module ProxyLayer where

import Control.Applicative
import qualified Control.Concurrent.Async   as C
import Control.Concurrent.STM
import Control.Monad
import Control.Monad.Trans
import Control.Proxy
import Control.Proxy.Concurrent
import qualified Control.Proxy.TCP          as N
import qualified Control.Monad.Reader       as R
import qualified Data.Map                   as M
import qualified Data.Set                   as S
import qualified Data.Traversable           as T
import Data.Word (Word16)

data ProxyRequest
 = Blargh
 | Blargh2

data ProxyResponse
 = Blargha
 | Blargha2

type MProxyT mt mb = (MonadIO mb, mt ~ R.ReaderT (TVar (M.Map WorkerIdentifier WorkerThread)) mb)
type WorkerIdentifier = (Int, Word16)
type WorkerThread = Input (Maybe (ProxyRequest, Output ProxyResponse))

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

query
  :: MProxyT mt mb
  => Maybe WorkerIdentifier
  -> Maybe N.Timeout
  -> ProxyRequest
  -> mt ProxyResponse
query = undefined

  where
  runQuery addr req timeout = undefined

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
  -> mt [a]
mapLayer f = do
  threadState <- R.ask
  workers <- liftIO . atomically $ readTVar threadState
  T.mapM f (keys workers)
