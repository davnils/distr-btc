{-# Language FlexibleContexts #-}

module HTrade.Shared.Utils where

import qualified Control.Concurrent.Async    as A
import Control.Concurrent (threadDelay)
import qualified Control.Exception           as E
import Control.Monad.Base
import Control.Proxy
import Control.Proxy.Binary
import qualified Control.Proxy.TCP           as N
import Control.Proxy.Safe
import Data.Binary (Binary)
import qualified Data.ByteString.Char8       as B
import Network.Socket (Socket)
import Network.Socket.Internal (SockAddr(..))

import HTrade.Shared.Types

backendPort :: Int
backendPort = 1111

ignoreProxy
  :: (Monad m, ProxyInternal p)
  => p a' a b' b m r
  -> p a' a b' b m ()
ignoreProxy p = (?>=) p $ \_ -> return_P ()

terminateD :: (Monad m, Proxy p) => () -> Pipe p (Maybe a) a m ()
terminateD () = runIdentityP go
  where
  go = do
    val <- request ()
    case val of
      Just a -> respond a >> go
      Nothing -> return ()

-- todo: replace
onJust
  :: Monad m
  => Maybe a
  -> (a -> m (Maybe b))
  -> m (Maybe b)
onJust Nothing _ = return Nothing
onJust (Just val) f = f val

-- Evaluate an IO action and catch ANY exceptions in an either value.
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

-- | Convert seconds to microseconds.
seconds
  :: Int
  -> MicroSeconds
seconds = (* 10^6)

-- | Wait for specified amount of time (in microseconds).
delay
  :: MonadBase IO m
  => MicroSeconds
  -> m ()
delay = liftBase . threadDelay
