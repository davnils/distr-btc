{-# Language FlexibleContexts #-}

module HTrade.Shared.Utils where

import qualified Control.Concurrent.Async    as A
import Control.Concurrent (threadDelay)
import qualified Control.Exception           as E
import Control.Monad.Base
import Data.Word (Word, Word16)
import Pipes

import HTrade.Shared.Types

backendPort :: Word16
backendPort = 1111

-- | Terminate the pipeline upon receiving 'Data.Maybe.Nothing'.
terminateD
  :: Monad m
  => Pipe (Maybe a) a m ()
terminateD = do
  val <- await
  case val of
    Just a -> yield a >> terminateD
    Nothing -> return ()

-- | Apply a function if the supplied value isn't 'Data.Maybe.Nothing'.
onJust
  :: Monad m
  => Maybe a
  -> (a -> m (Maybe b))
  -> m (Maybe b)
onJust Nothing _ = return Nothing
onJust (Just val) f = f val

-- | Evaluate an IO action and catch ANY exceptions in an either value.
tryAny :: IO a -> IO (Either E.SomeException a)
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
  :: Word
  -> MicroSeconds
seconds = (* 10^(6 :: Int))

-- | Wait for specified amount of time (in microseconds).
delay
  :: MonadBase IO m
  => MicroSeconds
  -> m ()
delay = liftBase . threadDelay . fromIntegral
