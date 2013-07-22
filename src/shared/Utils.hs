module Utils where

import Control.Proxy
import Control.Proxy.Binary
import qualified Control.Proxy.TCP           as N
import Control.Proxy.Safe
import Data.Binary (Binary)
import qualified Data.ByteString.Char8       as B
import Network.Socket (Socket)
import Network.Socket.Internal (SockAddr(..))

import Types

readPacket socket timeout' =
      N.socketReadTimeoutS timeout' 4096 socket
  >-> liftP . (mapD Just >-> decodeD >-> printD)

ignoreProxy
  :: (Monad m, ProxyInternal p)
  => p a' a b' b m r
  -> p a' a b' b m ()
ignoreProxy p = (?>=) p $ \_ -> return_P ()

writePacket
  :: (Binary x, Proxy p)
  => Socket
  -> Int
  -> ()
  -> p () x () B.ByteString IO ()
writePacket socket timeout' =
      encodeD
  >-> ignoreProxy . runEitherP . N.socketWriteTimeoutD timeout' socket

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
