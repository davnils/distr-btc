{-# Language ConstraintKinds, DeriveGeneric #-}

module HTrade.Shared.Types where

import Control.Concurrent.STM
import Control.Proxy
import Control.Proxy.Concurrent
import Control.Monad
import Control.Monad.Trans
import qualified Control.Monad.Reader        as R
import qualified Data.ByteString.Char8       as B
import Data.Binary (Binary)
import qualified Data.Map                    as M
import Data.Word (Word16)
import GHC.Generics (Generic)
import Network.Socket.Internal (SockAddr(..))

type ProxyVersion = (Int, Int)
type MarketTrade = Int -- TODO
type MarketOrderBook = Int -- TODO

data ProxyRequest
 = MarketRequest
   {
     _targetSite              :: B.ByteString,
     _targetPath              :: B.ByteString,
     _targetTradePath         :: B.ByteString,
     _targetOrderBookPath     :: B.ByteString,
     _timeout                 :: MarketTimeout
   }
 | StatusRequest
   {
   }
 | ReloadRequest
   {
     _performUpgrade          :: Bool,
     _reboot                  :: Bool
   }
 deriving (Eq, Generic, Show)

instance Binary ProxyRequest

data ProxyResponse
 = MarketReply
   {
     _status                  :: Bool,
     _status_reply            :: Maybe StatusReplyDetails
   }
 | StatusReply
   {
     _version                 :: ProxyVersion,
     _mem_load                :: Float,
     _cpu_load                :: Float,
     _response_mean           :: Float,
     _response_std            :: Float
   }
 deriving (Eq, Generic, Show)

instance Binary ProxyResponse

data StatusReplyDetails
 = StatusReplyDetails
   {
     _response_time           :: Int,
     _trades                  :: [MarketTrade],
     _orderBook               :: [MarketOrderBook]
   }
 deriving (Eq, Generic, Show)

instance Binary StatusReplyDetails

type MProxyT mt mb = (MonadIO mb, mt ~ R.ReaderT (TVar (M.Map WorkerIdentifier WorkerThread)) mb)
type WorkerIdentifier = (SockAddr, Word16)
type WorkerThread = Input (Maybe (ProxyRequest, Input (Maybe ProxyResponse)))
type MarketTimeout = Int
