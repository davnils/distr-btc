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
type MarketTrade = B.ByteString -- TODO
type MarketOrderBook = B.ByteString -- TODO

data ProxyRequest
 = MarketRequest
   {
     _targetSite              :: B.ByteString,
     _targetPath              :: B.ByteString,
     _targetTradePath         :: B.ByteString,
     _targetOrderBookPath     :: B.ByteString,
     _timeout                 :: MicroSeconds
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
     _status                  :: !Bool,
     _marketReply             :: Maybe MarketReplyDetails
   }
 | StatusReply
   {
     _version                 :: !ProxyVersion,
     _memLoad                 :: !Double,
     _cpuLoad                 :: !Double,
     _responseMean            :: !Double,
     _responseStd             :: !Double
   }
 | AcknowledgementReply
   {
   }
 deriving (Eq, Generic, Show)

instance Binary ProxyResponse

data MarketReplyDetails
 = MarketReplyDetails
   {
     _responseTime            :: MicroSeconds,
     _trades                  :: MarketTrade,
     _orderBook               :: MarketOrderBook
   }
 deriving (Eq, Generic, Show)

instance Binary MarketReplyDetails

type MProxyT mt mb = (MonadIO mb, Functor mb, mt ~ R.ReaderT (TVar (M.Map WorkerIdentifier WorkerThread)) mb)
type WorkerIdentifier = SockAddr
type WorkerThread = Input (Maybe (ProxyRequest, WorkerThreadQueryState))
type WorkerThreadQueryState = Input (Maybe ProxyResponse)

type MicroSeconds = Int
type ResponseTime = Int
