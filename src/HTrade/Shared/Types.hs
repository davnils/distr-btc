{-# Language DeriveGeneric #-}

module HTrade.Shared.Types where

import qualified Data.ByteString.Char8       as B
import Data.Binary (Binary)
import GHC.Generics (Generic)

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

type MicroSeconds = Int
type ResponseTime = Int
