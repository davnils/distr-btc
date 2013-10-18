{-# Language DeriveGeneric #-}

module HTrade.Shared.Types where

import qualified Data.ByteString.Char8       as B
import Data.Binary (Binary)
import Data.Int (Int64)
import Data.Word (Word)
import GHC.Generics (Generic)

type ProxyVersion = (Int, Int)
type TradesSerialized = B.ByteString
type OrderBookSerialized = B.ByteString
type TradeID = Int64

data ProxyRequest
 = MarketRequest
   {
     _targetSite              :: B.ByteString,
     _targetPath              :: B.ByteString,
     _targetTradePath         :: B.ByteString,
     _targetOrderBookPath     :: B.ByteString,
     _timeout                 :: MicroSeconds,
     _tradeID                 :: TradeID
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
     _trades                  :: TradesSerialized,
     _orderBook               :: OrderBookSerialized
   }
 deriving (Eq, Generic, Show)

instance Binary MarketReplyDetails

type MicroSeconds = Word
type ResponseTime = Int
