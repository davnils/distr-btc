module HTrade.Backend.Types where

import qualified Data.ByteString.Char8       as B

import HTrade.Shared.Types

data MarketIdentifier
  = MarketIdentifier
    {
      _marketName             :: B.ByteString,
      _currency               :: B.ByteString
    }

data MarketConfiguration
  = MarketConfiguration
    {
      _marketIdentifier       :: MarketIdentifier,
      _marketHost             :: B.ByteString,
      _marketPath             :: B.ByteString,
      _marketTrades           :: B.ByteString,
      _marketOrders           :: B.ByteString,
      _marketInterval         :: MicroSeconds
    }
