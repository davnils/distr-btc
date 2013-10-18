{-# Language
  FlexibleInstances,
  GeneralizedNewtypeDeriving,
  OverloadedStrings,
  ScopedTypeVariables,
  TypeSynonymInstances #-}

{-# OPTIONS_GHC -fno-warn-orphans #-}

module HTrade.Backend.Types where

import Control.Applicative ((<*>), (<$>))
import Control.Monad (guard, mzero)
import qualified Data.Aeson                  as A
import Data.Aeson ((.=), (.:))
import Data.Attoparsec.Number (Number (..))
import Data.Binary (Binary(..))
import qualified Data.Binary                 as BIN
import qualified Data.ByteString.Char8       as B
import qualified Data.ByteString.Lazy.Char8  as BL
import Data.Decimal (DecimalRaw(..), Decimal, realFracToDecimal)
import Data.Time.Clock (NominalDiffTime, UTCTime)
import Data.Time.Clock.POSIX (posixSecondsToUTCTime)
import qualified Data.Vector                 as V
import Data.Word (Word8)
import Database.Cassandra.CQL
import Network.Socket.Internal (SockAddr(..))

import HTrade.Shared.Types

-- | Precision used when parsing JSON values (workaround with doubles).
--   Keep track of https://github.com/bos/aeson/issues/146
decimalPrecision :: Word8
decimalPrecision = 10

data MarketIdentifier
  = MarketIdentifier
    {
      _marketName             :: B.ByteString,
      _currency               :: B.ByteString
    }
  deriving (Eq, Ord, Show)

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
  deriving (Eq, Ord, Show)

type WorkerIdentifier = SockAddr

type Bitcoin = Decimal

newtype OrderBookEntry = OrderBookEntry (Decimal, Bitcoin)
   deriving (Binary, Eq, Ord, Show)

instance CasType OrderBookEntry where
  getCas = BIN.decode . BL.fromStrict . unBlob <$> getCas
  putCas = putCas . Blob . BL.toStrict . BIN.encode
  casType _ = CBlob

data MarketOrderBook
 = MarketOrderBook
   {
     _asks                    :: ![OrderBookEntry],
     _bids                    :: ![OrderBookEntry]
   }
   deriving (Eq, Ord, Show)

data TradeEntry
 = TradeEntry
   {
     _time                    :: !UTCTime,
     _price                   :: !Decimal,
     _amount                  :: !Decimal,
     _transactionID           :: !TradeID
   }
   deriving (Eq, Ord, Show)

newtype Trades = Trades [TradeEntry]
   deriving (Eq, Ord, Show)

instance A.FromJSON Decimal where
  parseJSON doc = do
    (val :: Double) <- A.parseJSON doc
    return $ realFracToDecimal decimalPrecision val

instance A.FromJSON OrderBookEntry where
  parseJSON (A.Array a) = do
    guard $ V.length a == 2
    v1 <- A.parseJSON (a V.! 0)
    v2 <- A.parseJSON (a V.! 1)
    return $ OrderBookEntry (v1, v2)

  parseJSON _ = mzero

instance A.FromJSON MarketOrderBook where
  parseJSON (A.Object o) = MarketOrderBook
    <$> o .: "asks"
    <*> o .: "bids"

  parseJSON _ = mzero

instance A.FromJSON Trades where
  parseJSON (A.Array o) = Trades . toUTC <$> V.mapM A.parseJSON o
    where
    toUTC = V.toList . V.map convertToUTC
    convertToUTC (a1, a2, a3, a4) = TradeEntry (posixSecondsToUTCTime a1) a2 a3 a4

  parseJSON _ = mzero

instance A.FromJSON NominalDiffTime where
  parseJSON (A.Number (I t)) = return $ fromIntegral t
  parseJSON _ = mzero

instance A.ToJSON Decimal where
  toJSON = A.toJSON . toRational

instance A.ToJSON OrderBookEntry where
  toJSON (OrderBookEntry (v1, v2)) = A.Array serialized
    where
      serialized = V.concat $ map (V.singleton . A.toJSON) [v1, v2]

instance A.ToJSON MarketOrderBook where
  toJSON book = A.object ["asks" .= _asks book, "bids" .= _bids book]

instance Binary (DecimalRaw Integer) where
  put (Decimal places mantissa) = do
    put places
    put mantissa

  get = Decimal <$> get <*> get

type MarketStatus = Int

marketActive :: MarketStatus
marketActive = 1

marketInactive :: MarketStatus
marketInactive = 0
