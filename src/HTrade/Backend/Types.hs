{-# Language
  FlexibleInstances,
  GeneralizedNewtypeDeriving,
  OverloadedStrings,
  ScopedTypeVariables,
  TypeSynonymInstances #-}

{-# OPTIONS_GHC -fno-warn-orphans #-}

--------------------------------------------------------------------
-- |
-- Module: HTrade.Backend.Types
--
-- Various types and constants shared within the backend components.

module HTrade.Backend.Types where

import           Control.Applicative             ((<*>), (<$>))
import           Control.Monad                   (guard, mzero)
import qualified Data.Aeson                      as A
import           Data.Aeson                      ((.=), (.:))
import           Data.Attoparsec.Number          (Number (..))
import           Data.Binary                     (Binary(..))
import qualified Data.Binary                     as BIN
import qualified Data.ByteString.Char8           as B
import qualified Data.ByteString.Lazy.Char8      as BL
import           Data.Decimal                    (DecimalRaw(..), Decimal, realFracToDecimal)
import           Data.Time.Clock                 (NominalDiffTime, UTCTime)
import           Data.Time.Clock.POSIX           (posixSecondsToUTCTime)
import qualified Data.Vector                     as V
import           Data.Word                       (Word8)
import qualified Database.Cassandra.CQL          as DB
import           Network.Socket.Internal         (SockAddr(..))

import           HTrade.Shared.Types

-- | Precision used when parsing JSON values (workaround with doubles).
--   Keep track of https://github.com/bos/aeson/issues/146
decimalPrecision :: Word8
decimalPrecision = 10

-- | Unique identifier for each market.
data MarketIdentifier
  = MarketIdentifier
    {
      _marketName             :: B.ByteString,     -- ^ Name of market without spaces.
      _currency               :: B.ByteString      -- ^ Currency, all caps, no delimiters.
    }
  deriving (Eq, Ord, Show)

-- | Configuration associated with a market, loaded from disk.
data MarketConfiguration
  = MarketConfiguration
    {
      _marketIdentifier       :: MarketIdentifier, -- ^ Identifier indicating the market.
      _marketHost             :: B.ByteString,     -- ^ Host prefixed with \”http(s)://\”.
      _marketPath             :: B.ByteString,     -- ^ Market path shared by trades and order book.
      _marketTrades           :: B.ByteString,     -- ^ Path to trades subpath.
      _marketOrders           :: B.ByteString,     -- ^ Path to order book subpath.
      _marketInterval         :: MicroSeconds      -- ^ Interval at which to poll the market.
    }
  deriving (Eq, Ord, Show)

-- | Unique identifier of a worker.
type WorkerIdentifier = SockAddr

-- | Bitcoin alias used for convenience.
type Bitcoin = Decimal

-- | A single entry in the order book consisting of price and an amount.
newtype OrderBookEntry = OrderBookEntry (Decimal, Bitcoin)
   deriving (Binary, Eq, Ord, Show)

instance DB.CasType OrderBookEntry where
  getCas = BIN.decode . BL.fromStrict . DB.unBlob <$> DB.getCas
  putCas = DB.putCas . DB.Blob . BL.toStrict . BIN.encode
  casType _ = DB.CBlob

-- | Order book retrieved from a market consisting of the current asks and bids.
data MarketOrderBook
 = MarketOrderBook
   {
     _asks                    :: ![OrderBookEntry],
     _bids                    :: ![OrderBookEntry]
   }
   deriving (Eq, Ord, Show)

-- | Single trade entry consisting of timestamp, price, amount
--   and a monotonically increasing unique integer ID.
data TradeEntry
 = TradeEntry
   {
     _time                    :: !UTCTime,
     _price                   :: !Decimal,
     _amount                  :: !Decimal,
     _transactionID           :: !TradeID
   }
   deriving (Eq, Ord, Show)

-- | Trades retrieved from a market.
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

-- | Status of a market, indicates if it can be reached or not.
type MarketStatus = Int

-- TODO: Create separate ADT

-- | Indicates that the market was available when last probed.
marketActive :: MarketStatus
marketActive = 1

-- | Indicates that the market was unavailable when last probed.
marketInactive :: MarketStatus
marketInactive = 0
