{-# Language
  OverloadedStrings #-}

--------------------------------------------------------------------
-- |
-- Module: HTrade.Backend.Storage
--
-- Module defining all settings related to Cassandra and all schemas.

module HTrade.Backend.Storage where

import qualified Data.Text                       as T
import           Data.Monoid                     ((<>))
import qualified Database.Cassandra.CQL          as DB
import           Network                         (HostName)
import           Network.Socket                  (ServiceName)

-- | Field in Cassandra table.
type Field = T.Text

-- | Type of field in Cassandra table.
type CassandraType = T.Text

-- | Cassandra table in the market keyspace.
data CassandraTable
 = CassandraTable                         -- ^ Describe a table.
 {
   _tableName :: T.Text,                  -- ^ Name of table.
   _schema :: [(Field, CassandraType)],   -- ^ Schema as value-type tuples.
   _schemaProperties :: T.Text            -- ^ Additional suffix used by create table.
 }

-- | Retrieve all fields stored in a table.
cassandraTableFields :: CassandraTable -> [Field]
cassandraTableFields = filter (/= "primary key") . map fst . _schema

-- | Retrieve all fields in a table as a string wrapped with parentheses.
tableFieldsStr :: CassandraTable -> T.Text
tableFieldsStr = flatten . cassandraTableFields
  where
  flatten str = "(" <> T.intercalate ", " str <> ")"

-- | Build a schema string, as used by the create table command, from a table description.
buildCassandraSchema :: CassandraTable -> T.Text
buildCassandraSchema s = "(" <> T.intercalate ", " schemaStr <> ")" <> suffix (_schemaProperties s)
  where
  schemaStr = map (\(field, fieldType) -> field <> " " <> fieldType) $ _schema s
  suffix "" = ""
  suffix str = " " <> str

-- | Cassandra host to be used.
cassandraHost :: HostName
cassandraHost = "localhost"

-- | Cassandra port to be used.
cassandraPort :: ServiceName
cassandraPort = "9042"

-- | Cassandra keyspace to be used.
marketKeyspace :: DB.Keyspace
marketKeyspace = "market_data"

-- | Table storing raw content retrieved from market web servers.
marketRawTable :: CassandraTable
marketRawTable = CassandraTable
  "market_raw"
  [("id", "uuid"),
   ("market", "ascii"),
   ("retrieved", "timestamp"),
   ("orderbook", "blob"),
   ("trades", "blob"),
   ("elapsed", "int"),
   ("primary key", "(id)")]
   ""

-- | Table storing order books retrieved from markets.
marketOrderBookTable :: CassandraTable
marketOrderBookTable = CassandraTable
  "market_orders"
  [("market", "ascii"),
   ("day", "timestamp"),
   ("retrieved", "timestamp"),
   ("asks", "list<blob>"),
   ("bids", "list<blob>"),
   ("primary key", "((market, day), retrieved)")]
   "with clustering order by (retrieved desc)"

-- | Table storing trades retrieved from markets.
marketTradesTable :: CassandraTable
marketTradesTable = CassandraTable
  "market_trades"
  [("market", "ascii"),
   ("group", "bigint"),
   ("trade", "bigint"),
   ("time", "timestamp"),
   ("price", "decimal"),
   ("amount", "decimal"),
   ("primary key", "((market, group), trade)")]
  ""

-- | Table storing an identifier of the latest retrieved trade.
marketLastTradeTable :: CassandraTable
marketLastTradeTable = CassandraTable
  "market_last_trade"
  [("market", "ascii"),
   ("trade", "bigint"),
   ("time", "timestamp"),
   ("primary key", "(market)")]
  ""

-- | Table storing trade activity associated with a given day.
marketTradesDayTable :: CassandraTable
marketTradesDayTable = CassandraTable
  "market_trades_day"
  [("market", "ascii"),
   ("day", "timestamp"),
   ("first", "bigint"),
   ("last", "bigint"),
   ("primary key", "(market, day)")]
  ""

-- | Table storing monitored market status.
marketStatusTable :: CassandraTable
marketStatusTable = CassandraTable
  "market_status"
  [("market", "ascii"),
   ("day", "timestamp"),
   ("time", "timestamp"),
   ("status", "int"),
   ("primary key", "((market, day), time)")]
  "with clustering order by (time desc)"
