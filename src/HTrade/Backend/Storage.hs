{-# Language
  OverloadedStrings #-}

--------------------------------------------------------------------
-- |
-- Module: HTrade.Backend.Storage
--
-- Module defining all settings related to Cassandra and all schemas.

module HTrade.Backend.Storage where

import Database.Cassandra.CQL
import Data.Text (Text)
import Network (HostName)
import Network.Socket (ServiceName)

-- | Cassandra host to be used.
cassandraHost :: HostName
cassandraHost = "localhost"

-- | Cassandra port to be used.
cassandraPort :: ServiceName
cassandraPort = "9042"

-- | Cassandra keyspace to be used.
marketKeyspace :: Keyspace
marketKeyspace = "market_data"

-- | Name of table storing raw content retrieved from market web servers.
marketRawTable :: Text
marketRawTable = "market_raw"

-- | Associated schema.
marketRawDataSchema :: Text
marketRawDataSchema  = "(\
  \id uuid primary key,\
  \market ascii,\
  \retrieved timestamp,\
  \orderbook blob,\
  \trades blob,\
  \elapsed int\
\)"

-- | Name of table storing order books retrieved from markets.
marketOrderBookTable :: Text
marketOrderBookTable = "market_orders"

-- | Associated schema.
marketOrderBookSchema :: Text
marketOrderBookSchema = "(\
\  market ascii,\
\  day timestamp,\
\  retrieved timestamp,\
\  asks list<blob>,\
\  bids list<blob>,\
\  primary key ((market, day), retrieved)\
\) with clustering order by (retrieved desc)"

-- | Name of table storing trades retrieved from markets.
marketTradesTable :: Text
marketTradesTable = "market_trades"

-- | Associated schema.
marketTradesSchema :: Text
marketTradesSchema = "(\
\  market ascii,\
\  group bigint,\
\  trade bigint,\
\  time timestamp,\
\  price decimal,\
\  amount decimal,\
\  primary key ((market, group), trade)\
\)"

-- | Name of table storing an identifier of the latest retrieved trade.
marketLastTradeTable :: Text
marketLastTradeTable = "market_last_trade"

-- | Associated schema.
marketLastTradeSchema :: Text
marketLastTradeSchema = "(\
\  market ascii,\
\  trade bigint,\
\  time timestamp,\
\  primary key (market)\
\)"

-- | Name of table storing trade activity associated with a given day.
marketTradesDayTable :: Text
marketTradesDayTable  = "market_trades_day"

-- | Associated schema.
marketTradesDaySchema :: Text
marketTradesDaySchema = "(\
\  market ascii,\
\  day timestamp,\
\  first bigint,\
\  last bigint,\
\  primary key (market, day)\
\)"

-- | Name of table storing monitored market status.
marketStatusTable :: Text
marketStatusTable = "market_status"

-- | Associated schema.
marketStatusSchema :: Text
marketStatusSchema = "(\
\  market ascii,\
\  day timestamp,\
\  time timestamp,\
\  status int,\
\  primary key ((market, day), time)\
\) with clustering order by (time desc)"
