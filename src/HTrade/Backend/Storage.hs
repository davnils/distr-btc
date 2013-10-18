{-# Language OverloadedStrings #-}
module HTrade.Backend.Storage where

import Database.Cassandra.CQL
import Data.Text (Text)
import Network (HostName)
import Network.Socket (ServiceName)

cassandraHost :: HostName
cassandraHost = "localhost"

cassandraPort :: ServiceName
cassandraPort = "9042"

marketKeyspace :: Keyspace
marketKeyspace = "market_data"

marketRawTable :: Text
marketRawTable = "market_raw"

marketRawDataSchema :: Text
marketRawDataSchema  = "(\
  \id uuid primary key,\
  \market ascii,\
  \retrieved timestamp,\
  \orderbook blob,\
  \trades blob,\
  \elapsed int\
\)"

marketOrderBookTable :: Text
marketOrderBookTable = "market_orders"

marketOrderBookSchema :: Text
marketOrderBookSchema = "(\
\  market ascii,\
\  day timestamp,\
\  retrieved timestamp,\
\  asks list<blob>,\
\  bids list<blob>,\
\  primary key ((market, day), retrieved)\
\) with clustering order by (retrieved desc)"

marketTradesTable :: Text
marketTradesTable = "market_trades"

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

marketLastTradeTable :: Text
marketLastTradeTable = "market_last_trade"

marketLastTradeSchema :: Text
marketLastTradeSchema = "(\
\  market ascii,\
\  trade bigint,\
\  time timestamp,\
\  primary key (market)\
\)"

marketTradesDayTable :: Text
marketTradesDayTable  = "market_trades_day"

marketTradesDaySchema :: Text
marketTradesDaySchema = "(\
\  market ascii,\
\  day timestamp,\
\  first bigint,\
\  last bigint,\
\  primary key (market, day)\
\)"
