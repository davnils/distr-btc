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

