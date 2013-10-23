{-# Language
  DataKinds,
  OverloadedStrings  #-}

--------------------------------------------------------------------
-- |
-- Module: HTrade.Backend.Admin
--
-- Administration tool used to initialize and destroy C* tables.
-- This should be run in order to initialize empty tables
-- before running the backend.
--
-- Note that the keyspace must be created and destroyed manually.

module Main where

import Control.Monad (forM_)
import Database.Cassandra.CQL
import Data.Monoid ((<>))
import Data.Text (Text)
import System.Environment

import HTrade.Backend.Storage

-- | All tables adminstrated by this tool.
--   Pairs of table name and the associated schema.
--   Assumed to exist within the default key space.
tables :: [(Text, Text)]
tables =
  [
    (marketRawTable, marketRawDataSchema),
    (marketOrderBookTable, marketOrderBookSchema),
    (marketTradesTable, marketTradesSchema),
    (marketLastTradeTable, marketLastTradeSchema),
    (marketTradesDayTable, marketTradesDaySchema),
    (marketStatusTable, marketStatusSchema)
  ]

-- | Create all schemas, will fail if any already exist.
createSchema :: Cas ()
createSchema = forM_ tables $ \(table, schema) -> 
  executeSchema ALL (query $ "create table " <> table <> " " <> schema) ()

-- | Drop all schemas, will fail if any do not exist.
dropSchema :: Cas ()
dropSchema =  forM_ tables $ \(table, _) -> executeSchema ALL (query $ "drop table " <> table) ()

-- | Evaluate some Cassandra action over the default pool.
runAction :: Cas a -> IO a
runAction action = do
  pool <- newPool [(cassandraHost, cassandraPort)] marketKeyspace
  runCas pool action

-- | Expects a single argument and will either initialize or destroy all C* tables.
main :: IO ()
main = do
  args <- getArgs
  case args of
    ["init"]    -> runAction createSchema
    ["destroy"] -> runAction dropSchema
    _           -> putStrLn "Either specify init or destroy."
