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

import           Control.Monad                   (forM_)
import qualified Database.Cassandra.CQL          as DB
import           Data.Monoid                     ((<>))
import           System.Environment              (getArgs)

import           HTrade.Backend.Storage

-- | All tables adminstrated by this tool.
--   Assumed to exist within the default key space.
tables :: [CassandraTable]
tables =
  [
    marketRawTable,
    marketOrderBookTable,
    marketTradesTable,
    marketLastTradeTable,
    marketTradesDayTable,
    marketStatusTable
  ]

-- | Create all schemas, will fail if any already exist.
createSchema :: DB.Cas ()
createSchema = forM_ tables $ \table -> do
  let schema = buildCassandraSchema table
  DB.executeSchema DB.ALL (DB.query $ "create table " <> _tableName table <> " " <> schema) ()

-- | Drop all schemas, will fail if any do not exist.
dropSchema :: DB.Cas ()
dropSchema =  forM_ tables $ \table ->
  DB.executeSchema DB.ALL (DB.query $ "drop table " <> _tableName table) ()

-- | Evaluate some Cassandra action over the default pool.
runAction :: DB.Cas a -> IO a
runAction action = do
  pool <- DB.newPool [(cassandraHost, cassandraPort)] marketKeyspace
  DB.runCas pool action

-- | Expects a single argument and will either initialize or destroy all C* tables.
main :: IO ()
main = do
  args <- getArgs
  case args of
    ["init"]    -> runAction createSchema
    ["destroy"] -> runAction dropSchema
    _           -> putStrLn "Either specify init or destroy."
