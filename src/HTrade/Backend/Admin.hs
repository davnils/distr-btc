{-# Language DataKinds, OverloadedStrings  #-}

module Main where

import Control.Monad (forM_)
import Database.Cassandra.CQL
import Data.Monoid ((<>))
import Data.Text (Text)
import System.Environment

import HTrade.Backend.Storage

createSchema, dropSchema :: Cas ()

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

createSchema = forM_ tables $ \(table, schema) -> 
  executeSchema ALL (query $ "create table " <> table <> " " <> schema) ()

dropSchema =  forM_ tables $ \(table, _) -> executeSchema ALL (query $ "drop table " <> table) ()

runAction :: Cas a -> IO a
runAction action = do
  pool <- newPool [(cassandraHost, cassandraPort)] marketKeyspace
  runCas pool action

main :: IO ()
main = do
  args <- getArgs
  case args of
    ["init"]    -> runAction createSchema
    ["destroy"] -> runAction dropSchema
    _           -> putStrLn "Either specify init or destroy."
