{-# Language DataKinds, OverloadedStrings  #-}

module Main where

import Control.Monad (void)
import Database.Cassandra.CQL
import Data.Monoid ((<>))
import System.Environment

import HTrade.Backend.Storage

createSchema, dropSchema :: Cas ()

createSchema = void $ executeSchema ALL (query schemaQuery) ()
  where
  schemaQuery = "create table " <> marketRawTable <> " " <> marketRawDataSchema

dropSchema =  void $ executeSchema ALL (query $ "drop table " <> marketRawTable) ()

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
