{-# Language FlexibleContexts, GeneralizedNewtypeDeriving #-}

module HTrade.Backend.Configuration where

import Control.Applicative (Applicative, (<$>))
import qualified Control.Concurrent.Async.Lifted as C
import Control.Monad
-- import Control.Monad.Base
import Control.Monad.Trans
import Control.Monad.Trans.Maybe (runMaybeT)
import qualified Control.Monad.State             as S
import Control.Proxy.Concurrent
import qualified Data.ByteString.Char8           as B
import qualified Data.Map                        as M
import Data.Maybe (isNothing)
import Data.List (isPrefixOf, partition, deleteFirstsBy)
import qualified System.Directory                as D

import HTrade.Backend.MarketFetch
import HTrade.Backend.Types
import HTrade.Shared.Types

type ConfigState = M.Map MarketIdentifier (Input ControlMessage)

newtype MConfigT m a
 = MConfigT
 {
   runConfigMT :: S.StateT ConfigState m a
 }
 deriving (
   Applicative,
   Functor,
   Monad,
   MonadIO,
   MonadTrans,
   S.MonadState ConfigState
 )
-- | Parse a directory containing market configurations.
--   Returns a list of market identifers updated (loaded or refreshed) and any
--   invalid configuration files.
--   TODO: Handle exceptions
loadConfigurations
  :: MonadIO m
  => FilePath
  -> MConfigT m (Maybe ([MarketIdentifier], [FilePath]))
loadConfigurations dir = runMaybeT $ do
  liftIO (D.doesDirectoryExist dir) >>= guard
  files <- filter (\file -> not $ isPrefixOf "." file) <$> liftIO (D.getDirectoryContents dir)
  parsed <- zip files <$> liftIO (mapM parseConfigurationFile files)
  
  -- önskar att få ut:
  -- filnamn tillhörande alla som blev Nothing.
  -- v i (Just v) (oordnad) för processering.

  let (failedFiles, parsedConfs) = partitionParsed parsed []
  let loadedIdentifiers = map _marketIdentifier parsedConfs

  loaded <- M.toList <$> lift S.get

  let remove = deleteFirstsBy elem (map fst loaded) loadedIdentifiers
      -- updated = loaded `intersect` loadedIdentifiers
      -- new = loadedIdentifiers `intersect` loaded

  -- Remove entries that don't exist anymore
  withThreads remove $ \threadMap market -> do
    let Just chan = M.lookup market threadMap
    liftIO . atomically $ send chan Shutdown
    S.modify $ \s -> M.delete market

  -- Update existing threads
  {- withThreads updated $ \threadMap conf -> do
    let Just chan = M.lookup market threadMap
    liftIO . atomically . send chan $ LoadConfiguration market

  -- Create new threads
  withThreads new $ \threadMap conf -> do
    (input, output) <- liftIO $ spawn Single
    void . C.async $ marketThread output
    liftIO . atomically . send input $ LoadConfiguration market
    S.modify $ \s -> M.insert (_marketIdentifier conf) input -}

  -- Return paths with invalid configurations and affected markets
  return (loadedIdentifiers, failedFiles)

  where
  partitionParsed [] = id
  partitionParsed (e:t) (acc1, acc2) = case snd e of
    snd -> partitionParsed t (fst e : acc1, acc2)
    Just v -> partitionParsed t (acc1, v : acc2)

  withThreads l f = do
    threads <- lift S.get
    forM l $ f threads

parseConfigurationFile
  :: FilePath
  -> IO (Maybe MarketConfiguration)
parseConfigurationFile = undefined
