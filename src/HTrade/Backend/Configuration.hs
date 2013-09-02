{-# Language FlexibleContexts, GeneralizedNewtypeDeriving, OverloadedStrings #-}

module HTrade.Backend.Configuration (
  MConfigT, 
  withConfiguration,
  loadConfigurations,
  parseConfigurationFile
) where

import Control.Applicative (Applicative, (<$>))
import qualified Control.Concurrent.Async.Lifted as C
import Control.Error
import Control.Monad
import Control.Monad.Trans
import Control.Monad.Trans.Control
import qualified Control.Monad.State             as S
import Control.Proxy.Concurrent
import qualified Data.Configurator               as CF
import qualified Data.Map                        as M
import qualified Data.List                       as L
import qualified System.Directory                as D

import HTrade.Backend.MarketFetch
import qualified HTrade.Backend.ProxyLayer       as PL
import HTrade.Backend.Types

-- | Internal state used by configuration layer.
--   Maintains a mapping between loaded markets and their corresponding
--   input channels which are used to retrieve control messages.
type ConfigState = M.Map MarketIdentifier (Input ControlMessage)

-- | Configuration monad transformer managing market threads in relation 
--   to loaded and parsed configuration files.
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

-- | Run configuration-related functions over some arbitrary monad.
withConfiguration
  :: Monad m
  => MConfigT m a
  -> m a
withConfiguration routine = liftM fst $ S.runStateT (runConfigMT routine) M.empty

-- | Parse a directory containing market configurations.
--   Returns a list of market identifers updated (loaded or refreshed) and any
--   invalid configuration files.
--
--   TODO: Handle exceptions
loadConfigurations
  :: (Functor m, MonadBaseControl IO m, MonadIO m)
  => FilePath
  -> MConfigT (PL.MProxyT m) (Maybe ([MarketIdentifier], [FilePath]))
loadConfigurations dir = runMaybeT $ do
  liftIO (D.doesDirectoryExist dir) >>= guard
  files <- filter (\file -> not $ L.isPrefixOf "." file) <$> liftIO (D.getDirectoryContents dir)
  let withDirPrefix = map (\file -> dir ++ "/" ++ file) files
  parsed <- zip files <$> liftIO (mapM parseConfigurationFile withDirPrefix)

  let (failedFiles, parsedConfs) = partitionParsed parsed ([], [])
  let loadedIdentifiers = map _marketIdentifier parsedConfs

  current <- M.toList <$> lift S.get

  let currentLabels = map fst current
      remove = currentLabels L.\\ loadedIdentifiers
      new = filterWith notElem parsedConfs currentLabels
      updated = filterWith elem parsedConfs currentLabels

  -- Remove entries that don't exist anymore
  void . withThreads remove $ \threadMap market -> do
    let Just chan = M.lookup market threadMap
    liftIO . atomically $ send chan Shutdown
    S.modify $ M.delete market

  -- Create new threads
  void . withThreads new $ \_ conf -> do
    (input, output) <- liftIO $ spawn Single
    void . lift . lift . C.async $ marketThread output
    liftIO . atomically . send input $ LoadConfiguration conf
    S.modify $ M.insert (_marketIdentifier conf) input

  -- Update existing threads
  void . withThreads updated $ \threadMap conf -> do
    let Just chan = M.lookup (_marketIdentifier conf) threadMap
    liftIO . atomically . send chan $ LoadConfiguration conf

  -- Return affected markets and paths with invalid configurations 
  return (loadedIdentifiers, failedFiles)

  where
  partitionParsed [] acc = acc
  partitionParsed (e:t) (acc1, acc2) = case snd e of
    Nothing -> partitionParsed t (fst e : acc1, acc2)
    Just v -> partitionParsed t (acc1, v : acc2)

  withThreads l f = do
    threads <- lift S.get
    forM l $ f threads

  filterWith _ [] _ = []
  filterWith f (e:t) existing
    | f (_marketIdentifier e) existing = e : filterWith f t existing
    | otherwise = filterWith f t existing

-- | Parse a configuration file from the given file path.
--   Parser error and exceptions results in 'Data.Maybe.Nothing'.
--   All configuration files must be written according to the format
--   specified by the configurator package.
parseConfigurationFile
  :: FilePath
  -> IO (Maybe MarketConfiguration)
parseConfigurationFile path = runMaybeT $ do
  config <- hushT . syncIO $ CF.load [CF.Required path]
  let get name = MaybeT $ CF.lookup config name

  marketId <- liftM2 MarketIdentifier
    (get "name")
    (get "currency")

  liftM5 (MarketConfiguration marketId)
    (get "host")
    (get "path")
    (get "trades")
    (get "orderbook")
    (get "interval")
