--------------------------------------------------------------------
-- |
-- Module: HTrade.Backend.Backend
--
-- Main entry point for the backend service.
-- The backend service loads a set of configurations (initially from a default directory)
-- and continues to accept commands from the stdin.

module Main where

import Control.Applicative ((<$>))
import Data.List (intercalate)
import Data.Monoid ((<>))
import Control.Monad (liftM)
import Control.Monad.Trans
import System.IO

import qualified HTrade.Backend.Configuration    as C
import qualified HTrade.Backend.ProxyLayer       as PL
import qualified HTrade.Backend.Types            as PL
import HTrade.Shared.Utils (backendPort)

-- | Read, parse, and interpret a command from the given current directory.
runShell cwd = liftIO (putStr "> " >> hFlush stdout >> getLine) >>= parse . words
  where
  parse ("load":dir:[]) =
    C.loadConfigurations dir >>= printAndRepeat dir

  parse ("reload":[]) =
    C.loadConfigurations cwd >>= printAndRepeat cwd

  parse ("stats":[]) = do
    loaded <- map PL._marketName <$> C.getLoadedMarkets
    nodes <- lift PL.connectedNodes
    ready <- liftM (\r -> if r then "ready" else "not ready") (lift PL.ready)

    liftIO . putStrLn $
      "Loaded markets: " <> intercalate ", " (map show loaded) <> "\n" <>
      "Active proxy nodes: " <> show nodes <> " (layer is " <> ready <> ")"

    runShell cwd

  parse ("quit":[]) = C.terminateLoadedMarkets

  parse _ = liftIO (putStrLn $ unlines helpMsg) >> runShell cwd
  helpMsg = ["Commands:", "load <dir>", "reload", "stats", "quit"]

  printAndRepeat dir msg = liftIO (print msg) >> runShell dir

-- | Launch the backend service and load initial configurations
--   from the default directory.
--   Will only terminate after a quit command has been given.
main :: IO ()
main = PL.withLayer backendPort $ C.withConfiguration $ do
  confs <- C.loadConfigurations defaultDir
  liftIO $ putStrLn $ "Initialized with configurations: " ++ show confs
  runShell defaultDir
  liftIO $ putStrLn "Terminating"

  where
  defaultDir = "data/configurations/test"
