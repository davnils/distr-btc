module Main where

import Control.Concurrent (threadDelay)
import Control.Monad.Trans
import qualified HTrade.Backend.Configuration    as C
import qualified HTrade.Backend.ProxyLayer       as PL
import HTrade.Shared.Utils (backendPort)

main :: IO ()
main = PL.withLayer backendPort $ C.withConfiguration $ do
  liftIO $ putStrLn "Loading configurations.."
  confs <- C.loadConfigurations "data/configurations/test"
  liftIO $ putStrLn $ "Done, result: " ++ show confs
  liftIO $ threadDelay $ 60 * 10^6
  liftIO $ putStrLn "Terminating"
