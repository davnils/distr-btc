module Main where

import Control.Concurrent (threadDelay)
import Control.Monad.Trans
import qualified HTrade.Backend.Configuration    as C
import qualified HTrade.Backend.ProxyLayer       as PL
import HTrade.Shared.Utils (backendPort)

main :: IO ()
main = PL.withLayer backendPort $ C.withConfiguration $ do
  confs <- C.loadConfigurations "data/configurations/test"
  liftIO $ putStrLn $ "Loaded configurations: " ++ show confs
  liftIO $ threadDelay $ 60 * 10^6
  liftIO $ putStrLn "Terminating"
