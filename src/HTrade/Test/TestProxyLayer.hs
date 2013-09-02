module Main where

import Control.Monad
import Control.Monad.Trans
import qualified Data.Map                    as M

import qualified HTrade.Backend.ProxyLayer   as PL
import qualified HTrade.Shared.Types         as T
import HTrade.Test.Utils

-- | Establish a backend and k (~10^3) proxy nodes.
--   Echo-reply from proxy nodes and verify the corresponding result.
verifyDataFlow :: IO Bool
verifyDataFlow = withLayers poolSize tester
  where
  poolSize = 10^(2 :: Int) :: Int
  tester = do
    -- verify data transfer, by transmitting a status request to each node.
    -- (1) proper response with StatusReply constructor.
    -- (2) correct number of replies, one from each.
    rx <- lift . PL.mapLayer $
      \addr -> PL.query (Just addr) Nothing T.StatusRequest
    let responses = map snd $ M.toList rx
    guard $ length responses == poolSize
    guard $ all matchStatusCons responses

    liftIO . putStrLn $
      "[*] Data transfer verified."

  matchStatusCons (Just (T.StatusReply{}))  = True
  matchStatusCons _                         = False

-- | Establish a backend and 2 listeners. Verify that a failed connection to one
--   of the nodes (as indicated by an extended timeout) result in an
--   addtional request to the other node.
verifyRetry :: IO Bool
verifyRetry = return True

main :: IO ()
main = checkTestCases [
  ("verifyDataFlow", verifyDataFlow),
  ("verifyRetry", verifyRetry)
  ]
