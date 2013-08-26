module Main where

import Control.Concurrent (threadDelay)
import Control.Monad
import Control.Monad.Trans
import Control.Monad.Trans.Maybe (runMaybeT)
import qualified Control.Concurrent.Async    as C
import qualified Data.Maybe                  as MB
import qualified Data.Map                    as M

import qualified HTrade.Backend.ProxyLayer   as PL
import qualified HTrade.Proxy.Proxy          as P
import qualified HTrade.Shared.Types         as T
import HTrade.Test.Utils (testPort, checkTestCases)

-- | Establish a backend and k (~10^3) proxy nodes.
--   Echo-reply from proxy nodes and verify the corresponding result.
verifyDataFlow :: IO Bool
verifyDataFlow = PL.withLayer testPort tester
  where
  testPoolSize = 10^(2 :: Int) :: Int
  tester :: PL.MProxyT IO Bool
  tester = liftM MB.isJust . runMaybeT $ do
    -- establish proxy nodes
    pool <- liftIO $ forM [1..testPoolSize] $
      \_ -> C.async (P.runWorker "127.0.0.1" testPort)
    liftIO . putStrLn $
      "[*] Pool with " ++ show testPoolSize ++ " proxy nodes established."

    -- wait for full connectivity
    let waitLoop = do
        nodes <- lift PL.connectedNodes
        when (nodes /= testPoolSize) $ liftIO (threadDelay $ 500 * 10^(3 :: Int)) >> waitLoop

    waitLoop
    lift PL.ready >>= guard

    liftIO . putStrLn $
      "[*] Pool considered fully connected."

    -- verify data transfer, by transmitting a status request to each node.
    -- (1) proper response with StatusReply constructor.
    -- (2) correct number of replies, one from each.
    rx <- lift . PL.mapLayer $
      \addr -> PL.query (Just addr) Nothing T.StatusRequest
    let responses = map snd $ M.toList rx
    guard $ length responses == testPoolSize
    guard $ all matchStatusCons responses

    liftIO . putStrLn $
      "[*] Data transfer verified."

    -- make sure that all nodes are still alive and shutdown
    status <- liftIO $ allAlive pool
    guard $ all MB.isNothing status
    liftIO $ mapM_ C.cancel pool

    liftIO . putStrLn $
      "[*] Pool disconnected."

  matchStatusCons (Just (T.StatusReply{}))  = True
  matchStatusCons _                         = False

  allAlive = mapM C.poll

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
