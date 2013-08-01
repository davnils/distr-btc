module Main where

import HTrade.Proxy.Proxy

main :: IO ()
main = run defaultHost defaultPort
  where
  defaultHost = "127.0.0.1"
  defaultPort = 1234
