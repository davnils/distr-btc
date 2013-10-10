module Main where

import OpenSSL (withOpenSSL)

import HTrade.Proxy.Proxy
import HTrade.Shared.Utils (backendPort)

main :: IO ()
main = withOpenSSL $ run defaultHost defaultPort
  where
  defaultHost = "127.0.0.1"
  defaultPort = backendPort
