{-# Language OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

--------------------------------------------------------------------
-- |
-- Module: HTrade.Test.TestConfiguration
--
-- Test verifying the functionality of configuration parsing and loading.

module Main where

import           Control.Applicative             ((<$>), (<*>))
import           Control.Monad                   (guard, liftM2)
import           Control.Monad.Trans             (lift, liftIO)
import qualified Data.ByteString.Char8           as B
import           Data.Char                       (isAscii, isAlphaNum)
import qualified System.Directory                as D
import           System.IO                       (hClose, hPutStr)
import           System.IO.Temp                  (openTempFile)
import qualified Test.QuickCheck                 as Q
import qualified Test.QuickCheck.Monadic         as Q

import qualified HTrade.Backend.Configuration    as C
import qualified HTrade.Backend.Types            as T
import           HTrade.Test.Utils

-- | Quickcheck instance for bytestrings, limited to alphanum ascii.
instance Q.Arbitrary B.ByteString where
  arbitrary = fmap B.pack $ Q.suchThat Q.arbitrary filterTest
    where
    filterTest s = notElem '\\' s && and (map isAscii s ++ map isAlphaNum s)

-- | Quickcheck instance for market identifiers, only lifts existing.
instance Q.Arbitrary T.MarketIdentifier where
  arbitrary = liftM2 T.MarketIdentifier Q.arbitrary Q.arbitrary

-- | Quickcheck instance for market configurations, only lifts existing.
instance Q.Arbitrary T.MarketConfiguration where
  arbitrary = T.MarketConfiguration
    <$> Q.arbitrary
    <*> Q.arbitrary
    <*> Q.arbitrary
    <*> Q.arbitrary
    <*> Q.arbitrary
    <*> Q.arbitrary

-- | Generate a configuration, output it to a temporary file, 
--   and verify that the parsed results are equal.
verifyConfigurationParser :: IO Bool
verifyConfigurationParser = withQuickCheck $ \genConf -> Q.monadicIO $ do
  (path, handle) <- Q.run $ openTempFile "/tmp/" filePattern
  Q.run $ hPutStr handle (showConfiguration genConf) >> hClose handle
  parsedConf <- Q.run $ C.parseConfigurationFile path
  Q.assert $ parsedConf == Just genConf
  where
  filePattern = "configuration-parser-test.conf"

-- | Verify that the system is capable of:
--   (1) loading a well-defined configuration file
--   (2) Retaining the loaded configuration (consecutive updates)
verifyConfigurationLoader :: IO Bool
verifyConfigurationLoader = withLayers poolSize $ do
  -- setup temporary directory with a single configuration for testing
  liftIO $ writeTempConf confDir filePattern testConf

  -- load the given configuration
  liftIO $ putStrLn "[*] Loading test configuration"

  loadResult <- lift $ C.withConfiguration $ do
    let load = C.loadConfigurations confDir
    liftM2 (,) load load

  let check output = fmap fst output == Just [T._marketIdentifier testConf]
  guard $ check (fst loadResult) && check (snd loadResult)

  liftIO $ putStrLn "[*] Test configuration verified"

  -- cleanup
  liftIO $ D.removeDirectoryRecursive confDir
  where
  poolSize = 10
  confDir = "/tmp/sample_configurations/"
  filePattern = "configuration-loader-test.conf"
  testConf = T.MarketConfiguration
        (T.MarketIdentifier "testMarket" "sek")
        "http://localhost"
        ""
        "trades.json"
        "orders.json"
        1000000

-- | Call tests and indicate success in return code.
main :: IO ()
main = checkTestCases [
  ("verifyConfigurationParser", verifyConfigurationParser),
  ("verifyConfigurationLoader", verifyConfigurationLoader)
  ]
