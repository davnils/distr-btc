{-# OPTIONS_GHC -fno-warn-orphans #-}

module Main where

import Control.Applicative ((<$>), (<*>))
import Control.Monad
import qualified Data.ByteString.Char8        as B
import Data.Char (isAscii, isAlphaNum)
import Data.Monoid ((<>))
import System.IO (hClose, hPutStr)
import System.IO.Temp (openTempFile)
import qualified Test.QuickCheck              as Q
import qualified Test.QuickCheck.Monadic      as Q

import qualified HTrade.Backend.Configuration as C
import qualified HTrade.Backend.Types         as T
import HTrade.Test.Utils (checkTestCases, quickcheckTestRuns)

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

-- | Serialize a configuration to the configurator-format being used.
showConfiguration :: T.MarketConfiguration -> String
showConfiguration conf = unlines . map showRow $ 
  [
    ("name"       , s $ T._marketName $ T._marketIdentifier conf),
    ("currency"   , s $ T._currency $ T._marketIdentifier conf),
    ("host"       , s $ T._marketHost conf),
    ("path"       , s $ T._marketPath conf),
    ("trades"     , s $ T._marketTrades conf),
    ("orderbook"  , s $ T._marketOrders conf),
    ("interval"   , s $ T._marketInterval conf)
  ]
  where
  s :: Show a => a -> String
  s = show
  showRow (key, val) = key <> " = " <> val

-- | Generate a configuration, output it to a temporary file, 
--   and verify that the parsed results are equal.
verifyConfigurationParser :: IO Bool
verifyConfigurationParser  = do
  let args = Q.stdArgs { Q.maxSuccess = quickcheckTestRuns }
  res <- Q.quickCheckWithResult args tester
  case res of
    Q.Success{} -> return True
    _ -> return False
  where
  tester genConf = Q.monadicIO $ do
    (path, handle) <- Q.run $ openTempFile "/tmp/" "configuration-parser-test.conf"
    Q.run $ hPutStr handle (showConfiguration genConf) >> hClose handle
    parsedConf <- Q.run $ C.parseConfigurationFile path
    Q.assert $ parsedConf == Just genConf

-- | Call tests and indicate success in return code.
main :: IO ()
main = checkTestCases [
  ("verifyConfigurationParser", verifyConfigurationParser)
  ]
