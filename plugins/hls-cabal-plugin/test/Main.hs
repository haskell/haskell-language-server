{-# LANGUAGE OverloadedStrings #-}
module Main
  ( main
  ) where

import qualified Ide.Plugin.Cabal.Parse as Lib
import qualified Data.Text               as T
import qualified Language.LSP.Types.Lens as L
import           Ide.Plugin.Cabal
import           System.FilePath
import           Test.Hls
import           Test.Hls.Util           (onlyWorkForGhcVersions)
import           Test.Tasty.HUnit        (assertFailure, testCase, (@?=))

main :: IO ()
main = defaultTestRunner tests

pragmasPlugin :: PluginDescriptor IdeState
pragmasPlugin = descriptor mempty "cabal"

tests :: TestTree
tests =
  testGroup "cabal"
  [ testCase "parsing works" $ do
      parseRes <- Lib.parseCabalFile "test/testdata/simple.cabal"
      goldenShowStr <- readFile "test/testdata/simple.cabal.golden.txt"
      show parseRes @?= goldenShowStr
  ]

-- Orphans
instance Eq Lib.PWarning where
  Lib.PWarning pWarnType1 pos1 str1 == Lib.PWarning pWarnType2 pos2 str2 =
    pWarnType1 == pWarnType2 && pos1 == pos2 && str1 == str2

instance Eq Lib.PError where
  Lib.PError pos1 str1 == Lib.PError pos2 str2 =
    pos1 == pos2 && str1 == str2
