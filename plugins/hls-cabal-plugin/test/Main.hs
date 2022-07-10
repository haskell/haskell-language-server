{-# LANGUAGE OverloadedStrings #-}
module Main
  ( main
  ) where

import qualified Data.Text               as T
import qualified Language.LSP.Types.Lens as L
import           Ide.Plugin.Cabal
import           System.FilePath
import           Test.Hls
import           Test.Hls.Util           (onlyWorkForGhcVersions)

main :: IO ()
main = defaultTestRunner tests

pragmasPlugin :: PluginDescriptor IdeState
pragmasPlugin = descriptor mempty "cabal"

tests :: TestTree
tests =
  testGroup "cabal"
  []