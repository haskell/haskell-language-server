module Ide.PluginUtilsTest
    ( tests
    ) where

import           Ide.PluginUtils    (positionInRange)
import           Language.LSP.Types (Position (Position), Range (Range))
import           Test.Tasty
import           Test.Tasty.HUnit

tests :: TestTree
tests = testGroup "PluginUtils"
    [
    ]
