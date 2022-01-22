module Main where

import qualified Ide.PluginUtilsTest as PluginUtilsTest
import           Test.Tasty

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = testGroup "Main"
    [ PluginUtilsTest.tests
    ]
