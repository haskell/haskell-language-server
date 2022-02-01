module Main where

import qualified Ide.PluginUtilsTest          as PluginUtilsTest
import           Test.Tasty
import           Test.Tasty.Ingredients.Rerun

main :: IO ()
main = defaultMainWithRerun tests

tests :: TestTree
tests = testGroup "Main"
    [ PluginUtilsTest.tests
    ]
