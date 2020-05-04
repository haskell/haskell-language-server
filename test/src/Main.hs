module Main where

import Test.Tasty
import Ide.Plugin.Brittany (tests)

main :: IO ()
main = defaultMain tree

tree :: TestTree
tree = testGroup "HIE" [
    Ide.Plugin.Brittany.tests
    ]