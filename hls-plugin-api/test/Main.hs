module Main where

import           Ide.PluginUtils    (positionInRange)
import           Language.LSP.Types (Position (Position), Range (Range))
import           Test.Tasty
import           Test.Tasty.HUnit

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = testGroup "PluginUtils"
    [ testCase "positionInRange" $
        positionInRange (Position 1 10) (Range (Position 1 1) (Position 1 3)) @?= False
    ]
