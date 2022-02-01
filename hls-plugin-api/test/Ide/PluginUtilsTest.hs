module Ide.PluginUtilsTest
    ( tests
    ) where

import           Ide.PluginUtils    (positionInRange)
import           Language.LSP.Types (Position (Position), Range (Range))
import           Test.Tasty
import           Test.Tasty.HUnit

tests :: TestTree
tests = testGroup "PluginUtils"
    [ positionInRangeTest
    ]

positionInRangeTest :: TestTree
positionInRangeTest = testGroup "positionInRange"
    [ testCase "single line, after the end" $
        positionInRange (Position 1 10) (Range (Position 1 1) (Position 1 3)) @?= False
    , testCase "single line, before the begining" $
        positionInRange (Position 1 0) (Range (Position 1 1) (Position 1 6)) @?= False
    , testCase "single line, in range" $
        positionInRange (Position 1 5) (Range (Position 1 1) (Position 1 6)) @?= True
    , testCase "multiline, in range" $
        positionInRange (Position 3 5) (Range (Position 1 1) (Position 5 6)) @?= True
    , testCase "multiline, out of range" $
        positionInRange (Position 3 5) (Range (Position 3 6) (Position 4 10)) @?= False
    ]
