{-# LANGUAGE OverloadedLists #-}

module Ide.Plugin.CodeRangeTest (testTree) where

import qualified Data.Vector                as V
import           Ide.Plugin.CodeRange
import           Ide.Plugin.CodeRange.Rules
import           Test.Hls
import           Test.Tasty.HUnit

testTree :: TestTree
testTree =
    testGroup "CodeRange" [
        testGroup "findPosition" $
            let check :: Position -> CodeRange -> Maybe SelectionRange -> Assertion
                check position codeRange = (findPosition position codeRange @?=)

                mkCodeRange :: Position -> Position -> V.Vector CodeRange -> CodeRange
                mkCodeRange start end children = CodeRange (Range start end) children CodeKindRegion
            in [
                testCase "not in range" $ check
                    (Position 10 1)
                    (mkCodeRange (Position 1 1) (Position 5 10) [])
                    Nothing,
                testCase "in top level range" $ check
                    (Position 3 8)
                    (mkCodeRange (Position 1 1) (Position 5 10) [])
                    (Just $ SelectionRange (Range (Position 1 1) (Position 5 10)) Nothing),
                testCase "in the gap between children, in parent" $ check
                    (Position 3 6)
                    (mkCodeRange (Position 1 1) (Position 5 10) [
                        mkCodeRange (Position 1 1) (Position 3 6) [],
                        mkCodeRange (Position 3 7) (Position 5 10) []
                    ])
                    (Just $ SelectionRange (Range (Position 1 1) (Position 5 10)) Nothing),
                testCase "before all children, in parent" $ check
                    (Position 1 1)
                    (mkCodeRange (Position 1 1) (Position 5 10) [
                        mkCodeRange (Position 1 2) (Position 3 6) [],
                        mkCodeRange (Position 3 7) (Position 5 10) []
                    ])
                    (Just $ SelectionRange (Range (Position 1 1) (Position 5 10)) Nothing),
                testCase "in children, in parent" $ check
                    (Position 2 1)
                    (mkCodeRange (Position 1 1) (Position 5 10) [
                        mkCodeRange (Position 1 2) (Position 3 6) [],
                        mkCodeRange (Position 3 7) (Position 5 10) []
                    ])
                    (Just $ SelectionRange (Range (Position 1 2) (Position 3 6)) $ Just
                        ( SelectionRange (Range (Position 1 1) (Position 5 10)) Nothing
                        )
                    )
            ],
        testGroup "findFoldingRanges" $
            let check :: CodeRange -> [FoldingRange] -> Assertion
                check codeRange = (findFoldingRanges codeRange @?=)

                mkCodeRange :: Position -> Position -> V.Vector CodeRange -> CodeRange
                mkCodeRange start end children = CodeRange (Range start end) children CodeKindRegion
            in [
                testCase "General" $ check
                (mkCodeRange (Position 1 1) (Position 5 10) [])
                [FoldingRange 1 (Just 1) 5 (Just 10) (Just FoldingRangeRegion)]
            ]
    ]
