{-# LANGUAGE OverloadedLists #-}

module Ide.Plugin.CodeRangeTest (testTree) where

import qualified Data.Vector                as V
import           Ide.Plugin.CodeRange
import           Ide.Plugin.CodeRange.Rules
import           Test.Hls

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

        -- TODO: Some more tests can be added on strange cases like
        -- 1. lots of blank lines in between type signature and the body
        -- 2. lots of blank lines in the function itself
        -- etc.
        testGroup "findFoldingRanges" $
            let check :: CodeRange -> [FoldingRange] -> Assertion
                check codeRange = (findFoldingRanges codeRange @?=)

                mkCodeRange :: Position -> Position -> V.Vector CodeRange -> CodeRangeKind -> CodeRange
                mkCodeRange start end children crk = CodeRange (Range start end) children crk
            in [
                -- General test
                testCase "Test General Code Block" $ check
                    (mkCodeRange (Position 1 1) (Position 5 10) [] CodeKindRegion)
                    [],

                -- Tests for code kind
                testCase "Test Code Kind Region" $ check
                    (mkCodeRange (Position 1 1) (Position 5 10) [
                        mkCodeRange (Position 1 2) (Position 3 6) [] CodeKindRegion
                    ] CodeKindRegion)
                    [FoldingRange 1 (Just 2) 3 (Just 6) (Just FoldingRangeKind_Region) Nothing],
                testCase "Test Code Kind Comment" $ check
                    (mkCodeRange (Position 1 1) (Position 5 10) [
                        mkCodeRange (Position 1 2) (Position 3 6) [] CodeKindComment
                    ] CodeKindRegion)
                    [FoldingRange 1 (Just 2) 3 (Just 6) (Just FoldingRangeKind_Comment) Nothing],
                testCase "Test Code Kind Import" $ check
                    (mkCodeRange (Position 1 1) (Position 5 10) [
                        mkCodeRange (Position 1 2) (Position 3 6) [] CodeKindImports
                    ] CodeKindRegion)
                    [FoldingRange 1 (Just 2) 3 (Just 6) (Just FoldingRangeKind_Imports) Nothing],

                -- Test for Code Portions with children
                testCase "Test Children" $ check
                    (mkCodeRange (Position 1 1) (Position 5 10) [
                        mkCodeRange (Position 1 2) (Position 3 6) [
                            mkCodeRange (Position 1 3) (Position 1 5) [] CodeKindRegion
                        ] CodeKindRegion,
                        mkCodeRange (Position 3 7) (Position 5 10) [] CodeKindRegion
                    ] CodeKindRegion)
                    [ FoldingRange 1 (Just 2) 3 (Just 6) (Just FoldingRangeKind_Region) Nothing,
                        FoldingRange 1 (Just 3) 1 (Just 5) (Just FoldingRangeKind_Region) Nothing,
                        FoldingRange 3 (Just 7) 5 (Just 10) (Just FoldingRangeKind_Region) Nothing
                    ]
            ],

        testGroup "createFoldingRange" $
        let check :: CodeRange -> Maybe FoldingRange -> Assertion
            check codeRange = (createFoldingRange codeRange @?=)

            mkCodeRange :: Position -> Position -> V.Vector CodeRange -> CodeRangeKind -> CodeRange
            mkCodeRange start end children crk = CodeRange (Range start end) children crk
        in [
            -- General tests
            testCase "Test General Code Block" $ check
                (mkCodeRange (Position 1 1) (Position 5 10) [] CodeKindRegion)
                (Just (FoldingRange 1 (Just 1) 5 (Just 10) (Just FoldingRangeKind_Region) Nothing)),
            -- If a range has the same start and end line it need not be folded so Nothing is expected
            testCase "Test Same Start Line" $ check
                (mkCodeRange (Position 1 1) (Position 1 10) [] CodeKindRegion)
                (Just (FoldingRange 1 (Just 1) 1 (Just 10) (Just FoldingRangeKind_Region) Nothing))
        ]
    ]
