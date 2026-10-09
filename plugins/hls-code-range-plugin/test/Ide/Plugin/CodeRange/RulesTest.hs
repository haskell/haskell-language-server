{-# LANGUAGE OverloadedLists #-}

module Ide.Plugin.CodeRange.RulesTest (testTree) where

import           Control.Monad.Trans.Writer.CPS
import           Data.Bifunctor                 (Bifunctor (second))
import qualified Data.Vector                    as V
import           Ide.Plugin.CodeRange.Rules
import           Test.Hls

testTree :: TestTree
testTree =
    testGroup "CodeRange.Rules" [
        testGroup "removeInterleaving" $
            let check :: [CodeRange] -> ([CodeRange], [Log]) -> Assertion
                check input want =
                    second (fmap LogEq) (runWriter (removeInterleaving input)) @?= second (fmap LogEq) want
                mkNode :: UInt -> UInt -> CodeRange
                mkNode startCol endCol =
                    CodeRange (Range (Position 1 startCol) (Position 1 endCol)) [] CodeKindRegion
            in [
                testCase "empty list" $ check [] ([], []),
                testCase "one" $ check [mkNode 1 5] ([mkNode 1 5], []),
                testCase "two, without intersection" $ check [mkNode 1 5, mkNode 5 6] ([mkNode 1 5, mkNode 5 6], []),
                testCase "two, with intersection" $ let (x, y) = (mkNode 1 5, mkNode 2 4)
                    in check [x, y] ([mkNode 1 2, mkNode 2 4], [LogFoundInterleaving x y]),
                testCase "three, with intersection" $ let (x, y, z) = (mkNode 1 10, mkNode 2 6, mkNode 4 12)
                    in check [x, y, z] ([mkNode 1 2, mkNode 2 4, mkNode 4 12],
                        [LogFoundInterleaving x y, LogFoundInterleaving y z])
            ],
        testGroup "simplify" $
            let mkNode :: UInt -> UInt -> V.Vector CodeRange -> CodeRange
                mkNode startCol endCol children =
                    CodeRange (Range (Position 1 startCol) (Position 1 endCol)) children CodeKindRegion
            in [
                testCase "one level should not change" $
                    let codeRange = mkNode 1 5 []
                     in codeRange @=? simplify codeRange,
                testCase "dedup 3 nested layers" $
                    let input =
                            mkNode 1 10 [
                                mkNode 1 5 [],
                                mkNode 5 10 [
                                    mkNode 5 10 [
                                        mkNode 5 10 [
                                            mkNode 6 10 []
                                        ]
                                    ]
                                ]
                            ]
                        want =
                            mkNode 1 10 [
                                mkNode 1 5 [],
                                mkNode 5 10 [
                                    mkNode 6 10 []
                                ]
                            ]
                     in want @=? simplify input,
                testCase "should not dedup node that has multiple children" $
                    let input =
                            mkNode 1 10 [
                                mkNode 1 10 [],
                                mkNode 2 10 []
                            ]
                     in simplify input @?= input,
                testCase "dedup simple two layers" $
                    let input = mkNode 1 10 [ mkNode 1 10 []]
                     in simplify input @?= mkNode 1 10 []
            ]
    ]

newtype LogEq = LogEq Log
    deriving Show

instance Eq LogEq where
    LogEq (LogShake _) == LogEq (LogShake _) = True
    LogEq LogNoAST == LogEq LogNoAST = True
    LogEq (LogFoundInterleaving left right) == LogEq (LogFoundInterleaving left' right') =
        left == left' && right == right'
    LogEq _ == LogEq _ = False
