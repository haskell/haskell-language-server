{-# LANGUAGE OverloadedLists #-}

module Ide.Plugin.CodeRange.RulesTest (testTree) where

import           Ide.Plugin.CodeRange.Rules
import           Test.Hls
import           Test.Tasty.HUnit

testTree :: TestTree
testTree =
    testGroup "Rules" [
        testGroup "simplify" $
            let simpleCodeRange startCol endCol children =
                    CodeRange (Range (Position 1 startCol) (Position 1 endCol)) children CodeKindRegion
            in [
                testCase "one level should not change" $
                    let codeRange = simpleCodeRange 1 5 []
                     in codeRange @=? simplify codeRange,
                testCase "dedup 3 nested layers" $
                    let input =
                            simpleCodeRange 1 10 [
                                simpleCodeRange 1 5 [],
                                simpleCodeRange 5 10 [
                                    simpleCodeRange 5 10 [
                                        simpleCodeRange 5 10 [
                                            simpleCodeRange 6 10 []
                                        ]
                                    ]
                                ]
                            ]
                        want =
                            simpleCodeRange 1 10 [
                                simpleCodeRange 1 5 [],
                                simpleCodeRange 5 10 [
                                    simpleCodeRange 6 10 []
                                ]
                            ]
                     in want @=? simplify input,
                testCase "should not dedup node that has multiple children" $
                    let input =
                            simpleCodeRange 1 10 [
                                simpleCodeRange 1 10 [],
                                simpleCodeRange 2 10 []
                            ]
                     in simplify input @?= input,
                testCase "dedup simple two layers" $
                    let input = simpleCodeRange 1 10 [ simpleCodeRange 1 10 []]
                     in simplify input @?= simpleCodeRange 1 10 []
            ]
    ]
