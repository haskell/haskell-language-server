{-# LANGUAGE OverloadedStrings #-}

module ConstructorHoverTests (tests) where

import           Config
import           Hover
import           Test.Hls
import           Test.Hls.FileSystem (copyDir)

tests :: TestTree
tests =
    testGroup
        "constructor hover (#2904)"
        [ testGroup
            "Constructors.hs"
            [ test "A" "Constructors.hs" (Position 2 9) [ExpectHoverText ["A :: A"]]
            , test "B" "Constructors.hs" (Position 3 9) [ExpectHoverText ["B :: Int -> Word -> Bool -> B"], ExpectHoverExcludeText ["%1 ->"]]
            , test "C" "Constructors.hs" (Position 4 9) [ExpectHoverText ["C :: [Int] -> Bool -> C"], ExpectHoverExcludeText ["%1 ->"]]
            , test "D" "Constructors.hs" (Position 5 9) [ExpectHoverText ["D :: Int -> Bool -> D"], ExpectHoverExcludeText ["%1 ->"]]
            , test "E" "Constructors.hs" (Position 6 9) [ExpectHoverText ["E :: Int -> [Bool] -> E"], ExpectHoverExcludeText ["%1 ->"]]
            , test "F" "Constructors.hs" (Position 7 12) [ExpectHoverText ["F :: Int -> F"], ExpectHoverExcludeText ["%1 ->"]]
            ]
        , testGroup
            "ConstructorsLinear.hs"
            [ test "A" "ConstructorsLinear.hs" (Position 3 9) [ExpectHoverText ["A :: A"]]
            , test "B" "ConstructorsLinear.hs" (Position 4 9) [ExpectHoverText ["B :: Int %1 -> Word %1 -> Bool %1 -> B"]]
            , test "C" "ConstructorsLinear.hs" (Position 5 9) [ExpectHoverText ["C :: [Int] %1 -> Bool %1 -> C"]]
            , test "D" "ConstructorsLinear.hs" (Position 6 9) [ExpectHoverText ["D :: Int %1 -> Bool %1 -> D"]]
            , test "E" "ConstructorsLinear.hs" (Position 7 9) [ExpectHoverText ["E :: Int %1 -> [Bool] %1 -> E"]]
            , test "F" "ConstructorsLinear.hs" (Position 8 12) [ExpectHoverText ["F :: Int %1 -> F"]]
            ]
        ]

test :: String -> FilePath -> Position -> [Expect] -> TestTree
test title fileName pos expectations =
    testWithDummyPlugin title (mkIdeTestFs [copyDir "hover"]) $ do
        doc <- openDoc fileName "haskell"
        waitForProgressDone
        hover <- getHover doc pos
        checkHover hover expectations
