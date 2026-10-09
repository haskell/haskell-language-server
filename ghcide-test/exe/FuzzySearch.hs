module FuzzySearch (tests) where

import           Data.Maybe            (isJust, mapMaybe)
import           Data.Text             (Text)
import qualified Data.Text             as Text
import           Prelude               hiding (filter)
import           Test.Tasty
import           Test.Tasty.HUnit
import           Test.Tasty.QuickCheck
import           Text.Fuzzy.Parallel

tests :: TestTree
tests =
  testGroup
    "Fuzzy search"
    [ testGroup "match"
        [ testCase "empty" $
            match "" "" @?= Just 0
        , testCase "camel case" $
            match "myImportantField" "myImportantField" @?= Just 262124
        , testCase "a" $
            mapMaybe (matchInput "a") ["", "a", "aa", "aaa", "A", "AA", "aA", "Aa"]
              @?= [("a",3),("aa",3),("aaa",3),("aA",3),("Aa",1)]
        ,  testCase "lowercase words" $
            mapMaybe (matchInput "abc") ["abc", "abcd", "axbc", "axbxc", "def"]
              @?= [("abc", 25), ("abcd", 25), ("axbc", 7), ("axbxc", 5)]
        , testCase "lower upper mix" $
            mapMaybe (matchInput "abc") ["abc", "aBc", "axbC", "axBxC", "def"]
              @?= [("abc", 25), ("aBc", 25), ("axbC", 7), ("axBxC", 5)]
        , testCase "prefixes" $
            mapMaybe (matchInput "alpha") (Text.inits "alphabet")
              @?= [("alpha", 119), ("alphab", 119), ("alphabe", 119), ("alphabet", 119)]
        , testProperty "x `isSubsequenceOf` y => match x y returns Just"
            prop_matchIfSubsequence
        ]
    ]
  where
    matchInput :: Text -> Text -> Maybe (Text, Int)
    matchInput needle candidate = (candidate,) <$> match needle candidate

prop_matchIfSubsequence :: Property
prop_matchIfSubsequence =
  forAll genNonEmptyText $ \haystack ->
    forAll (genSubsequence haystack) $ \needle ->
        isJust (match needle haystack)
  where
    genNonEmptyText =
      Text.pack <$> listOf1 (elements $ ['a'..'z'] ++ ['A'..'Z'] ++ ['0'..'9'])

    genSubsequence :: Text -> Gen Text
    genSubsequence =
        fmap Text.pack . sublistOf . Text.unpack
