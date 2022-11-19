{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications  #-}

module Ide.PluginUtilsTest
    ( tests
    ) where

import           Data.Char             (isPrint)
import qualified Data.Set              as Set
import qualified Data.Text             as T
import qualified Ide.Plugin.RangeMap   as RangeMap
import           Ide.PluginUtils       (positionInRange, unescape)
import           Language.LSP.Types    (Position (..), Range (Range), UInt,
                                        isSubrangeOf)
import           Test.Tasty
import           Test.Tasty.HUnit
import           Test.Tasty.QuickCheck

tests :: TestTree
tests = testGroup "PluginUtils"
    [ unescapeTest
    , localOption (QuickCheckMaxSize 10000) $
        testProperty "RangeMap-List filtering identical" (prop_rangemapListEq @Int)
    ]

unescapeTest :: TestTree
unescapeTest = testGroup "unescape"
    [ testCase "no double quote" $
        unescape "hello世界" @?= "hello世界"
    , testCase "whole string quoted" $
        unescape "\"hello\\19990\\30028\"" @?= "\"hello世界\""
    , testCase "text before quotes should not be unescaped" $
        unescape "\\19990a\"hello\\30028\"" @?= "\\19990a\"hello界\""
    , testCase "some text after quotes" $
        unescape "\"hello\\19990\\30028\"abc" @?= "\"hello世界\"abc"
    , testCase "many pairs of quote" $
        unescape "oo\"hello\\19990\\30028\"abc\"\1087\1088\1080\1074\1077\1090\"hh" @?= "oo\"hello世界\"abc\"привет\"hh"
    , testCase "double quote itself should not be unescaped" $
        unescape "\"\\\"\\19990o\"" @?= "\"\\\"世o\""
    , testCase "control characters should not be escaped" $
        unescape "\"\\n\\t\"" @?= "\"\\n\\t\""
    ]

genRange :: Gen Range
genRange = do
  x1 <- genPosition
  delta <- genRangeLength
  let x2 = x1 { _character = _character x1 + delta }
  pure $ Range x1 x2
  where
    genRangeLength :: Gen UInt
    genRangeLength = fromInteger <$> chooseInteger (5, 50)

genPosition :: Gen Position
genPosition = Position
  <$> (fromInteger <$> chooseInteger (0, 1000))
  <*> (fromInteger <$> chooseInteger (0, 150))

instance Arbitrary Range where
  arbitrary = genRange

prop_rangemapListEq :: (Show a, Eq a, Ord a) => Range -> [(Range, a)] -> Property
prop_rangemapListEq r xs =
  Set.fromList ((map snd . filter (isSubrangeOf r . fst)) xs)
    === Set.fromList (RangeMap.filterByRange r (RangeMap.fromList' xs))
