{-# LANGUAGE OverloadedStrings #-}

module Ide.PluginUtilsTest
    ( tests
    ) where

import           Data.Char          (isPrint)
import qualified Data.Text          as T
import           Ide.PluginUtils    (positionInRange, unescape)
import           Language.LSP.Types (Position (Position), Range (Range))
import           Test.Tasty
import           Test.Tasty.HUnit

tests :: TestTree
tests = testGroup "PluginUtils"
    [ unescapeTest
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
