{-# LANGUAGE OverloadedStrings #-}
module Symbol (tests) where

import Control.Monad.IO.Class
import Data.List
import Language.Haskell.LSP.Test as Test
import Language.Haskell.LSP.Types
import Language.Haskell.LSP.Types.Capabilities
import Test.Hls.Util
import Test.Tasty
import Test.Tasty.ExpectedFailure (ignoreTestBecause)
import Test.Tasty.HUnit

tests :: TestTree
tests = testGroup "document symbols" [
      pre310Tests
    , v310Tests
    ]

v310Tests :: TestTree
v310Tests = testGroup "3.10 hierarchical document symbols" [
    ignoreTestBecause "Broken" $ testCase "provides nested data types and constructors" $ runSession hlsCommand fullCaps "test/testdata" $ do
        doc <- openDoc "Symbols.hs" "haskell"
        Left symbs <- getDocumentSymbols doc

        let myData = DocumentSymbol "MyData" (Just "") SkClass Nothing myDataR myDataSR (Just (List [a, b]))
            a = DocumentSymbol "A" (Just "") SkConstructor Nothing aR aSR (Just mempty)
            b = DocumentSymbol "B" (Just "") SkConstructor Nothing bR bSR (Just mempty)

        liftIO $ myData `elem` symbs @? "Contains symbol"

    ,ignoreTestBecause "Broken" $ testCase "provides nested where functions" $ runSession hlsCommand fullCaps "test/testdata" $ do
        doc <- openDoc "Symbols.hs" "haskell"
        Left symbs <- getDocumentSymbols doc

        let foo = DocumentSymbol "foo" (Just "") SkFunction Nothing fooR fooSR (Just (List [bar]))
            bar = DocumentSymbol "bar" (Just "") SkFunction Nothing barR barSR (Just (List [dog, cat]))
            dog = DocumentSymbol "dog" (Just "") SkVariable Nothing dogR dogSR (Just mempty)
            cat = DocumentSymbol "cat" (Just "") SkVariable Nothing catR catSR (Just mempty)

        liftIO $ foo `elem` symbs @? "Contains symbol"

    , ignoreTestBecause "Broken" $ testCase "provides pattern synonyms" $ runSession hlsCommand fullCaps "test/testdata" $ do
        doc <- openDoc "Symbols.hs" "haskell"
        Left symbs <- getDocumentSymbols doc

        let testPattern = DocumentSymbol "TestPattern"
                (Just "") SkFunction Nothing testPatternR testPatternSR (Just mempty)

        liftIO $ testPattern `elem` symbs @? "Contains symbol"
    ]

-- TODO: Test module, imports

pre310Tests :: TestTree
pre310Tests = testGroup "pre 3.10 symbol information" [
    ignoreTestBecause "Broken" $ testCase "provides nested data types and constructors" $ runSession hlsCommand oldCaps "test/testdata" $ do
        doc@(TextDocumentIdentifier testUri) <- openDoc "Symbols.hs" "haskell"
        Right symbs <- getDocumentSymbols doc

        let myData = SymbolInformation "MyData" SkClass Nothing (Location testUri myDataR) Nothing
            a = SymbolInformation "A" SkConstructor Nothing (Location testUri aR) (Just "MyData")
            b = SymbolInformation "B" SkConstructor Nothing (Location testUri bR) (Just "MyData")

        liftIO $ [myData, a, b] `isInfixOf` symbs @? "Contains symbols"

    ,ignoreTestBecause "Broken" $ testCase "provides nested where functions" $ runSession hlsCommand oldCaps "test/testdata" $ do
        doc@(TextDocumentIdentifier testUri) <- openDoc "Symbols.hs" "haskell"
        Right symbs <- getDocumentSymbols doc

        let foo = SymbolInformation "foo" SkFunction Nothing (Location testUri fooR) Nothing
            bar = SymbolInformation "bar" SkFunction Nothing (Location testUri barR) (Just "foo")
            dog = SymbolInformation "dog" SkVariable Nothing (Location testUri dogR) (Just "bar")
            cat = SymbolInformation "cat" SkVariable Nothing (Location testUri catR) (Just "bar")

        -- Order is important!
        liftIO $ [foo, bar, dog, cat] `isInfixOf` symbs @? "Contains symbols"
    ]

oldCaps :: ClientCapabilities
oldCaps = capsForVersion (LSPVersion 3 9)
-- Some common ranges and selection ranges in Symbols.hs
fooSR :: Range
fooSR = Range (Position 5 0) (Position 5 3)
fooR :: Range
fooR  = Range (Position 5 0) (Position 7 43)
barSR :: Range
barSR = Range (Position 6 8) (Position 6 11)
barR :: Range
barR  = Range (Position 6 8) (Position 7 43)
dogSR :: Range
dogSR = Range (Position 7 17) (Position 7 20)
dogR :: Range
dogR  = Range (Position 7 16) (Position 7 43)
catSR :: Range
catSR = Range (Position 7 22) (Position 7 25)
catR :: Range
catR  = Range (Position 7 16) (Position 7 43)
myDataSR :: Range
myDataSR = Range (Position 9 5) (Position 9 11)
myDataR :: Range
myDataR  = Range (Position 9 0) (Position 10 22)
aSR :: Range
aSR = Range (Position 9 14) (Position 9 15)
aR :: Range
aR  = Range (Position 9 14) (Position 9 19)
bSR :: Range
bSR = Range (Position 10 14) (Position 10 15)
bR :: Range
bR  = Range (Position 10 14) (Position 10 22)
testPatternSR :: Range
testPatternSR = Range (Position 13 8) (Position 13 19)
testPatternR :: Range
testPatternR = Range (Position 13 0) (Position 13 27)
