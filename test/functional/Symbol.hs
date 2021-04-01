{-# LANGUAGE OverloadedStrings #-}
module Symbol (tests) where

import           Control.Lens                    (_Just, ix, to, (^?))
import           Data.List
import           Language.LSP.Test               as Test
import           Language.LSP.Types.Capabilities
import qualified Language.LSP.Types.Lens         as L
import           Test.Hls
import           Test.Hls.Command

tests :: TestTree
tests = testGroup "document symbols" [
      pre310Tests
    , v310Tests
    ]

v310Tests :: TestTree
v310Tests = testGroup "3.10 hierarchical document symbols" [
    testCase "provides nested data types and constructors" $ runSession hlsCommand fullCaps "test/testdata" $ do
        doc <- openDoc "Symbols.hs" "haskell"
        Left symbs <- getDocumentSymbols doc

        let myData = DocumentSymbol "MyData" Nothing SkStruct Nothing Nothing myDataR myDataSR (Just (List [a, b]))
            a = DocumentSymbol "A" Nothing SkConstructor Nothing Nothing aR aSR Nothing
            b = DocumentSymbol "B" Nothing SkConstructor Nothing Nothing bR bSR Nothing
        let myData' = symbs ^? ix 0 . L.children . _Just .to fromList . ix 2

        liftIO $ Just myData == myData' @? "Contains symbol"

    , ignoreTestBecause "extracting symbols from nested wheres not supported" $ testCase "provides nested where functions" $ runSession hlsCommand fullCaps "test/testdata" $ do
        doc <- openDoc "Symbols.hs" "haskell"
        Left symbs <- getDocumentSymbols doc

        let foo = DocumentSymbol "foo" Nothing SkFunction Nothing Nothing fooR fooSR (Just (List [bar]))
            bar = DocumentSymbol "bar" Nothing SkFunction Nothing Nothing barR barSR (Just (List [dog, cat]))
            dog = DocumentSymbol "dog" Nothing SkVariable Nothing Nothing dogR dogSR (Just mempty)
            cat = DocumentSymbol "cat" Nothing SkVariable Nothing Nothing catR catSR (Just mempty)
        let foo' = symbs ^? ix 0 . L.children . _Just .to fromList . ix 1

        liftIO $ Just foo == foo' @? "Contains symbol"

    , ignoreTestBecause "extracting pattern synonym symbols not supported" $ testCase "provides pattern synonyms" $ runSession hlsCommand fullCaps "test/testdata" $ do
        doc <- openDoc "Symbols.hs" "haskell"
        Left symbs <- getDocumentSymbols doc

        let testPattern = DocumentSymbol "TestPattern"
                Nothing SkFunction Nothing Nothing testPatternR testPatternSR (Just mempty)
        let testPattern' = symbs ^? ix 0 . L.children . _Just .to fromList . ix 3

        liftIO $ Just testPattern == testPattern' @? "Contains symbol"

    , testCase "provides imports" $ runSession hlsCommand fullCaps "test/testdata" $ do
        doc <- openDoc "Symbols.hs" "haskell"
        Left symbs <- getDocumentSymbols doc

        let imports = DocumentSymbol "imports" Nothing SkModule Nothing Nothing importsR importsSR (Just (List [importDataMaybe]))
            importDataMaybe = DocumentSymbol "import Data.Maybe" Nothing SkModule Nothing Nothing importDataMaybeR importDataMaybeSR Nothing
        let imports' = symbs ^? ix 0 . L.children . _Just .to fromList . ix 0

        liftIO $ Just imports == imports' @? "Contains symbol"
    ]

pre310Tests :: TestTree
pre310Tests = testGroup "pre 3.10 symbol information" [
    testCase "provides nested data types and constructors" $ runSession hlsCommand oldCaps "test/testdata" $ do
        doc@(TextDocumentIdentifier testUri) <- openDoc "Symbols.hs" "haskell"
        Right symbs <- getDocumentSymbols doc

        let myData = SymbolInformation "MyData" SkStruct Nothing Nothing (Location testUri myDataR) (Just "Symbols")
            a = SymbolInformation "A" SkConstructor Nothing Nothing (Location testUri aR) (Just "MyData")
            b = SymbolInformation "B" SkConstructor Nothing Nothing (Location testUri bR) (Just "MyData")

        liftIO $ [myData, a, b] `isInfixOf` symbs @? "Contains symbols"

    , ignoreTestBecause "extracting symbols from nested wheres not supported" $ testCase "provides nested where functions" $ runSession hlsCommand oldCaps "test/testdata" $ do
        doc@(TextDocumentIdentifier testUri) <- openDoc "Symbols.hs" "haskell"
        Right symbs <- getDocumentSymbols doc

        let foo = SymbolInformation "foo" SkFunction Nothing Nothing (Location testUri fooR) (Just "Symbols")
            bar = SymbolInformation "bar" SkFunction Nothing Nothing (Location testUri barR) (Just "foo")
            dog = SymbolInformation "dog" SkVariable Nothing Nothing (Location testUri dogR) (Just "bar")
            cat = SymbolInformation "cat" SkVariable Nothing Nothing (Location testUri catR) (Just "bar")

        -- Order is important!
        liftIO $ [foo, bar, dog, cat] `isInfixOf` symbs @? "Contains symbols"

    , ignoreTestBecause "extracting pattern synonym symbols not supported" $ testCase "provides pattern synonyms" $ runSession hlsCommand oldCaps "test/testdata" $ do
        doc@(TextDocumentIdentifier testUri) <- openDoc "Symbols.hs" "haskell"
        Right symbs <- getDocumentSymbols doc

        let testPattern = SymbolInformation "TestPattern"
                SkFunction Nothing Nothing (Location testUri testPatternR) (Just "Symbols")

        liftIO $ testPattern `elem` symbs @? "Contains symbols"

    , testCase "provides imports" $ runSession hlsCommand oldCaps "test/testdata" $ do
        doc@(TextDocumentIdentifier testUri) <- openDoc "Symbols.hs" "haskell"
        Right symbs <- getDocumentSymbols doc

        let imports = SymbolInformation "imports" SkModule Nothing Nothing (Location testUri importsR) (Just "Symbols")
            importDataMaybe = SymbolInformation "import Data.Maybe" SkModule Nothing Nothing (Location testUri importDataMaybeR) (Just "imports")

        liftIO $ [imports, importDataMaybe] `isInfixOf` symbs @? "Contains symbol"
    ]

oldCaps :: ClientCapabilities
oldCaps = capsForVersion (LSPVersion 3 9)

fromList :: List a -> [a]
fromList (List a) = a

-- Some common ranges and selection ranges in Symbols.hs
importsR :: Range
importsR = Range (Position 3 0) (Position 3 17)
importsSR :: Range
importsSR = Range (Position 3 0) (Position 3 17)
importDataMaybeR :: Range
importDataMaybeR = Range (Position 3 0) (Position 3 17)
importDataMaybeSR :: Range
importDataMaybeSR = Range (Position 3 0) (Position 3 17)
fooSR :: Range
fooSR = Range (Position 5 0) (Position 7 43)
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
myDataSR = Range (Position 9 0) (Position 10 22)
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
