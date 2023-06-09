{-# LANGUAGE OverloadedStrings #-}
module Symbol (tests) where

import           Control.Lens                       (_Just, ix, to, (^?))
import           Data.List
import           Language.LSP.Protocol.Capabilities
import qualified Language.LSP.Protocol.Lens         as L
import           Language.LSP.Test                  as Test
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
        Right symbs <- getDocumentSymbols doc

        let myData = DocumentSymbol "MyData" Nothing SymbolKind_Struct Nothing Nothing myDataR myDataSR (Just [a, b])
            a = DocumentSymbol "A" Nothing SymbolKind_Constructor Nothing Nothing aR aSR Nothing
            b = DocumentSymbol "B" Nothing SymbolKind_Constructor Nothing Nothing bR bSR Nothing
        let myData' = symbs ^? ix 0 . L.children . _Just . ix 2

        liftIO $ Just myData == myData' @? "Contains symbol"

    , ignoreTestBecause "extracting symbols from nested wheres not supported" $ testCase "provides nested where functions" $ runSession hlsCommand fullCaps "test/testdata" $ do
        doc <- openDoc "Symbols.hs" "haskell"
        Right symbs <- getDocumentSymbols doc

        let foo = DocumentSymbol "foo" Nothing SymbolKind_Function Nothing Nothing fooR fooSR (Just [bar])
            bar = DocumentSymbol "bar" Nothing SymbolKind_Function Nothing Nothing barR barSR (Just [dog, cat])
            dog = DocumentSymbol "dog" Nothing SymbolKind_Variable Nothing Nothing dogR dogSR (Just mempty)
            cat = DocumentSymbol "cat" Nothing SymbolKind_Variable Nothing Nothing catR catSR (Just mempty)
        let foo' = symbs ^? ix 0 . L.children . _Just . ix 1

        liftIO $ Just foo == foo' @? "Contains symbol"

    , ignoreTestBecause "extracting pattern synonym symbols not supported" $ testCase "provides pattern synonyms" $ runSession hlsCommand fullCaps "test/testdata" $ do
        doc <- openDoc "Symbols.hs" "haskell"
        Right symbs <- getDocumentSymbols doc

        let testPattern = DocumentSymbol "TestPattern"
                Nothing SymbolKind_Function Nothing Nothing testPatternR testPatternSR (Just mempty)
        let testPattern' = symbs ^? ix 0 . L.children . _Just . ix 3

        liftIO $ Just testPattern == testPattern' @? "Contains symbol"

    , testCase "provides imports" $ runSession hlsCommand fullCaps "test/testdata" $ do
        doc <- openDoc "Symbols.hs" "haskell"
        Right symbs <- getDocumentSymbols doc

        let imports = DocumentSymbol "imports" Nothing SymbolKind_Module Nothing Nothing importsR importsSR (Just [importDataMaybe])
            importDataMaybe = DocumentSymbol "import Data.Maybe" Nothing SymbolKind_Module Nothing Nothing importDataMaybeR importDataMaybeSR Nothing
        let imports' = symbs ^? ix 0 . L.children . _Just . ix 0

        liftIO $ Just imports == imports' @? "Contains symbol"
    ]

pre310Tests :: TestTree
pre310Tests = testGroup "pre 3.10 symbol information" [
    testCase "provides nested data types and constructors" $ runSession hlsCommand oldCaps "test/testdata" $ do
        doc@(TextDocumentIdentifier testUri) <- openDoc "Symbols.hs" "haskell"
        Left symbs <- getDocumentSymbols doc

        let myData = SymbolInformation "MyData" SymbolKind_Struct Nothing (Just "Symbols") Nothing (Location testUri myDataR)
            a = SymbolInformation "A" SymbolKind_Constructor Nothing (Just "MyData") Nothing (Location testUri aR)
            b = SymbolInformation "B" SymbolKind_Constructor Nothing (Just "MyData") Nothing (Location testUri bR)

        liftIO $ [myData, a, b] `isInfixOf` symbs @? "Contains symbols"

    , ignoreTestBecause "extracting symbols from nested wheres not supported" $ testCase "provides nested where functions" $ runSession hlsCommand oldCaps "test/testdata" $ do
        doc@(TextDocumentIdentifier testUri) <- openDoc "Symbols.hs" "haskell"
        Left symbs <- getDocumentSymbols doc

        let foo = SymbolInformation "foo" SymbolKind_Function Nothing (Just "Symbols") Nothing (Location testUri fooR)
            bar = SymbolInformation "bar" SymbolKind_Function Nothing (Just "foo") Nothing (Location testUri barR)
            dog = SymbolInformation "dog" SymbolKind_Variable Nothing (Just "bar") Nothing (Location testUri dogR)
            cat = SymbolInformation "cat" SymbolKind_Variable Nothing (Just "bar") Nothing (Location testUri catR)

        -- Order is important!
        liftIO $ [foo, bar, dog, cat] `isInfixOf` symbs @? "Contains symbols"

    , ignoreTestBecause "extracting pattern synonym symbols not supported" $ testCase "provides pattern synonyms" $ runSession hlsCommand oldCaps "test/testdata" $ do
        doc@(TextDocumentIdentifier testUri) <- openDoc "Symbols.hs" "haskell"
        Left symbs <- getDocumentSymbols doc

        let testPattern = SymbolInformation "TestPattern"
                SymbolKind_Function Nothing (Just "Symbols") Nothing (Location testUri testPatternR)

        liftIO $ testPattern `elem` symbs @? "Contains symbols"

    , testCase "provides imports" $ runSession hlsCommand oldCaps "test/testdata" $ do
        doc@(TextDocumentIdentifier testUri) <- openDoc "Symbols.hs" "haskell"
        Left symbs <- getDocumentSymbols doc

        let imports = SymbolInformation "imports" SymbolKind_Module Nothing (Just "Symbols") Nothing (Location testUri importsR)
            importDataMaybe = SymbolInformation "import Data.Maybe" SymbolKind_Module Nothing (Just "imports") Nothing (Location testUri importDataMaybeR)

        liftIO $ [imports, importDataMaybe] `isInfixOf` symbs @? "Contains symbol"
    ]

oldCaps :: ClientCapabilities
oldCaps = capsForVersion (LSPVersion 3 9)

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
