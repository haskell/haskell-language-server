{-# LANGUAGE DisambiguateRecordFields #-}
{-# LANGUAGE OverloadedStrings        #-}

module Outline (
    outlineTests
) where

import           Ide.Plugin.Cabal.Outline (defDocumentSymbol)
import           Test.Hls
import           Utils

testSymbols :: (HasCallStack) => TestName -> FilePath -> [DocumentSymbol] -> TestTree
testSymbols testName path expectedSymbols =
  runCabalTestCaseSession testName "outline-cabal" $ do
    docId <- openDoc path "cabal"
    symbols <- getDocumentSymbols docId
    liftIO $ symbols @?= Right expectedSymbols

outlineTests :: TestTree
outlineTests =
  testGroup
    "Cabal Outline Tests"
    [ testSymbols
        "cabal Field outline test"
        "field.cabal"
        [fieldDocumentSymbol],
      testSymbols
        "cabal FieldLine outline test"
        "fieldline.cabal"
        [fieldLineDocumentSymbol],
      testSymbols
        "cabal Section outline test"
        "section.cabal"
        [sectionDocumentSymbol],
      testSymbols
        "cabal SectionArg outline test"
        "sectionarg.cabal"
        [sectionArgDocumentSymbol]
    ]
  where
    fieldDocumentSymbol :: DocumentSymbol
    fieldDocumentSymbol = (defDocumentSymbol (Range {_start = Position {_line = 0, _character = 0},
                                                                 _end = Position {_line = 0, _character = 8}}))
                                    { _name = "homepage",
                                      _kind = SymbolKind_Field,
                                      _children = Nothing
                                    }
    fieldLineDocumentSymbol :: DocumentSymbol
    fieldLineDocumentSymbol = (defDocumentSymbol (Range {_start = Position {_line = 0, _character = 0},
                                                                     _end = Position {_line = 0, _character = 13}}))
                                    { _name = "cabal-version",
                                      _kind = SymbolKind_Field,
                                      _children = Nothing -- the values of fieldLine are removed from the outline
                                    }
    sectionDocumentSymbol :: DocumentSymbol
    sectionDocumentSymbol = (defDocumentSymbol (Range {_start = Position {_line = 0, _character = 2},
                                                                     _end = Position {_line = 0, _character = 15}}))
                                    { _name = "build-depends",
                                      _kind = SymbolKind_Field,
                                      _children = Nothing -- the values of fieldLine are removed from the outline
                                    }
    sectionArgDocumentSymbol :: DocumentSymbol
    sectionArgDocumentSymbol = (defDocumentSymbol (Range {_start = Position {_line = 0, _character = 2},
                                                                     _end = Position {_line = 0, _character = 19}}))
                                    { _name = "if os ( windows )",
                                      _kind = SymbolKind_Object,
                                      _children = Just $ [sectionArgChildrenDocumentSymbol]                                    }
    sectionArgChildrenDocumentSymbol :: DocumentSymbol
    sectionArgChildrenDocumentSymbol = (defDocumentSymbol (Range {_start = Position {_line = 1, _character = 4},
                                                                     _end = Position {_line = 1, _character = 17}}))
                                    { _name = "build-depends",
                                      _kind = SymbolKind_Field,
                                      _children = Nothing
                                    }
