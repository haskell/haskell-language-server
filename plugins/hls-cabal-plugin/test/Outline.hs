{-# LANGUAGE DisambiguateRecordFields #-}
{-# LANGUAGE OverloadedStrings        #-}

module Outline (
    outlineTests
) where

import qualified Ide.Plugin.Cabal.Outline    as Outline
import qualified Language.LSP.Protocol.Types as LSP
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
    fieldDocumentSymbol = (Outline.defDocumentSymbol (LSP.Range {_start = LSP.Position {_line = 0, _character = 0},
                                                                 _end = LSP.Position {_line = 0, _character = 8}}))
                                    { LSP._name = "homepage",
                                      LSP._kind = LSP.SymbolKind_Field,
                                      LSP._children = Nothing
                                    }
    fieldLineDocumentSymbol = (Outline.defDocumentSymbol (LSP.Range {_start = LSP.Position {_line = 0, _character = 0},
                                                                     _end = LSP.Position {_line = 0, _character = 13}}))
                                    { LSP._name = "cabal-version",
                                      LSP._kind = LSP.SymbolKind_Field,
                                      LSP._children = Nothing -- the values of fieldLine are removed from the outline
                                    }
    sectionDocumentSymbol = (Outline.defDocumentSymbol (LSP.Range {_start = LSP.Position {_line = 0, _character = 2},
                                                                     _end = LSP.Position {_line = 0, _character = 15}}))
                                    { LSP._name = "build-depends",
                                      LSP._kind = LSP.SymbolKind_Field,
                                      LSP._children = Nothing -- the values of fieldLine are removed from the outline
                                    }
    sectionArgDocumentSymbol = (Outline.defDocumentSymbol (LSP.Range {_start = LSP.Position {_line = 0, _character = 2},
                                                                     _end = LSP.Position {_line = 0, _character = 19}}))
                                    { LSP._name = "if os ( windows )",
                                      LSP._kind = LSP.SymbolKind_Object,
                                      LSP._children = Just $ [sectionArgChildrenDocumentSymbol]                                    }
    sectionArgChildrenDocumentSymbol = (Outline.defDocumentSymbol (LSP.Range {_start = LSP.Position {_line = 1, _character = 4},
                                                                     _end = LSP.Position {_line = 1, _character = 17}}))
                                    { LSP._name = "build-depends",
                                      LSP._kind = LSP.SymbolKind_Field,
                                      LSP._children = Nothing
                                    }
