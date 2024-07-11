{-# LANGUAGE OverloadedStrings #-}

module Outline (
  outlineTests,
) where

import Language.LSP.Protocol.Types (
  DocumentSymbol (..),
  Position (..),
  Range (..),
 )
import Test.Hls qualified as T
import Utils

testSymbols :: (T.HasCallStack) => T.TestName -> FilePath -> [DocumentSymbol] -> T.TestTree
testSymbols testName path expectedSymbols =
  runCabalTestCaseSession testName "outline-cabal" $ do
    docId <- T.openDoc path "cabal"
    symbols <- T.getDocumentSymbols docId
    T.liftIO $ symbols T.@?= Right expectedSymbols

outlineTests :: T.TestTree
outlineTests =
  T.testGroup
    "Cabal Outline Tests"
    [ testSymbols
        "cabal Field outline test"
        "field.cabal"
        [fieldDocumentSymbol]
    , testSymbols
        "cabal FieldLine outline test"
        "fieldline.cabal"
        [fieldLineDocumentSymbol]
    , testSymbols
        "cabal Section outline test"
        "section.cabal"
        [sectionDocumentSymbol]
    , testSymbols
        "cabal SectionArg outline test"
        "sectionarg.cabal"
        [sectionArgDocumentSymbol]
    ]
 where
  fieldDocumentSymbol :: DocumentSymbol
  fieldDocumentSymbol =
    ( defDocumentSymbol
        ( Range { _start = Position{_line = 0, _character = 0}
                , _end = Position{_line = 0, _character = 8} })
    )
      { _name = "homepage"
      , _kind = T.SymbolKind_Field
      , _children = Nothing
      }
  fieldLineDocumentSymbol :: DocumentSymbol
  fieldLineDocumentSymbol =
    ( defDocumentSymbol
        ( Range { _start = Position{_line = 0, _character = 0}
                , _end = Position{_line = 0, _character = 13} })
    )
      { _name = "cabal-version"
      , _kind = T.SymbolKind_Field
      , _children = Nothing -- the values of fieldLine are removed from the outline
      }
  sectionDocumentSymbol :: DocumentSymbol
  sectionDocumentSymbol =
    ( defDocumentSymbol
        ( Range { _start = Position{_line = 0, _character = 2}
                , _end = Position{_line = 0, _character = 15} })
    )
      { _name = "build-depends"
      , _kind = T.SymbolKind_Field
      , _children = Nothing -- the values of fieldLine are removed from the outline
      }
  sectionArgDocumentSymbol :: DocumentSymbol
  sectionArgDocumentSymbol =
    ( defDocumentSymbol
        ( Range { _start = Position{_line = 0, _character = 2}
                , _end = Position{_line = 0, _character = 19} })
    )
      { _name = "if os ( windows )"
      , _kind = T.SymbolKind_Object
      , _children = Just $ [sectionArgChildrenDocumentSymbol]
      }
  sectionArgChildrenDocumentSymbol :: DocumentSymbol
  sectionArgChildrenDocumentSymbol =
    ( defDocumentSymbol
        ( Range { _start = Position{_line = 1, _character = 4}
                , _end = Position{_line = 1, _character = 17} })
    )
      { _name = "build-depends"
      , _kind = T.SymbolKind_Field
      , _children = Nothing
      }

defDocumentSymbol :: Range -> DocumentSymbol
defDocumentSymbol range =
  DocumentSymbol
    { _detail = Nothing
    , _deprecated = Nothing
    , _name = ""
    , _kind = T.SymbolKind_File
    , _range = range
    , _selectionRange = range
    , _children = Nothing
    , _tags = Nothing
    }
