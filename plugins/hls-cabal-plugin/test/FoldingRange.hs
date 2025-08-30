{-# LANGUAGE OverloadedStrings #-}

module FoldingRange (
  foldingRangeTests,
) where

import           Language.LSP.Protocol.Message (Method (Method_TextDocumentFoldingRange, Method_TextDocumentSelectionRange),
                                                SMethod (SMethod_TextDocumentFoldingRange, SMethod_TextDocumentSelectionRange))
import qualified Language.LSP.Protocol.Types   as LSP
import qualified Test.Hls                      as T
import           Utils

testFoldingRanges :: (T.HasCallStack)
                  => T.TestName
                  -> FilePath
                  -> [LSP.FoldingRange]
                  -> T.TestTree
testFoldingRanges testName path expectedRanges =
  runCabalTestCaseSession testName "folding-range-cabal" $ do
    docId <- T.openDoc path "cabal"
    ranges <- getFoldingRanges docId
    T.liftIO $ ranges T.@?= Right expectedRanges

foldingRangeTests :: T.TestTree
foldingRangeTests =
  T.testGroup "Cabal FoldingRange Tests"
    [ testFoldingRanges
        "cabal Field folding range test"
        "field.cabal"
        [fieldFoldingRange]
    , testFoldingRanges
        "cabal FieldLine folding range test"
        "fieldline.cabal"
        [fieldLineFoldingRange]
    , testFoldingRanges
        "cabal Section folding range test"
        "section.cabal"
        [sectionFoldingRange]
    , testFoldingRanges
        "cabal SectionArg folding range test"
        "sectionarg.cabal"
        [sectionArgFoldingRange]
    ]

-- Expected folding range for field.cabal
fieldFoldingRange :: LSP.FoldingRange
fieldFoldingRange =
  (defFoldingRange (LSP.Position 0 0))
    { LSP._endLine       = 0
    , LSP._endCharacter  = Just 8
    , LSP._collapsedText = Just "homepage"
    }

-- Expected folding range for fieldline.cabal
fieldLineFoldingRange :: LSP.FoldingRange
fieldLineFoldingRange =
  (defFoldingRange (LSP.Position 0 0))
    { LSP._endLine       = 0
    , LSP._endCharacter  = Just 13
    , LSP._collapsedText = Just "cabal-version"
    }

-- Expected folding range for section.cabal
sectionFoldingRange :: LSP.FoldingRange
sectionFoldingRange =
  (defFoldingRange (LSP.Position 0 2))
    { LSP._endLine       = 0
    , LSP._endCharacter  = Just 15
    , LSP._collapsedText = Just "build-depends"
    }

-- Expected folding range for sectionarg.cabal
sectionArgFoldingRange :: LSP.FoldingRange
sectionArgFoldingRange =
  (defFoldingRange (LSP.Position 0 2))
    { LSP._endLine       = 1
    , LSP._endCharacter  = Just 17
    , LSP._collapsedText = Just "if os(windows)"
    }

getFoldingRanges :: LSP.TextDocumentIdentifier -> Session (Either ResponseError [LSP.FoldingRange])
getFoldingRanges docId = do
  let params = LSP.FoldingRangeParams docId Nothing
  request SMethod_TextDocumentFoldingRange params

defFoldingRange :: LSP.Position -> LSP.FoldingRange
defFoldingRange startPos =
  LSP.FoldingRange
    { LSP._startLine      = LSP._line startPos
    , LSP._startCharacter = Just (LSP._character startPos)
    , LSP._endLine        = LSP._line startPos
    , LSP._endCharacter   = Just (LSP._character startPos)
    , LSP._kind           = Nothing
    , LSP._collapsedText  = Nothing
    }
