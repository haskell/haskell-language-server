{-# LANGUAGE OverloadedStrings #-}

module FoldingRange (
  outlineTests,
) where

import           Language.LSP.Protocol.Types (DocumentSymbol (..),
                                              Position (..), Range (..))
import qualified Test.Hls                    as T
import           Utils

testFoldingRanges :: (T.HasCallStack)
                  => T.TestName
                  -> FilePath
                  -> [LSP.FoldingRange]
                  -> T.TestTree
testFoldingRanges testName path expectedRanges =
  runCabalTestCaseSession testName "outline-cabal" $ do
    docId <- T.openDoc path "cabal"
    ranges <- T.getFoldingRanges docId
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
    { _endLine       = 0
    , _endCharacter  = Just 8
    , _collapsedText = Just "homepage"
    }

-- Expected folding range for fieldline.cabal
fieldLineFoldingRange :: LSP.FoldingRange
fieldLineFoldingRange =
  (defFoldingRange (LSP.Position 0 0))
    { _endLine       = 0
    , _endCharacter  = Just 13
    , _collapsedText = Just "cabal-version"
    }

-- Expected folding range for section.cabal
sectionFoldingRange :: LSP.FoldingRange
sectionFoldingRange =
  (defFoldingRange (LSP.Position 0 2))
    { _endLine       = 0
    , _endCharacter  = Just 15
    , _collapsedText = Just "build-depends"
    }

-- Expected folding range for sectionarg.cabal
sectionArgFoldingRange :: LSP.FoldingRange
sectionArgFoldingRange =
  (defFoldingRange (LSP.Position 0 2))
    { _endLine       = 1
    , _endCharacter  = Just 17
    , _collapsedText = Just "if os ( windows )"
    }

getFoldingRanges :: LSP.TextDocumentIdentifier -> Session (Either ResponseError [FoldingRange])
getFoldingRanges docId = do
  let params = LSP.FoldingRangeParams docId Nothing
  request SMethod_TextDocumentFoldingRange params
