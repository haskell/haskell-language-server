{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}


module FoldingRange (foldingRangeTests) where

import qualified Data.ByteString.Char8         as C8
import           Distribution.Fields.Field     (Field (..), Name (..))
import qualified Distribution.Parsec.Position  as Cabal
import           Ide.Plugin.Cabal.FoldingRange (foldingRangeForField)
import qualified Language.LSP.Protocol.Types   as LSP
import           Test.Hls


foldingRangeTests :: TestTree
foldingRangeTests = testGroup "FoldingRange minimal tests"
  [ testCase "Field produces collapsed text 'homepage'" $ do
      let field = Field (Name (Cabal.Position 0 0) (C8.pack "homepage")) []
      case foldingRangeForField field of
        Just LSP.FoldingRange{..} ->
          _collapsedText @?= Just "homepage"
        Nothing ->
          assertFailure "Expected a FoldingRange for field"
  ]

-- {-# LANGUAGE OverloadedStrings #-}

-- module FoldingRange (
--   foldingRangeTests,
-- ) where

-- import           Language.LSP.Protocol.Types (Position (..), FoldingRange (..))
-- import qualified Test.Hls                    as T
-- import           Utils

-- defFoldingRange :: Position -> FoldingRange
-- defFoldingRange (Position line char) =
--   FoldingRange
--     { _startLine      = line
--     , _startCharacter = Just char
--     , _endLine        = line
--     , _endCharacter   = Just char
--     , _kind           = Nothing
--     , _collapsedText  = Nothing
--     }

-- testFoldingRanges :: (T.HasCallStack)
--                   => T.TestName
--                   -> FilePath
--                   -> [FoldingRange]
--                   -> T.TestTree
-- testFoldingRanges testName path expectedRanges =
--   runCabalTestCaseSession testName "folding-range-cabal" $ do
--     docId  <- T.openDoc path "cabal"
--     ranges <- T.getFoldingRanges docId
--     T.liftIO $ ranges T.@?= Right expectedRanges

-- foldingRangeTests :: T.TestTree
-- foldingRangeTests =
--   T.testGroup "Cabal FoldingRange Tests"
--     [ testFoldingRanges
--         "cabal Field folding range test"
--         "field.cabal"
--         [fieldFoldingRange]
--     , testFoldingRanges
--         "cabal FieldLine folding range test"
--         "fieldline.cabal"
--         [fieldLineFoldingRange]
--     , testFoldingRanges
--         "cabal Section folding range test"
--         "section.cabal"
--         [sectionFoldingRange]
--     , testFoldingRanges
--         "cabal SectionArg folding range test"
--         "sectionarg.cabal"
--         [sectionArgFoldingRange]
--     ]


-- fieldFoldingRange :: FoldingRange
-- fieldFoldingRange =
--   (defFoldingRange (Position 0 0))
--     { _endLine       = 0
--     , _endCharacter  = Just 8
--     , _collapsedText = Just "homepage"
--     }

-- fieldLineFoldingRange :: FoldingRange
-- fieldLineFoldingRange =
--   (defFoldingRange (Position 0 0))
--     { _endLine       = 0
--     , _endCharacter  = Just 13
--     , _collapsedText = Just "cabal-version"
--     }

-- sectionFoldingRange :: FoldingRange
-- sectionFoldingRange =
--   (defFoldingRange (Position 0 2))
--     { _endLine       = 0
--     , _endCharacter  = Just 15
--     , _collapsedText = Just "build-depends"
--     }

-- sectionArgFoldingRange :: FoldingRange
-- sectionArgFoldingRange =
--   (defFoldingRange (Position 0 2))
--     { _endLine       = 1
--     , _endCharacter  = Just 17
--     , _collapsedText = Just "if os(windows)"
--     }

