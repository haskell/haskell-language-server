{-# LANGUAGE AllowAmbiguousTypes   #-}
{-# LANGUAGE CPP                   #-}
{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE PatternSynonyms       #-}
{-# LANGUAGE PolyKinds             #-}
{-# OPTIONS_GHC -Wno-deprecations -Wno-unticked-promoted-constructors #-}

module Extract (extractTests) where

import           Data.List.Extra
import qualified Data.Text                         as T
import qualified Development.IDE.Plugin.CodeAction as Refactor
import           Development.IDE.Types.Location
import           Language.LSP.Test
import           Language.LSP.Types                hiding
                                                   (SemanticTokenAbsolute (length, line),
                                                    SemanticTokenRelative (length),
                                                    SemanticTokensEdit (_start),
                                                    mkRange)
import           Test.Hls


pattern R :: UInt -> UInt -> UInt -> UInt -> Range
pattern R x y x' y' = Range (Position x y) (Position x' y')

extractTests :: TestTree
extractTests =
  testGroup
    "extract code"
    [ mkGoldenExtractTest "ExtractSimple" (R 0 6 0 28)
    , mkGoldenExtractTest "ExtractSimple" (R 0 7 0 28)
    , mkGoldenExtractTest "ExtractSimple" (R 0 23 0 28)
    , mkGoldenExtractTest "ExtractFreeVars" (R 3 7 3 19)
    , mkGoldenExtractTest "ExtractFreeVars" (R 1 5 3 19)
    -- TODO this case uses record wild cards, and the current implementation handles it sub-optimally.
    , mkGoldenExtractTest "ExtractFreeVarsComplex" (R 6 5 8 39)
    , mkGoldenExtractTest "ExtractFromDeclWithSig" (R 1 7 1 28)
    , mkGoldenExtractTest "ExtractFromWhere" (R 2 11 2 16)
    ]

mkGoldenExtractTest :: FilePath -> Range -> TestTree
mkGoldenExtractTest testFileName range@(Range (Position sl sc) (Position el ec)) = do
    let action docB = do
          _ <- waitForAllProgressDone
          actions <- getCodeActions docB range
          InR action@CodeAction {_title = actionTitle} : _ <-
            filter (\(InR CodeAction {_title = x}) -> "Extract" `isPrefixOf` T.unpack x)
              <$> getCodeActions docB range
          executeCodeAction action
        rangeName = show sl <> "_" <> show sc <> "_" <> show el <> "_" <> show ec
    goldenWithHaskellDoc
      (Refactor.extractCodePluginDescriptor mempty "ghcide-code-actions-extract")
      (testFileName <> " " <> rangeName <> " (golden)")
      "test/data/golden/extract"
      testFileName
      (rangeName <> ".expected")
      "hs"
      action

