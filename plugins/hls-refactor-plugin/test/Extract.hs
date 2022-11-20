{-# LANGUAGE AllowAmbiguousTypes   #-}
{-# LANGUAGE CPP                   #-}
{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE ImplicitParams        #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE MultiWayIf            #-}
{-# LANGUAGE NamedFieldPuns        #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE PatternSynonyms       #-}
{-# LANGUAGE PolyKinds             #-}
{-# LANGUAGE RecordWildCards       #-}
{-# LANGUAGE TypeOperators         #-}
{-# OPTIONS_GHC -Wno-deprecations -Wno-unticked-promoted-constructors #-}

module Extract (extractTests) where

import           Control.Applicative.Combinators
import           Control.Lens                             ((^.))
import           Control.Monad
import           Data.Default
import           Data.Foldable
import           Data.List.Extra
import           Data.Maybe
import qualified Data.Text                                as T
import           Data.Tuple.Extra
import           Development.IDE.GHC.Util
import           Development.IDE.Plugin.Completions.Types (extendImportCommandId)
import           Development.IDE.Test
import           Development.IDE.Types.Location
import           Development.Shake                        (getDirectoryFilesIO)
import           Ide.Types
import           Language.LSP.Test
import           Language.LSP.Types                       hiding
                                                          (SemanticTokenAbsolute (length, line),
                                                           SemanticTokenRelative (length),
                                                           SemanticTokensEdit (_start),
                                                           mkRange)
import qualified Language.LSP.Types                       as LSP
import           Language.LSP.Types.Capabilities
import qualified Language.LSP.Types.Lens                  as L
import           System.Directory
import           System.FilePath
import           System.Info.Extra                        (isMac, isWindows)
import qualified System.IO.Extra
import           System.IO.Extra                          hiding (withTempDir)
import           System.Time.Extra
import           Test.Tasty
import           Test.Tasty.ExpectedFailure
import           Test.Tasty.HUnit
import           Text.Regex.TDFA                          ((=~))


import           Development.IDE.Plugin.CodeAction        (matchRegExMultipleImports, bindingsPluginDescriptor)
import           Test.Hls

import qualified Development.IDE.Plugin.CodeAction        as Refactor
import qualified Development.IDE.Plugin.HLS.GhcIde        as GhcIde
import Debug.Trace (traceM, traceShowM)


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

