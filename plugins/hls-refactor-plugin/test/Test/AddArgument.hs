{-# LANGUAGE AllowAmbiguousTypes   #-}
{-# LANGUAGE CPP                   #-}
{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE PolyKinds             #-}

module Test.AddArgument (tests) where

import           Data.List.Extra
import qualified Data.Text                         as T
import           Development.IDE.Types.Location
import           Language.LSP.Test
import           Language.LSP.Types                hiding
                                                   (SemanticTokenAbsolute (length, line),
                                                    SemanticTokenRelative (length),
                                                    SemanticTokensEdit (_start),
                                                    mkRange)
import           Test.Tasty
import           Test.Tasty.HUnit


import           Test.Hls

import qualified Development.IDE.Plugin.CodeAction as Refactor

tests :: TestTree
tests =
  testGroup
    "add argument"
#if !MIN_VERSION_ghc(9,2,1)
    []
#else
    [ mkGoldenAddArgTest' "Hole" (r 0 0 0 50) "_new_def",
      mkGoldenAddArgTest "NoTypeSuggestion" (r 0 0 0 50),
      mkGoldenAddArgTest "MultipleDeclAlts" (r 0 0 0 50),
      mkGoldenAddArgTest "AddArgWithSig" (r 1 0 1 50),
      mkGoldenAddArgTest "AddArgWithSigAndDocs" (r 8 0 8 50),
      mkGoldenAddArgTest "AddArgFromLet" (r 2 0 2 50),
      mkGoldenAddArgTest "AddArgFromWhere" (r 3 0 3 50),
      mkGoldenAddArgTest "AddArgFromWhereComments" (r 3 0 3 50),
      mkGoldenAddArgTest "AddArgWithTypeSynSig" (r 2 0 2 50),
      mkGoldenAddArgTest "AddArgWithTypeSynSigContravariant" (r 2 0 2 50),
      mkGoldenAddArgTest "AddArgWithLambda" (r 1 0 1 50),
      mkGoldenAddArgTest "MultiSigFirst" (r 2 0 2 50),
      mkGoldenAddArgTest "MultiSigLast" (r 2 0 2 50),
      mkGoldenAddArgTest "MultiSigMiddle" (r 2 0 2 50)
    ]
  where
    r x y x' y' = Range (Position x y) (Position x' y')

mkGoldenAddArgTest :: FilePath -> Range -> TestTree
mkGoldenAddArgTest testFileName range = mkGoldenAddArgTest' testFileName range "new_def"

-- Make a golden test for the add argument action. Given varName is the name of the variable not yet defined.
mkGoldenAddArgTest' :: FilePath -> Range -> T.Text -> TestTree
mkGoldenAddArgTest' testFileName range varName = do
    let action docB = do
          _ <- waitForDiagnostics
          InR action@CodeAction {_title = actionTitle} : _ <-
            filter (\(InR CodeAction {_title = x}) -> "Add" `isPrefixOf` T.unpack x)
              <$> getCodeActions docB range
          liftIO $ actionTitle @?= ("Add argument ‘" <> varName <> "’ to function")
          executeCodeAction action
    goldenWithHaskellDoc
      (mkPluginTestDescriptor Refactor.bindingsPluginDescriptor "ghcide-code-actions-bindings")
      (testFileName <> " (golden)")
      "test/data/golden/add-arg"
      testFileName
      "expected"
      "hs"
      action
#endif
