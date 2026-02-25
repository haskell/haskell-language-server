{-# LANGUAGE AllowAmbiguousTypes   #-}
{-# LANGUAGE CPP                   #-}
{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE OverloadedStrings     #-}

module Test.AddArgument (tests) where

import           Data.List                         (find)
import qualified Data.Text                         as T
import           Development.IDE.Types.Location
import           Language.LSP.Protocol.Types       hiding
                                                   (SemanticTokensEdit (_start),
                                                    mkRange)
import           Language.LSP.Test
import           Test.Tasty
import           Test.Tasty.HUnit


import           Test.Hls
import qualified Test.Hls.FileSystem               as FS

import qualified Development.IDE.Plugin.CodeAction as Refactor
import           System.FilePath                   ((<.>))

tests :: TestTree
tests =
  testGroup
    "add argument"
    [ mkGoldenAddArgTest' "Hole" (r 0 0 0 50) "_new_def",
      mkGoldenAddArgTest "NoTypeSuggestion" (r 0 0 0 50),
      mkGoldenAddArgTest "MultipleDeclAlts" (r 0 0 0 50),
      mkGoldenAddArgTest "AddArgWithSig" (r 1 0 1 50),
      mkGoldenAddArgTest "AddArgWithSigAndDocs" (r 8 0 8 50),
      mkGoldenAddArgTest "AddArgFromLet" (r 2 0 2 50),
      mkGoldenAddArgTest "AddArgFromWhere" (r 3 0 3 50),
      -- TODO can we make this work for GHC 9.10+?
      knownBrokenForGhcVersions [GHC910, GHC912, GHC914] "In GHC 9.10+ end-of-line comment annotation is in different place" $
          mkGoldenAddArgTest "AddArgFromWhereComments" (r 3 0 3 50),
      mkGoldenAddArgTest "AddArgWithTypeSynSig" (r 2 0 2 50),
      mkGoldenAddArgTest "AddArgWithTypeSynSigContravariant" (r 2 0 2 50),
      mkGoldenAddArgTest "AddArgWithLambda" (r 1 0 1 50),
      mkGoldenAddArgTest "MultiSigFirst" (r 2 0 2 50),
      mkGoldenAddArgTest "MultiSigLast" (r 2 0 2 50),
      mkGoldenAddArgTest "MultiSigMiddle" (r 2 0 2 50),
      mkNoAddArgForQualifiedNameTest
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
          let matchAction a = case a of
                InR CodeAction {_title = t} -> "Add" `T.isPrefixOf` t
                _                           -> False
          InR action@CodeAction {_title = actionTitle} : _ <-
            filter matchAction <$> getCodeActions docB range
          liftIO $ actionTitle @?= ("Add argument ‘" <> varName <> "’ to function")
          executeCodeAction action
    goldenWithHaskellDocInTmpDir
      def
      (mkPluginTestDescriptor Refactor.bindingsPluginDescriptor "ghcide-code-actions-bindings")
      (testFileName <> " (golden)")
      (FS.mkVirtualFileTree "plugins/hls-refactor-plugin/test/data/golden/add-arg" (FS.directProject $ testFileName <.> "hs"))
      testFileName
      "expected"
      "hs"
      action

-- | Verify that the "Add argument" code action is NOT offered for qualified names (e.g. NE.toList).
-- We also check that a diagnostic exists on the relevant line, to confirm we're testing the right spot.
mkNoAddArgForQualifiedNameTest :: TestTree
mkNoAddArgForQualifiedNameTest =
    testCase "No add argument for qualified names" $ runSessionWithServerInTmpDir def
      (mkPluginTestDescriptor Refactor.bindingsPluginDescriptor "ghcide-code-actions-bindings")
      (FS.mkVirtualFileTree "plugins/hls-refactor-plugin/test/data/add-arg" (FS.directProject "QualifiedName.hs"))
      $ do
        doc <- openDoc "QualifiedName.hs" "haskell"
        diags <- waitForDiagnostics
        -- Verify we got a diagnostic about NE.toList not being in scope
        let hasNotInScopeDiag = any (\(Diagnostic {_message = msg}) -> "NE.toList" `T.isInfixOf` msg) diags
        liftIO $ assertBool "Expected a 'not in scope' diagnostic for NE.toList" hasNotInScopeDiag
        -- Verify that the "Add argument" code action is NOT offered
        actions <- getCodeActions doc (Range (Position 5 0) (Position 5 50))
        let addArgAction = find (\case
              InR CodeAction {_title = t} -> "Add argument" `T.isPrefixOf` t
              _                           -> False) actions
        liftIO $ addArgAction @?= Nothing

