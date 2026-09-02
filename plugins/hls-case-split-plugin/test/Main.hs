{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedLists   #-}
{-# LANGUAGE OverloadedStrings #-}

module Main
  ( main
  ) where

import           Control.Lens               (Prism', prism', (^.), (^..), (^?))
import           Data.Text                  (Text)
import qualified Ide.Plugin.CaseSplit       as CS
import qualified Language.LSP.Protocol.Lens as L
import           System.FilePath
import           Test.Hls                   hiding (waitForDiagnosticsFrom)
import qualified Test.Hls.FileSystem        as FS

main :: IO ()
main = defaultTestRunner tests

caseSplitPlugin :: PluginTestDescriptor CS.Log
caseSplitPlugin = mkPluginTestDescriptor CS.descriptor "case split"

tests :: TestTree
tests = testGroup
  "case split"
  [ codeActionTests
  ]

codeActionTests :: TestTree
codeActionTests = testGroup
  "code actions" $ let title = CS.caseSplitPluginCodeActionTitle in
  [ goldenWithClass "No patterns, no braces" "TNoPatternsNoBraces" $
      Prelude.flip inspectCodeAction [title]
  , goldenWithClass "Some patterns, no braces" "TSomePatternsNoBraces" $
      Prelude.flip inspectCodeAction [title]
  , goldenWithClass "Some patterns, with braces" "TSomePatternsWithBraces" $
      Prelude.flip inspectCodeAction [title]
  , goldenWithClass "No patterns, with braces" "TNoPatternsWithBraces" $
      Prelude.flip inspectCodeAction [title]

  -- Comments preserved
  , goldenWithClass "No patterns, no braces, comment after `of`" "TNoPatternsNoBracesWithComment" $
      Prelude.flip inspectCodeAction [title]
  , goldenWithClass "Some patterns, no braces, comment after `of`" "TSomePatternsNoBracesWithComment" $
      Prelude.flip inspectCodeAction [title]

  -- Windows support
  , goldenWithClass "Like TNoPatternsNoBraces, but lacks line terminator at EOF" "TNoPatternsNoBracesWindows" $
      Prelude.flip inspectCodeAction [title]

  -- Patterns with irregular indentation
  , goldenWithClass "Jagged patterns, no braces" "TJaggedNoBraces" $
      Prelude.flip inspectCodeAction [title]
  , goldenWithClass "Jagged patterns, with braces" "TJaggedWithBraces" $
      Prelude.flip inspectCodeAction [title]

  -- Patterns on one line
  , goldenWithClass "Some patterns on one line, no braces" "TSomePatternsOnOneLineNoBraces" $
      Prelude.flip inspectCodeAction [title]
  , goldenWithClass "Some patterns on one line, with braces" "TSomePatternsOnOneLineWithBraces" $
      Prelude.flip inspectCodeAction [title]

  -- Records
  , goldenWithClass "Records' field names are ignored" "TRecordsFieldNamesIgnored" $
      Prelude.flip inspectCodeAction [title]
  , goldenWithClass "Too many fields are collapsed" "TManyFields" $
      Prelude.flip inspectCodeAction [title]

  -- GADTs
  , goldenWithClass "GADT - simple" "TGADTsimple" $
      Prelude.flip inspectCodeAction [title]
  , goldenWithClass "GADT - advanced" "TGADTadvanced" $
      Prelude.flip inspectCodeAction [title]

  -- LambdaCase
  , goldenWithClass "LambdaCase, no patterns, no braces" "TLambdaCaseNoPatternsNoBraces" $
      Prelude.flip inspectCodeAction [title]
  , goldenWithClass "LambdaCase, no patterns, with braces" "TLambdaCaseNoPatternsWithBraces" $
      Prelude.flip inspectCodeAction [title]
  , goldenWithClass "LambdaCase, some patterns, no braces" "TLambdaCaseSomePatternsNoBraces" $
      Prelude.flip inspectCodeAction [title]
  , goldenWithClass "LambdaCase, some patterns, with braces" "TLambdaCaseSomePatternsWithBraces" $
      Prelude.flip inspectCodeAction [title]
  , goldenWithClass "LambdaCase in `do`, no patterns, no braces" "TLambdaCaseInDoNoPatternsNoBraces" $
      Prelude.flip inspectCodeAction [title]
  , goldenWithClass "LambdaCase in `do`, no patterns, with braces" "TLambdaCaseInDoNoPatternsWithBraces" $
      Prelude.flip inspectCodeAction [title]
  , goldenWithClass "LambdaCase in `do`, some patterns, no braces" "TLambdaCaseInDoSomePatternsNoBraces" $
      Prelude.flip inspectCodeAction [title]
  , goldenWithClass "LambdaCase in `do`, some patterns, with braces" "TLambdaCaseInDoSomePatternsWithBraces" $
      Prelude.flip inspectCodeAction [title]

  -- Inside where
  , expectNoCodeActionAvailable "Inside `where`, without signature" "TInsideWhereWithoutSignature"
  , goldenWithClass "Inside `where`" "TInsideWhere" $
      Prelude.flip inspectCodeAction [title]
  , goldenWithClass "Inside nested `where`" "TInsideNestedWhere" $
      Prelude.flip inspectCodeAction [title]

  -- Overlapping diagnostics
  , goldenWithClass "Expression is `_`" "TExpressionIsUnderscore" $
      Prelude.flip inspectCodeAction [title]
  , goldenWithRange "Overlapping pattern matches" "TOverlappingExistingPatterns" $
      Range (Position 15 4) (Position 15 5)

  -- Inside let
  , goldenWithClass "Inside `let`'s declarations" "TInsideLetDeclarations" $
      Prelude.flip inspectCodeAction [title]
  , goldenWithClass "Inside `let`'s expression" "TInsideLetExpression" $
      Prelude.flip inspectCodeAction [title]

  -- Inside do
  , goldenWithClass "Inside `let`'s declarations inside `do`" "TInsideLetDeclarationsInsideDo" $
      Prelude.flip inspectCodeAction [title]
  , goldenWithClass "Inside `let`'s expression inside `do`" "TInsideLetExpressionInsideDo" $
      Prelude.flip inspectCodeAction [title]
  , goldenWithClass "Inside `do`" "TInsideDo" $
      Prelude.flip inspectCodeAction [title]

  -- Nested case expressions
  , goldenWithClass "Complete `case` nested in incomplete `case`" "TCompleteCaseInsideIncompleteCase" $
      Prelude.flip inspectCodeAction [title]
  , goldenWithRange "Incomplete `case` nested in complete `case`" "TIncompleteCaseInsideCompleteCase" $
      Range (Position 15 16) (Position 15 17)
  , goldenWithRange "Incomplete `case` nested in incomplete `case`" "TIncompleteCaseInsideIncompleteCase" $
      Range (Position 15 30) (Position 15 31)

  -- Pattern synonyms and COMPLETE pragma
  , goldenWithClass "Pattern synonyms not declared `COMPLETE` are ignored" "TPatternSynonyms" $
      Prelude.flip inspectCodeAction [title]
  , goldenWithClass "If some pattern synonyms are declared `COMPLETE`, actual constructs are ignored" "TPatternSynonymsWithCompletePragma" $
      Prelude.flip inspectCodeAction [title]
  , goldenWithClass "In presence of `COMPLETE` constructors, only add them" "TOrdinaryCtorsWithCompletePragma" $
      Prelude.flip inspectCodeAction [title]
  , goldenWithClass "In presence of `COMPLETE` constructors and patterns, only add them" "TPatternSynAndSomeCtorsAreComplete" $
      Prelude.flip inspectCodeAction [title]

  -- Extreme cursor positions (here we're considering the cursor as being
  -- 0-chars wide and sitting between characters, i.e. a `Range c c`, which in
  -- line with the LSP's specs).
  , expectCodeActionsAvailable "Cursor right before the space before the `c` of `case`" "TNoPatternsNoBraces"
      (Range (Position 12 7) (Position 12 7))
      []

  , expectCodeActionsAvailable "Cursor right before `c` of `case`" "TNoPatternsNoBraces"
      (Range (Position 12 8) (Position 12 8))
      [ CS.caseSplitPluginCodeActionTitle
      ]

  , expectCodeActionsAvailable "Cursor right after `f` of `of`" "TNoPatternsNoBraces"
      (Range (Position 12 17) (Position 12 17))
      [ CS.caseSplitPluginCodeActionTitle
      ]

  , expectCodeActionsAvailable "Selection from right after the `f` of `of` to start of next line" "TNoPatternsNoBraces"
      (Range (Position 12 17) (Position 13 0))
      []

  -- Support UnicodeSyntax
  , goldenWithClass "Use → instead of -> when UnicodeSyntax is On" "TUnicodeArrow" $
      Prelude.flip inspectCodeAction [title]

  -- Some more corner cases
  , expectNoCodeActionAvailable "No action on `Int`" "TInt"
  , expectNoCodeActionAvailable "Cannot see through condition of a single catch-all pattern" "TWithCond"
  , goldenWithClass "Ignore catch-all pattern in presence of non-catch-all pattern" "TWithCondAndPat" $
      Prelude.flip inspectCodeAction [title]
  ]

waitForDiagnosticsFrom :: TextDocumentIdentifier -> Session [Diagnostic]
waitForDiagnosticsFrom doc = do
    diagsNot <- skipManyTill anyMessage (message SMethod_TextDocumentPublishDiagnostics)
    let diags = diagsNot ^. L.params . L.diagnostics
    if doc ^. L.uri /= diagsNot ^. L.params . L.uri
       || ((not .) . any) ((\case Just (InR "GHC-62161") -> True
                                  _ -> False) . (^. L.code)) diags
       then waitForDiagnosticsFrom doc
       else return diags

_CACodeAction :: Prism' (Command |? CodeAction) CodeAction
_CACodeAction = prism' InR $ \case
  InR action -> Just action
  _          -> Nothing

expectCodeActionsAvailable :: TestName -> FilePath -> Range -> [Text] -> TestTree
expectCodeActionsAvailable title path range actionTitles =
  testCase title $ do
    runSessionWithServerInTmpDir def caseSplitPlugin (mkFs $ FS.directProject (path <.> "hs")) $ do
      doc <- openDoc (path <.> "hs") "haskell"
      _ <- waitForDiagnosticsFrom doc
      caResults <- getCodeActions doc range
      liftIO $ map (^? _CACodeAction . L.title) caResults
        @?= expectedActions
    where
      expectedActions = Just <$> actionTitles

expectNoCodeActionAvailable :: TestName -> FilePath -> TestTree
expectNoCodeActionAvailable title path = expectCodeActionsAvailable title path anywhere []
  where
    anywhere = Range (Position 0 0) (Position 999 999)

goldenWithRange :: TestName -> FilePath -> Range -> TestTree
goldenWithRange title path range =
  goldenWithHaskellDocInTmpDir def caseSplitPlugin title (mkFs $ FS.directProject (path <.> "hs")) path "expected" "hs" $ \doc -> do
    _ <- waitForDiagnosticsFrom doc
    [action] <- concatMap (^.. _CACodeAction) <$> getCodeActions doc range
    executeCodeAction action

goldenWithClass :: TestName -> FilePath -> ([Command |? CodeAction] -> IO CodeAction) -> TestTree
goldenWithClass title path findAction =
  goldenWithHaskellDocInTmpDir def caseSplitPlugin title (mkFs $ FS.directProject (path <.> "hs")) path "expected" "hs" $ \doc -> do
    _ <- waitForDiagnosticsFrom doc
    actions <- getAllCodeActions doc
    action <- liftIO $ findAction actions
    executeCodeAction action

testDataDir :: FilePath
testDataDir = "plugins" </> "hls-case-split-plugin" </> "test" </> "testdata"

mkFs :: [FS.FileTree] -> FS.VirtualFileTree
mkFs = FS.mkVirtualFileTree testDataDir
