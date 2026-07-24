{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedLists   #-}
{-# LANGUAGE OverloadedStrings #-}

module Main
  ( main
  ) where

import           Control.Lens                  (Prism', prism', (^.),
                                                (^..), (^?))
import           Data.Foldable                 (find)
import qualified Data.Text                     as T
import qualified Ide.Plugin.CaseSplit          as CS
import qualified Language.LSP.Protocol.Lens    as L
import           System.FilePath
import           Test.Hls                      hiding (waitForDiagnosticsFrom)
import qualified Test.Hls.FileSystem           as FS

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
  "code actions" $ let title = "Add placeholders for all missing patterns" in
  [ goldenWithClass "No patterns, no braces" "TNoPatternsNoBraces" $
      getActionByTitle title
  , goldenWithClass "Some patterns, no braces" "TSomePatternsNoBraces" $
      getActionByTitle title
  , goldenWithClass "Some patterns, with braces" "TSomePatternsSomeBraces" $
      getActionByTitle title
  , goldenWithClass "No patterns, with braces" "TNoPatternsSomeBraces" $
      getActionByTitle title

  -- Patterns on one line
  , goldenWithClass "Some patterns on one line, no braces" "TSomePatternsOnOneLineNoBraces" $
      getActionByTitle title
  , goldenWithClass "Some patterns on one line, with braces" "TSomePatternsOnOneLineSomeBraces" $
      getActionByTitle title

  -- Records
  , goldenWithClass "Records' field names are ignored" "TRecordsFieldNamesIgnored" $
      getActionByTitle title

  -- LambdaCase
  , expectNoCodeActionAvailable "LambdaCase" "TLambdaCase" -- TODO fix plugin for LambdaCase

  -- Inside where
  , expectNoCodeActionAvailable "Inside `where`, without signature" "TInsideWhereWithoutSignature"
  , goldenWithClass "Inside `where`" "TInsideWhere" $
      getActionByTitle title
  , goldenWithClass "Inside nested `where`" "TInsideNestedWhere" $
      getActionByTitle title

  -- Holes
  , goldenWithClass "Expression is `_`" "TExpressionIsUnderscore" $
      getActionByTitle title

  -- Inside let
  , goldenWithClass "Inside `let`'s declarations" "TInsideLetDeclarations" $
      getActionByTitle title
  , goldenWithClass "Inside `let`'s expression" "TInsideLetExpression" $
      getActionByTitle title

  -- Inside do
  , goldenWithClass "Inside `let`'s declarations inside `do`" "TInsideLetDeclarationsInsideDo" $
      getActionByTitle title
  , goldenWithClass "Inside `let`'s expression inside `do`" "TInsideLetExpressionInsideDo" $
      getActionByTitle title
  , goldenWithClass "Inside `do`" "TInsideDo" $
      getActionByTitle title

  -- Nested case expressions
  , goldenWithClass "Complete `case` nested in incomplete `case`" "TCompleteCaseInsideIncompleteCase" $
      getActionByTitle title
  , goldenWithRange "Incomplete `case` nested in complete `case`" "TIncompleteCaseInsideCompleteCase" $
      Range (Position 15 16) (Position 15 17)
  , goldenWithRange "Incomplete `case` nested in incomplete `case`" "TIncompleteCaseInsideIncompleteCase" $
      Range (Position 15 30) (Position 15 31)
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

-- TODO: give appropriate name
goldenWithRange :: TestName -> FilePath -> Range -> TestTree
goldenWithRange title path range =
  goldenWithHaskellDocInTmpDir def caseSplitPlugin title (mkFs $ FS.directProject (path <.> "hs")) path "expected" "hs" $ \doc -> do
    _ <- waitForDiagnosticsFrom doc
    [action] <- concatMap (^.. _CACodeAction) <$> getCodeActions doc range
    executeCodeAction action

goldenWithClass :: TestName -> FilePath -> ([CodeAction] -> Session CodeAction) -> TestTree
goldenWithClass title path findAction =
  goldenWithHaskellDocInTmpDir def caseSplitPlugin title (mkFs $ FS.directProject (path <.> "hs")) path "expected" "hs" $ \doc -> do
    _ <- waitForDiagnosticsFrom doc
    actions <- concatMap (^.. _CACodeAction) <$> getAllCodeActions doc
    action <- findAction actions
    executeCodeAction action

getActionByTitle :: T.Text -> [CodeAction] -> Session CodeAction
getActionByTitle title actions =
  case find (\a -> a ^. L.title == title) actions of
    Just a -> pure a
    Nothing -> liftIO $ assertFailure $ "Action " <> show title <> " not found in " <> show [a ^. L.title | a <- actions]

expectNoCodeActionAvailable :: TestName -> FilePath -> TestTree
expectNoCodeActionAvailable title path =
  testCase title $ do
    runSessionWithServer def caseSplitPlugin testDataDir $ do
      doc <- openDoc (path <.> "hs") "haskell"
      _ <- waitForDiagnosticsFrom doc
      caResults <- getAllCodeActions doc
      liftIO $ map (^? _CACodeAction . L.title) caResults
        @?= expectedActions
    where
      expectedActions = []

testDataDir :: FilePath
testDataDir = "plugins" </> "hls-case-split-plugin" </> "test" </> "testdata"

mkFs :: [FS.FileTree] -> FS.VirtualFileTree
mkFs = FS.mkVirtualFileTree testDataDir
