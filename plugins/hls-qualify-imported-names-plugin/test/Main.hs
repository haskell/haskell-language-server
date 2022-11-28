{-# LANGUAGE BlockArguments    #-}
{-# LANGUAGE MultiWayIf        #-}
{-# LANGUAGE NamedFieldPuns    #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TypeOperators     #-}

module Main (main) where

import           Data.Foldable                   (find)
import           Data.Text                       (Text)
import qualified Ide.Plugin.QualifyImportedNames as QualifyImportedNames
import           System.FilePath                 ((</>))
import           Test.Hls                        (CodeAction (CodeAction, _title),
                                                  Command (Command), IdeState,
                                                  MonadIO (liftIO),
                                                  PluginDescriptor,
                                                  PluginTestDescriptor,
                                                  Position (Position),
                                                  Range (Range), Session,
                                                  TestName, TestTree,
                                                  TextDocumentIdentifier,
                                                  assertBool, assertFailure,
                                                  defaultTestRunner,
                                                  executeCodeAction,
                                                  getCodeActions,
                                                  goldenWithHaskellDoc,
                                                  mkPluginTestDescriptor',
                                                  openDoc, rename,
                                                  runSessionWithServer,
                                                  testCase, testGroup,
                                                  type (|?) (InR), (@?=))

import           Prelude

-- 1's based
data Point = Point {
  line   :: !Int,
  column :: !Int
}

makePoint line column
  | line >= 1 && column >= 1 = Point line column
  | otherwise = error "Line or column is less than 1."

isNotEmpty :: Foldable f => f a -> Bool
isNotEmpty = not . isEmpty

isEmpty :: Foldable f => f a -> Bool
isEmpty = null

makeCodeActionNotFoundAtString :: Point -> String
makeCodeActionNotFoundAtString Point {..} =
  "CodeAction not found at line: " <> show line <> ", column: " <> show column

makeCodeActionFoundAtString :: Point -> String
makeCodeActionFoundAtString Point {..} =
  "CodeAction found at line: " <> show line <> ", column: " <> show column

main :: IO ()
main = defaultTestRunner $ testGroup "Qualify Imported Names"
  [
    testCase "No CodeAction when not at import" $
      runSessionWithServer pluginDescriptor testDataDir $ do
        let point = makePoint 1 1
        document <- openDoc "NoImport.hs" "haskell"
        actions <- getCodeActions document $ pointToRange point
        liftIO $ assertBool (makeCodeActionFoundAtString point) (isEmpty actions)
  , testCase "No CodeAction when import is qualified" $
      runSessionWithServer pluginDescriptor testDataDir $ do
        let point = makePoint 3 1
        document <- openDoc "QualifiedImport.hs" "haskell"
        actions <- getCodeActions document $ pointToRange point
        liftIO $ assertBool (makeCodeActionFoundAtString point) (isEmpty actions)
  , codeActionGoldenTest
      "CodeAction qualifies names with alias if imported module is aliased"
      "AliasedImport"
      (makePoint 3 1)
  , codeActionGoldenTest
      "CodeAction qualifies names with module name if imported module is not aliased"
      "UnaliasedImport"
      (makePoint 3 1)
  , codeActionGoldenTest
      "CodeAction qualifies only names in import's explicit non-hiding list"
      "ExplicitImport"
      (makePoint 4 1)
  , codeActionGoldenTest
      "CodeAction qualifies only names outside of import's explicit hiding list"
      "ExplicitHidingImport"
      (makePoint 4 1)
  , codeActionGoldenTest
      "CodeAction can qualify names not defined in modules they are imported from"
      "Reexported"
      (makePoint 3 1)
  , codeActionGoldenTest
      "CodeAction can qualify explicitly imported Prelude"
      "ExplicitPrelude"
      (makePoint 3 1)
  , codeActionGoldenTest
      "CodeAction qualifies only imported names"
      "OnlyImportedNames"
      (makePoint 3 1)
  , codeActionGoldenTest
      "CodeAction qualifies parenthesized operators properly"
      "Parenthesized"
      (makePoint 3 1)
  , codeActionGoldenTest
      "CodeAction qualifies backticked operators properly"
      "Backticked"
      (makePoint 3 1)
  , codeActionGoldenTest
      "CodeAction qualifies parenthesized and backticked operators on the same line properly"
      "SameLine"
      (makePoint 3 1)
  , codeActionGoldenTest
      "CodeAction doesn't qualify already qualified names"
      "NoDoubleQualify"
      (makePoint 3 1)
  ]

codeActionGoldenTest :: TestName -> FilePath -> Point -> TestTree
codeActionGoldenTest testCaseName goldenFilename point =
  goldenWithQualifyImportedNames testCaseName goldenFilename $ \document -> do
    actions <- getCodeActions document $ pointToRange point
    case find ((== Just "Qualify imported names") . getCodeActionTitle) actions of
      Just (InR codeAction) -> executeCodeAction codeAction
      _ -> liftIO $ assertFailure $ makeCodeActionNotFoundAtString point

testDataDir :: String
testDataDir = "test" </> "data"

pluginDescriptor :: PluginTestDescriptor ()
pluginDescriptor = mkPluginTestDescriptor' QualifyImportedNames.descriptor "qualifyImportedNames"

getCodeActionTitle :: (Command |? CodeAction) -> Maybe Text
getCodeActionTitle commandOrCodeAction
  | InR CodeAction {_title} <- commandOrCodeAction = Just _title
  | otherwise = Nothing

goldenWithQualifyImportedNames :: TestName -> FilePath -> (TextDocumentIdentifier -> Session ()) -> TestTree
goldenWithQualifyImportedNames testName path =
  goldenWithHaskellDoc pluginDescriptor testName testDataDir path "expected" "hs"

pointToRange :: Point -> Range
pointToRange Point {..}
  | line <- fromIntegral $ subtract 1 line
  , column <- fromIntegral $ subtract 1 column =
      Range (Position line column) (Position line $ column + 1)

