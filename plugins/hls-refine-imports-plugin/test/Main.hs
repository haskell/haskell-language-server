{-# LANGUAGE DisambiguateRecordFields #-}
{-# LANGUAGE NamedFieldPuns           #-}
{-# LANGUAGE OverloadedStrings        #-}
{-# LANGUAGE TypeOperators            #-}
{-# LANGUAGE ViewPatterns             #-}

module Main (main) where

import qualified Data.ByteString.Lazy     as LBS
import           Data.Foldable            (find, forM_)
import           Data.Text                (Text)
import qualified Data.Text                as T
import           Data.Text.Encoding       (encodeUtf8)
import qualified Ide.Plugin.RefineImports as RefineImports
import           System.FilePath          ((<.>), (</>))
import           Test.Hls

main :: IO ()
main = defaultTestRunner $
  testGroup
    "Refine Imports"
    [ codeActionGoldenTest "WithOverride" 3 1
    , codeLensGoldenTest "UsualCase" 1
    ]

plugin :: PluginDescriptor IdeState
plugin = RefineImports.descriptor "refineImports"

-- code action tests

codeActionGoldenTest :: FilePath -> Int -> Int -> TestTree
codeActionGoldenTest fp l c = goldenGitDiff (fp <> " (golden)") goldenFilePath $
  runSessionWithServer plugin testDataDir $ do
    doc <- openDoc hsFilePath "haskell"
    actions <- getCodeActions doc (pointRange l c)
    case find ((== Just "Refine all imports") . caTitle) actions of
      Just (InR x) -> do
        executeCodeAction x
        LBS.fromStrict . encodeUtf8 <$> documentContents doc
      _ -> liftIO $ assertFailure "Unable to find CodeAction"
  where
    hsFilePath = fp <.> "hs"
    goldenFilePath = testDataDir </> fp <.> "expected" <.> "hs"

caTitle :: (Command |? CodeAction) -> Maybe Text
caTitle (InR CodeAction {_title}) = Just _title
caTitle _                         = Nothing


-- code lens tests

codeLensGoldenTest :: FilePath -> Int -> TestTree
codeLensGoldenTest fp codeLensIdx = goldenGitDiff (fp <> " (golden)") goldenFilePath $
  runSessionWithServer plugin testDataDir $ do
    doc <- openDoc hsFilePath "haskell"
    codeLens <- (!! codeLensIdx) <$> getCodeLensesBy isRefineImports doc
    mapM_ executeCmd
      [c | CodeLens{_command = Just c} <- [codeLens]]
    LBS.fromStrict . encodeUtf8 <$> documentContents doc
  where
    hsFilePath = fp <.> "hs"
    goldenFilePath = testDataDir </> fp <.> "expected" <.> "hs"

getCodeLensesBy :: (CodeLens -> Bool) -> TextDocumentIdentifier -> Session [CodeLens]
getCodeLensesBy f doc = filter f <$> getCodeLenses doc

isRefineImports :: CodeLens -> Bool
isRefineImports (CodeLens _ (Just (Command _ cmd _)) _)
  | ":refineImports:" `T.isInfixOf` cmd = True
isRefineImports _ = False

-- Execute command and wait for result
executeCmd :: Command -> Session ()
executeCmd cmd = do
    executeCommand cmd
    _resp <- skipManyTill anyMessage (message SWorkspaceApplyEdit)
    -- liftIO $ print _resp
    return ()

-- helpers

testDataDir :: String
testDataDir = "test" </> "testdata"

pointRange :: Int -> Int -> Range
pointRange
  (subtract 1 -> line)
  (subtract 1 -> col) =
    Range (Position line col) (Position line $ col + 1)
