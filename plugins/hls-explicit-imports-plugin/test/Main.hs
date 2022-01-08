{-# LANGUAGE DisambiguateRecordFields #-}
{-# LANGUAGE NamedFieldPuns           #-}
{-# LANGUAGE OverloadedStrings        #-}
{-# LANGUAGE TypeOperators            #-}
{-# LANGUAGE ViewPatterns             #-}

module Main
  ( main
  ) where

import           Data.Foldable              (find, forM_)
import           Data.Text                  (Text)
import qualified Data.Text                  as T
import qualified Ide.Plugin.ExplicitImports as ExplicitImports
import           System.FilePath            ((<.>), (</>))
import           Test.Hls

explicitImportsPlugin :: PluginDescriptor IdeState
explicitImportsPlugin = ExplicitImports.descriptor mempty "explicitImports"


main :: IO ()
main = defaultTestRunner $
  testGroup
    "Refine Imports"
    [ codeActionGoldenTest "UsualCase" 3 0
    , codeLensGoldenTest "UsualCase" 0
    , testCase "No CodeAction when exported" $
      runSessionWithServer explicitImportsPlugin testDataDir $ do
        doc <- openDoc "Exported.hs" "haskell"
        action <- getCodeActions doc (pointRange 3 0)
        liftIO $ action @?= []
    , testCase "No CodeLens when exported" $
      runSessionWithServer explicitImportsPlugin testDataDir $ do
        doc <- openDoc "Exported.hs" "haskell"
        lenses <- getCodeLenses doc
        liftIO $ lenses @?= []
    ]

-- code action tests

codeActionGoldenTest :: FilePath -> Int -> Int -> TestTree
codeActionGoldenTest fp l c = goldenWithExplicitImports fp $ \doc -> do
  actions <- getCodeActions doc (pointRange l c)
  case find ((== Just "Make all imports explicit") . caTitle) actions of
    Just (InR x) -> executeCodeAction x
    _            -> liftIO $ assertFailure "Unable to find CodeAction"

caTitle :: (Command |? CodeAction) -> Maybe Text
caTitle (InR CodeAction {_title}) = Just _title
caTitle _                         = Nothing

-- code lens tests

codeLensGoldenTest :: FilePath -> Int -> TestTree
codeLensGoldenTest fp codeLensIdx = goldenWithExplicitImports fp $ \doc -> do
  codeLens <- (!! codeLensIdx) <$> getCodeLensesBy isExplicitImports doc
  mapM_ executeCmd
    [c | CodeLens{_command = Just c} <- [codeLens]]

getCodeLensesBy :: (CodeLens -> Bool) -> TextDocumentIdentifier -> Session [CodeLens]
getCodeLensesBy f doc = filter f <$> getCodeLenses doc

isExplicitImports :: CodeLens -> Bool
isExplicitImports (CodeLens _ (Just (Command _ cmd _)) _)
  | ":explicitImports:" `T.isInfixOf` cmd = True
isExplicitImports _ = False

-- Execute command and wait for result
executeCmd :: Command -> Session ()
executeCmd cmd = do
    executeCommand cmd
    _resp <- skipManyTill anyMessage (message SWorkspaceApplyEdit)
    -- liftIO $ print _resp
    return ()

-- helpers

goldenWithExplicitImports :: FilePath -> (TextDocumentIdentifier -> Session ()) -> TestTree
goldenWithExplicitImports fp = goldenWithHaskellDoc explicitImportsPlugin (fp <> " (golden)") testDataDir fp "expected" "hs"

testDataDir :: String
testDataDir = "test" </> "testdata"

pointRange :: Int -> Int -> Range
pointRange
  (subtract 1 -> fromIntegral -> line)
  (subtract 1 -> fromIntegral -> col) =
    Range (Position line col) (Position line $ col + 1)
