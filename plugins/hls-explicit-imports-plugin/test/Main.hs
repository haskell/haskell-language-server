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

explicitImportsPlugin :: PluginTestDescriptor ExplicitImports.Log
explicitImportsPlugin = mkPluginTestDescriptor ExplicitImports.descriptor "explicitImports"

longModule :: T.Text
longModule = "F" <> T.replicate 80 "o"

main :: IO ()
main = defaultTestRunner $
  testGroup
    "Make imports explicit"
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
    , testGroup "Title abbreviation"
      [ testCase "not abbreviated" $
          let i = "import " <> T.replicate 70 "F" <> " (Athing, Bthing, Cthing)"
          in ExplicitImports.abbreviateImportTitle i @?= i
      , testCase "abbreviated in module name" $
          let i = "import " <> T.replicate 120 "F" <> " (Athing, Bthing, Cthing)"
              o = "import " <> T.replicate 97 "F" <> " ... (3 items)"
          in ExplicitImports.abbreviateImportTitle i @?= o
      , testCase "abbreviated in import list" $
          let i = "import " <> T.replicate 78 "F" <> " (Athing, Bthing, Cthing, Dthing, Ething)"
              o = "import " <> T.replicate 78 "F" <> " (Athing, Bthing, ... (3 items))"
          in ExplicitImports.abbreviateImportTitle i @?= o
      -- This one breaks earlier in the same import item, but still splits the list in the same place
      , testCase "abbreviated in import list (slightly shorter module)" $
          let i = "import " <> T.replicate 76 "F" <> " (Athing, Bthing, Cthing, Dthing, Ething)"
              o = "import " <> T.replicate 76 "F" <> " (Athing, Bthing, ... (3 items))"
          in ExplicitImports.abbreviateImportTitle i @?= o
      -- This one breaks later in the same import item, but still splits the list in the same place
      , testCase "abbreviated in import list (slightly longer module)" $
          let i = "import " <> T.replicate 80 "F" <> " (Athing, Bthing, Cthing, Dthing, Ething)"
              o = "import " <> T.replicate 80 "F" <> " (Athing, Bthing, ... (3 items))"
          in ExplicitImports.abbreviateImportTitle i @?= o
      ]
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
