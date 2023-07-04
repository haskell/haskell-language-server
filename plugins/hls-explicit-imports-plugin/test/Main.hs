{-# LANGUAGE DisambiguateRecordFields #-}
{-# LANGUAGE NamedFieldPuns           #-}
{-# LANGUAGE OverloadedStrings        #-}
{-# LANGUAGE TypeOperators            #-}
{-# LANGUAGE ViewPatterns             #-}

module Main
  ( main
  ) where

import           Control.Lens                  ((^.))
import           Data.Foldable                 (find)
import           Data.Text                     (Text)
import qualified Data.Text                     as T
import qualified Ide.Plugin.ExplicitImports    as ExplicitImports
import qualified Language.LSP.Protocol.Lens    as L
import           Language.LSP.Protocol.Message
import           System.FilePath               ((</>))
import           Test.Hls

explicitImportsPlugin :: PluginTestDescriptor ExplicitImports.Log
explicitImportsPlugin = mkPluginTestDescriptor ExplicitImports.descriptor "explicitImports"

main :: IO ()
main = defaultTestRunner $
  testGroup
    "Make imports explicit"
    [ codeActionAllGoldenTest "UsualCase" 3 0
    , codeActionAllResolveGoldenTest "UsualCase" 3 0
    , codeActionOnlyGoldenTest "OnlyThis" 3 0
    , codeActionOnlyResolveGoldenTest "OnlyThis" 3 0
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

codeActionAllGoldenTest :: FilePath -> Int -> Int -> TestTree
codeActionAllGoldenTest fp l c = goldenWithExplicitImports " code action" fp codeActionNoResolveCaps $ \doc -> do
  actions <- getCodeActions doc (pointRange l c)
  case find ((== Just "Make all imports explicit") . caTitle) actions of
    Just (InR x) -> executeCodeAction x
    _            -> liftIO $ assertFailure "Unable to find CodeAction"

codeActionAllResolveGoldenTest :: FilePath -> Int -> Int -> TestTree
codeActionAllResolveGoldenTest fp l c = goldenWithExplicitImports " code action resolve" fp codeActionResolveCaps $ \doc -> do
  actions <- getCodeActions doc (pointRange l c)
  Just (InR x) <- pure $ find ((== Just "Make all imports explicit") . caTitle) actions
  resolved <- resolveCodeAction x
  executeCodeAction resolved

codeActionOnlyGoldenTest :: FilePath -> Int -> Int -> TestTree
codeActionOnlyGoldenTest fp l c = goldenWithExplicitImports " code action" fp codeActionNoResolveCaps $ \doc -> do
  actions <- getCodeActions doc (pointRange l c)
  case find ((== Just "Make this import explicit") . caTitle) actions of
    Just (InR x) -> executeCodeAction x
    _            -> liftIO $ assertFailure "Unable to find CodeAction"

codeActionOnlyResolveGoldenTest :: FilePath -> Int -> Int -> TestTree
codeActionOnlyResolveGoldenTest fp l c = goldenWithExplicitImports " code action resolve" fp codeActionResolveCaps $ \doc -> do
  actions <- getCodeActions doc (pointRange l c)
  Just (InR x) <- pure $ find ((== Just "Make this import explicit") . caTitle) actions
  resolved <- resolveCodeAction x
  executeCodeAction resolved

resolveCodeAction :: CodeAction -> Session CodeAction
resolveCodeAction ca = do
  resolveResponse <- request SMethod_CodeActionResolve ca
  Right resolved <- pure $ resolveResponse ^. L.result
  pure resolved

caTitle :: (Command |? CodeAction) -> Maybe Text
caTitle (InR CodeAction {_title}) = Just _title
caTitle _                         = Nothing

-- code lens tests

codeLensGoldenTest :: FilePath -> Int -> TestTree
codeLensGoldenTest fp _ = goldenWithExplicitImports " code lens" fp codeActionNoResolveCaps $ \doc -> do
  (codeLens: _) <- getCodeLenses doc
  CodeLens {_command = Just c} <- resolveCodeLens codeLens
  executeCmd c

resolveCodeLens :: CodeLens -> Session CodeLens
resolveCodeLens cl = do
  resolveResponse <- request SMethod_CodeLensResolve cl
  Right resolved <- pure $ resolveResponse ^. L.result
  pure resolved

-- Execute command and wait for result
executeCmd :: Command -> Session ()
executeCmd cmd = do
    executeCommand cmd
    _resp <- skipManyTill anyMessage (message SMethod_WorkspaceApplyEdit)
    -- liftIO $ print _resp
    return ()

-- helpers

goldenWithExplicitImports :: String -> FilePath -> ClientCapabilities -> (TextDocumentIdentifier -> Session ()) -> TestTree
goldenWithExplicitImports title fp caps = goldenWithHaskellAndCaps caps explicitImportsPlugin (fp <> title <> " (golden)") testDataDir fp "expected" "hs"

testDataDir :: String
testDataDir = "test" </> "testdata"

pointRange :: Int -> Int -> Range
pointRange
  (subtract 1 -> fromIntegral -> line)
  (subtract 1 -> fromIntegral -> col) =
    Range (Position line col) (Position line $ col + 1)
