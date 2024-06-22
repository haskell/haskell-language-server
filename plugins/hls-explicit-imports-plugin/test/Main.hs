{-# LANGUAGE DisambiguateRecordFields #-}
{-# LANGUAGE LambdaCase               #-}
{-# LANGUAGE OverloadedStrings        #-}
{-# LANGUAGE ViewPatterns             #-}
module Main
  ( main
  ) where

import           Control.Lens                  ((^.))
import           Data.Either.Extra
import           Data.Foldable                 (find)
import           Data.Text                     (Text)
import qualified Data.Text                     as T
import           Data.Traversable              (for)
import qualified Ide.Plugin.ExplicitImports    as ExplicitImports
import qualified Language.LSP.Protocol.Lens    as L
import           Language.LSP.Protocol.Message
import           System.FilePath               ((</>))
import           Test.Hls

explicitImportsPlugin :: PluginTestDescriptor ExplicitImports.Log
explicitImportsPlugin = mkPluginTestDescriptor ExplicitImports.descriptor "explicitImports"

main :: IO ()
main = defaultTestRunner $ testGroup "import-actions"
  [testGroup
    "Refine Imports"
    [ codeActionGoldenTest "RefineWithOverride" 3 1
    , codeLensGoldenTest isRefineImports "RefineUsualCase" 1
    , codeLensGoldenTest isRefineImports "RefineQualified" 0
    , codeLensGoldenTest isRefineImports "RefineQualifiedExplicit" 0
    ],
  testGroup
    "Make imports explicit"
    [ codeActionAllGoldenTest "ExplicitUsualCase" 3 0
    , codeActionAllResolveGoldenTest "ExplicitUsualCase" 3 0
    , inlayHintsTest "ExplicitUsualCase" 2 $ (@=?)
        [InlayHint
           { _position = Position {_line = 2, _character = 16}
           , _label = InL "( a1 )"
           , _kind = Just InlayHintKind_Type
           , _textEdits = Just [TextEdit (Range (Position 2 0) (Position 2 16)) "import ExplicitA ( a1 )"]
           , _tooltip = Nothing
           , _paddingLeft = Just True
           , _paddingRight = Nothing
           , _data_ = Nothing
           }]
    , codeActionOnlyGoldenTest "ExplicitOnlyThis" 3 0
    , codeActionOnlyResolveGoldenTest "ExplicitOnlyThis" 3 0
    , inlayHintsTest "ExplicitOnlyThis" 3 $ (@=?)
        [InlayHint
           { _position = Position {_line = 3, _character = 16}
           , _label = InL "( b1 )"
           , _kind = Just InlayHintKind_Type
           , _textEdits = Just [TextEdit (Range (Position 3 0) (Position 3 16)) "import ExplicitB ( b1 )"]
           , _tooltip = Nothing
           , _paddingLeft = Just True
           , _paddingRight = Nothing
           , _data_ = Nothing
           }]
    , codeLensGoldenTest notRefineImports "ExplicitUsualCase" 0
    , codeActionBreakFile "ExplicitBreakFile" 4 0
    , inlayHintsTest "ExplicitBreakFile" 3 $ (@=?)
        [InlayHint
           { _position = Position {_line = 3, _character = 16}
           , _label = InL "( a1 )"
           , _kind = Just InlayHintKind_Type
           , _textEdits = Just [TextEdit (Range (Position 3 0) (Position 3 16)) "import ExplicitA ( a1 )"]
           , _tooltip = Nothing
           , _paddingLeft = Just True
           , _paddingRight = Nothing
           , _data_ = Nothing
           }]
    , codeActionStaleAction "ExplicitStaleAction" 4 0
    , testCase "No CodeAction when exported" $
      runSessionWithServer def explicitImportsPlugin testDataDir $ do
        doc <- openDoc "ExplicitExported.hs" "haskell"
        action <- getCodeActions doc (pointRange 3 0)
        liftIO $ action @?= []
    , testCase "No CodeLens when exported" $
      runSessionWithServer def explicitImportsPlugin testDataDir $ do
        doc <- openDoc "ExplicitExported.hs" "haskell"
        lenses <- getCodeLenses doc
        liftIO $ lenses @?= []
    , testCase "No InlayHints when exported" $
      runSessionWithServer def explicitImportsPlugin testDataDir $ do
        doc <- openDoc "ExplicitExported.hs" "haskell"
        inlayHints <- getInlayHints doc (pointRange 3 0)
        liftIO $ inlayHints @?= []
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
    , testGroup "Title abbreviation without module"
      [ testCase "not abbreviated" $
          let i = "import M (" <> T.replicate 70 "F" <> ", Athing, Bthing, Cthing)"
              o = "(FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF, Athing, Bthing, Cthing)"
          in ExplicitImports.abbreviateImportTitleWithoutModule i @?= o
      , testCase "abbreviated that drop module name" $
          let i = "import " <> T.replicate 120 "F" <> " (Athing, Bthing, Cthing)"
              o = "(Athing, Bthing, Cthing)"
          in ExplicitImports.abbreviateImportTitleWithoutModule i @?= o
      , testCase "abbreviated in import list" $
          let i = "import M (Athing, Bthing, " <> T.replicate 100 "F" <> ", Cthing, Dthing, Ething)"
              o = "(Athing, Bthing, ... (4 items))"
          in ExplicitImports.abbreviateImportTitleWithoutModule i @?= o
      ]
    ]]

-- code action tests

codeActionAllGoldenTest :: FilePath -> Int -> Int -> TestTree
codeActionAllGoldenTest fp l c = goldenWithImportActions " code action" fp codeActionNoResolveCaps $ \doc -> do
  actions <- getCodeActions doc (pointRange l c)
  case find ((== Just "Make all imports explicit") . caTitle) actions of
    Just (InR x) -> executeCodeAction x
    _            -> liftIO $ assertFailure "Unable to find CodeAction"

codeActionBreakFile :: FilePath -> Int -> Int -> TestTree
codeActionBreakFile fp l c = goldenWithImportActions " code action" fp codeActionNoResolveCaps $ \doc -> do
  _ <- getCodeLenses doc
  changeDoc doc [edit]
  actions <- getCodeActions doc (pointRange l c)
  case find ((== Just "Make all imports explicit") . caTitle) actions of
    Just (InR x) -> executeCodeAction x
    _            -> liftIO $ assertFailure "Unable to find CodeAction"
  where edit = TextDocumentContentChangeEvent $ InL
          TextDocumentContentChangePartial
            { _range = pointRange 2 29
            , _rangeLength = Nothing
            , _text = "x"
            }

codeActionStaleAction :: FilePath -> Int -> Int -> TestTree
codeActionStaleAction fp l c = goldenWithImportActions " code action" fp codeActionResolveCaps $ \doc -> do
  _ <- waitForDiagnostics
  actions <- getCodeActions doc (pointRange l c)
  changeDoc doc [edit]
  _ <- waitForDiagnostics
  case find ((== Just "Make this import explicit") . caTitle) actions of
    Just (InR x) ->
      maybeResolveCodeAction x >>=
        \case Just _  -> liftIO $ assertFailure "Code action still valid"
              Nothing -> pure ()
    _            -> liftIO $ assertFailure "Unable to find CodeAction"
  where edit = TextDocumentContentChangeEvent $ InL
          TextDocumentContentChangePartial
            { _range = Range (Position 6 0) (Position 6 0)
            , _rangeLength = Nothing
            , _text = "\ntesting = undefined"
            }

codeActionAllResolveGoldenTest :: FilePath -> Int -> Int -> TestTree
codeActionAllResolveGoldenTest fp l c = goldenWithImportActions " code action resolve" fp codeActionResolveCaps $ \doc -> do
  actions <- getCodeActions doc (pointRange l c)
  Just (InR x) <- pure $ find ((== Just "Make all imports explicit") . caTitle) actions
  resolved <- resolveCodeAction x
  executeCodeAction resolved

codeActionOnlyGoldenTest :: FilePath -> Int -> Int -> TestTree
codeActionOnlyGoldenTest fp l c = goldenWithImportActions " code action" fp codeActionNoResolveCaps $ \doc -> do
  actions <- getCodeActions doc (pointRange l c)
  case find ((== Just "Make this import explicit") . caTitle) actions of
    Just (InR x) -> executeCodeAction x
    _            -> liftIO $ assertFailure "Unable to find CodeAction"

codeActionOnlyResolveGoldenTest :: FilePath -> Int -> Int -> TestTree
codeActionOnlyResolveGoldenTest fp l c = goldenWithImportActions " code action resolve" fp codeActionResolveCaps $ \doc -> do
  actions <- getCodeActions doc (pointRange l c)
  Just (InR x) <- pure $ find ((== Just "Make this import explicit") . caTitle) actions
  resolved <- resolveCodeAction x
  executeCodeAction resolved

maybeResolveCodeAction :: CodeAction -> Session (Maybe CodeAction)
maybeResolveCodeAction ca = do
  resolveResponse <- request SMethod_CodeActionResolve ca
  let resolved = resolveResponse ^. L.result
  pure $ eitherToMaybe resolved

caTitle :: (Command |? CodeAction) -> Maybe Text
caTitle (InR CodeAction {_title}) = Just _title
caTitle _                         = Nothing

-- code lens tests

codeLensGoldenTest :: (CodeLens -> Bool) -> FilePath -> Int -> TestTree
codeLensGoldenTest predicate fp i = goldenWithImportActions " code lens" fp codeActionNoResolveCaps $ \doc -> do
  codeLenses <- getCodeLenses doc
  resolvedCodeLenses <- for codeLenses resolveCodeLens
  (CodeLens {_command = Just c}) <- pure (filter predicate resolvedCodeLenses !! i)
  executeCmd c

notRefineImports :: CodeLens -> Bool
notRefineImports (CodeLens _ (Just (Command text _ _)) _)
  | "Refine imports to" `T.isPrefixOf` text = False
notRefineImports _ = True

inlayHintsTest :: FilePath -> UInt -> ([InlayHint] -> Assertion) -> TestTree
inlayHintsTest fp line assert = testCase (fp ++ " inlay hints") $ runSessionWithServer def explicitImportsPlugin testDataDir $ do
  doc <- openDoc (fp ++ ".hs") "haskell"
  inlayHints <- getInlayHints doc (lineRange line)
  liftIO $ assert inlayHints
  where
    -- zero-based position
    lineRange line = Range (Position line 0) (Position line 1000)

-- Execute command and wait for result
executeCmd :: Command -> Session ()
executeCmd cmd = do
    executeCommand cmd
    _resp <- skipManyTill anyMessage (message SMethod_WorkspaceApplyEdit)
    -- liftIO $ print _resp
    return ()

-- helpers

goldenWithImportActions :: String -> FilePath -> ClientCapabilities -> (TextDocumentIdentifier -> Session ()) -> TestTree
goldenWithImportActions title fp caps = goldenWithHaskellAndCaps def caps explicitImportsPlugin (fp <> title <> " (golden)") testDataDir fp "expected" "hs"

testDataDir :: String
testDataDir = "plugins" </> "hls-explicit-imports-plugin" </> "test" </> "testdata"

pointRange :: Int -> Int -> Range
pointRange
  (subtract 1 -> fromIntegral -> line)
  (subtract 1 -> fromIntegral -> col) =
    Range (Position line col) (Position line $ col + 1)

-------------------------------------------------------------------------------
-- code action tests

codeActionGoldenTest :: FilePath -> Int -> Int -> TestTree
codeActionGoldenTest fp l c = goldenWithImportActions "" fp codeActionNoResolveCaps $ \doc -> do
  actions <- getCodeActions doc (pointRange l c)
  case find ((== Just "Refine all imports") . caTitle) actions of
    Just (InR x) -> executeCodeAction x
    _            -> liftIO $ assertFailure "Unable to find CodeAction"

isRefineImports :: CodeLens -> Bool
isRefineImports (CodeLens _ (Just (Command txt _ _)) _)
  | "Refine imports to" `T.isInfixOf` txt = True
isRefineImports _ = False
