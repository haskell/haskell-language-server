{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedLists   #-}
{-# LANGUAGE OverloadedStrings #-}

module Main
  ( main
  ) where

import           Control.Exception             (catch)
import           Control.Lens                  (Prism', prism', view, (^.),
                                                (^..), (^?))
import           Control.Monad                 (void)
import           Data.Foldable                 (find)
import qualified Data.List                     as List
import           Data.Maybe
import qualified Data.Text                     as T
import qualified Ide.Plugin.Class              as Class
import qualified Language.LSP.Protocol.Lens    as L
import           Language.LSP.Protocol.Message
import           System.FilePath
import           Test.Hls

main :: IO ()
main = defaultTestRunner tests

classPlugin :: PluginTestDescriptor Class.Log
classPlugin = mkPluginTestDescriptor Class.descriptor "class"

tests :: TestTree
tests = testGroup
  "class"
  [ codeActionTests
  , codeLensTests
  ]

codeActionTests :: TestTree
codeActionTests = testGroup
  "code actions"
  [ expectCodeActionsAvailable "Produces addMinimalMethodPlaceholders code actions for one instance" "T1"
      [ "Add placeholders for '=='"
      , "Add placeholders for '==' with signature(s)"
      , "Add placeholders for '/='"
      , "Add placeholders for '/=' with signature(s)"
      , "Add placeholders for all missing methods"
      , "Add placeholders for all missing methods with signature(s)"
      ]
  , goldenWithClass "Creates a placeholder for '=='" "T1" "eq" $
      getActionByTitle "Add placeholders for '=='"
  , goldenWithClass "Creates a placeholder for '/='" "T1" "ne" $
      getActionByTitle "Add placeholders for '/='"
  , goldenWithClass "Creates a placeholder for both '==' and '/='" "T1" "all" $
      getActionByTitle "Add placeholders for all missing methods"
  , goldenWithClass "Creates a placeholder for 'fmap'" "T2" "fmap" $
      getActionByTitle "Add placeholders for 'fmap'"
  , goldenWithClass "Creates a placeholder for multiple methods 1" "T3" "1" $
      getActionByTitle "Add placeholders for 'f','g'"
  , goldenWithClass "Creates a placeholder for multiple methods 2" "T3" "2" $
      getActionByTitle "Add placeholders for 'g','h'"
  , goldenWithClass "Creates a placeholder for a method starting with '_'" "T4" "" $
      getActionByTitle "Add placeholders for '_f'"
  , goldenWithClass "Creates a placeholder for '==' with extra lines" "T5" "" $
      getActionByTitle "Add placeholders for '=='"
  , goldenWithClass "Creates a placeholder for only the unimplemented methods of multiple methods" "T6" "1" $
      getActionByTitle "Add placeholders for 'g'"
  , goldenWithClass "Creates a placeholder for other two methods" "T6" "2" $
      getActionByTitle "Add placeholders for 'g','h'"
  , goldenWithClass "Creates a placeholder when all top-level decls are indented" "T7" "" $
      getActionByTitle "Add placeholders for 'g','h','i'"
  , testGroup "with preprocessors"
    [ knownBrokenInEnv [GhcVer GHC910]
        "See issue https://github.com/haskell/haskell-language-server/issues/4731 for details." $
        goldenWithClass "Creates a placeholder for '<>'" "T8" "diamond" $
      getActionByTitle "Add placeholders for '<>'"
      ]
  , goldenWithClass "Don't insert pragma with GHC2021" "InsertWithGHC2021Enabled" "" $
      getActionByTitle "Add placeholders for '==' with signature(s)"
  , goldenWithClass "Insert pragma if not exist" "InsertWithoutPragma" "" $
      getActionByTitle "Add placeholders for '==' with signature(s)"
  , goldenWithClass "Don't insert pragma if exist" "InsertWithPragma" "" $
      getActionByTitle "Add placeholders for '==' with signature(s)"
  , goldenWithClass "Only insert pragma once" "InsertPragmaOnce" "" $
      getActionByTitle "Add placeholders for 'pure','<*>' with signature(s)"
  , expectCodeActionsAvailable "No code action available when minimal requirements meet" "MinimalDefinitionMeet" []
  , expectCodeActionsAvailable "Add placeholders for all missing methods is unavailable when all methods are required" "AllMethodsRequired"
      [ "Add placeholders for 'f','g'"
      , "Add placeholders for 'f','g' with signature(s)"
      ]
  , testCase "Update text document version" $ runSessionWithServer def classPlugin testDataDir $ do
    doc <- createDoc "Version.hs" "haskell" "module Version where"
    ver1 <- (^. L.version) <$> getVersionedDoc doc
    liftIO $ ver1 @?= 0

    -- Change the doc to ensure the version is not 0
    changeDoc doc
        [ TextDocumentContentChangeEvent . InR . TextDocumentContentChangeWholeDocument $
            T.unlines ["module Version where", "data A a = A a", "instance Functor A where"]
        ]
    ver2 <- (^. L.version) <$> getVersionedDoc doc
    _ <- waitForDiagnostics
    liftIO $ ver2 @?= 1

    -- Execute the action and see what the version is
    action <- head . concatMap (^.. _CACodeAction) <$> getAllCodeActions doc
    executeCodeAction action
    _ <- waitForDiagnostics
    ver3 <- (^. L.version) <$> getVersionedDoc doc
    liftIO $ ver3 @?= 2
    pure mempty
  ]

codeLensTests :: TestTree
codeLensTests = testGroup
    "code lens"
    [ testCase "Has code lens" $ do
        runSessionWithServer def classPlugin testDataDir $ do
            doc <- openDoc "CodeLensSimple.hs" "haskell"
            lens <- getAndResolveCodeLenses doc
            let titles = map (^. L.title) $ mapMaybe (^. L.command) lens
            liftIO $ List.sort titles @?=
                [ "(==) :: A -> A -> Bool"
                , "(==) :: B -> B -> Bool"
                ]
    , testCase "No lens for TH" $ do
        runSessionWithServer def classPlugin testDataDir $ do
            doc <- openDoc "TH.hs" "haskell"
            lens <- getAndResolveCodeLenses doc
            liftIO $ length lens @?= 0
    , testCase "Do not construct error action!, Ticket3942one" $ do
        runSessionWithServer def classPlugin testDataDir $ do
            doc <- openDoc "Ticket3942one.hs" "haskell"
            _ <- waitForDiagnosticsFrom doc
            lens <- getAllCodeActions doc
            -- should switch to `liftIO $ length lens @?= 2, when Ticket3942 is entirely fixed`
            -- current fix is just to make sure the code does not throw an exception that would mess up
            -- the client UI.
            liftIO $ length lens > 0 @?= True
        `catch` \(e :: SessionException) -> do
          liftIO $ assertFailure $ "classPluginTestError: "++ show e
    , goldenCodeLens "Apply code lens" "CodeLensSimple" 0
    , goldenCodeLens "Apply code lens for local class" "LocalClassDefine" 0
    , goldenCodeLens "Apply code lens on the same line" "Inline" 0
    , goldenCodeLens "Don't insert pragma while existing" "CodeLensWithPragma" 0
    , goldenCodeLens "Don't insert pragma while GHC2021 enabled" "CodeLensWithGHC2021" 0
    , goldenCodeLens "Qualified name" "Qualified" 0
    , goldenCodeLens "Type family" "TypeFamily" 0
    , testCase "keep stale lens" $ do
        runSessionWithServer def classPlugin testDataDir $ do
            doc <- openDoc "Stale.hs" "haskell"
            oldLens <- getAndResolveCodeLenses doc
            let edit = TextEdit (mkRange 4 11 4 12) "" -- Remove the `_`
            _ <- applyEdit doc edit
            newLens <- getAndResolveCodeLenses doc
            liftIO $ (view L.command <$> newLens ) @?= (view L.command <$> oldLens)
    ]

_CACodeAction :: Prism' (Command |? CodeAction) CodeAction
_CACodeAction = prism' InR $ \case
  InR action -> Just action
  _          -> Nothing

goldenCodeLens :: TestName -> FilePath -> Int -> TestTree
goldenCodeLens title path idx =
    goldenWithHaskellDoc def classPlugin title testDataDir path "expected" "hs" $ \doc -> do
        lens <- getAndResolveCodeLenses doc
        executeCommand $ fromJust $ (List.sort lens !! idx) ^. L.command
        void $ skipManyTill anyMessage (message SMethod_WorkspaceApplyEdit)

goldenWithClass ::TestName -> FilePath -> FilePath -> ([CodeAction] -> Session CodeAction) -> TestTree
goldenWithClass title path desc findAction =
  goldenWithHaskellDoc def classPlugin title testDataDir path (desc <.> "expected") "hs" $ \doc -> do
    _ <- waitForDiagnosticsFrom doc
    actions <- concatMap (^.. _CACodeAction) <$> getAllCodeActions doc
    action <- findAction actions
    executeCodeAction action
    void $ skipManyTill anyMessage (getDocumentEdit doc)

getActionByTitle :: T.Text -> [CodeAction] -> Session CodeAction
getActionByTitle title actions = case find (\a -> a ^. L.title == title) actions of
    Just a -> pure a
    Nothing -> liftIO $ assertFailure $ "Action " <> show title <> " not found in " <> show [a ^. L.title | a <- actions]

expectCodeActionsAvailable :: TestName -> FilePath -> [T.Text] -> TestTree
expectCodeActionsAvailable title path actionTitles =
  testCase title $ do
    runSessionWithServer def classPlugin testDataDir $ do
      doc <- openDoc (path <.> "hs") "haskell"
      _ <- waitForDiagnosticsFrom doc
      caResults <- getAllCodeActions doc
      liftIO $ map (^? _CACodeAction . L.title) caResults
        @?= expectedActions
    where
      expectedActions = Just <$> actionTitles

testDataDir :: FilePath
testDataDir = "plugins" </> "hls-class-plugin" </> "test" </> "testdata"
