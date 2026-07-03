
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs     #-}

module CradleTests (tests) where

import           Config                          (checkDefs, dummyPlugin,
                                                  lspTestCaps, mkIdeTestFs, mkL,
                                                  runWithExtraFiles,
                                                  testWithDummyPluginEmpty')
import           Control.Applicative.Combinators
import           Control.Lens                    ((^.))
import           Control.Monad.IO.Class          (liftIO)
import qualified Data.Aeson                      as A
import           Data.Proxy                      (Proxy (..))
import qualified Data.Text                       as T
import           Development.IDE.GHC.Util
import           Development.IDE.Plugin.Test     (TestRequest (..),
                                                  WaitForIdeRuleResult (..))
import           Development.IDE.Test            (expectDiagnostics,
                                                  expectDiagnosticsWithTags,
                                                  expectNoMoreDiagnostics,
                                                  isReferenceReady,
                                                  waitForAction)
import           Development.IDE.Types.Location
import           GHC.TypeLits                    (symbolVal)
import           Ide.Types                       (Config (..),
                                                  SessionLoadingPreferenceConfig (..))
import qualified Language.LSP.Protocol.Lens      as L
import           Language.LSP.Protocol.Message
import           Language.LSP.Protocol.Types     hiding
                                                 (SemanticTokenAbsolute (..),
                                                  SemanticTokenRelative (..),
                                                  SemanticTokensEdit (..),
                                                  mkRange)
import           Language.LSP.Test
import           System.FilePath
import           Test.Hls                        (TestConfig (..), def,
                                                  runSessionWithTestConfig,
                                                  waitForBuildQueue, setHlsConfig)
import           Test.Hls.FileSystem
import           Test.Hls.Util                   (EnvSpec (..), OS (..),
                                                  ignoreInEnv)
import           Test.Tasty
import           Test.Tasty.HUnit
import Control.Monad (when)


tests :: TestTree
tests = testGroup "cradle"
    [testGroup "dependencies" $ bothLoadings1 sessionDepsArePickedUp
    ,testGroup "ignore-fatal" $ bothLoadings1 ignoreFatalWarning
    ,testGroup "loading" $ bothLoadings $ \initM -> [loadCradleOnlyonce initM, retryFailedCradle initM]
    ,testGroup "regression.batch" batchLoadRegressionTests
    ,testGroup "multi"   (multiTests "multi")
    ,testGroup "multi-unit" (multiTests "multi-unit")
    ,testGroup "sub-directory" $ bothLoadings1 simpleSubDirectoryTest
    ,testGroup "multi-unit-rexport" $ bothLoadings1 multiRexportTest
    ]

-- | Test both with the default `componentsLoading` and WholeProject.
bothLoadings :: (Session () -> [TestTree]) -> [TestTree]
bothLoadings m = [testGroup "default" (m (return ()))
                , testGroup "whole-project" (m setWholeProjectLoading)]
bothLoadings1 :: (Session () -> TestTree) -> [TestTree]
bothLoadings1 m = bothLoadings $ \ initM -> [m initM]

loadCradleOnlyonce :: Session () -> TestTree
loadCradleOnlyonce initM = testGroup "load cradle only once"
    [ testWithDummyPluginEmpty' "implicit" implicit
    , testWithDummyPluginEmpty' "direct"   direct
    ]
    where
        direct dir = do
            liftIO $ atomicFileWriteStringUTF8 (dir </> "hie.yaml")
                "cradle: {direct: {arguments: []}}"
            test dir
        implicit dir = test dir
        test _dir = do
            initM
            doc <- createDoc "B.hs" "haskell" "module B where\nimport Data.Foo"
            msgs <- someTill (skipManyTill anyMessage cradleLoadedMessage) (skipManyTill anyMessage (message SMethod_TextDocumentPublishDiagnostics))
            liftIO $ length msgs @?= 1
            changeDoc doc [TextDocumentContentChangeEvent . InR . TextDocumentContentChangeWholeDocument $ "module B where\nimport Data.Maybe"]
            msgs <- manyTill (skipManyTill anyMessage cradleLoadedMessage) (skipManyTill anyMessage (message SMethod_TextDocumentPublishDiagnostics))
            liftIO $ length msgs @?= 0
            _ <- createDoc "A.hs" "haskell" "module A where\nimport LoadCradleBar"
            msgs <- manyTill (skipManyTill anyMessage cradleLoadedMessage) (skipManyTill anyMessage (message SMethod_TextDocumentPublishDiagnostics))
            liftIO $ length msgs @?= 0

retryFailedCradle :: Session () -> TestTree
retryFailedCradle initM = testWithDummyPluginEmpty' "retry failed" $ \dir -> do
  initM
  -- The false cradle always fails
  let hieContents = "cradle: {bios: {shell: \"false\"}}"
      hiePath = dir </> "hie.yaml"
  liftIO $ atomicFileWriteString hiePath hieContents
  let aPath = dir </> "A.hs"
  doc <- createDoc aPath "haskell" "main = return ()"
  WaitForIdeRuleResult {..} <- waitForAction "TypeCheck" doc
  liftIO $ "Test assumption failed: cradle should error out" `assertBool` not ideResultSuccess

  -- Fix the cradle and typecheck again
  let validCradle = "cradle: {bios: {shell: \"echo A.hs\"}}"
  liftIO $ atomicFileWriteStringUTF8 hiePath $ T.unpack validCradle
  sendNotification SMethod_WorkspaceDidChangeWatchedFiles $ DidChangeWatchedFilesParams
         [FileEvent (filePathToUri $ dir </> "hie.yaml") FileChangeType_Changed ]

  WaitForIdeRuleResult {..} <- waitForAction "TypeCheck" doc
  liftIO $ "No joy after fixing the cradle" `assertBool` ideResultSuccess


cradleLoadedMessage :: Session FromServerMessage
cradleLoadedMessage = satisfy $ \case
        FromServerMess (SMethod_CustomMethod p) (NotMess _) -> symbolVal p == cradleLoadedMethod
        _                                            -> False

cradleLoadedMethod :: String
cradleLoadedMethod = "ghcide/cradle/loaded"

ignoreFatalWarning :: Session () -> TestTree
ignoreFatalWarning initM = testCase "ignore-fatal-warning" $ runWithExtraFiles "ignore-fatal" $ \dir -> do
    initM
    let srcPath = dir </> "IgnoreFatal.hs"
    src <- liftIO $ readFileUtf8 srcPath
    _ <- createDoc srcPath "haskell" src
    expectNoMoreDiagnostics 5

simpleSubDirectoryTest :: Session () -> TestTree
simpleSubDirectoryTest initM =
  testCase "simple-subdirectory" $ runWithExtraFiles "cabal-exe" $ \dir -> do
    initM
    let mainPath = dir </> "a/src/Main.hs"
    mainSource <- liftIO $ readFileUtf8 mainPath
    _mdoc <- createDoc mainPath "haskell" mainSource
    expectDiagnosticsWithTags
      [("a/src/Main.hs", [(DiagnosticSeverity_Warning,(2,0), "Top-level binding", Just "GHC-38417", Nothing)]) -- So that we know P has been loaded
      ]
    expectNoMoreDiagnostics 0.5

multiTests :: FilePath -> [TestTree]
multiTests odir =
    [ testGroup "default" $ tests (return ())
    , testGroup "whole-project" $ tests setWholeProjectLoading
    ]
  where
    tests initM =
      [ ignoreForWindows testName $ testCase testName $ runWithExtraFiles odir $ \dir -> initM >> test dir
      | (name,test) <-
      [ ("test",simpleMultiTest)
      , ("test2",simpleMultiTest2)
      , ("test3",simpleMultiTest3)
      , ("def-test",simpleMultiDefTest)
      ]
      ,
      let testName = multiTestName odir name
      ]
    ignoreForWindows testName
        | testName == "simple-multi-def-test" = ignoreInEnv [HostOS Windows] "Test is flaky on Windows, see #4270"
        | otherwise = id

multiTestName :: FilePath -> String -> String
multiTestName dir name = "simple-" ++ dir ++ "-" ++ name

simpleMultiTest :: FilePath -> Session ()
simpleMultiTest = \dir -> do
    let aPath = dir </> "a/A.hs"
        bPath = dir </> "b/B.hs"
    adoc <- openDoc aPath "haskell"
    bdoc <- openDoc bPath "haskell"
    WaitForIdeRuleResult {..} <- waitForAction "TypeCheck" adoc
    liftIO $ assertBool "A should typecheck" ideResultSuccess
    WaitForIdeRuleResult {..} <- waitForAction "TypeCheck" bdoc
    liftIO $ assertBool "B should typecheck" ideResultSuccess
    locs <- getDefinitions bdoc (Position 2 7)
    let fooL = mkL (adoc ^. L.uri) 2 0 2 3
    checkDefs locs (pure [fooL])
    expectNoMoreDiagnostics 0.5

-- Like simpleMultiTest but open the files in the other order
simpleMultiTest2 :: FilePath -> Session ()
simpleMultiTest2 = \dir -> do
    let aPath = dir </> "a/A.hs"
        bPath = dir </> "b/B.hs"
    bdoc <- openDoc bPath "haskell"
    WaitForIdeRuleResult {} <- waitForAction "TypeCheck" bdoc
    TextDocumentIdentifier auri <- openDoc aPath "haskell"
    skipManyTill anyMessage $ isReferenceReady aPath
    locs <- getDefinitions bdoc (Position 2 7)
    let fooL = mkL auri 2 0 2 3
    checkDefs locs (pure [fooL])
    expectNoMoreDiagnostics 0.5

-- Now with 3 components
simpleMultiTest3 :: FilePath -> Session ()
simpleMultiTest3 = \ dir -> do
    let aPath = dir </> "a/A.hs"
        bPath = dir </> "b/B.hs"
        cPath = dir </> "c/C.hs"
    bdoc <- openDoc bPath "haskell"
    WaitForIdeRuleResult {} <- waitForAction "TypeCheck" bdoc
    TextDocumentIdentifier auri <- openDoc aPath "haskell"
    skipManyTill anyMessage $ isReferenceReady aPath
    cdoc <- openDoc cPath "haskell"
    WaitForIdeRuleResult {} <- waitForAction "TypeCheck" cdoc
    locs <- getDefinitions cdoc (Position 2 7)
    let fooL = mkL auri 2 0 2 3
    checkDefs locs (pure [fooL])
    expectNoMoreDiagnostics 0.5

runRegressionMultiOpenAThenB :: FilePath -> Session ()
runRegressionMultiOpenAThenB dir = do
    let aPath = dir </> "a/A.hs"
        bPath = dir </> "b/B.hs"
    adoc <- openDoc aPath "haskell"
    bdoc <- openDoc bPath "haskell"
    _ <- waitForBuildQueue
    [aRes, bRes] <- waitForTypeChecksBatched [adoc, bdoc]
    liftIO $ assertBool "A should typecheck" (ideResultSuccess aRes)
    liftIO $ assertBool "B should typecheck" (ideResultSuccess bRes)
    locs <- getDefinitions bdoc (Position 2 7)
    let fooL = mkL (adoc ^. L.uri) 2 0 2 3
    checkDefs locs (pure [fooL])
    expectNoMoreDiagnostics 0.5

runRegressionMultiOpenBThenA :: FilePath -> Session ()
runRegressionMultiOpenBThenA dir = do
    let aPath = dir </> "a/A.hs"
        bPath = dir </> "b/B.hs"
    bdoc <- openDoc bPath "haskell"
    adoc <- openDoc aPath "haskell"
    _ <- waitForBuildQueue
    [bRes, aRes] <- waitForTypeChecksBatched [bdoc, adoc]
    liftIO $ assertBool "B should typecheck" (ideResultSuccess bRes)
    liftIO $ assertBool "A should typecheck" (ideResultSuccess aRes)
    locs <- getDefinitions bdoc (Position 2 7)
    let TextDocumentIdentifier auri = adoc
    let fooL = mkL auri 2 0 2 3
    checkDefs locs (pure [fooL])
    expectNoMoreDiagnostics 0.5

runRegressionMultiOpenBThenAThenC :: FilePath -> Session ()
runRegressionMultiOpenBThenAThenC dir = do
    let aPath = dir </> "a/A.hs"
        bPath = dir </> "b/B.hs"
        cPath = dir </> "c/C.hs"
    bdoc <- openDoc bPath "haskell"
    adoc <- openDoc aPath "haskell"
    cdoc <- openDoc cPath "haskell"
    _ <- waitForBuildQueue
    [bRes, aRes, cRes] <- waitForTypeChecksBatched [bdoc, adoc, cdoc]
    liftIO $ assertBool "B should typecheck" (ideResultSuccess bRes)
    liftIO $ assertBool "A should typecheck" (ideResultSuccess aRes)
    liftIO $ assertBool "C should typecheck" (ideResultSuccess cRes)
    locs <- getDefinitions cdoc (Position 2 7)
    let TextDocumentIdentifier auri = adoc
    let fooL = mkL auri 2 0 2 3
    checkDefs locs (pure [fooL])
    expectNoMoreDiagnostics 0.5

sendTestRequest :: TestRequest -> Session A.Value
sendTestRequest req = do
  let method = SMethod_CustomMethod (Proxy @"test")
  reqId <- sendRequest method (A.toJSON req)
  TResponseMessage{_result} <- skipManyTill anyMessage $ responseForId method reqId
  case _result of
    Left err -> liftIO (assertFailure $ "test plugin request failed: " <> show err) >> pure A.Null
    Right val -> pure val

waitForTypeChecksBatched :: [TextDocumentIdentifier] -> Session [WaitForIdeRuleResult]
waitForTypeChecksBatched docs = do
  let uris = map (\TextDocumentIdentifier{_uri} -> _uri) docs
  val <- sendTestRequest (WaitForIdeRules "TypeCheck" uris)
  case A.fromJSON val of
    A.Success res -> pure res
    A.Error parseErr -> liftIO (assertFailure $ "batched typecheck parse failed: " <> parseErr) >> pure []

batchLoadRegressionTests :: [TestTree]
batchLoadRegressionTests =
  -- Note [Batch regression scheduling semantics]
  -- `didOpen` alone does not enqueue session-loader pending files.
  -- Pending entries come from GhcSession demand. For these tests, the `test`
  -- plugin uses `WaitForIdeRules` plus a pending-size barrier in session-loader
  -- to force all requested files into pending before load begins.
  [ testCase "m1-open-a-then-b-batch-pending-and-success" $
      runWithExtraFilesMultiComponent "multi" runRegressionMultiOpenAThenB
  , testCase "m2-open-b-then-a-batch-pending-and-success" $
      runWithExtraFilesMultiComponent "multi" runRegressionMultiOpenBThenA
  , testCase "m3-open-b-then-a-then-c-batch-pending-and-success" $
      runWithExtraFilesMultiComponent "multi" runRegressionMultiOpenBThenAThenC
  , testCase "f1-batch-pending-failure-isolates-broken-file" $
      runWithExtraFilesMultiComponent "multi" regressionBatchFailureIsolatesBrokenFile
  , testCase "f2-failed-file-keeps-failing-until-cradle-fix" $
      runWithExtraFilesMultiComponent "multi" regressionFailedFileKeepsFailingUntilFix
  , testCase "r1-failed-file-recovers-after-cradle-fix" $
      runWithExtraFilesMultiComponent "multi" regressionFailedFileRecoversAfterFix
  , testCase "s1-no-stale-outcomes-across-restart-paths" $
      runWithExtraFilesMultiComponent "multi" regressionNoStaleOutcomesOnRestart
  ]
  ++ [ testGroup "whole-project"
  [ testCase "m1-open-a-then-b-batch-pending-and-success" $
      runWithExtraFilesMultiComponent' PreferMultiWholeProjectLoading "multi" runRegressionMultiOpenAThenB
  , testCase "m2-open-b-then-a-batch-pending-and-success" $
      runWithExtraFilesMultiComponent' PreferMultiWholeProjectLoading "multi" runRegressionMultiOpenBThenA
  , testCase "m3-open-b-then-a-then-c-batch-pending-and-success" $
      runWithExtraFilesMultiComponent' PreferMultiWholeProjectLoading "multi" runRegressionMultiOpenBThenAThenC
  , testCase "f1-batch-pending-failure-does-not-isolate-broken-component" $
      runWithExtraFilesMultiComponent' PreferMultiWholeProjectLoading "multi" regressionBatchFailureDoesNotIsolateBrokenComponent
  , testCase "f2-failed-file-keeps-failing-until-cradle-fix" $
      runWithExtraFilesMultiComponent' PreferMultiWholeProjectLoading "multi" $ regressionFailedFileKeepsFailingUntilFix' False
  , testCase "r1-failed-file-recovers-after-cradle-fix" $
      runWithExtraFilesMultiComponent' PreferMultiWholeProjectLoading "multi" regressionFailedFileRecoversAfterFix
  , testCase "s1-no-stale-outcomes-across-restart-paths" $
      runWithExtraFilesMultiComponent' PreferMultiWholeProjectLoading "multi" $ regressionNoStaleOutcomesOnRestart' False
  ]]

runWithExtraFilesMultiComponent :: String -> (FilePath -> Session a) -> IO a
runWithExtraFilesMultiComponent = runWithExtraFilesMultiComponent' PreferMultiComponentLoading

runWithExtraFilesMultiComponent' :: SessionLoadingPreferenceConfig -> String -> (FilePath -> Session a) -> IO a
runWithExtraFilesMultiComponent' sesLoading dirName action = do
  let vfs = mkIdeTestFs [copyDir dirName]
      lspConfig :: Config
      lspConfig = def { componentsLoading = sesLoading }
      conf :: TestConfig ()
      conf = def
        { testPluginDescriptor = dummyPlugin
        , testDirLocation = Right vfs
        , testConfigCaps = lspTestCaps
        , testShiftRoot = True
        , testDisableKick = True
        , testLspConfig = lspConfig
        }
  runSessionWithTestConfig conf action

brokenMultiHieYaml :: T.Text
brokenMultiHieYaml = T.unlines
  [ "cradle:"
  , "  cabal:"
  , "    - path: \"./a\""
  , "      component: \"lib:a\""
  , "    - path: \"./b\""
  , "      component: \"lib:does-not-exist\""
  , "    - path: \"./c\""
  , "      component: \"lib:c\""
  ]

writeBrokenMultiHieYaml :: FilePath -> Session ()
writeBrokenMultiHieYaml dir =
  liftIO $ atomicFileWriteStringUTF8 (dir </> "hie.yaml") (T.unpack brokenMultiHieYaml)

notifyHieYamlChanged :: FilePath -> Session ()
notifyHieYamlChanged dir =
  sendNotification SMethod_WorkspaceDidChangeWatchedFiles $ DidChangeWatchedFilesParams
    [FileEvent (filePathToUri $ dir </> "hie.yaml") FileChangeType_Changed]

assertTypeCheckSuccess :: TextDocumentIdentifier -> String -> Session ()
assertTypeCheckSuccess doc msg = do
  WaitForIdeRuleResult {..} <- waitForAction "TypeCheck" doc
  liftIO $ assertBool msg ideResultSuccess

assertTypeCheckFailure :: TextDocumentIdentifier -> String -> Session ()
assertTypeCheckFailure doc msg = do
  WaitForIdeRuleResult {..} <- waitForAction "TypeCheck" doc
  liftIO $ assertBool msg (not ideResultSuccess)

setWholeProjectLoading :: Session ()
setWholeProjectLoading = do
  setIgnoringConfigurationRequests False
  setHlsConfig def{componentsLoading = PreferMultiWholeProjectLoading}

regressionBatchFailureIsolatesBrokenFile :: FilePath -> Session ()
regressionBatchFailureIsolatesBrokenFile dir = do
  writeBrokenMultiHieYaml dir
  let aPath = dir </> "a/A.hs"
      bPath = dir </> "b/B.hs"
  adoc <- openDoc aPath "haskell"
  bdoc <- openDoc bPath "haskell"
  _ <- waitForBuildQueue
  [aRes, bRes] <- waitForTypeChecksBatched [adoc, bdoc]
  liftIO $ assertBool "A should typecheck when B cradle mapping is broken" (ideResultSuccess aRes)
  liftIO $ assertBool "B should fail with a broken cradle mapping" (not $ ideResultSuccess bRes)

-- | With whole-project loading a failed component blocks the whole session.
regressionBatchFailureDoesNotIsolateBrokenComponent :: FilePath -> Session ()
regressionBatchFailureDoesNotIsolateBrokenComponent dir = do
  writeBrokenMultiHieYaml dir
  let aPath = dir </> "a/A.hs"
      bPath = dir </> "b/B.hs"
  adoc <- openDoc aPath "haskell"
  bdoc <- openDoc bPath "haskell"
  _ <- waitForBuildQueue
  [aRes, bRes] <- waitForTypeChecksBatched [adoc, bdoc]
  liftIO $ assertBool "A should not typecheck when B cradle mapping is broken" (not $ ideResultSuccess aRes)
  liftIO $ assertBool "B should fail with a broken cradle mapping" (not $ ideResultSuccess bRes)

regressionFailedFileKeepsFailingUntilFix :: FilePath -> Session ()
regressionFailedFileKeepsFailingUntilFix = regressionFailedFileKeepsFailingUntilFix' True

regressionFailedFileKeepsFailingUntilFix' :: Bool -> FilePath -> Session ()
regressionFailedFileKeepsFailingUntilFix' isolated dir = do
  writeBrokenMultiHieYaml dir
  let aPath = dir </> "a/A.hs"
      bPath = dir </> "b/B.hs"
      cPath = dir </> "c/C.hs"
  bdoc <- openDoc bPath "haskell"
  assertTypeCheckFailure bdoc "B should fail with broken cradle mapping"

  bSource <- liftIO $ readFileUtf8 bPath
  changeDoc bdoc
    [TextDocumentContentChangeEvent . InR . TextDocumentContentChangeWholeDocument $ bSource <> "\n"]
  assertTypeCheckFailure bdoc "B should keep failing until the cradle is fixed"

  when isolated $ do
    adoc <- openDoc aPath "haskell"
    cdoc <- openDoc cPath "haskell"
    assertTypeCheckSuccess adoc "A should still typecheck while B remains broken"
    assertTypeCheckSuccess cdoc "C should still typecheck while B remains broken"

regressionFailedFileRecoversAfterFix :: FilePath -> Session ()
regressionFailedFileRecoversAfterFix dir = do
  let hiePath = dir </> "hie.yaml"
      bPath = dir </> "b/B.hs"
  validHie <- liftIO $ readFileUtf8 hiePath
  writeBrokenMultiHieYaml dir

  bdoc <- openDoc bPath "haskell"
  assertTypeCheckFailure bdoc "B should fail before fixing the cradle"

  liftIO $ atomicFileWriteStringUTF8 hiePath (T.unpack validHie)
  notifyHieYamlChanged dir

  bSource <- liftIO $ readFileUtf8 bPath
  changeDoc bdoc
    [TextDocumentContentChangeEvent . InR . TextDocumentContentChangeWholeDocument $ bSource <> "\n"]
  assertTypeCheckSuccess bdoc "B should recover after restoring the cradle"

regressionNoStaleOutcomesOnRestart :: FilePath -> Session ()
regressionNoStaleOutcomesOnRestart = regressionNoStaleOutcomesOnRestart' True

regressionNoStaleOutcomesOnRestart' :: Bool -> FilePath -> Session ()
regressionNoStaleOutcomesOnRestart' isolated dir = do
  let hiePath = dir </> "hie.yaml"
      aPath = dir </> "a/A.hs"
      bPath = dir </> "b/B.hs"
      cPath = dir </> "c/C.hs"
  validHie <- liftIO $ readFileUtf8 hiePath
  writeBrokenMultiHieYaml dir

  bdoc <- openDoc bPath "haskell"
  assertTypeCheckFailure bdoc "B should fail before cradle fix"

  when isolated $ do
    adoc <- openDoc aPath "haskell"
    assertTypeCheckSuccess adoc "A should remain healthy while B is broken"

  liftIO $ atomicFileWriteStringUTF8 hiePath (T.unpack validHie)
  notifyHieYamlChanged dir

  cdoc <- openDoc cPath "haskell"
  assertTypeCheckSuccess cdoc "C should typecheck after cradle restart"

  bSource <- liftIO $ readFileUtf8 bPath
  changeDoc bdoc
    [TextDocumentContentChangeEvent . InR . TextDocumentContentChangeWholeDocument $ bSource <> "\n"]
  assertTypeCheckSuccess bdoc "B should not keep stale failure after cradle restart"

-- Like simpleMultiTest but open the files in component 'a' in a separate session
simpleMultiDefTest :: FilePath -> Session ()
simpleMultiDefTest = \dir -> do
    let aPath = dir </> "a/A.hs"
        bPath = dir </> "b/B.hs"
    adoc <- openDoc aPath "haskell"
    skipManyTill anyMessage $ isReferenceReady aPath
    closeDoc adoc
    bSource <- liftIO $ readFileUtf8 bPath
    bdoc <- createDoc bPath "haskell" bSource
    locs <- getDefinitions bdoc (Position 2 7)
    let fooL = mkL (adoc ^. L.uri) 2 0 2 3
    checkDefs locs (pure [fooL])
    expectNoMoreDiagnostics 0.5

multiRexportTest :: Session () -> TestTree
multiRexportTest initM =
  testCase "multi-unit-reexport-test"  $ runWithExtraFiles "multi-unit-reexport" $ \dir -> do
    initM
    let cPath = dir </> "c/C.hs"
    cdoc <- openDoc cPath "haskell"
    WaitForIdeRuleResult {} <- waitForAction "TypeCheck" cdoc
    locs <- getDefinitions cdoc (Position 3 7)
    let aPath = dir </> "a/A.hs"
    let fooL = mkL (filePathToUri aPath) 2 0 2 3
    checkDefs locs (pure [fooL])
    expectNoMoreDiagnostics 0.5

sessionDepsArePickedUp :: Session () -> TestTree
sessionDepsArePickedUp initM = testWithDummyPluginEmpty'
  "session-deps-are-picked-up"
  $ \dir -> do
    initM
    liftIO $
      atomicFileWriteStringUTF8
        (dir </> "hie.yaml")
        "cradle: {direct: {arguments: []}}"
    -- Open without OverloadedStrings and expect an error.
    doc <- createDoc "Foo.hs" "haskell" fooContent
    expectDiagnostics [("Foo.hs", [(DiagnosticSeverity_Error, (3, 6), "Couldn't match type", Just "GHC-83865")])]

    -- Update hie.yaml to enable OverloadedStrings.
    liftIO $
      atomicFileWriteStringUTF8
        (dir </> "hie.yaml")
        "cradle: {direct: {arguments: [-XOverloadedStrings]}}"
    sendNotification SMethod_WorkspaceDidChangeWatchedFiles $ DidChangeWatchedFilesParams
        [FileEvent (filePathToUri $ dir </> "hie.yaml") FileChangeType_Changed ]
    -- Send change event.
    let change =
          TextDocumentContentChangeEvent $ InL TextDocumentContentChangePartial
              { _range = Range (Position 4 0) (Position 4 0)
              , _rangeLength = Nothing
              , _text = "\n"
              }
    changeDoc doc [change]
    -- Now no errors.
    expectDiagnostics [("Foo.hs", [])]
  where
    fooContent =
      T.unlines
        [ "module Foo where",
          "import Data.Text",
          "foo :: Text",
          "foo = \"hello\""
        ]
