
{-# LANGUAGE CPP       #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs     #-}

module CradleTests (tests) where

import           Config                          (Expect (..), assertDefsFile,
                                                  checkDefs, dummyPlugin,
                                                  lspTestCaps, mkIdeTestFs, mkL,
                                                  runWithExtraFiles,
                                                  testWithDummyPluginEmpty')
import           Control.Applicative.Combinators
import           Control.Lens                    ((^.))
import           Control.Monad                   (when)
import           Control.Monad.IO.Class          (liftIO)
import qualified Data.Aeson                      as A
import           Data.Proxy                      (Proxy (..))
import qualified Data.Text                       as T
import           Development.IDE.GHC.Util
import           Development.IDE.Plugin.Test     (TestRequest (..),
                                                  WaitForIdeRuleResult (..))
import           Development.IDE.Test            (expectCurrentDiagnostics,
                                                  expectDiagnostics,
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
import           Test.Hls                        (GhcVersion (..),
                                                  TestConfig (..), def,
                                                  expectFailBecause,
                                                  ignoreTestBecause,
                                                  runSessionWithTestConfig,
                                                  setHlsConfig,
                                                  waitForBuildQueue)
import           Test.Hls.FileSystem
import           Test.Hls.Util                   (EnvSpec (..), OS (..),
                                                  ignoreForGhcVersions,
                                                  ignoreInEnv)
import           Test.Tasty
import           Test.Tasty.HUnit

defComponentLoadingConf :: SessionLoadingPreferenceConfig
defComponentLoadingConf = componentsLoading def

wholeProjectConf :: SessionLoadingPreferenceConfig
wholeProjectConf = PreferMultiWholeProjectLoading

tests :: TestTree
tests = testGroup "cradle"
  [ testGroup "whole project"
    [ testGroup "dependencies" [sessionDepsArePickedUp wholeProjectConf]
    , testGroup "ignore-fatal" [ignoreFatalWarning wholeProjectConf]
    , testGroup "loading" [loadCradleOnlyOnce wholeProjectConf, retryFailedCradle wholeProjectConf]
    , testGroup "regression.batch" (batchLoadRegressionTests wholeProjectConf)
    , testGroup "cross-cradle" [crossCradleBatchIsolationTest wholeProjectConf]
    , testGroup "multi"   (multiTests wholeProjectConf "multi")
    , testGroup "multi-unit" (multiTests wholeProjectConf "multi-unit")
    , testGroup "sub-directory" [simpleSubDirectoryTest wholeProjectConf]
    , testGroup "multi-unit-rexport" [multiRexportTest wholeProjectConf]
    , testGroup "multi-unit-import-resolution" (multiUnitImportResolutionTests wholeProjectConf)
    , testGroup "undeclared-module" (undeclaredModuleTests wholeProjectConf)
    ]
  , testGroup "default"
    [ testGroup "dependencies" [sessionDepsArePickedUp defComponentLoadingConf]
    , testGroup "ignore-fatal" [ignoreFatalWarning defComponentLoadingConf]
    , testGroup "loading" [loadCradleOnlyOnce defComponentLoadingConf, retryFailedCradle defComponentLoadingConf]
    , testGroup "regression.batch" (batchLoadRegressionTests defComponentLoadingConf)
    , testGroup "cross-cradle" [crossCradleBatchIsolationTest defComponentLoadingConf]
    , testGroup "multi"   (multiTests defComponentLoadingConf "multi")
    , testGroup "multi-unit" (multiTests defComponentLoadingConf "multi-unit")
    , testGroup "sub-directory" [simpleSubDirectoryTest defComponentLoadingConf]
    , testGroup "multi-unit-rexport" [multiRexportTest defComponentLoadingConf]
    , testGroup "multi-unit-import-resolution" (multiUnitImportResolutionTests defComponentLoadingConf)
    , testGroup "undeclared-module" (undeclaredModuleTests defComponentLoadingConf)
    ]
  ]

loadCradleOnlyOnce :: SessionLoadingPreferenceConfig -> TestTree
loadCradleOnlyOnce conf = testGroup "load cradle only once"
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
            setComponentsLoadingPreference conf
            doc <- createDoc "B.hs" "haskell" "module B where\nimport Data.Foo"
            msgs <- someTill (skipManyTill anyMessage cradleLoadedMessage) (skipManyTill anyMessage (message SMethod_TextDocumentPublishDiagnostics))
            liftIO $ length msgs @?= 1
            changeDoc doc [TextDocumentContentChangeEvent . InR . TextDocumentContentChangeWholeDocument $ "module B where\nimport Data.Maybe"]
            msgs <- manyTill (skipManyTill anyMessage cradleLoadedMessage) (skipManyTill anyMessage (message SMethod_TextDocumentPublishDiagnostics))
            liftIO $ length msgs @?= 0
            _ <- createDoc "A.hs" "haskell" "module A where\nimport LoadCradleBar"
            msgs <- manyTill (skipManyTill anyMessage cradleLoadedMessage) (skipManyTill anyMessage (message SMethod_TextDocumentPublishDiagnostics))
            liftIO $ length msgs @?= 0

retryFailedCradle :: SessionLoadingPreferenceConfig -> TestTree
retryFailedCradle conf = testWithDummyPluginEmpty' "retry failed" $ \dir -> do
  setComponentsLoadingPreference conf
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

ignoreFatalWarning :: SessionLoadingPreferenceConfig -> TestTree
ignoreFatalWarning conf = testCase "ignore-fatal-warning" $ runWithExtraFiles "ignore-fatal" $ \dir -> do
    setComponentsLoadingPreference conf
    let srcPath = dir </> "IgnoreFatal.hs"
    src <- liftIO $ readFileUtf8 srcPath
    _ <- createDoc srcPath "haskell" src
    expectNoMoreDiagnostics 5

simpleSubDirectoryTest :: SessionLoadingPreferenceConfig -> TestTree
simpleSubDirectoryTest conf =
  testCase "simple-subdirectory" $ runWithExtraFiles "cabal-exe" $ \dir -> do
    setComponentsLoadingPreference conf
    let mainPath = dir </> "a/src/Main.hs"
    mainSource <- liftIO $ readFileUtf8 mainPath
    _mdoc <- createDoc mainPath "haskell" mainSource
    expectDiagnosticsWithTags
      [("a/src/Main.hs", [(DiagnosticSeverity_Warning,(2,0), "Top-level binding", Just "GHC-38417", Nothing)]) -- So that we know P has been loaded
      ]
    expectNoMoreDiagnostics 0.5

multiTests :: SessionLoadingPreferenceConfig -> FilePath -> [TestTree]
multiTests conf odir =
  [ runOneTest testName test
  | (name,test) <-
  [ ("test",simpleMultiTest)
  , ("test2",simpleMultiTest2)
  , ("test3",simpleMultiTest3)
  , ("def-test",simpleMultiDefTest)
  ]
  ,
  let testName = multiTestName odir name
  ]
  where
    runOneTest testName act = ignoreForWindows testName $ testCase testName $ runWithExtraFiles odir $ \dir -> do
      setComponentsLoadingPreference conf
      act dir
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

-- | Several files across different components of the same cradle are all
-- pending before the first load starts. Ensure they are submitted in the same
-- batch.
runRegressionInitialOpenSingleBatchLoad :: FilePath -> Session ()
runRegressionInitialOpenSingleBatchLoad dir = do
    let aPath = dir </> "a/A.hs"
        bPath = dir </> "b/B.hs"
        cPath = dir </> "c/C.hs"
    adoc <- openDoc aPath "haskell"
    bdoc <- openDoc bPath "haskell"
    cdoc <- openDoc cPath "haskell"
    _ <- waitForBuildQueue
    (results, loads) <- waitForTypeChecksCountingCradleLoads [adoc, bdoc, cdoc]
    liftIO $ do
      assertBool "A, B and C should all typecheck" (all ideResultSuccess results)
      assertEqual "cradle loads for the initial batch of files" 1 loads

sendTestRequest :: TestRequest -> Session A.Value
sendTestRequest req = do
  let method = SMethod_CustomMethod (Proxy @"test")
  reqId <- sendRequest method (A.toJSON req)
  TResponseMessage{_result} <- skipManyTill anyMessage $ responseForId method reqId
  case _result of
    Left err -> liftIO (assertFailure $ "test plugin request failed: " <> show err) >> pure A.Null
    Right val -> pure val

-- | Like 'waitForTypeChecksBatched', but additionally count the
-- @ghcide/cradle/loaded@ notifications the server emits while satisfying the
-- request, i.e. how many cradle loads it took to serve all the files.
waitForTypeChecksCountingCradleLoads :: [TextDocumentIdentifier] -> Session ([WaitForIdeRuleResult], Int)
waitForTypeChecksCountingCradleLoads docs = do
  let uris = map (\TextDocumentIdentifier{_uri} -> _uri) docs
      method = SMethod_CustomMethod (Proxy @"test")
  reqId <- sendRequest method (A.toJSON (WaitForIdeRules "TypeCheck" uris))
  let
    go loads = do
      next <- skipManyTill anyMessage $
        (Left <$> cradleLoadedMessage) <|> (Right <$> responseForId method reqId)
      case next of
        Left _ -> go (loads + 1)
        Right TResponseMessage{_result} -> case _result of
          Left err -> liftIO $ assertFailure $ "test plugin request failed: " <> show err
          Right val -> case A.fromJSON val of
            A.Success res -> pure (res, loads)
            A.Error parseErr -> liftIO $ assertFailure $ "batched typecheck parse failed: " <> parseErr
  go 0

waitForTypeChecksBatched :: [TextDocumentIdentifier] -> Session [WaitForIdeRuleResult]
waitForTypeChecksBatched docs = do
  let uris = map (\TextDocumentIdentifier{_uri} -> _uri) docs
  val <- sendTestRequest (WaitForIdeRules "TypeCheck" uris)
  case A.fromJSON val of
    A.Success res -> pure res
    A.Error parseErr -> liftIO (assertFailure $ "batched typecheck parse failed: " <> parseErr) >> pure []

batchLoadRegressionTests :: SessionLoadingPreferenceConfig -> [TestTree]
batchLoadRegressionTests conf =
  -- Note [Batch regression scheduling semantics]
  -- `didOpen` alone does not enqueue session-loader pending files.
  -- Pending entries come from GhcSession demand. For these tests, the `test`
  -- plugin uses `WaitForIdeRules` plus a pending-size barrier in session-loader
  -- to force all requested files into pending before load begins.
  [ testCase "m1-open-a-then-b-batch-pending-and-success" $
      runWithExtraFilesMultiComponent conf "multi" runRegressionMultiOpenAThenB
  , testCase "m2-open-b-then-a-batch-pending-and-success" $
      runWithExtraFilesMultiComponent conf "multi" runRegressionMultiOpenBThenA
  , testCase "m3-open-b-then-a-then-c-batch-pending-and-success" $
      runWithExtraFilesMultiComponent conf "multi" runRegressionMultiOpenBThenAThenC
  , testCase "m4-initial-multi-file-open-loads-cradle-once" $
      runWithExtraFilesMultiComponent conf "multi" runRegressionInitialOpenSingleBatchLoad
  , expectBrokenWithWholeProjectLoading conf $
    testCase "f1-batch-pending-failure-isolates-broken-file" $
      runWithExtraFilesMultiComponent conf "multi" regressionBatchFailureIsolatesBrokenFile
  , expectBrokenWithWholeProjectLoading conf $
    testCase "f2-failed-file-keeps-failing-until-cradle-fix" $
      runWithExtraFilesMultiComponent conf "multi" regressionFailedFileKeepsFailingUntilFix
  , onlyWholeProjectLoading conf $
    testCase "f3-batch-pending-failure-does-not-isolate-broken-component" $
      runWithExtraFilesMultiComponent conf "multi" regressionBatchFailureDoesNotIsolateBrokenComponent
  , testCase "r1-failed-file-recovers-after-cradle-fix" $
      runWithExtraFilesMultiComponent conf "multi" regressionFailedFileRecoversAfterFix
  , expectBrokenWithWholeProjectLoading conf $
    testCase "s1-no-stale-outcomes-across-restart-paths" $
      runWithExtraFilesMultiComponent conf "multi" regressionNoStaleOutcomesOnRestart
  , testCase "s2-no-stale-outcomes-across-restart-paths" $
      runWithExtraFilesMultiComponent conf "multi" regressionNoStaleOutcomesOnRestartNotHealthyInBetween
  ]

-- | A module the user has written but not added to the cabal file yet is the
-- normal state of code under development, so it has to work: it lies under an
-- import path of a component, which is where GHC's own finder would look for
-- it, so it is compiled as part of that component and only warned about.
undeclaredModuleTests :: SessionLoadingPreferenceConfig -> [TestTree]
undeclaredModuleTests conf =
  [ testCase "a module missing from the cabal file still loads" $
      withUndeclared $ \_dir -> do
        udoc <- openDoc ("a" </> "Undeclared.hs") "haskell"
        assertTypeCheckSuccess udoc "the undeclared module should typecheck"
        diags <- getCurrentDiagnostics udoc
        -- Only the whole project load knows the file is missing from the cabal
        -- file. Loading one component at a time asks the build tool about this
        -- very file, and it answers with the options of the component it lies
        -- in, so nothing distinguishes it from a module that is listed.
        when (conf == wholeProjectConf) $ liftIO $ assertBool
          ("expected a warning about the cabal file, got: " <> show diags)
          (any isMissingFromCabalWarning diags)
  , testCase "importing a module missing from the cabal file still loads" $
      withUndeclared $ \dir -> do
        liftIO $ atomicFileWriteString (dir </> "a" </> "A.hs") $ unlines
          [ "module A where"
          , "import Undeclared"
          , "foo :: Int"
          , "foo = u"
          ]
        adoc <- openDoc ("a" </> "A.hs") "haskell"
        assertTypeCheckSuccess adoc "the importing module should typecheck"
        expectCurrentDiagnostics adoc []
  ]
  where
    withUndeclared act = runWithExtraFilesMultiComponent conf "multi" $ \dir -> do
      -- Undeclared.hs is under a's hs-source-dirs but is in no cabal field
      liftIO $ atomicFileWriteString (dir </> "a" </> "Undeclared.hs") $ unlines
        [ "module Undeclared where"
        , "u :: Int"
        , "u = 1"
        ]
      act dir
    isMissingFromCabalWarning d =
         d ^. L.severity == Just DiagnosticSeverity_Warning
      && "cabal" `T.isInfixOf` (d ^. L.message)

expectBrokenWithWholeProjectLoading :: SessionLoadingPreferenceConfig -> TestTree -> TestTree
expectBrokenWithWholeProjectLoading conf =
  if conf == wholeProjectConf
    then expectFailBecause "We can't load the whole project if the hie.yaml file is invalid"
    else id

onlyWholeProjectLoading :: SessionLoadingPreferenceConfig -> TestTree -> TestTree
onlyWholeProjectLoading conf =
  if conf == wholeProjectConf
    then id
    else ignoreTestBecause "This test only works with PreferMultiWholeProjectLoading"

runWithExtraFilesMultiComponent :: SessionLoadingPreferenceConfig -> String -> (FilePath -> Session a) -> IO a
runWithExtraFilesMultiComponent sesLoading dirName action = do
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

setComponentsLoadingPreference :: SessionLoadingPreferenceConfig -> Session ()
setComponentsLoadingPreference pref = do
  setIgnoringConfigurationRequests False
  setHlsConfig def{componentsLoading = pref}

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
regressionFailedFileKeepsFailingUntilFix dir = do
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
regressionNoStaleOutcomesOnRestart dir = do
  let hiePath = dir </> "hie.yaml"
      aPath = dir </> "a/A.hs"
      bPath = dir </> "b/B.hs"
      cPath = dir </> "c/C.hs"
  validHie <- liftIO $ readFileUtf8 hiePath
  writeBrokenMultiHieYaml dir

  bdoc <- openDoc bPath "haskell"
  assertTypeCheckFailure bdoc "B should fail before cradle fix"

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

-- | Like 'regressionNoStaleOutcomesOnRestart', but we don't check that
-- unrelated components can still be loaded.
--
-- When we load the whole project, we can't load intermediate components, since the hie.yaml =
-- is broken.
regressionNoStaleOutcomesOnRestartNotHealthyInBetween :: FilePath -> Session ()
regressionNoStaleOutcomesOnRestartNotHealthyInBetween dir = do
  let hiePath = dir </> "hie.yaml"
      bPath = dir </> "b/B.hs"
      cPath = dir </> "c/C.hs"
  validHie <- liftIO $ readFileUtf8 hiePath
  writeBrokenMultiHieYaml dir

  bdoc <- openDoc bPath "haskell"
  assertTypeCheckFailure bdoc "B should fail before cradle fix"

  liftIO $ atomicFileWriteStringUTF8 hiePath (T.unpack validHie)
  notifyHieYamlChanged dir

  cdoc <- openDoc cPath "haskell"
  assertTypeCheckSuccess cdoc "C should typecheck after cradle restart"

  bSource <- liftIO $ readFileUtf8 bPath
  changeDoc bdoc
    [TextDocumentContentChangeEvent . InR . TextDocumentContentChangeWholeDocument $ bSource <> "\n"]
  assertTypeCheckSuccess bdoc "B should not keep stale failure after cradle restart"

-- | Files loaded by one cradle must not be handed to another cradle's
-- multi-component load. Here @standalone/Standalone.hs@ is owned by a direct
-- cradle; once it is loaded, opening @a/A.hs@ (owned by the root cabal cradle)
-- used to batch the standalone file into @cabal repl@, which cannot map it to
-- any component and fails wholesale, poisoning the load of A.
crossCradleBatchIsolationTest :: SessionLoadingPreferenceConfig -> TestTree
crossCradleBatchIsolationTest conf =
  testCase "direct-cradle-file-does-not-poison-cabal-load" $
    runWithExtraFilesMultiComponent conf "cross-cradle" $ \dir -> do
      let standalonePath = dir </> "standalone/Standalone.hs"
          aPath = dir </> "a/A.hs"
      sdoc <- openDoc standalonePath "haskell"
      assertTypeCheckSuccess sdoc "standalone file (direct cradle) should typecheck"
      adoc <- openDoc aPath "haskell"
      assertTypeCheckSuccess adoc
        "cabal-cradle file should typecheck after a direct-cradle file was loaded"

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

multiRexportTest :: SessionLoadingPreferenceConfig -> TestTree
multiRexportTest conf =
  testCase "multi-unit-reexport-test"  $ runWithExtraFiles "multi-unit-reexport" $ \dir -> do
    setComponentsLoadingPreference conf
    let cPath = dir </> "c/C.hs"
    cdoc <- openDoc cPath "haskell"
    WaitForIdeRuleResult {} <- waitForAction "TypeCheck" cdoc
    locs <- getDefinitions cdoc (Position 3 7)
    let aPath = dir </> "a/A.hs"
    let fooL = mkL (filePathToUri aPath) 2 0 2 3
    checkDefs locs (pure [fooL])
    expectNoMoreDiagnostics 0.5

-- | Tests that import resolution respects home unit boundaries: which units
-- are visible from the importing unit, and in which order they are searched.
multiUnitImportResolutionTests :: SessionLoadingPreferenceConfig -> [TestTree]
multiUnitImportResolutionTests conf =
  [ testCase "visibility" $ runWithExtraFiles "multi-unit-visibility" $ \_dir -> do
      setComponentsLoadingPreference conf
      -- bbb does not depend on aaa, so aaa's module Priv must not be visible
      bdoc <- openDoc ("bbb" </> "B.hs") "haskell"
      expectDiagnostics [("bbb" </> "B.hs", [(DiagnosticSeverity_Error, (1, 7), "Could not find module", Nothing)])]
      locs <- getDefinitions bdoc (Position 1 7)
      checkDefs locs (pure [ExpectNoDefinitions])
  , testCase "own unit shadows other units" $ runWithExtraFiles "multi-unit-shadow" $ \dir -> do
      setComponentsLoadingPreference conf
      -- M lives in unit aaa: its import of X must resolve to aaa's own X,
      -- not the X of the unrelated unit zzz
      mdoc <- openDoc ("aaa" </> "M.hs") "haskell"
      assertTypeCheckSuccess mdoc "M should typecheck using aaa's own X"
      locs <- getDefinitions mdoc (Position 1 7)
      assertDefsFile (dir </> "aaa" </> "X.hs") locs
  , ignoreForGhcVersions [GHC96, GHC98, GHC910] "Renaming reexports only exist from GHC 9.12"
    $ testCase "renaming reexport resolves to the original module" $
      runWithExtraFiles "multi-unit-reexport-rename" $ \dir -> do
      setComponentsLoadingPreference conf
      -- rrr reexports Internal.Impl as Facade, so importing Facade has to
      -- find rrr's Internal.Impl, under its own name
      mdoc <- openDoc ("mmm" </> "M.hs") "haskell"
      assertTypeCheckSuccess mdoc "M should typecheck through the renaming reexport"
      locs <- getDefinitions mdoc (Position 1 7)
      assertDefsFile (dir </> "rrr" </> "Internal" </> "Impl.hs") locs
  , testCase "package import picks the named unit" $ runWithExtraFiles "multi-unit-pkgimport" $ \dir -> do
      setComponentsLoadingPreference conf
      -- the package-qualified import names unit ppp: it must resolve to
      -- ppp's A, not qqq's
      mdoc <- openDoc ("mmm" </> "M.hs") "haskell"
      assertTypeCheckSuccess mdoc "M should typecheck using ppp's A"
      locs <- getDefinitions mdoc (Position 1 13)
      assertDefsFile (dir </> "ppp" </> "A.hs") locs
  ]

sessionDepsArePickedUp :: SessionLoadingPreferenceConfig -> TestTree
sessionDepsArePickedUp conf = testWithDummyPluginEmpty'
  "session-deps-are-picked-up"
  $ \dir -> do
    setComponentsLoadingPreference conf
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
