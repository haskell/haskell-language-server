
{-# LANGUAGE GADTs #-}

module CradleTests (tests) where

import           Control.Applicative.Combinators
import           Control.Monad.IO.Class          (liftIO)
import qualified Data.Text                       as T
import           Development.IDE.GHC.Compat      (GhcVersion (..))
import           Development.IDE.GHC.Util
import           Development.IDE.Test            (expectDiagnostics,
                                                  expectDiagnosticsWithTags,
                                                  expectNoMoreDiagnostics,
                                                  isReferenceReady,
                                                  waitForAction)
import           Development.IDE.Types.Location
import qualified Language.LSP.Protocol.Lens      as L
import           Language.LSP.Protocol.Message
import           Language.LSP.Protocol.Types     hiding
                                                 (SemanticTokenAbsolute (..),
                                                  SemanticTokenRelative (..),
                                                  SemanticTokensEdit (..),
                                                  mkRange)
import           Language.LSP.Test
import           System.FilePath
import           System.IO.Extra                 hiding (withTempDir)
-- import Test.QuickCheck.Instances ()
import           Config
import           Config                          (checkDefs, mkL)
import           Control.Lens                    ((^.))
import           Development.IDE.Plugin.Test     (WaitForIdeRuleResult (..))
import           GHC.TypeLits                    (symbolVal)
import           Test.Hls                        (ignoreForGhcVersions)
import           Test.Tasty
import           Test.Tasty.HUnit


tests :: TestTree
tests = testGroup "cradle"
    [testGroup "dependencies" [sessionDepsArePickedUp]
    ,testGroup "ignore-fatal" [ignoreFatalWarning]
    ,testGroup "loading" [loadCradleOnlyonce, retryFailedCradle]
    ,testGroup "multi"   (multiTests "multi")
    ,ignoreForGhcVersions [GHC92] "multiple units not supported on 9.2"
       $ testGroup "multi-unit" (multiTests "multi-unit")
    ,testGroup "sub-directory"   [simpleSubDirectoryTest]
    ,ignoreForGhcVersions [GHC92] "multiple units not supported on 9.2"
      $ testGroup "multi-unit-rexport" [multiRexportTest]
    ]

loadCradleOnlyonce :: TestTree
loadCradleOnlyonce = testGroup "load cradle only once"
    [ testWithDummyPluginEmpty' "implicit" implicit
    , testWithDummyPluginEmpty' "direct"   direct
    ]
    where
        direct dir = do
            liftIO $ writeFileUTF8 (dir </> "hie.yaml")
                "cradle: {direct: {arguments: []}}"
            test dir
        implicit dir = test dir
        test _dir = do
            doc <- createDoc "B.hs" "haskell" "module B where\nimport Data.Foo"
            msgs <- someTill (skipManyTill anyMessage cradleLoadedMessage) (skipManyTill anyMessage (message SMethod_TextDocumentPublishDiagnostics))
            liftIO $ length msgs @?= 1
            changeDoc doc [TextDocumentContentChangeEvent . InR . TextDocumentContentChangeWholeDocument $ "module B where\nimport Data.Maybe"]
            msgs <- manyTill (skipManyTill anyMessage cradleLoadedMessage) (skipManyTill anyMessage (message SMethod_TextDocumentPublishDiagnostics))
            liftIO $ length msgs @?= 0
            _ <- createDoc "A.hs" "haskell" "module A where\nimport LoadCradleBar"
            msgs <- manyTill (skipManyTill anyMessage cradleLoadedMessage) (skipManyTill anyMessage (message SMethod_TextDocumentPublishDiagnostics))
            liftIO $ length msgs @?= 0

retryFailedCradle :: TestTree
retryFailedCradle = testWithDummyPluginEmpty' "retry failed" $ \dir -> do
  -- The false cradle always fails
  let hieContents = "cradle: {bios: {shell: \"false\"}}"
      hiePath = dir </> "hie.yaml"
  liftIO $ writeFile hiePath hieContents
  let aPath = dir </> "A.hs"
  doc <- createDoc aPath "haskell" "main = return ()"
  WaitForIdeRuleResult {..} <- waitForAction "TypeCheck" doc
  liftIO $ "Test assumption failed: cradle should error out" `assertBool` not ideResultSuccess

  -- Fix the cradle and typecheck again
  let validCradle = "cradle: {bios: {shell: \"echo A.hs\"}}"
  liftIO $ writeFileUTF8 hiePath $ T.unpack validCradle
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

ignoreFatalWarning :: TestTree
ignoreFatalWarning = testCase "ignore-fatal-warning" $ runWithExtraFiles "ignore-fatal" $ \dir -> do
    let srcPath = dir </> "IgnoreFatal.hs"
    src <- liftIO $ readFileUtf8 srcPath
    _ <- createDoc srcPath "haskell" src
    expectNoMoreDiagnostics 5

simpleSubDirectoryTest :: TestTree
simpleSubDirectoryTest =
  testCase "simple-subdirectory" $ runWithExtraFiles "cabal-exe" $ \dir -> do
    let mainPath = dir </> "a/src/Main.hs"
    mainSource <- liftIO $ readFileUtf8 mainPath
    _mdoc <- createDoc mainPath "haskell" mainSource
    expectDiagnosticsWithTags
      [("a/src/Main.hs", [(DiagnosticSeverity_Warning,(2,0), "Top-level binding", Nothing)]) -- So that we know P has been loaded
      ]
    expectNoMoreDiagnostics 0.5

multiTests :: FilePath -> [TestTree]
multiTests dir =
  [simpleMultiTest dir, simpleMultiTest2 dir, simpleMultiTest3 dir, simpleMultiDefTest dir]

multiTestName :: FilePath -> String -> String
multiTestName dir name = "simple-" ++ dir ++ "-" ++ name

simpleMultiTest :: FilePath -> TestTree
simpleMultiTest variant = testCase (multiTestName variant "test") $ runWithExtraFiles variant $ \dir -> do
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
simpleMultiTest2 :: FilePath -> TestTree
simpleMultiTest2 variant = testCase (multiTestName variant "test2") $ runWithExtraFiles variant $ \dir -> do
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
simpleMultiTest3 :: FilePath -> TestTree
simpleMultiTest3 variant =
  testCase (multiTestName variant "test3") $ runWithExtraFiles variant $ \dir -> do
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

-- Like simpleMultiTest but open the files in component 'a' in a separate session
simpleMultiDefTest :: FilePath -> TestTree
simpleMultiDefTest variant = testCase (multiTestName variant "def-test") $ runWithExtraFiles variant $ \dir -> do
    let aPath = dir </> "a/A.hs"
        bPath = dir </> "b/B.hs"
    adoc <- liftIO $ runInDir dir $ do
      aSource <- liftIO $ readFileUtf8 aPath
      adoc <- createDoc aPath "haskell" aSource
      skipManyTill anyMessage $ isReferenceReady aPath
      closeDoc adoc
      pure adoc
    bSource <- liftIO $ readFileUtf8 bPath
    bdoc <- createDoc bPath "haskell" bSource
    locs <- getDefinitions bdoc (Position 2 7)
    let fooL = mkL (adoc ^. L.uri) 2 0 2 3
    checkDefs locs (pure [fooL])
    expectNoMoreDiagnostics 0.5

multiRexportTest :: TestTree
multiRexportTest =
  testCase "multi-unit-reexport-test"  $ runWithExtraFiles "multi-unit-reexport" $ \dir -> do
    let cPath = dir </> "c/C.hs"
    cdoc <- openDoc cPath "haskell"
    WaitForIdeRuleResult {} <- waitForAction "TypeCheck" cdoc
    locs <- getDefinitions cdoc (Position 3 7)
    let aPath = dir </> "a/A.hs"
    let fooL = mkL (filePathToUri aPath) 2 0 2 3
    checkDefs locs (pure [fooL])
    expectNoMoreDiagnostics 0.5

sessionDepsArePickedUp :: TestTree
sessionDepsArePickedUp = testWithDummyPluginEmpty'
  "session-deps-are-picked-up"
  $ \dir -> do
    liftIO $
      writeFileUTF8
        (dir </> "hie.yaml")
        "cradle: {direct: {arguments: []}}"
    -- Open without OverloadedStrings and expect an error.
    doc <- createDoc "Foo.hs" "haskell" fooContent
    expectDiagnostics [("Foo.hs", [(DiagnosticSeverity_Error, (3, 6), "Couldn't match type")])]

    -- Update hie.yaml to enable OverloadedStrings.
    liftIO $
      writeFileUTF8
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
