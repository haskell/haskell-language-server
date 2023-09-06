{-# LANGUAGE DataKinds                #-}
{-# LANGUAGE DisambiguateRecordFields #-}
{-# LANGUAGE GADTs                    #-}
{-# LANGUAGE LambdaCase               #-}
{-# LANGUAGE NamedFieldPuns           #-}
{-# LANGUAGE OverloadedLists          #-}
{-# LANGUAGE OverloadedStrings        #-}
{-# LANGUAGE PolyKinds                #-}
{-# LANGUAGE RankNTypes               #-}
{-# LANGUAGE TypeApplications         #-}
module Test.Hls
  ( module Test.Tasty.HUnit,
    module Test.Tasty,
    module Test.Tasty.ExpectedFailure,
    module Test.Hls.Util,
    module Language.LSP.Protocol.Types,
    module Language.LSP.Protocol.Message,
    module Language.LSP.Test,
    module Control.Monad.IO.Class,
    module Control.Applicative.Combinators,
    defaultTestRunner,
    goldenGitDiff,
    goldenWithHaskellDoc,
    goldenWithHaskellDocInTmpDir,
    goldenWithHaskellAndCaps,
    goldenWithHaskellAndCapsInTmpDir,
    goldenWithCabalDoc,
    goldenWithHaskellDocFormatter,
    goldenWithHaskellDocFormatterInTmpDir,
    goldenWithCabalDocFormatter,
    goldenWithCabalDocFormatterInTmpDir,
    def,
    -- * Running HLS for integration tests
    runSessionWithServer,
    runSessionWithServerAndCaps,
    runSessionWithServerInTmpDir,
    runSessionWithServerAndCapsInTmpDir,
    runSessionWithServer',
    runSessionWithServerInTmpDir',
    -- * Helpful re-exports
    PluginDescriptor,
    IdeState,
    -- * Assertion helper functions
    waitForProgressDone,
    waitForAllProgressDone,
    waitForBuildQueue,
    waitForTypecheck,
    waitForAction,
    hlsConfigToClientConfig,
    setHlsConfig,
    getLastBuildKeys,
    waitForKickDone,
    waitForKickStart,
    -- * Plugin descriptor helper functions for tests
    PluginTestDescriptor,
    pluginTestRecorder,
    mkPluginTestDescriptor,
    mkPluginTestDescriptor',
    -- * Re-export logger types
    -- Avoids slightly annoying ghcide imports when they are unnecessary.
    WithPriority(..),
    Recorder,
    Priority(..),
    )
where

import           Control.Applicative.Combinators
import           Control.Concurrent.Async           (async, cancel, wait)
import           Control.Concurrent.Extra
import           Control.Exception.Base
import           Control.Lens.Extras                (is)
import           Control.Monad                      (guard, unless, void)
import           Control.Monad.Extra                (forM)
import           Control.Monad.IO.Class
import           Data.Aeson                         (Result (Success),
                                                     Value (Null), fromJSON,
                                                     toJSON)
import qualified Data.Aeson                         as A
import           Data.ByteString.Lazy               (ByteString)
import           Data.Default                       (def)
import qualified Data.Map                           as M
import           Data.Maybe                         (fromMaybe)
import           Data.Proxy                         (Proxy (Proxy))
import qualified Data.Text                          as T
import qualified Data.Text.Lazy                     as TL
import qualified Data.Text.Lazy.Encoding            as TL
import           Development.IDE                    (IdeState)
import           Development.IDE.Main               hiding (Log)
import qualified Development.IDE.Main               as Ghcide
import qualified Development.IDE.Main               as IDEMain
import           Development.IDE.Plugin.Test        (TestRequest (GetBuildKeysBuilt, WaitForIdeRule, WaitForShakeQueue),
                                                     WaitForIdeRuleResult (ideResultSuccess))
import qualified Development.IDE.Plugin.Test        as Test
import           Development.IDE.Types.Options
import           GHC.IO.Handle
import           GHC.Stack                          (emptyCallStack)
import           GHC.TypeLits
import           Ide.Logger                         (Doc, Logger (Logger),
                                                     Pretty (pretty),
                                                     Priority (..),
                                                     Recorder (Recorder, logger_),
                                                     WithPriority (WithPriority, priority),
                                                     cfilter, cmapWithPrio,
                                                     logWith,
                                                     makeDefaultStderrRecorder,
                                                     (<+>))
import           Ide.Types
import           Language.LSP.Protocol.Capabilities
import           Language.LSP.Protocol.Message
import           Language.LSP.Protocol.Types        hiding (Null)
import           Language.LSP.Test
import           Prelude                            hiding (log)
import           System.Directory                   (getCurrentDirectory,
                                                     setCurrentDirectory)
import           System.Environment                 (lookupEnv)
import           System.FilePath
import           System.IO.Extra                    (newTempDir, withTempDir)
import           System.IO.Unsafe                   (unsafePerformIO)
import           System.Process.Extra               (createPipe)
import           System.Time.Extra
import qualified Test.Hls.FileSystem                as FS
import           Test.Hls.FileSystem
import           Test.Hls.Util
import           Test.Tasty                         hiding (Timeout)
import           Test.Tasty.ExpectedFailure
import           Test.Tasty.Golden
import           Test.Tasty.HUnit
import           Test.Tasty.Ingredients.Rerun
import           Test.Tasty.Runners                 (NumThreads (..))

data Log
  = LogIDEMain IDEMain.Log
  | LogTestHarness LogTestHarness

instance Pretty Log where
  pretty = \case
    LogIDEMain log     -> pretty log
    LogTestHarness log -> pretty log

data LogTestHarness
  = LogTestDir FilePath
  | LogCleanup
  | LogNoCleanup


instance Pretty LogTestHarness where
  pretty = \case
    LogTestDir dir -> "Test Project located in directory:" <+> pretty dir
    LogCleanup     -> "Cleaned up temporary directory"
    LogNoCleanup   -> "No cleanup of temporary directory"

-- | Run 'defaultMainWithRerun', limiting each single test case running at most 10 minutes
defaultTestRunner :: TestTree -> IO ()
defaultTestRunner = defaultMainWithRerun . adjustOption (const $ NumThreads 1) . adjustOption (const $ mkTimeout 600000000)

gitDiff :: FilePath -> FilePath -> [String]
gitDiff fRef fNew = ["git", "-c", "core.fileMode=false", "diff", "--no-index", "--text", "--exit-code", fRef, fNew]

goldenGitDiff :: TestName -> FilePath -> IO ByteString -> TestTree
goldenGitDiff name = goldenVsStringDiff name gitDiff

goldenWithHaskellDoc
  :: Pretty b
  => Config
  -> PluginTestDescriptor b
  -> TestName
  -> FilePath
  -> FilePath
  -> FilePath
  -> FilePath
  -> (TextDocumentIdentifier -> Session ())
  -> TestTree
goldenWithHaskellDoc = goldenWithDoc "haskell"

goldenWithHaskellDocInTmpDir
  :: Pretty b
  => Config
  -> PluginTestDescriptor b
  -> TestName
  -> VirtualFileTree
  -> FilePath
  -> FilePath
  -> FilePath
  -> (TextDocumentIdentifier -> Session ())
  -> TestTree
goldenWithHaskellDocInTmpDir = goldenWithDocInTmpDir "haskell"

goldenWithHaskellAndCaps
  :: Pretty b
  => Config
  -> ClientCapabilities
  -> PluginTestDescriptor b
  -> TestName
  -> FilePath
  -> FilePath
  -> FilePath
  -> FilePath
  -> (TextDocumentIdentifier -> Session ())
  -> TestTree
goldenWithHaskellAndCaps config clientCaps plugin title testDataDir path desc ext act =
  goldenGitDiff title (testDataDir </> path <.> desc <.> ext)
  $ runSessionWithServerAndCaps config plugin clientCaps testDataDir
  $ TL.encodeUtf8 . TL.fromStrict
  <$> do
    doc <- openDoc (path <.> ext) "haskell"
    void waitForBuildQueue
    act doc
    documentContents doc

goldenWithHaskellAndCapsInTmpDir
  :: Pretty b
  => Config
  -> ClientCapabilities
  -> PluginTestDescriptor b
  -> TestName
  -> VirtualFileTree
  -> FilePath
  -> FilePath
  -> FilePath
  -> (TextDocumentIdentifier -> Session ())
  -> TestTree
goldenWithHaskellAndCapsInTmpDir config clientCaps plugin title tree path desc ext act =
  goldenGitDiff title (vftOriginalRoot tree </> path <.> desc <.> ext)
  $ runSessionWithServerAndCapsInTmpDir config plugin clientCaps tree
  $ TL.encodeUtf8 . TL.fromStrict
  <$> do
    doc <- openDoc (path <.> ext) "haskell"
    void waitForBuildQueue
    act doc
    documentContents doc

goldenWithCabalDoc
  :: Pretty b
  => Config
  -> PluginTestDescriptor b
  -> TestName
  -> FilePath
  -> FilePath
  -> FilePath
  -> FilePath
  -> (TextDocumentIdentifier -> Session ())
  -> TestTree
goldenWithCabalDoc = goldenWithDoc "cabal"

goldenWithDoc
  :: Pretty b
  => T.Text
  -> Config
  -> PluginTestDescriptor b
  -> TestName
  -> FilePath
  -> FilePath
  -> FilePath
  -> FilePath
  -> (TextDocumentIdentifier -> Session ())
  -> TestTree
goldenWithDoc fileType config plugin title testDataDir path desc ext act =
  goldenGitDiff title (testDataDir </> path <.> desc <.> ext)
  $ runSessionWithServer config plugin testDataDir
  $ TL.encodeUtf8 . TL.fromStrict
  <$> do
    doc <- openDoc (path <.> ext) fileType
    void waitForBuildQueue
    act doc
    documentContents doc

goldenWithDocInTmpDir
  :: Pretty b
  => T.Text
  -> Config
  -> PluginTestDescriptor b
  -> TestName
  -> VirtualFileTree
  -> FilePath
  -> FilePath
  -> FilePath
  -> (TextDocumentIdentifier -> Session ())
  -> TestTree
goldenWithDocInTmpDir fileType config plugin title tree path desc ext act =
  goldenGitDiff title (vftOriginalRoot tree </> path <.> desc <.> ext)
  $ runSessionWithServerInTmpDir config plugin tree
  $ TL.encodeUtf8 . TL.fromStrict
  <$> do
    doc <- openDoc (path <.> ext) fileType
    void waitForBuildQueue
    act doc
    documentContents doc

-- ------------------------------------------------------------
-- Helper function for initialising plugins under test
-- ------------------------------------------------------------

-- | Plugin under test where a fitting recorder is injected.
type PluginTestDescriptor b = Recorder (WithPriority b) -> IdePlugins IdeState

-- | Wrap a plugin you want to test, and inject a fitting recorder as required.
--
-- If you want to write the logs to stderr, run your tests with
-- "HLS_TEST_PLUGIN_LOG_STDERR=1", e.g.
--
-- @
--   HLS_TEST_PLUGIN_LOG_STDERR=1 cabal test <test-suite-of-plugin>
-- @
--
--
-- To write all logs to stderr, including logs of the server, use:
--
-- @
--   HLS_TEST_LOG_STDERR=1 cabal test <test-suite-of-plugin>
-- @
mkPluginTestDescriptor
  :: (Recorder (WithPriority b) -> PluginId -> PluginDescriptor IdeState)
  -> PluginId
  -> PluginTestDescriptor b
mkPluginTestDescriptor pluginDesc plId recorder = IdePlugins [pluginDesc recorder plId]

-- | Wrap a plugin you want to test.
--
-- Ideally, try to migrate this plugin to co-log logger style architecture.
-- Therefore, you should prefer 'mkPluginTestDescriptor' to this if possible.
mkPluginTestDescriptor'
  :: (PluginId -> PluginDescriptor IdeState)
  -> PluginId
  -> PluginTestDescriptor b
mkPluginTestDescriptor' pluginDesc plId _recorder = IdePlugins [pluginDesc plId]

-- | Initialise a recorder that can be instructed to write to stderr by
-- setting the environment variable "HLS_TEST_PLUGIN_LOG_STDERR=1" before
-- running the tests.
--
-- On the cli, use for example:
--
-- @
--   HLS_TEST_PLUGIN_LOG_STDERR=1 cabal test <test-suite-of-plugin>
-- @
--
-- To write all logs to stderr, including logs of the server, use:
--
-- @
--   HLS_TEST_LOG_STDERR=1 cabal test <test-suite-of-plugin>
-- @
pluginTestRecorder :: Pretty a => IO (Recorder (WithPriority a))
pluginTestRecorder = do
  (recorder, _) <- initialiseTestRecorder ["HLS_TEST_PLUGIN_LOG_STDERR", "HLS_TEST_LOG_STDERR"]
  pure recorder

-- | Generic recorder initialisation for plugins and the HLS server for test-cases.
--
-- The created recorder writes to stderr if any of the given environment variables
-- have been set to a value different to @0@.
-- We allow multiple values, to make it possible to define a single environment variable
-- that instructs all recorders in the test-suite to write to stderr.
--
-- We have to return the base logger function for HLS server logging initialisation.
-- See 'runSessionWithServer'' for details.
initialiseTestRecorder :: Pretty a => [String] -> IO (Recorder (WithPriority a), WithPriority (Doc ann) -> IO ())
initialiseTestRecorder envVars = do
    docWithPriorityRecorder <- makeDefaultStderrRecorder Nothing
    -- There are potentially multiple environment variables that enable this logger
    definedEnvVars <- forM envVars (\var -> fromMaybe "0" <$> lookupEnv var)
    let logStdErr = any (/= "0") definedEnvVars

        docWithFilteredPriorityRecorder =
          if logStdErr then cfilter (\WithPriority{ priority } -> priority >= Debug) docWithPriorityRecorder
          else mempty

        Recorder {logger_} = docWithFilteredPriorityRecorder

    pure (cmapWithPrio pretty docWithFilteredPriorityRecorder, logger_)

-- ------------------------------------------------------------
-- Run an HLS server testing a specific plugin
-- ------------------------------------------------------------

runSessionWithServer :: Pretty b => Config -> PluginTestDescriptor b -> FilePath -> Session a -> IO a
runSessionWithServer config plugin fp act = do
  recorder <- pluginTestRecorder
  runSessionWithServer' (plugin recorder) config def fullCaps fp act

runSessionWithServerAndCaps :: Pretty b => Config -> PluginTestDescriptor b -> ClientCapabilities -> FilePath -> Session a -> IO a
runSessionWithServerAndCaps config plugin caps fp act = do
  recorder <- pluginTestRecorder
  runSessionWithServer' (plugin recorder) config def caps fp act

runSessionWithServerInTmpDir :: Pretty b => Config -> PluginTestDescriptor b -> VirtualFileTree -> Session a -> IO a
runSessionWithServerInTmpDir config plugin tree act = do
  recorder <- pluginTestRecorder
  runSessionWithServerInTmpDir' (plugin recorder) config def fullCaps tree act

runSessionWithServerAndCapsInTmpDir :: Pretty b => Config ->  PluginTestDescriptor b -> ClientCapabilities -> VirtualFileTree -> Session a -> IO a
runSessionWithServerAndCapsInTmpDir config plugin caps tree act = do
  recorder <- pluginTestRecorder
  runSessionWithServerInTmpDir' (plugin recorder) config def caps tree act

-- | Host a server, and run a test session on it.
--
-- Creates a temporary directory, and materializes the VirtualFileTree
-- in the temporary directory.
--
-- To debug test cases and verify the file system is correctly set up,
-- you should set the environment variable 'HLS_TEST_HARNESS_NO_TESTDIR_CLEANUP=1'.
-- Further, we log the temporary directory location on startup. To view
-- the logs, set the environment variable 'HLS_TEST_HARNESS_STDERR=1'.
--
-- Example invocation to debug test cases:
--
-- @
--   HLS_TEST_HARNESS_NO_TESTDIR_CLEANUP=1 HLS_TEST_HARNESS_STDERR=1 cabal test <plugin-name>
-- @
--
-- Don't forget to use 'TASTY_PATTERN' to debug only a subset of tests.
--
-- For plugin test logs, look at the documentation of 'mkPluginTestDescriptor'.
--
-- Note: cwd will be shifted into a temporary directory in @Session a@
runSessionWithServerInTmpDir' ::
  -- | Plugins to load on the server.
  --
  -- For improved logging, make sure these plugins have been initalised with
  -- the recorder produced by @pluginTestRecorder@.
  IdePlugins IdeState ->
  -- | lsp config for the server
  Config ->
  -- | config for the test session
  SessionConfig ->
  ClientCapabilities ->
  VirtualFileTree ->
  Session a ->
  IO a
runSessionWithServerInTmpDir' plugins conf sessConf caps tree act = withLock lockForTempDirs $ do
  (recorder, _) <- initialiseTestRecorder
    ["LSP_TEST_LOG_STDERR", "HLS_TEST_HARNESS_STDERR", "HLS_TEST_LOG_STDERR"]

  -- Do not clean up the temporary directory if this variable is set to anything but '0'.
  -- Aids debugging.
  cleanupTempDir <- lookupEnv "HLS_TEST_HARNESS_NO_TESTDIR_CLEANUP"
  let runTestInDir = case cleanupTempDir of
        Just val
          | val /= "0" -> \action -> do
            (tempDir, _) <- newTempDir
            a <- action tempDir
            logWith recorder Debug $ LogNoCleanup
            pure a

        _ -> \action -> do
          a <- withTempDir action
          logWith recorder Debug $ LogCleanup
          pure a

  runTestInDir $ \tmpDir -> do
    logWith recorder Info $ LogTestDir tmpDir
    _fs <- FS.materialiseVFT tmpDir tree
    runSessionWithServer' plugins conf sessConf caps tmpDir act

goldenWithHaskellDocFormatter
  :: Pretty b
  => Config
  -> PluginTestDescriptor b -- ^ Formatter plugin to be used
  -> String -- ^ Name of the formatter to be used
  -> PluginConfig
  -> TestName -- ^ Title of the test
  -> FilePath -- ^ Directory of the test data to be used
  -> FilePath -- ^ Path to the testdata to be used within the directory
  -> FilePath -- ^ Additional suffix to be appended to the output file
  -> FilePath -- ^ Extension of the output file
  -> (TextDocumentIdentifier -> Session ())
  -> TestTree
goldenWithHaskellDocFormatter config plugin formatter conf title testDataDir path desc ext act =
  let config' = config { formattingProvider = T.pack formatter , plugins = M.singleton (PluginId $ T.pack formatter) conf }
  in goldenGitDiff title (testDataDir </> path <.> desc <.> ext)
  $ runSessionWithServer config' plugin testDataDir
  $ TL.encodeUtf8 . TL.fromStrict
  <$> do
    doc <- openDoc (path <.> ext) "haskell"
    void waitForBuildQueue
    act doc
    documentContents doc

goldenWithCabalDocFormatter
  :: Pretty b
  => Config
  -> PluginTestDescriptor b -- ^ Formatter plugin to be used
  -> String -- ^ Name of the formatter to be used
  -> PluginConfig
  -> TestName -- ^ Title of the test
  -> FilePath -- ^ Directory of the test data to be used
  -> FilePath -- ^ Path to the testdata to be used within the directory
  -> FilePath -- ^ Additional suffix to be appended to the output file
  -> FilePath -- ^ Extension of the output file
  -> (TextDocumentIdentifier -> Session ())
  -> TestTree
goldenWithCabalDocFormatter config plugin formatter conf title testDataDir path desc ext act =
  let config' = config { cabalFormattingProvider = T.pack formatter , plugins = M.singleton (PluginId $ T.pack formatter) conf }
  in goldenGitDiff title (testDataDir </> path <.> desc <.> ext)
  $ runSessionWithServer config' plugin testDataDir
  $ TL.encodeUtf8 . TL.fromStrict
  <$> do
    doc <- openDoc (path <.> ext) "cabal"
    void waitForBuildQueue
    act doc
    documentContents doc

goldenWithHaskellDocFormatterInTmpDir
  :: Pretty b
  => Config
  -> PluginTestDescriptor b -- ^ Formatter plugin to be used
  -> String -- ^ Name of the formatter to be used
  -> PluginConfig
  -> TestName -- ^ Title of the test
  -> VirtualFileTree -- ^ Virtual representation of the test project
  -> FilePath -- ^ Path to the testdata to be used within the directory
  -> FilePath -- ^ Additional suffix to be appended to the output file
  -> FilePath -- ^ Extension of the output file
  -> (TextDocumentIdentifier -> Session ())
  -> TestTree
goldenWithHaskellDocFormatterInTmpDir config plugin formatter conf title tree path desc ext act =
  let config' = config { formattingProvider = T.pack formatter , plugins = M.singleton (PluginId $ T.pack formatter) conf }
  in goldenGitDiff title (vftOriginalRoot tree </> path <.> desc <.> ext)
  $ runSessionWithServerInTmpDir config' plugin tree
  $ TL.encodeUtf8 . TL.fromStrict
  <$> do
    doc <- openDoc (path <.> ext) "haskell"
    void waitForBuildQueue
    act doc
    documentContents doc

goldenWithCabalDocFormatterInTmpDir
  :: Pretty b
  => Config
  -> PluginTestDescriptor b -- ^ Formatter plugin to be used
  -> String -- ^ Name of the formatter to be used
  -> PluginConfig
  -> TestName -- ^ Title of the test
  -> VirtualFileTree -- ^ Virtual representation of the test project
  -> FilePath -- ^ Path to the testdata to be used within the directory
  -> FilePath -- ^ Additional suffix to be appended to the output file
  -> FilePath -- ^ Extension of the output file
  -> (TextDocumentIdentifier -> Session ())
  -> TestTree
goldenWithCabalDocFormatterInTmpDir config plugin formatter conf title tree path desc ext act =
  let config' = config { cabalFormattingProvider = T.pack formatter , plugins = M.singleton (PluginId $ T.pack formatter) conf }
  in goldenGitDiff title (vftOriginalRoot tree </> path <.> desc <.> ext)
  $ runSessionWithServerInTmpDir config' plugin tree
  $ TL.encodeUtf8 . TL.fromStrict
  <$> do
    doc <- openDoc (path <.> ext) "cabal"
    void waitForBuildQueue
    act doc
    documentContents doc

-- | Restore cwd after running an action
keepCurrentDirectory :: IO a -> IO a
keepCurrentDirectory = bracket getCurrentDirectory setCurrentDirectory . const

{-# NOINLINE lock #-}
-- | Never run in parallel
lock :: Lock
lock = unsafePerformIO newLock


{-# NOINLINE lockForTempDirs #-}
-- | Never run in parallel
lockForTempDirs :: Lock
lockForTempDirs = unsafePerformIO newLock

-- | Host a server, and run a test session on it
-- Note: cwd will be shifted into @root@ in @Session a@
runSessionWithServer' ::
  -- | Plugins to load on the server.
  --
  -- For improved logging, make sure these plugins have been initalised with
  -- the recorder produced by @pluginTestRecorder@.
  IdePlugins IdeState ->
  -- | lsp config for the server
  Config ->
  -- | config for the test session
  SessionConfig ->
  ClientCapabilities ->
  FilePath ->
  Session a ->
  IO a
runSessionWithServer' plugins conf sconf caps root s =  withLock lock $ keepCurrentDirectory $ do
    (inR, inW) <- createPipe
    (outR, outW) <- createPipe

    -- Allow three environment variables, because "LSP_TEST_LOG_STDERR" has been used before,
    -- (thus, backwards compatibility) and "HLS_TEST_SERVER_LOG_STDERR" because it
    -- uses a more descriptive name.
    -- It is also in better accordance with 'pluginTestRecorder' which uses "HLS_TEST_PLUGIN_LOG_STDERR".
    -- At last, "HLS_TEST_LOG_STDERR" is intended to enable all logging for the server and the plugins
    -- under test.
    (recorder, logger_) <- initialiseTestRecorder
      ["LSP_TEST_LOG_STDERR", "HLS_TEST_SERVER_LOG_STDERR", "HLS_TEST_LOG_STDERR"]

    let
        sconf' = sconf { lspConfig = hlsConfigToClientConfig conf }
        -- exists until old logging style is phased out
        logger = Logger $ \p m -> logger_ (WithPriority p emptyCallStack (pretty m))

        hlsPlugins = IdePlugins [Test.blockCommandDescriptor "block-command"] <> plugins

        arguments@Arguments{ argsIdeOptions, argsLogger } =
            testing (cmapWithPrio LogIDEMain recorder) logger hlsPlugins

        ideOptions config ghcSession =
            let defIdeOptions = argsIdeOptions config ghcSession
            in defIdeOptions
                    { optTesting = IdeTesting True
                    , optCheckProject = pure False
                    }

    server <- async $
        Ghcide.defaultMain (cmapWithPrio LogIDEMain recorder)
            arguments
                { argsHandleIn = pure inR
                , argsHandleOut = pure outW
                , argsDefaultHlsConfig = conf
                , argsLogger = argsLogger
                , argsIdeOptions = ideOptions
                }

    x <- runSessionWithHandles inW outR sconf' caps root s
    hClose inW
    timeout 3 (wait server) >>= \case
        Just () -> pure ()
        Nothing -> do
            putStrLn "Server does not exit in 3s, canceling the async task..."
            (t, _) <- duration $ cancel server
            putStrLn $ "Finishing canceling (took " <> showDuration t <> "s)"
    pure x

-- | Wait for the next progress end step
waitForProgressDone :: Session ()
waitForProgressDone = skipManyTill anyMessage $ satisfyMaybe $ \case
  FromServerMess  SMethod_Progress  (TNotificationMessage _ _ (ProgressParams _ v)) | is _workDoneProgressEnd v-> Just ()
  _ -> Nothing

-- | Wait for all progress to be done
-- Needs at least one progress done notification to return
waitForAllProgressDone :: Session ()
waitForAllProgressDone = loop
  where
    loop = do
      ~() <- skipManyTill anyMessage $ satisfyMaybe $ \case
        FromServerMess  SMethod_Progress  (TNotificationMessage _ _ (ProgressParams _ v)) | is _workDoneProgressEnd v -> Just ()
        _ -> Nothing
      done <- null <$> getIncompleteProgressSessions
      unless done loop

-- | Wait for the build queue to be empty
waitForBuildQueue :: Session Seconds
waitForBuildQueue = do
    let m = SMethod_CustomMethod (Proxy @"test")
    waitId <- sendRequest m (toJSON WaitForShakeQueue)
    (td, resp) <- duration $ skipManyTill anyMessage $ responseForId m waitId
    case resp of
        TResponseMessage{_result=Right Null} -> return td
        -- assume a ghcide binary lacking the WaitForShakeQueue method
        _                                    -> return 0

callTestPlugin :: (A.FromJSON b) => TestRequest -> Session (Either ResponseError b)
callTestPlugin cmd = do
    let cm = SMethod_CustomMethod (Proxy @"test")
    waitId <- sendRequest cm (A.toJSON cmd)
    TResponseMessage{_result} <- skipManyTill anyMessage $ responseForId cm waitId
    return $ do
      e <- _result
      case A.fromJSON e of
        A.Error err -> Left $ ResponseError (InR ErrorCodes_InternalError) (T.pack err) Nothing
        A.Success a -> pure a

waitForAction :: String -> TextDocumentIdentifier -> Session (Either ResponseError WaitForIdeRuleResult)
waitForAction key TextDocumentIdentifier{_uri} =
    callTestPlugin (WaitForIdeRule key _uri)

waitForTypecheck :: TextDocumentIdentifier -> Session (Either ResponseError Bool)
waitForTypecheck tid = fmap ideResultSuccess <$> waitForAction "typecheck" tid

getLastBuildKeys :: Session (Either ResponseError [T.Text])
getLastBuildKeys = callTestPlugin GetBuildKeysBuilt

hlsConfigToClientConfig :: Config -> A.Object
hlsConfigToClientConfig config = [("haskell", toJSON config)]

-- | Set the HLS client configuration, and wait for the server to update to use it.
-- Note that this will only work if we are not ignoring configuration requests, you
-- may need to call @setIgnoringConfigurationRequests False@ first.
setHlsConfig :: Config -> Session ()
setHlsConfig config = do
  setConfig $ hlsConfigToClientConfig config
  -- wait until we get the workspace/configuration request from the server, so
  -- we know things are settling. This only works if we're not skipping config
  -- requests!
  skipManyTill anyMessage (void configurationRequest)

waitForKickDone :: Session ()
waitForKickDone = void $ skipManyTill anyMessage nonTrivialKickDone

waitForKickStart :: Session ()
waitForKickStart = void $ skipManyTill anyMessage nonTrivialKickStart

nonTrivialKickDone :: Session ()
nonTrivialKickDone = kick (Proxy @"kick/done") >>= guard . not . null

nonTrivialKickStart :: Session ()
nonTrivialKickStart = kick (Proxy @"kick/start") >>= guard . not . null

kick :: KnownSymbol k => Proxy k -> Session [FilePath]
kick proxyMsg = do
  NotMess TNotificationMessage{_params} <- customNotification proxyMsg
  case fromJSON _params of
    Success x -> return x
    other     -> error $ "Failed to parse kick/done details: " <> show other
