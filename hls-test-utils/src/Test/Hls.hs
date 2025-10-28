{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE OverloadedLists       #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE RecordWildCards       #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeApplications      #-}
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
    goldenWithTestConfig,
    def,
    -- * Running HLS for integration tests
    runSessionWithServer,
    runSessionWithServerInTmpDir,
    runSessionWithTestConfig,
    -- * Running parameterised tests for a set of test configurations
    parameterisedCursorTest,
    -- * Helpful re-exports
    PluginDescriptor,
    IdeState,
    -- * Helpers for expected test case failuers
    BrokenBehavior(..),
    ExpectBroken(..),
    unCurrent,
    -- * Assertion helper functions
    waitForProgressDone,
    waitForAllProgressDone,
    waitForBuildQueue,
    waitForProgressBegin,
    waitForTypecheck,
    waitForAction,
    hlsConfigToClientConfig,
    setHlsConfig,
    getLastBuildKeys,
    waitForKickDone,
    waitForKickStart,
    -- * Plugin descriptor helper functions for tests
    PluginTestDescriptor,
    hlsPluginTestRecorder,
    mkPluginTestDescriptor,
    mkPluginTestDescriptor',
    -- * Re-export logger types
    -- Avoids slightly annoying ghcide imports when they are unnecessary.
    WithPriority(..),
    Recorder,
    Priority(..),
    captureKickDiagnostics,
    kick,
    TestConfig(..)
    )
where

import           Control.Applicative.Combinators
import           Control.Concurrent.Async                 (async, cancel, wait)
import           Control.Concurrent.Extra
import           Control.Exception.Safe
import           Control.Lens                             ((^.))
import           Control.Lens.Extras                      (is)
import           Control.Monad                            (guard, unless, void)
import           Control.Monad.Extra                      (forM)
import           Control.Monad.IO.Class
import           Data.Aeson                               (Result (Success),
                                                           Value (Null),
                                                           fromJSON, toJSON)
import qualified Data.Aeson                               as A
import           Data.ByteString.Lazy                     (ByteString)
import           Data.Default                             (Default, def)
import qualified Data.Map                                 as M
import           Data.Maybe                               (fromMaybe, mapMaybe)
import           Data.Proxy                               (Proxy (Proxy))
import qualified Data.Text                                as T
import qualified Data.Text.Lazy                           as TL
import qualified Data.Text.Lazy.Encoding                  as TL
import           Development.IDE                          (IdeState,
                                                           LoggingColumn (ThreadIdColumn),
                                                           defaultLayoutOptions,
                                                           layoutPretty,
                                                           renderStrict)
import           Development.IDE.Main                     hiding (Log)
import qualified Development.IDE.Main                     as IDEMain
import           Development.IDE.Plugin.Completions.Types (PosPrefixInfo)
import           Development.IDE.Plugin.Test              (TestRequest (GetBuildKeysBuilt, WaitForIdeRule, WaitForShakeQueue),
                                                           WaitForIdeRuleResult (ideResultSuccess))
import qualified Development.IDE.Plugin.Test              as Test
import           Development.IDE.Types.Options
import           GHC.IO.Handle
import           GHC.TypeLits
import           Ide.Logger                               (Pretty (pretty),
                                                           Priority (..),
                                                           Recorder,
                                                           WithPriority (WithPriority, priority),
                                                           cfilter,
                                                           cmapWithPrio,
                                                           defaultLoggingColumns,
                                                           logWith,
                                                           makeDefaultStderrRecorder,
                                                           (<+>))
import qualified Ide.Logger                               as Logger
import           Ide.PluginUtils                          (idePluginsToPluginDesc,
                                                           pluginDescToIdePlugins)
import           Ide.Types
import           Language.LSP.Protocol.Capabilities
import qualified Language.LSP.Protocol.Lens               as L
import           Language.LSP.Protocol.Message
import qualified Language.LSP.Protocol.Message            as LSP
import           Language.LSP.Protocol.Types              hiding (Null)
import qualified Language.LSP.Server                      as LSP
import           Language.LSP.Test
import           Prelude                                  hiding (log)
import           System.Directory                         (canonicalizePath,
                                                           createDirectoryIfMissing,
                                                           getCurrentDirectory,
                                                           getTemporaryDirectory,
                                                           makeAbsolute,
                                                           setCurrentDirectory)
import           System.Environment                       (lookupEnv, setEnv)
import           System.FilePath
import           System.IO.Extra                          (newTempDirWithin)
import           System.IO.Unsafe                         (unsafePerformIO)
import           System.Process.Extra                     (createPipe)
import           System.Time.Extra
import qualified Test.Hls.FileSystem                      as FS
import           Test.Hls.FileSystem
import           Test.Hls.Util
import           Test.Tasty                               hiding (Timeout)
import           Test.Tasty.ExpectedFailure
import           Test.Tasty.Golden
import           Test.Tasty.HUnit
import           Test.Tasty.Ingredients.Rerun

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

data BrokenBehavior = Current | Ideal

data ExpectBroken (k :: BrokenBehavior) a where
  BrokenCurrent :: a -> ExpectBroken 'Current a
  BrokenIdeal :: a -> ExpectBroken 'Ideal a

unCurrent :: ExpectBroken 'Current a -> a
unCurrent (BrokenCurrent a) = a

-- | Run 'defaultMainWithRerun', limiting each single test case running at most 10 minutes
defaultTestRunner :: TestTree -> IO ()
defaultTestRunner = defaultMainWithRerun . adjustOption (const $ mkTimeout 600000000)

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
goldenWithHaskellDoc = goldenWithDoc LanguageKind_Haskell

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
goldenWithHaskellDocInTmpDir = goldenWithDocInTmpDir LanguageKind_Haskell

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
  $ runSessionWithTestConfig def {
    testDirLocation = Left testDataDir,
    testConfigCaps = clientCaps,
    testLspConfig = config,
    testPluginDescriptor = plugin
  }
  $ const
--   runSessionWithServerAndCaps config plugin clientCaps testDataDir
  $ TL.encodeUtf8 . TL.fromStrict
  <$> do
    doc <- openDoc (path <.> ext) "haskell"
    void waitForBuildQueue
    act doc
    documentContents doc

goldenWithTestConfig
  :: Pretty b
  => TestConfig b
  -> TestName
  -> VirtualFileTree
  -> FilePath
  -> FilePath
  -> FilePath
  -> (TextDocumentIdentifier -> Session ())
  -> TestTree
goldenWithTestConfig config title tree path desc ext act =
  goldenGitDiff title (vftOriginalRoot tree </> path <.> desc <.> ext)
  $ runSessionWithTestConfig config $ const
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
  $
  runSessionWithTestConfig def {
    testDirLocation = Right tree,
    testConfigCaps = clientCaps,
    testLspConfig = config,
    testPluginDescriptor = plugin
  } $ const
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
goldenWithCabalDoc = goldenWithDoc (LanguageKind_Custom "cabal")

goldenWithDoc
  :: Pretty b
  => LanguageKind
  -> Config
  -> PluginTestDescriptor b
  -> TestName
  -> FilePath
  -> FilePath
  -> FilePath
  -> FilePath
  -> (TextDocumentIdentifier -> Session ())
  -> TestTree
goldenWithDoc languageKind config plugin title testDataDir path desc ext act =
  goldenGitDiff title (testDataDir </> path <.> desc <.> ext)
  $ runSessionWithServer config plugin testDataDir
  $ TL.encodeUtf8 . TL.fromStrict
  <$> do
    doc <- openDoc (path <.> ext) languageKind
    void waitForBuildQueue
    act doc
    documentContents doc

goldenWithDocInTmpDir
  :: Pretty b
  => LanguageKind
  -> Config
  -> PluginTestDescriptor b
  -> TestName
  -> VirtualFileTree
  -> FilePath
  -> FilePath
  -> FilePath
  -> (TextDocumentIdentifier -> Session ())
  -> TestTree
goldenWithDocInTmpDir languageKind config plugin title tree path desc ext act =
  goldenGitDiff title (vftOriginalRoot tree </> path <.> desc <.> ext)
  $ runSessionWithServerInTmpDir config plugin tree
  $ TL.encodeUtf8 . TL.fromStrict
  <$> do
    doc <- openDoc (path <.> ext) languageKind
    void waitForBuildQueue
    act doc
    documentContents doc

-- | A parameterised test is similar to a normal test case but allows to run
-- the same test case multiple times with different inputs.
-- A 'parameterisedCursorTest' allows to define a test case based on an input file
-- that specifies one or many cursor positions via the identification value '^'.
--
-- For example:
--
-- @
--  parameterisedCursorTest "Cursor Test" [__i|
--       foo = 2
--        ^
--       bar = 3
--       baz = foo + bar
--         ^
--       |]
--       ["foo", "baz"]
--       (\input cursor -> findFunctionNameUnderCursor input cursor)
-- @
--
-- Assuming a fitting implementation for 'findFunctionNameUnderCursor'.
--
-- This test definition will run the test case 'findFunctionNameUnderCursor' for
-- each cursor position, each in its own isolated 'testCase'.
-- Cursor positions are identified via the character '^', which points to the
-- above line as the actual cursor position.
-- Lines containing '^' characters, are removed from the final text, that is
-- passed to the testing function.
--
-- TODO: Many Haskell and Cabal source may contain '^' characters for good reasons.
-- We likely need a way to change the character for certain test cases in the future.
--
-- The quasi quoter '__i' is very helpful to define such tests, as it additionally
-- allows to interpolate haskell values and functions. We reexport this quasi quoter
-- for easier usage.
parameterisedCursorTest :: (Show a, Eq a) => String -> T.Text -> [a] -> (T.Text -> PosPrefixInfo -> IO a) -> TestTree
parameterisedCursorTest title content expectations act
  | lenPrefs /= lenExpected = error $ "parameterisedCursorTest: Expected " <> show lenExpected <> " cursors but found: " <> show lenPrefs
  | otherwise = testGroup title $
      map singleTest testCaseSpec
  where
    lenPrefs = length prefInfos
    lenExpected = length expectations
    (cleanText, prefInfos) = extractCursorPositions content

    testCaseSpec = zip [1 ::Int ..] (zip expectations prefInfos)

    singleTest (n, (expected, info)) = testCase (title <> " " <> show n) $ do
      actual <- act cleanText info
      assertEqual (mkParameterisedLabel info) expected actual

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

-- | Initialize a recorder that can be instructed to write to stderr by
-- setting one of the environment variables:
--
-- * HLS_TEST_HARNESS_STDERR=1
-- * HLS_TEST_LOG_STDERR=1
--
-- "HLS_TEST_LOG_STDERR" is intended to enable all logging for the server and the plugins
-- under test.
hlsHelperTestRecorder :: Pretty a => IO (Recorder (WithPriority a))
hlsHelperTestRecorder = initializeTestRecorder ["HLS_TEST_HARNESS_STDERR", "HLS_TEST_LOG_STDERR"]


-- | Initialize a recorder that can be instructed to write to stderr by
-- setting one of the environment variables:
--
-- * HLS_TEST_PLUGIN_LOG_STDERR=1
-- * HLS_TEST_LOG_STDERR=1
--
-- before running the tests.
--
-- "HLS_TEST_LOG_STDERR" is intended to enable all logging for the server and the plugins
-- under test.
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
hlsPluginTestRecorder :: Pretty a => IO (Recorder (WithPriority a))
hlsPluginTestRecorder = initializeTestRecorder ["HLS_TEST_PLUGIN_LOG_STDERR", "HLS_TEST_LOG_STDERR"]

-- | Generic recorder initialization for plugins and the HLS server for test-cases.
--
-- The created recorder writes to stderr if any of the given environment variables
-- have been set to a value different to @0@.
-- We allow multiple values, to make it possible to define a single environment variable
-- that instructs all recorders in the test-suite to write to stderr.
--
-- We have to return the base logger function for HLS server logging initialisation.
-- See 'runSessionWithServer'' for details.
initializeTestRecorder :: Pretty a => [String] -> IO (Recorder (WithPriority a))
initializeTestRecorder envVars = do
    docWithPriorityRecorder <- makeDefaultStderrRecorder (Just $ ThreadIdColumn : defaultLoggingColumns)
    -- lspClientLogRecorder
    -- There are potentially multiple environment variables that enable this logger
    definedEnvVars <- forM envVars (fmap (fromMaybe "0") . lookupEnv)
    let logStdErr = any (/= "0") definedEnvVars

        docWithFilteredPriorityRecorder =
          if logStdErr then cfilter (\WithPriority{ priority } -> priority >= Debug) docWithPriorityRecorder
          else mempty

    pure (cmapWithPrio pretty docWithFilteredPriorityRecorder)

-- ------------------------------------------------------------
-- Run an HLS server testing a specific plugin
-- ------------------------------------------------------------

runSessionWithServerInTmpDir :: Pretty b => Config -> PluginTestDescriptor b -> VirtualFileTree -> Session a -> IO a
runSessionWithServerInTmpDir config plugin tree act =
    runSessionWithTestConfig def
    {testLspConfig=config, testPluginDescriptor = plugin,  testDirLocation=Right tree}
    (const act)

-- | Same as 'withTemporaryDataAndCacheDirectory', but materialises the given
-- 'VirtualFileTree' in the temporary directory.
withVfsTestDataDirectory :: VirtualFileTree -> (FileSystem -> IO a) ->  IO a
withVfsTestDataDirectory tree act = do
    withTemporaryDataAndCacheDirectory $ \tmpRoot -> do
        fs <- FS.materialiseVFT tmpRoot tree
        act fs

-- | Run an action in a temporary directory.
-- Sets the 'XDG_CACHE_HOME' environment variable to a temporary directory as well.
--
-- This sets up a temporary directory for HLS tests to run.
-- Typically, HLS tests copy their test data into the directory and then launch
-- the HLS session in that directory.
-- This makes sure that the tests are run in isolation, which is good for correctness
-- but also important to have fast tests.
--
-- For improved isolation, we also make sure the 'XDG_CACHE_HOME' environment
-- variable points to a temporary directory. So, we never share interface files
-- or the 'hiedb' across tests.
withTemporaryDataAndCacheDirectory :: (FilePath -> IO a) -> IO a
withTemporaryDataAndCacheDirectory act = withLock lockForTempDirs $ do
    testRoot <- setupTestEnvironment
    helperRecorder <- hlsHelperTestRecorder
    -- Do not clean up the temporary directory if this variable is set to anything but '0'.
    -- Aids debugging.
    cleanupTempDir <- lookupEnv "HLS_TEST_HARNESS_NO_TESTDIR_CLEANUP"
    let runTestInDir action = case cleanupTempDir of
            Just val | val /= "0" -> do
                (tempDir, cacheHome, _) <- setupTemporaryTestDirectories testRoot
                a <- withTempCacheHome cacheHome (action tempDir)
                logWith helperRecorder Debug LogNoCleanup
                pure a

            _ -> do
                (tempDir, cacheHome, cleanup) <- setupTemporaryTestDirectories testRoot
                a <- withTempCacheHome cacheHome (action tempDir) `finally`  cleanup
                logWith helperRecorder Debug LogCleanup
                pure a
    runTestInDir $ \tmpDir' -> do
        -- we canonicalize the path, so that we do not need to do
        -- canonicalization during the test when we compare two paths
        tmpDir <- canonicalizePath tmpDir'
        logWith helperRecorder Info $ LogTestDir tmpDir
        act tmpDir
    where
        cache_home_var = "XDG_CACHE_HOME"
        -- Set the dir for "XDG_CACHE_HOME".
        -- When the operation finished, make sure the old value is restored.
        withTempCacheHome tempCacheHomeDir act =
            bracket
              (do
                old_cache_home <- lookupEnv cache_home_var
                setEnv cache_home_var tempCacheHomeDir
                pure old_cache_home)
              (\old_cache_home ->
                maybe (pure ()) (setEnv cache_home_var) old_cache_home
                )
              (\_ -> act)

        -- Set up a temporary directory for the test files and one for the 'XDG_CACHE_HOME'.
        -- The 'XDG_CACHE_HOME' is important for independent test runs, i.e. completely empty
        -- caches.
        setupTemporaryTestDirectories testRoot = do
            (tempTestCaseDir, cleanup1) <- newTempDirWithin testRoot
            (tempCacheHomeDir, cleanup2) <- newTempDirWithin testRoot
            pure (tempTestCaseDir, tempCacheHomeDir, cleanup1 >> cleanup2)

runSessionWithServer :: Pretty b => Config -> PluginTestDescriptor b -> FilePath -> Session a -> IO a
runSessionWithServer config plugin fp act =
    runSessionWithTestConfig def {
        testLspConfig=config
        , testPluginDescriptor=plugin
        , testDirLocation = Left fp
        } (const act)


instance Default (TestConfig b) where
  def = TestConfig {
    testDirLocation = Right $ VirtualFileTree [] "",
    testClientRoot = Nothing,
    testServerRoot = Nothing,
    testShiftRoot = False,
    testDisableKick = False,
    testDisableDefaultPlugin = False,
    testPluginDescriptor = mempty,
    testLspConfig = def,
    testConfigSession = def,
    testConfigCaps = fullLatestClientCaps,
    testCheckProject = False
  }

-- | Setup the test environment for isolated tests.
--
-- This creates a directory in the temporary directory that will be
-- reused for running isolated tests.
-- It returns the root to the testing directory that tests should use.
-- This directory is not fully cleaned between reruns.
-- However, it is totally safe to delete the directory between runs.
setupTestEnvironment :: IO FilePath
setupTestEnvironment = do
  mRootDir <- lookupEnv "HLS_TEST_ROOTDIR"
  case mRootDir of
    Nothing -> do
      tmpDirRoot <- getTemporaryDirectory
      let testRoot = tmpDirRoot </> "hls-test-root"
      createDirectoryIfMissing True testRoot
      pure testRoot
    Just rootDir -> do
      createDirectoryIfMissing True rootDir
      pure rootDir

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

data TestConfig b = TestConfig
  {
    testDirLocation          :: Either FilePath VirtualFileTree
    -- ^ The file tree to use for the test, either a directory or a virtual file tree
    -- if using a virtual file tree,
    -- Creates a temporary directory, and materializes the VirtualFileTree
    -- in the temporary directory.
    --
    -- To debug test cases and verify the file system is correctly set up,
    -- you should set the environment variable 'HLS_TEST_HARNESS_NO_TESTDIR_CLEANUP=1'.
    -- Further, we log the temporary directory location on startup. To view
    -- the logs, set the environment variable 'HLS_TEST_HARNESS_STDERR=1'.
    -- Example invocation to debug test cases:
    --
    -- @
    --   HLS_TEST_HARNESS_NO_TESTDIR_CLEANUP=1 HLS_TEST_HARNESS_STDERR=1 cabal test <plugin-name>
    -- @
    --
    -- Don't forget to use 'TASTY_PATTERN' to debug only a subset of tests.
    --
    -- For plugin test logs, look at the documentation of 'mkPluginTestDescriptor'.
  , testShiftRoot            :: Bool
    -- ^ Whether to shift the current directory to the root of the project
  , testClientRoot           :: Maybe FilePath
    -- ^ Specify the root of (the client or LSP context),
    -- if Nothing it is the same as the testDirLocation
    -- if Just, it is subdirectory of the testDirLocation
  , testServerRoot           :: Maybe FilePath
    -- ^ Specify root of the server, in exe, it can be specify in command line --cwd,
    -- or just the server start directory
    -- if Nothing it is the same as the testDirLocation
    -- if Just, it is subdirectory of the testDirLocation
  , testDisableKick          :: Bool
    -- ^ Whether to disable the kick action
  , testDisableDefaultPlugin :: Bool
    -- ^ Whether to disable the default plugin comes with ghcide
  , testCheckProject         :: Bool
    -- ^ Whether to typecheck check the project after the session is loaded
  , testPluginDescriptor     :: PluginTestDescriptor b
    -- ^ Plugin to load on the server.
  , testLspConfig            :: Config
    -- ^ lsp config for the server
  , testConfigSession        :: SessionConfig
    -- ^ config for the test session
  , testConfigCaps           :: ClientCapabilities
    -- ^ Client capabilities
  }


wrapClientLogger :: Pretty a => Recorder (WithPriority a) ->
    IO (Recorder (WithPriority a), LSP.LanguageContextEnv Config -> IO ())
wrapClientLogger logger = do
    (lspLogRecorder', cb1) <- Logger.withBacklog Logger.lspClientLogRecorder
    let lspLogRecorder = cmapWithPrio (renderStrict . layoutPretty defaultLayoutOptions. pretty) lspLogRecorder'
    return (lspLogRecorder <> logger, cb1)

-- | Host a server, and run a test session on it.
--
-- Environment variables are used to influence logging verbosity, test cleanup and test execution:
--
-- * @LSP_TIMEOUT@: Set a specific test timeout in seconds.
-- * @LSP_TEST_LOG_MESSAGES@: Log the LSP messages between the client and server.
-- * @LSP_TEST_LOG_STDERR@: Log the stderr of the server to the stderr of this process.
-- * @HLS_TEST_HARNESS_STDERR@: Log test setup messages.
--
-- Test specific environment variables:
--
-- * @HLS_TEST_PLUGIN_LOG_STDERR@: Log all messages of the hls plugin under test to stderr.
-- * @HLS_TEST_LOG_STDERR@: Log all HLS messages to stderr.
-- * @HLS_TEST_HARNESS_NO_TESTDIR_CLEANUP@: Don't remove the test directories after test execution.
--
-- For more detail of the test configuration, see 'TestConfig'
runSessionWithTestConfig :: Pretty b => TestConfig b -> (FilePath -> Session a) -> IO a
runSessionWithTestConfig TestConfig{..} session =
    runSessionInVFS testDirLocation $ \root -> shiftRoot root $ do
    (inR, inW) <- createPipe
    (outR, outW) <- createPipe
    let serverRoot = fromMaybe root testServerRoot
    let clientRoot = fromMaybe root testClientRoot

    (recorder, cb1) <- wrapClientLogger =<< hlsPluginTestRecorder
    (recorderIde, cb2) <- wrapClientLogger =<< hlsHelperTestRecorder
    -- This plugin just installs a handler for the `initialized` notification, which then
    -- picks up the LSP environment and feeds it to our recorders
    let lspRecorderPlugin = pluginDescToIdePlugins [(defaultPluginDescriptor "LSPRecorderCallback" "Internal plugin")
          { pluginNotificationHandlers = mkPluginNotificationHandler LSP.SMethod_Initialized $ \_ _ _ _ -> do
              env <- LSP.getLspEnv
              liftIO $ (cb1 <> cb2) env
          }]

    let plugins = testPluginDescriptor recorder <> lspRecorderPlugin
    timeoutOverride <- fmap read <$> lookupEnv "LSP_TIMEOUT"
    let sconf' = testConfigSession { lspConfig = hlsConfigToClientConfig testLspConfig, messageTimeout = fromMaybe (messageTimeout defaultConfig) timeoutOverride}
        arguments = testingArgs serverRoot recorderIde plugins
    server <- async $
        IDEMain.defaultMain (cmapWithPrio LogIDEMain recorderIde)
            arguments { argsHandleIn = pure inR , argsHandleOut = pure outW }
    result <- runSessionWithHandles inW outR sconf' testConfigCaps clientRoot (session root)
    hClose inW
    timeout 3 (wait server) >>= \case
        Just () -> pure ()
        Nothing -> do
            putStrLn "Server does not exit in 3s, canceling the async task..."
            (t, _) <- duration $ cancel server
            putStrLn $ "Finishing canceling (took " <> showDuration t <> "s)"
    pure result

    where
        shiftRoot shiftTarget f  =
            if testShiftRoot
                then withLock lock $ keepCurrentDirectory $ setCurrentDirectory shiftTarget >> f
                else f
        runSessionInVFS (Left testConfigRoot) act = do
            root <- makeAbsolute testConfigRoot
            withTemporaryDataAndCacheDirectory (const $ act root)
        runSessionInVFS (Right vfs) act =
            withVfsTestDataDirectory vfs $ \fs -> do
                act (fsRoot fs)
        testingArgs prjRoot recorderIde plugins =
            let
                arguments@Arguments{ argsHlsPlugins, argsIdeOptions, argsLspOptions } = defaultArguments (cmapWithPrio LogIDEMain recorderIde) prjRoot plugins
                argsHlsPlugins' = if testDisableDefaultPlugin
                                then plugins
                                else argsHlsPlugins
                hlsPlugins = pluginDescToIdePlugins $ idePluginsToPluginDesc argsHlsPlugins'
                    ++ [Test.blockCommandDescriptor "block-command", Test.plugin]
                ideOptions config sessionLoader = (argsIdeOptions config sessionLoader){
                    optTesting = IdeTesting True
                    , optCheckProject = pure testCheckProject
                    }
            in
                arguments
                { argsHlsPlugins = hlsPlugins
                , argsIdeOptions = ideOptions
                , argsLspOptions = argsLspOptions { LSP.optProgressStartDelay = 0, LSP.optProgressUpdateDelay = 0 }
                , argsDefaultHlsConfig = testLspConfig
                , argsProjectRoot = prjRoot
                , argsDisableKick = testDisableKick
                }

-- | Wait for the next progress begin step
waitForProgressBegin :: Session ()
waitForProgressBegin = skipManyTill anyMessage $ satisfyMaybe $ \case
  FromServerMess  SMethod_Progress  (TNotificationMessage _ _ (ProgressParams _ v)) | is _workDoneProgressBegin v-> Just ()
  _ -> Nothing

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

callTestPlugin :: (A.FromJSON b) => TestRequest -> Session (Either (TResponseError @ClientToServer (Method_CustomMethod "test")) b)
callTestPlugin cmd = do
    let cm = SMethod_CustomMethod (Proxy @"test")
    waitId <- sendRequest cm (A.toJSON cmd)
    TResponseMessage{_result} <- skipManyTill anyMessage $ responseForId cm waitId
    return $ do
      e <- _result
      case A.fromJSON e of
        A.Error err -> Left $ TResponseError (InR ErrorCodes_InternalError) (T.pack err) Nothing
        A.Success a -> pure a

waitForAction :: String -> TextDocumentIdentifier -> Session (Either (TResponseError @ClientToServer (Method_CustomMethod "test")) WaitForIdeRuleResult)
waitForAction key TextDocumentIdentifier{_uri} =
    callTestPlugin (WaitForIdeRule key _uri)

waitForTypecheck :: TextDocumentIdentifier -> Session (Either (TResponseError @ClientToServer (Method_CustomMethod "test")) Bool)
waitForTypecheck tid = fmap ideResultSuccess <$> waitForAction "typecheck" tid

getLastBuildKeys :: Session (Either (TResponseError @ClientToServer (Method_CustomMethod "test")) [T.Text])
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

captureKickDiagnostics :: Session () -> Session () -> Session [Diagnostic]
captureKickDiagnostics start done = do
    _ <- skipManyTill anyMessage start
    messages <- manyTill anyMessage done
    pure $ concat $ mapMaybe diagnostics messages
    where
        diagnostics :: FromServerMessage' a -> Maybe [Diagnostic]
        diagnostics = \msg -> case msg of
            FromServerMess SMethod_TextDocumentPublishDiagnostics diags -> Just (diags ^. L.params . L.diagnostics)
            _ -> Nothing

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
