module Test.Hls.TestEnv
  ( HlsLogStderr(..)
  , HlsPluginLogStderr(..)
  , HlsHarnessStderr(..)
  , HlsHarnessNoTestdirCleanup(..)
  , HlsTestRootDir(..)
  , LspTimeout(..)
  , hlsTestOptions
  , wrapCliTestOptions
  ) where

import           Control.Monad      (guard)
import           Data.Data          (Proxy (..))
import           Data.Foldable      (traverse_)
import           Data.Maybe         (catMaybes)
import           System.Environment (lookupEnv, setEnv, unsetEnv)
import           Test.Tasty         (TestTree, askOption, withResource)
import           Test.Tasty.Options (IsOption (defaultValue, optionCLParser, optionHelp, optionName, parseValue),
                                     OptionDescription (..), flagCLParser,
                                     safeRead, safeReadBool)

newtype HlsLogStderr = HlsLogStderr Bool
instance IsOption HlsLogStderr where
  defaultValue = HlsLogStderr False
  parseValue s = HlsLogStderr <$> safeReadBool s
  optionName = pure "log-stderr"
  optionHelp = pure "Log all HLS messages to stderr"
  optionCLParser = flagCLParser Nothing (HlsLogStderr True)

newtype HlsPluginLogStderr = HlsPluginLogStderr Bool
instance IsOption HlsPluginLogStderr where
  defaultValue = HlsPluginLogStderr False
  parseValue s = HlsPluginLogStderr <$> safeReadBool s
  optionName = pure "plugin-log-stderr"
  optionHelp = pure "Log all messages of the hls plugin under test to stderr"
  optionCLParser = flagCLParser Nothing (HlsPluginLogStderr True)

newtype HlsHarnessStderr = HlsHarnessStderr Bool
instance IsOption HlsHarnessStderr where
  defaultValue = HlsHarnessStderr False
  parseValue s = HlsHarnessStderr <$> safeReadBool s
  optionName = pure "test-harness-log-stderr"
  optionHelp = pure "Log test setup messages to stderr"
  optionCLParser = flagCLParser Nothing (HlsHarnessStderr True)

newtype HlsHarnessNoTestdirCleanup = HlsHarnessNoTestdirCleanup Bool
instance IsOption HlsHarnessNoTestdirCleanup where
  defaultValue = HlsHarnessNoTestdirCleanup False
  parseValue s = HlsHarnessNoTestdirCleanup <$> safeReadBool s
  optionName = pure "test-harness-no-testdir-cleanup"
  optionHelp = pure "Don't remove the test directories after test execution"
  optionCLParser = flagCLParser Nothing (HlsHarnessNoTestdirCleanup True)

newtype HlsTestRootDir = HlsTestRootDir (Maybe FilePath)
instance IsOption HlsTestRootDir where
  defaultValue = HlsTestRootDir Nothing
  parseValue s = Just . HlsTestRootDir . Just $ s
  optionName = pure "test-root-dir"
  optionHelp = pure "Root directory for test file isolation"

newtype LspTimeout = LspTimeout (Maybe Int)
instance IsOption LspTimeout where
  defaultValue = LspTimeout Nothing
  parseValue s = LspTimeout . Just <$> safeRead s
  optionName = pure "lsp-timeout"
  optionHelp = pure "Set a specific test timeout in seconds"

hlsTestOptions :: [OptionDescription]
hlsTestOptions =
  [ Option (Proxy @HlsLogStderr)
  , Option (Proxy @HlsPluginLogStderr)
  , Option (Proxy @HlsHarnessStderr)
  , Option (Proxy @HlsHarnessNoTestdirCleanup)
  , Option (Proxy @HlsTestRootDir)
  , Option (Proxy @LspTimeout)
  ]

-- | Use tasty cli options to set legacy environment variables
wrapCliTestOptions :: TestTree -> TestTree
wrapCliTestOptions tree =
    askOption $ \(HlsLogStderr logStderr) ->
    askOption $ \(HlsPluginLogStderr pluginStderr) ->
    askOption $ \(HlsTestRootDir rootDir) ->
    askOption $ \(HlsHarnessStderr harnessStderr) ->
    askOption $ \(HlsHarnessNoTestdirCleanup harnessNoTestdirCleanup) ->
    askOption $ \(LspTimeout timeout) ->
    let overrides = catMaybes
            [ ("HLS_TEST_LOG_STDERR", "1")                 <$ guard logStderr
            , ("HLS_TEST_PLUGIN_LOG_STDERR", "1")          <$ guard pluginStderr
            , ("HLS_TEST_ROOTDIR",)                        <$> rootDir
            , ("HLS_TEST_HARNESS_STDERR", "1")             <$ guard harnessStderr
            , ("HLS_TEST_HARNESS_NO_TESTDIR_CLEANUP", "1") <$ guard harnessNoTestdirCleanup
            , ("LSP_TIMEOUT",) . show                      <$> timeout
            ]
    in withResource (setOverrides overrides) restoreEnvs (const tree)

setOverrides :: [(String, String)] -> IO [(String, Maybe String)]
setOverrides = traverse $ \(k, v) -> do
  old <- lookupEnv k
  setEnv k v
  pure (k, old)

restoreEnvs :: [(String, Maybe String)] -> IO ()
restoreEnvs = traverse_ $ \(k, mv) -> maybe (unsetEnv k) (setEnv k) mv
