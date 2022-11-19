{-# LANGUAGE DisambiguateRecordFields #-}
{-# LANGUAGE GADTs                    #-}
{-# LANGUAGE LambdaCase               #-}
{-# LANGUAGE NamedFieldPuns           #-}
{-# LANGUAGE OverloadedStrings        #-}
{-# LANGUAGE PolyKinds                #-}
module Test.Hls
  ( module Test.Tasty.HUnit,
    module Test.Tasty,
    module Test.Tasty.ExpectedFailure,
    module Test.Hls.Util,
    module Language.LSP.Types,
    module Language.LSP.Test,
    module Control.Monad.IO.Class,
    module Control.Applicative.Combinators,
    defaultTestRunner,
    goldenGitDiff,
    goldenWithHaskellDoc,
    goldenWithHaskellDocFormatter,
    goldenWithCabalDocFormatter,
    def,
    runSessionWithServer,
    runSessionWithServerFormatter,
    runSessionWithCabalServerFormatter,
    runSessionWithServer',
    waitForProgressDone,
    waitForAllProgressDone,
    PluginDescriptor,
    IdeState,
    waitForBuildQueue,
    waitForTypecheck,
    waitForAction,
    sendConfigurationChanged,
    getLastBuildKeys,
    waitForKickDone,
    waitForKickStart,
    )
where

import           Control.Applicative.Combinators
import           Control.Concurrent.Async        (async, cancel, wait)
import           Control.Concurrent.Extra
import           Control.Exception.Base
import           Control.Monad                   (guard, unless, void)
import           Control.Monad.IO.Class
import           Data.Aeson                      (Result (Success),
                                                  Value (Null), fromJSON,
                                                  toJSON)
import qualified Data.Aeson                      as A
import           Data.ByteString.Lazy            (ByteString)
import           Data.Default                    (def)
import qualified Data.Map                        as M
import           Data.Maybe                      (fromMaybe)
import qualified Data.Text                       as T
import qualified Data.Text.Lazy                  as TL
import qualified Data.Text.Lazy.Encoding         as TL
import           Development.IDE                 (IdeState)
import           Development.IDE.Main            hiding (Log)
import qualified Development.IDE.Main            as Ghcide
import qualified Development.IDE.Main            as IDEMain
import           Development.IDE.Plugin.Test     (TestRequest (GetBuildKeysBuilt, WaitForIdeRule, WaitForShakeQueue),
                                                  WaitForIdeRuleResult (ideResultSuccess))
import qualified Development.IDE.Plugin.Test     as Test
import           Development.IDE.Types.Logger    (Logger (Logger),
                                                  Pretty (pretty),
                                                  Priority (Debug),
                                                  Recorder (Recorder, logger_),
                                                  WithPriority (WithPriority, priority),
                                                  cfilter, cmapWithPrio,
                                                  makeDefaultStderrRecorder)
import           Development.IDE.Types.Options
import           GHC.IO.Handle
import           GHC.Stack                       (emptyCallStack)
import           Ide.Plugin.Config               (Config, PluginConfig,
                                                  cabalFormattingProvider,
                                                  formattingProvider, plugins)
import           Ide.PluginUtils                 (idePluginsToPluginDesc,
                                                  pluginDescToIdePlugins)
import           Ide.Types
import           Language.LSP.Test
import           Language.LSP.Types              hiding
                                                 (SemanticTokenAbsolute (length, line),
                                                  SemanticTokenRelative (length),
                                                  SemanticTokensEdit (_start))
import           Language.LSP.Types.Capabilities (ClientCapabilities)
import           Prelude                         hiding (log)
import           System.Directory                (getCurrentDirectory,
                                                  setCurrentDirectory)
import           System.Environment              (lookupEnv)
import           System.FilePath
import           System.IO.Unsafe                (unsafePerformIO)
import           System.Process.Extra            (createPipe)
import           System.Time.Extra
import           Test.Hls.Util
import           Test.Tasty                      hiding (Timeout)
import           Test.Tasty.ExpectedFailure
import           Test.Tasty.Golden
import           Test.Tasty.HUnit
import           Test.Tasty.Ingredients.Rerun
import           Test.Tasty.Runners              (NumThreads (..))

newtype Log = LogIDEMain IDEMain.Log

instance Pretty Log where
  pretty = \case
    LogIDEMain log -> pretty log

-- | Run 'defaultMainWithRerun', limiting each single test case running at most 10 minutes
defaultTestRunner :: TestTree -> IO ()
defaultTestRunner = defaultMainWithRerun . adjustOption (const $ NumThreads 1) . adjustOption (const $ mkTimeout 600000000)

gitDiff :: FilePath -> FilePath -> [String]
gitDiff fRef fNew = ["git", "-c", "core.fileMode=false", "diff", "--no-index", "--text", "--exit-code", fRef, fNew]

goldenGitDiff :: TestName -> FilePath -> IO ByteString -> TestTree
goldenGitDiff name = goldenVsStringDiff name gitDiff

goldenWithHaskellDoc
  :: PluginDescriptor IdeState
  -> TestName
  -> FilePath
  -> FilePath
  -> FilePath
  -> FilePath
  -> (TextDocumentIdentifier -> Session ())
  -> TestTree
goldenWithHaskellDoc plugin title testDataDir path desc ext act =
  goldenGitDiff title (testDataDir </> path <.> desc <.> ext)
  $ runSessionWithServer plugin testDataDir
  $ TL.encodeUtf8 . TL.fromStrict
  <$> do
    doc <- openDoc (path <.> ext) "haskell"
    void waitForBuildQueue
    act doc
    documentContents doc


runSessionWithServer :: PluginDescriptor IdeState -> FilePath -> Session a -> IO a
runSessionWithServer plugin = runSessionWithServer' [plugin] def def fullCaps

runSessionWithServerFormatter :: PluginDescriptor IdeState -> String -> PluginConfig -> FilePath -> Session a -> IO a
runSessionWithServerFormatter plugin formatter conf =
  runSessionWithServer'
    [plugin]
    def
      { formattingProvider = T.pack formatter
      , plugins = M.singleton (T.pack formatter) conf
      }
    def
    fullCaps

goldenWithHaskellDocFormatter
  :: PluginDescriptor IdeState -- ^ Formatter plugin to be used
  -> String -- ^ Name of the formatter to be used
  -> PluginConfig
  -> TestName -- ^ Title of the test
  -> FilePath -- ^ Directory of the test data to be used
  -> FilePath -- ^ Path to the testdata to be used within the directory
  -> FilePath -- ^ Additional suffix to be appended to the output file
  -> FilePath -- ^ Extension of the output file
  -> (TextDocumentIdentifier -> Session ())
  -> TestTree
goldenWithHaskellDocFormatter plugin formatter conf title testDataDir path desc ext act =
  goldenGitDiff title (testDataDir </> path <.> desc <.> ext)
  $ runSessionWithServerFormatter plugin formatter conf testDataDir
  $ TL.encodeUtf8 . TL.fromStrict
  <$> do
    doc <- openDoc (path <.> ext) "haskell"
    void waitForBuildQueue
    act doc
    documentContents doc

goldenWithCabalDocFormatter
  :: PluginDescriptor IdeState -- ^ Formatter plugin to be used
  -> String -- ^ Name of the formatter to be used
  -> PluginConfig
  -> TestName -- ^ Title of the test
  -> FilePath -- ^ Directory of the test data to be used
  -> FilePath -- ^ Path to the testdata to be used within the directory
  -> FilePath -- ^ Additional suffix to be appended to the output file
  -> FilePath -- ^ Extension of the output file
  -> (TextDocumentIdentifier -> Session ())
  -> TestTree
goldenWithCabalDocFormatter plugin formatter conf title testDataDir path desc ext act =
  goldenGitDiff title (testDataDir </> path <.> desc <.> ext)
  $ runSessionWithCabalServerFormatter plugin formatter conf testDataDir
  $ TL.encodeUtf8 . TL.fromStrict
  <$> do
    doc <- openDoc (path <.> ext) "cabal"
    void waitForBuildQueue
    act doc
    documentContents doc

runSessionWithCabalServerFormatter :: PluginDescriptor IdeState -> String -> PluginConfig -> FilePath -> Session a -> IO a
runSessionWithCabalServerFormatter plugin formatter conf =
  runSessionWithServer'
    [plugin]
    def
      { cabalFormattingProvider = T.pack formatter
      , plugins = M.singleton (T.pack formatter) conf
      }
    def
    fullCaps

-- | Restore cwd after running an action
keepCurrentDirectory :: IO a -> IO a
keepCurrentDirectory = bracket getCurrentDirectory setCurrentDirectory . const

{-# NOINLINE lock #-}
-- | Never run in parallel
lock :: Lock
lock = unsafePerformIO newLock


-- | Host a server, and run a test session on it
-- Note: cwd will be shifted into @root@ in @Session a@
runSessionWithServer' ::
  -- | plugins to load on the server
  [PluginDescriptor IdeState] ->
  -- | lsp config for the server
  Config ->
  -- | config for the test session
  SessionConfig ->
  ClientCapabilities ->
  FilePath ->
  Session a ->
  IO a
runSessionWithServer' plugins conf sconf caps root s = withLock lock $ keepCurrentDirectory $ do
    (inR, inW) <- createPipe
    (outR, outW) <- createPipe

    docWithPriorityRecorder <- makeDefaultStderrRecorder Nothing Debug

    logStdErr <- fromMaybe "0" <$> lookupEnv "LSP_TEST_LOG_STDERR"

    let
        docWithFilteredPriorityRecorder@Recorder{ logger_ } =
            if logStdErr == "0" then mempty
            else cfilter (\WithPriority{ priority } -> priority >= Debug) docWithPriorityRecorder

        -- exists until old logging style is phased out
        logger = Logger $ \p m -> logger_ (WithPriority p emptyCallStack (pretty m))

        recorder = cmapWithPrio pretty docWithFilteredPriorityRecorder

        arguments@Arguments{ argsHlsPlugins, argsIdeOptions, argsLogger } = defaultArguments (cmapWithPrio LogIDEMain recorder) logger

        hlsPlugins =
            plugins
            ++ [Test.blockCommandDescriptor "block-command", Test.plugin]
            ++ idePluginsToPluginDesc argsHlsPlugins
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
                , argsHlsPlugins = pluginDescToIdePlugins hlsPlugins
                }

    x <- runSessionWithHandles inW outR sconf caps root s
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
  FromServerMess SProgress (NotificationMessage _ _ (ProgressParams _ (End _))) -> Just ()
  _ -> Nothing

-- | Wait for all progress to be done
-- Needs at least one progress done notification to return
waitForAllProgressDone :: Session ()
waitForAllProgressDone = loop
  where
    loop = do
      ~() <- skipManyTill anyMessage $ satisfyMaybe $ \case
        FromServerMess SProgress (NotificationMessage _ _ (ProgressParams _ (End _))) -> Just ()
        _ -> Nothing
      done <- null <$> getIncompleteProgressSessions
      unless done loop

-- | Wait for the build queue to be empty
waitForBuildQueue :: Session Seconds
waitForBuildQueue = do
    let m = SCustomMethod "test"
    waitId <- sendRequest m (toJSON WaitForShakeQueue)
    (td, resp) <- duration $ skipManyTill anyMessage $ responseForId m waitId
    case resp of
        ResponseMessage{_result=Right Null} -> return td
        -- assume a ghcide binary lacking the WaitForShakeQueue method
        _                                   -> return 0

callTestPlugin :: (A.FromJSON b) => TestRequest -> Session (Either ResponseError b)
callTestPlugin cmd = do
    let cm = SCustomMethod "test"
    waitId <- sendRequest cm (A.toJSON cmd)
    ResponseMessage{_result} <- skipManyTill anyMessage $ responseForId cm waitId
    return $ do
      e <- _result
      case A.fromJSON e of
        A.Error err -> Left $ ResponseError InternalError (T.pack err) Nothing
        A.Success a -> pure a

waitForAction :: String -> TextDocumentIdentifier -> Session (Either ResponseError WaitForIdeRuleResult)
waitForAction key TextDocumentIdentifier{_uri} =
    callTestPlugin (WaitForIdeRule key _uri)

waitForTypecheck :: TextDocumentIdentifier -> Session (Either ResponseError Bool)
waitForTypecheck tid = fmap ideResultSuccess <$> waitForAction "typecheck" tid

getLastBuildKeys :: Session (Either ResponseError [T.Text])
getLastBuildKeys = callTestPlugin GetBuildKeysBuilt

sendConfigurationChanged :: Value -> Session ()
sendConfigurationChanged config =
  sendNotification SWorkspaceDidChangeConfiguration (DidChangeConfigurationParams config)

waitForKickDone :: Session ()
waitForKickDone = void $ skipManyTill anyMessage nonTrivialKickDone

waitForKickStart :: Session ()
waitForKickStart = void $ skipManyTill anyMessage nonTrivialKickStart

nonTrivialKickDone :: Session ()
nonTrivialKickDone = kick "done" >>= guard . not . null

nonTrivialKickStart :: Session ()
nonTrivialKickStart = kick "start" >>= guard . not . null

kick :: T.Text -> Session [FilePath]
kick msg = do
  NotMess NotificationMessage{_params} <- customNotification $ "kick/" <> msg
  case fromJSON _params of
    Success x -> return x
    other     -> error $ "Failed to parse kick/done details: " <> show other
