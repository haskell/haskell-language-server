{-# LANGUAGE GADTs             #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE NamedFieldPuns    #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
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
    def,
    runSessionWithServer,
    runSessionWithServerFormatter,
    runSessionWithServer',
    waitForProgressDone,
    waitForAllProgressDone,
    PluginDescriptor,
    IdeState,
    waitForBuildQueue
    ,waitForTypecheck,waitForAction)
where

import           Control.Applicative.Combinators
import           Control.Concurrent.Async        (async, cancel, wait)
import           Control.Concurrent.Extra
import           Control.Exception.Base
import           Control.Monad                   (unless, void)
import           Control.Monad.IO.Class
import           Data.Aeson                      (Value (Null), toJSON)
import qualified Data.Aeson                      as A
import           Data.ByteString.Lazy            (ByteString)
import           Data.Default                    (def)
import qualified Data.Text                       as T
import qualified Data.Text.Lazy                  as TL
import qualified Data.Text.Lazy.Encoding         as TL
import           Development.IDE                 (IdeState, noLogging)
import           Development.IDE.Graph           (ShakeOptions (shakeThreads))
import           Development.IDE.Main
import qualified Development.IDE.Main            as Ghcide
import           Development.IDE.Plugin.Test     (TestRequest (WaitForIdeRule, WaitForShakeQueue),
                                                  WaitForIdeRuleResult (ideResultSuccess))
import           Development.IDE.Types.Options
import           GHC.IO.Handle
import           Ide.Plugin.Config               (Config, formattingProvider)
import           Ide.PluginUtils                 (idePluginsToPluginDesc, pluginDescToIdePlugins)
import           Ide.Types
import           Language.LSP.Test
import           Language.LSP.Types              hiding
                                                 (SemanticTokenAbsolute (length, line),
                                                  SemanticTokenRelative (length),
                                                  SemanticTokensEdit (_start))
import           Language.LSP.Types.Capabilities (ClientCapabilities)
import           System.Directory                (getCurrentDirectory,
                                                  setCurrentDirectory)
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

-- | Run 'defaultMainWithRerun', limiting each single test case running at most 10 minutes
defaultTestRunner :: TestTree -> IO ()
defaultTestRunner = defaultMainWithRerun . adjustOption (const $ mkTimeout 600000000)

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

goldenWithHaskellDocFormatter
  :: PluginDescriptor IdeState
  -> String
  -> TestName
  -> FilePath
  -> FilePath
  -> FilePath
  -> FilePath
  -> (TextDocumentIdentifier -> Session ())
  -> TestTree
goldenWithHaskellDocFormatter plugin formatter title testDataDir path desc ext act =
  goldenGitDiff title (testDataDir </> path <.> desc <.> ext)
  $ runSessionWithServerFormatter plugin formatter testDataDir
  $ TL.encodeUtf8 . TL.fromStrict
  <$> do
    doc <- openDoc (path <.> ext) "haskell"
    void waitForBuildQueue
    act doc
    documentContents doc

runSessionWithServer :: PluginDescriptor IdeState -> FilePath -> Session a -> IO a
runSessionWithServer plugin = runSessionWithServer' [plugin] def def fullCaps

runSessionWithServerFormatter :: PluginDescriptor IdeState -> String -> FilePath -> Session a -> IO a
runSessionWithServerFormatter plugin formatter =
  runSessionWithServer'
    [plugin]
    def {formattingProvider = T.pack formatter}
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
runSessionWithServer' plugin conf sconf caps root s = withLock lock $ keepCurrentDirectory $ do
  (inR, inW) <- createPipe
  (outR, outW) <- createPipe
  server <-
    async $
      Ghcide.defaultMain
        testing
          { argsHandleIn = pure inR,
            argsHandleOut = pure outW,
            argsDefaultHlsConfig = conf,
            argsLogger = pure noLogging,
            argsIdeOptions = \config sessionLoader ->
              let ideOptions = (argsIdeOptions def config sessionLoader) {optTesting = IdeTesting True}
               in ideOptions {optShakeOptions = (optShakeOptions ideOptions) {shakeThreads = 2}},
            argsHlsPlugins = pluginDescToIdePlugins $ plugin ++ idePluginsToPluginDesc (argsHlsPlugins testing)
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

-- | Wait for all progress to be done
-- Needs at least one progress done notification to return
waitForProgressDone :: Session ()
waitForProgressDone = loop
  where
    loop = do
      () <- skipManyTill anyMessage $ satisfyMaybe $ \case
        FromServerMess SProgress (NotificationMessage _ _ (ProgressParams _ (End _))) -> Just ()
        _ -> Nothing
      done <- null <$> getIncompleteProgressSessions
      unless done loop

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

waitForAction :: String -> TextDocumentIdentifier -> Session (Either ResponseError WaitForIdeRuleResult)
waitForAction key TextDocumentIdentifier{_uri} = do
    let cm = SCustomMethod "test"
    waitId <- sendRequest cm (A.toJSON $ WaitForIdeRule key _uri)
    ResponseMessage{_result} <- skipManyTill anyMessage $ responseForId cm waitId
    return $ do
      e <- _result
      case A.fromJSON e of
        A.Error e   -> Left $ ResponseError InternalError (T.pack e) Nothing
        A.Success a -> pure a

waitForTypecheck :: TextDocumentIdentifier -> Session (Either ResponseError Bool)
waitForTypecheck tid = fmap ideResultSuccess <$> waitForAction "typecheck" tid
