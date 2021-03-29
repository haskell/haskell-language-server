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
    def,
    runSessionWithServer,
    runSessionWithServerFormatter,
    runSessionWithServer',
    PluginDescriptor,
    IdeState,
  )
where

import           Control.Applicative.Combinators
import           Control.Concurrent.Async          (async, wait)
import           Control.Exception.Base
import           Control.Monad.IO.Class
import           Data.ByteString.Lazy              (ByteString)
import           Data.Default                      (def)
import qualified Data.Text                         as T
import           Development.IDE                   (IdeState, hDuplicateTo')
import           Development.IDE.Main
import qualified Development.IDE.Main              as Ghcide
import qualified Development.IDE.Plugin.HLS.GhcIde as Ghcide
import           Development.IDE.Types.Options
import           Development.Shake                 (ShakeOptions (shakeThreads))
import           GHC.IO.Handle
import           Ide.Plugin.Config                 (Config, formattingProvider)
import           Ide.PluginUtils                   (pluginDescToIdePlugins)
import           Ide.Types
import           Language.LSP.Test
import           Language.LSP.Types
import           Language.LSP.Types.Capabilities   (ClientCapabilities)
import           System.Directory                  (getCurrentDirectory,
                                                    setCurrentDirectory)
import           System.IO.Extra
import           System.Process.Extra              (createPipe)
import           System.Time.Extra
import           Test.Hls.Util
import           Test.Tasty                        hiding (Timeout)
import           Test.Tasty.ExpectedFailure
import           Test.Tasty.Golden
import           Test.Tasty.HUnit
import           Test.Tasty.Ingredients.Rerun

-- | Run 'defaultMainWithRerun', and silence stderr
defaultTestRunner :: TestTree -> IO ()
defaultTestRunner = silenceStderr . defaultMainWithRerun

gitDiff :: FilePath -> FilePath -> [String]
gitDiff fRef fNew = ["git", "diff", "--no-index", "--text", "--exit-code", fRef, fNew]

goldenGitDiff :: TestName -> FilePath -> IO ByteString -> TestTree
goldenGitDiff name = goldenVsStringDiff name gitDiff

runSessionWithServer :: PluginDescriptor IdeState -> FilePath -> Session a -> IO a
runSessionWithServer plugin = runSessionWithServer' [plugin] def def fullCaps

runSessionWithServerFormatter :: PluginDescriptor IdeState -> String -> FilePath -> Session a -> IO a
runSessionWithServerFormatter plugin formatter =
  runSessionWithServer'
    [plugin]
    def {formattingProvider = T.pack formatter}
    def
    fullCaps

-- | Run an action, with stderr silenced
silenceStderr :: IO () -> IO ()
silenceStderr action = withTempFile $ \temp ->
  bracket (openFile temp ReadWriteMode) hClose $ \h -> do
    old <- hDuplicate stderr
    buf <- hGetBuffering stderr
    h `hDuplicateTo'` stderr
    bracket_
      action
      (hClose old)
      (old `hDuplicateTo'` stderr >> hSetBuffering stderr buf)

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
runSessionWithServer' plugin conf sconf caps root s = do
  (inR, inW) <- createPipe
  (outR, outW) <- createPipe
  -- restore cwd after running the session; otherwise the path to test data will be invalid
  cwd <- getCurrentDirectory
  server <-
    async $
      Ghcide.defaultMain
        def
          { argsHandleIn = pure inR,
            argsHandleOut = pure outW,
            argsDefaultHlsConfig = conf,
            argsIdeOptions = \config sessionLoader ->
              let ideOptions = (argsIdeOptions def config sessionLoader) {optTesting = IdeTesting True}
               in ideOptions {optShakeOptions = (optShakeOptions ideOptions) {shakeThreads = 2}},
            argsHlsPlugins = pluginDescToIdePlugins $ plugin ++ Ghcide.descriptors
          }

  x <-
    runSessionWithHandles inW outR sconf caps root s
      `finally` setCurrentDirectory cwd
  wait server
  sleep 0.5
  pure x
