
{-# LANGUAGE GADTs           #-}
{-# LANGUAGE PatternSynonyms #-}

module TestUtils where

import           Control.Applicative.Combinators
import           Control.Concurrent.Async
import           Control.Exception               (bracket_, finally)
import           Control.Lens                    ((.~))
import qualified Control.Lens                    as Lens
import qualified Control.Lens.Extras             as Lens
import           Control.Monad
import           Control.Monad.IO.Class          (liftIO)
import           Data.Foldable
import           Data.Function                   ((&))
import           Data.Maybe
import qualified Data.Text                       as T
import           Development.IDE.GHC.Compat      (GhcVersion (..), ghcVersion)
import           Development.IDE.GHC.Util
import qualified Development.IDE.Main            as IDE
import           Development.IDE.Test            (canonicalizeUri,
                                                  configureCheckProject,
                                                  expectNoMoreDiagnostics)
import           Development.IDE.Test.Runfiles
import           Development.IDE.Types.Location
import           Development.Shake               (getDirectoryFilesIO)
import           Ide.Logger                      (Recorder, WithPriority,
                                                  cmapWithPrio)
import qualified Language.LSP.Protocol.Lens      as L
import           Language.LSP.Protocol.Message
import           Language.LSP.Protocol.Types     hiding
                                                 (SemanticTokenAbsolute (..),
                                                  SemanticTokenRelative (..),
                                                  SemanticTokensEdit (..),
                                                  mkRange)
import           Language.LSP.Test
import           System.Directory
import           System.Environment.Blank        (getEnv, setEnv, unsetEnv)
import           System.FilePath
import           System.Info.Extra               (isMac, isWindows)
import qualified System.IO.Extra
import           System.Process.Extra            (createPipe)
import           Test.Tasty
import           Test.Tasty.ExpectedFailure
import           Test.Tasty.HUnit

import           LogType

-- | Wait for the next progress begin step
waitForProgressBegin :: Session ()
waitForProgressBegin = skipManyTill anyMessage $ satisfyMaybe $ \case
  FromServerMess  SMethod_Progress  (TNotificationMessage _ _ (ProgressParams _ v)) | Lens.is _workDoneProgressBegin v-> Just ()
  _ -> Nothing

-- | Wait for the first progress end step
-- Also implemented in hls-test-utils Test.Hls
waitForProgressDone :: Session ()
waitForProgressDone = skipManyTill anyMessage $ satisfyMaybe $ \case
  FromServerMess  SMethod_Progress  (TNotificationMessage _ _ (ProgressParams _ v)) | Lens.is _workDoneProgressEnd v -> Just ()
  _ -> Nothing

-- | Wait for all progress to be done
-- Needs at least one progress done notification to return
-- Also implemented in hls-test-utils Test.Hls
waitForAllProgressDone :: Session ()
waitForAllProgressDone = loop
  where
    loop = do
      ~() <- skipManyTill anyMessage $ satisfyMaybe $ \case
        FromServerMess  SMethod_Progress  (TNotificationMessage _ _ (ProgressParams _ v)) |Lens.is _workDoneProgressEnd v-> Just ()
        _ -> Nothing
      done <- null <$> getIncompleteProgressSessions
      unless done loop

run :: Session a -> IO a
run s = run' (const s)

run' :: (FilePath -> Session a) -> IO a
run' s = withTempDir $ \dir -> runInDir dir (s dir)

runInDir :: FilePath -> Session a -> IO a
runInDir dir = runInDir' dir "." "." []

-- | Takes a directory as well as relative paths to where we should launch the executable as well as the session root.
runInDir' :: FilePath -> FilePath -> FilePath -> [String] -> Session a -> IO a
runInDir' = runInDir'' lspTestCaps

runInDir''
    :: ClientCapabilities
    -> FilePath
    -> FilePath
    -> FilePath
    -> [String]
    -> Session b
    -> IO b
runInDir'' lspCaps dir startExeIn startSessionIn extraOptions s = do

  ghcideExe <- locateGhcideExecutable
  let startDir = dir </> startExeIn
  let projDir = dir </> startSessionIn

  createDirectoryIfMissing True startDir
  createDirectoryIfMissing True projDir
  -- Temporarily hack around https://github.com/mpickering/hie-bios/pull/56
  -- since the package import test creates "Data/List.hs", which otherwise has no physical home
  createDirectoryIfMissing True $ projDir ++ "/Data"

  shakeProfiling <- getEnv "SHAKE_PROFILING"
  let cmd = unwords $
       [ghcideExe, "--lsp", "--test", "--verify-core-file", "--verbose", "-j2", "--cwd", startDir
       ] ++ ["--shake-profiling=" <> dir | Just dir <- [shakeProfiling]
       ] ++ extraOptions
  -- HIE calls getXgdDirectory which assumes that HOME is set.
  -- Only sets HOME if it wasn't already set.
  setEnv "HOME" "/homeless-shelter" False
  conf <- getConfigFromEnv
  runSessionWithConfig conf cmd lspCaps projDir $ do
      configureCheckProject False
      s

-- | Version of 'System.IO.Extra.withTempDir' that canonicalizes the path
-- Which we need to do on macOS since the $TMPDIR can be in @/private/var@ or
-- @/var@
withTempDir :: (FilePath -> IO a) -> IO a
withTempDir f = System.IO.Extra.withTempDir $ \dir -> do
  dir' <- canonicalizePath dir
  f dir'

lspTestCaps :: ClientCapabilities
lspTestCaps = fullCaps { _window = Just $ WindowClientCapabilities (Just True) Nothing Nothing }

getConfigFromEnv :: IO SessionConfig
getConfigFromEnv = do
  logColor <- fromMaybe True <$> checkEnv "LSP_TEST_LOG_COLOR"
  timeoutOverride <- fmap read <$> getEnv "LSP_TIMEOUT"
  return defaultConfig
    { messageTimeout = fromMaybe (messageTimeout defaultConfig) timeoutOverride
    , logColor
    }
  where
    checkEnv :: String -> IO (Maybe Bool)
    checkEnv s = fmap convertVal <$> getEnv s
    convertVal "0" = False
    convertVal _   = True

testSessionWait :: HasCallStack => String -> Session () -> TestTree
testSessionWait name = testSession name .
      -- Check that any diagnostics produced were already consumed by the test case.
      --
      -- If in future we add test cases where we don't care about checking the diagnostics,
      -- this could move elsewhere.
      --
      -- Experimentally, 0.5s seems to be long enough to wait for any final diagnostics to appear.
      ( >> expectNoMoreDiagnostics 0.5)

testSession :: String -> Session () -> TestTree
testSession name = testCase name . run

xfail :: TestTree -> String -> TestTree
xfail = flip expectFailBecause

ignoreInWindowsBecause :: String -> TestTree -> TestTree
ignoreInWindowsBecause = ignoreFor (BrokenForOS Windows)

knownBrokenForGhcVersions :: [GhcVersion] -> String -> TestTree -> TestTree
knownBrokenForGhcVersions ghcVers = knownBrokenFor (BrokenForGHC ghcVers)

data BrokenOS = Linux | MacOS | Windows deriving (Show)

data IssueSolution = Broken | Ignore deriving (Show)

data BrokenTarget =
    BrokenSpecific BrokenOS [GhcVersion]
    -- ^Broken for `BrokenOS` with `GhcVersion`
    | BrokenForOS BrokenOS
    -- ^Broken for `BrokenOS`
    | BrokenForGHC [GhcVersion]
    -- ^Broken for `GhcVersion`
    deriving (Show)

-- | Ignore test for specific os and ghc with reason.
ignoreFor :: BrokenTarget -> String -> TestTree -> TestTree
ignoreFor = knownIssueFor Ignore

-- | Known broken for specific os and ghc with reason.
knownBrokenFor :: BrokenTarget -> String -> TestTree -> TestTree
knownBrokenFor = knownIssueFor Broken

-- | Deal with `IssueSolution` for specific OS and GHC.
knownIssueFor :: IssueSolution -> BrokenTarget -> String -> TestTree -> TestTree
knownIssueFor solution = go . \case
    BrokenSpecific bos vers -> isTargetOS bos && isTargetGhc vers
    BrokenForOS bos         -> isTargetOS bos
    BrokenForGHC vers       -> isTargetGhc vers
    where
        isTargetOS = \case
            Windows -> isWindows
            MacOS   -> isMac
            Linux   -> not isWindows && not isMac

        isTargetGhc = elem ghcVersion

        go True = case solution of
            Broken -> expectFailBecause
            Ignore -> ignoreTestBecause
        go False = const id



testSessionWithExtraFiles :: FilePath -> String -> (FilePath -> Session ()) -> TestTree
testSessionWithExtraFiles prefix name = testCase name . runWithExtraFiles prefix

testSession' :: String -> (FilePath -> Session ()) -> TestTree
testSession' name = testCase name . run'



mkRange :: UInt -> UInt -> UInt -> UInt -> Range
mkRange a b c d = Range (Position a b) (Position c d)


runWithExtraFiles :: FilePath -> (FilePath -> Session a) -> IO a
runWithExtraFiles prefix s = withTempDir $ \dir -> do
  copyTestDataFiles dir prefix
  runInDir dir (s dir)

copyTestDataFiles :: FilePath -> FilePath -> IO ()
copyTestDataFiles dir prefix = do
  -- Copy all the test data files to the temporary workspace
  testDataFiles <- getDirectoryFilesIO ("ghcide/test/data" </> prefix) ["//*"]
  for_ testDataFiles $ \f -> do
    createDirectoryIfMissing True $ dir </> takeDirectory f
    copyFile ("ghcide/test/data" </> prefix </> f) (dir </> f)

withLongTimeout :: IO a -> IO a
withLongTimeout = bracket_ (setEnv "LSP_TIMEOUT" "120" True) (unsetEnv "LSP_TIMEOUT")



lspTestCapsNoFileWatches :: ClientCapabilities
lspTestCapsNoFileWatches = lspTestCaps & L.workspace . Lens._Just . L.didChangeWatchedFiles .~ Nothing

testIde :: Recorder (WithPriority Log) -> IDE.Arguments -> Session () -> IO ()
testIde recorder arguments session = do
    config <- getConfigFromEnv
    cwd <- getCurrentDirectory
    (hInRead, hInWrite) <- createPipe
    (hOutRead, hOutWrite) <- createPipe

    let server = IDE.defaultMain (cmapWithPrio LogIDEMain recorder) arguments
            { IDE.argsHandleIn = pure hInRead
            , IDE.argsHandleOut = pure hOutWrite
            }

    withTempDir $ \dir -> do
        flip finally (setCurrentDirectory cwd) $ withAsync server $ \_ ->
            runSessionWithHandles hInWrite hOutRead config lspTestCaps dir session
