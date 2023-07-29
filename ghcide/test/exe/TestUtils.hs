{-# LANGUAGE AllowAmbiguousTypes   #-}
{-# LANGUAGE CPP                   #-}
{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE ImplicitParams        #-}
{-# LANGUAGE MultiWayIf            #-}
{-# LANGUAGE OverloadedLabels      #-}
{-# LANGUAGE PatternSynonyms       #-}
{-# LANGUAGE PolyKinds             #-}
{-# LANGUAGE TypeOperators         #-}

module TestUtils where

import           Control.Applicative.Combinators
import           Control.Concurrent
import           Control.Exception                    (bracket_, catch, finally)
import qualified Control.Lens                         as Lens
import qualified Control.Lens.Extras                  as Lens
import           Control.Monad
import           Control.Monad.IO.Class               (MonadIO, liftIO)
import           Data.Aeson                           (toJSON)
import qualified Data.Aeson                           as A
import           Data.Default
import           Data.Foldable
import           Data.List.Extra
import           Data.Maybe
import           Data.Proxy
import           Data.Row
import qualified Data.Set                             as Set
import qualified Data.Text                            as T
import           Data.Text.Utf16.Rope                 (Rope)
import qualified Data.Text.Utf16.Rope                 as Rope
import           Development.IDE.Core.PositionMapping (PositionResult (..),
                                                       fromCurrent,
                                                       positionResultToMaybe,
                                                       toCurrent)
import           Development.IDE.GHC.Compat           (GhcVersion (..),
                                                       ghcVersion)
import           Development.IDE.GHC.Util
import qualified Development.IDE.Main                 as IDE
import           Development.IDE.Plugin.TypeLenses    (typeLensCommandId)
import           Development.IDE.Spans.Common
import           Development.IDE.Test                 (Cursor, canonicalizeUri,
                                                       configureCheckProject,
                                                       diagnostic,
                                                       expectCurrentDiagnostics,
                                                       expectDiagnostics,
                                                       expectDiagnosticsWithTags,
                                                       expectNoMoreDiagnostics,
                                                       flushMessages,
                                                       getInterfaceFilesDir,
                                                       getStoredKeys,
                                                       isReferenceReady,
                                                       referenceReady,
                                                       standardizeQuotes,
                                                       waitForAction, waitForGC,
                                                       waitForTypecheck)
import           Development.IDE.Test.Runfiles
import qualified Development.IDE.Types.Diagnostics    as Diagnostics
import           Development.IDE.Types.Location
import           Development.Shake                    (getDirectoryFilesIO)
import           Ide.Plugin.Config
import qualified Language.LSP.Protocol.Lens           as L
import           Language.LSP.Protocol.Message
import           Language.LSP.Protocol.Types          hiding
                                                      (SemanticTokenAbsolute (..),
                                                       SemanticTokenRelative (..),
                                                       SemanticTokensEdit (..),
                                                       mkRange)
import           Language.LSP.Test
import           Language.LSP.VFS                     (VfsLog, applyChange)
import           Network.URI
import           System.Directory
import           System.Environment.Blank             (getEnv, setEnv, unsetEnv)
import           System.Exit                          (ExitCode (ExitSuccess))
import           System.FilePath
import           System.Info.Extra                    (isMac, isWindows)
import qualified System.IO.Extra
import           System.IO.Extra                      hiding (withTempDir)
import           System.Mem                           (performGC)
import           System.Process.Extra                 (CreateProcess (cwd),
                                                       createPipe, proc,
                                                       readCreateProcessWithExitCode)
import           Test.QuickCheck
-- import Test.QuickCheck.Instances ()
import           Control.Concurrent.Async
import           Control.Lens                         (to, (.~), (^.))
import           Control.Monad.Extra                  (whenJust)
import           Data.Function                        ((&))
import           Data.Functor.Identity                (runIdentity)
import           Data.IORef
import           Data.IORef.Extra                     (atomicModifyIORef_)
import           Data.String                          (IsString (fromString))
import           Data.Tuple.Extra
import           Development.IDE.Core.FileStore       (getModTime)
import qualified Development.IDE.Plugin.HLS.GhcIde    as Ghcide
import           Development.IDE.Plugin.Test          (TestRequest (BlockSeconds),
                                                       WaitForIdeRuleResult (..),
                                                       blockCommandId)
import qualified FuzzySearch
import           GHC.Stack                            (emptyCallStack)
import           GHC.TypeLits                         (symbolVal)
import qualified HieDbRetry
import           Ide.Logger                           (Logger (Logger),
                                                       LoggingColumn (DataColumn, PriorityColumn),
                                                       Pretty (pretty),
                                                       Priority (Debug),
                                                       Recorder (Recorder, logger_),
                                                       WithPriority (WithPriority, priority),
                                                       cfilter, cmapWithPrio,
                                                       makeDefaultStderrRecorder,
                                                       toCologActionWithPrio)
import           Ide.PluginUtils                      (pluginDescToIdePlugins)
import           Ide.Types
import qualified Progress
import           System.Time.Extra
import qualified Test.QuickCheck.Monadic              as MonadicQuickCheck
import           Test.QuickCheck.Monadic              (forAllM, monadicIO)
import           Test.Tasty
import           Test.Tasty.ExpectedFailure
import           Test.Tasty.HUnit
import           Test.Tasty.Ingredients.Rerun
import           Test.Tasty.QuickCheck
import           Text.Printf                          (printf)
import           Text.Regex.TDFA                      ((=~))

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

ignoreInWindowsForGHC810 :: TestTree -> TestTree
ignoreInWindowsForGHC810 =
    ignoreFor (BrokenSpecific Windows [GHC810]) "tests are unreliable in windows for ghc 8.10"

ignoreForGHC92Plus :: String -> TestTree -> TestTree
ignoreForGHC92Plus = ignoreFor (BrokenForGHC [GHC92, GHC94, GHC96])

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
        go False = \_ -> id

data Expect
  = ExpectRange Range -- Both gotoDef and hover should report this range
  | ExpectLocation Location
--  | ExpectDefRange Range -- Only gotoDef should report this range
  | ExpectHoverRange Range -- Only hover should report this range
  | ExpectHoverText [T.Text] -- the hover message must contain these snippets
  | ExpectHoverExcludeText [T.Text] -- the hover message must _not_ contain these snippets
  | ExpectHoverTextRegex T.Text -- the hover message must match this pattern
  | ExpectExternFail -- definition lookup in other file expected to fail
  | ExpectNoDefinitions
  | ExpectNoHover
--  | ExpectExtern -- TODO: as above, but expected to succeed: need some more info in here, once we have some working examples
  deriving Eq

mkR :: UInt -> UInt -> UInt -> UInt -> Expect
mkR startLine startColumn endLine endColumn = ExpectRange $ mkRange startLine startColumn endLine endColumn

mkL :: Uri -> UInt -> UInt -> UInt -> UInt -> Expect
mkL uri startLine startColumn endLine endColumn = ExpectLocation $ Location uri $ mkRange startLine startColumn endLine endColumn



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
  testDataFiles <- getDirectoryFilesIO ("test/data" </> prefix) ["//*"]
  for_ testDataFiles $ \f -> do
    createDirectoryIfMissing True $ dir </> takeDirectory f
    copyFile ("test/data" </> prefix </> f) (dir </> f)

withLongTimeout :: IO a -> IO a
withLongTimeout = bracket_ (setEnv "LSP_TIMEOUT" "120" True) (unsetEnv "LSP_TIMEOUT")



lspTestCapsNoFileWatches :: ClientCapabilities
lspTestCapsNoFileWatches = lspTestCaps & L.workspace . Lens._Just . L.didChangeWatchedFiles .~ Nothing

openTestDataDoc :: FilePath -> Session TextDocumentIdentifier
openTestDataDoc path = do
  source <- liftIO $ readFileUtf8 $ "test/data" </> path
  createDoc path "haskell" source

pattern R :: UInt -> UInt -> UInt -> UInt -> Range
pattern R x y x' y' = Range (Position x y) (Position x' y')

checkDefs :: Definition |? ([DefinitionLink] |? Null) -> Session [Expect] -> Session ()
checkDefs (defToLocation -> defs) mkExpectations = traverse_ check =<< mkExpectations where
  check (ExpectRange expectedRange) = do
    assertNDefinitionsFound 1 defs
    assertRangeCorrect (head defs) expectedRange
  check (ExpectLocation expectedLocation) = do
    assertNDefinitionsFound 1 defs
    liftIO $ do
      canonActualLoc <- canonicalizeLocation (head defs)
      canonExpectedLoc <- canonicalizeLocation expectedLocation
      canonActualLoc @?= canonExpectedLoc
  check ExpectNoDefinitions = do
    assertNDefinitionsFound 0 defs
  check ExpectExternFail = liftIO $ assertFailure "Expecting to fail to find in external file"
  check _ = pure () -- all other expectations not relevant to getDefinition

  assertNDefinitionsFound :: Int -> [a] -> Session ()
  assertNDefinitionsFound n defs = liftIO $ assertEqual "number of definitions" n (length defs)

  assertRangeCorrect Location{_range = foundRange} expectedRange =
    liftIO $ expectedRange @=? foundRange

canonicalizeLocation :: Location -> IO Location
canonicalizeLocation (Location uri range) = Location <$> canonicalizeUri uri <*> pure range

defToLocation :: Definition |? ([DefinitionLink] |? Null) -> [Location]
defToLocation (InL (Definition (InL l))) = [l]
defToLocation (InL (Definition (InR ls))) = ls
defToLocation (InR (InL defLink)) = (\(DefinitionLink LocationLink{_targetUri,_targetRange}) -> Location _targetUri _targetRange) <$> defLink
defToLocation (InR (InR Null)) = []

-- | Ghc 9 doesn't include the $-sign in TH warnings like earlier versions did
thDollarIdx :: UInt
thDollarIdx | ghcVersion >= GHC90 = 1
            | otherwise = 0
