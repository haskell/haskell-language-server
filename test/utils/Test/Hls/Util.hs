{-# LANGUAGE CPP, OverloadedStrings, NamedFieldPuns, MultiParamTypeClasses #-}
module Test.Hls.Util
  (
      codeActionSupportCaps
    , dummyLspFuncs
    , expectCodeAction
    , expectDiagnostic
    , expectNoMoreDiagnostics
    , failIfSessionTimeout
    , flushStackEnvironment
    , fromAction
    , fromCommand
    , getHspecFormattedConfig
    , ghcVersion, GhcVersion(..)
    , hlsCommand
    , hlsCommandExamplePlugin
    , hlsCommandVomit
    , ignoreForGhcVersions
    , inspectCodeAction
    , inspectCommand
    , inspectDiagnostic
    , knownBrokenForGhcVersions
    , logFilePath
    , setupBuildToolFiles
    , waitForDiagnosticsFrom
    , waitForDiagnosticsFromSource
    , waitForDiagnosticsFromSourceWithTimeout
    , withCurrentDirectoryInTmp
  )
where

import           Control.Exception (throwIO, catch)
import           Control.Monad
import           Control.Monad.IO.Class
import           Control.Applicative.Combinators (skipManyTill, (<|>))
import           Control.Lens ((^.))
import           Data.Default
import           Data.List (intercalate)
import           Data.List.Extra (find)
import           Data.Maybe
import qualified Data.Text as T
import           Language.Haskell.LSP.Core
import           Language.Haskell.LSP.Messages (FromServerMessage(NotLogMessage))
import           Language.Haskell.LSP.Types
import qualified Language.Haskell.LSP.Test as Test
import qualified Language.Haskell.LSP.Types.Lens as L
import qualified Language.Haskell.LSP.Types.Capabilities as C
import           System.Directory
import           System.Environment
import           System.Time.Extra (Seconds, sleep)
import           System.FilePath
import           System.IO.Temp
import           System.IO.Unsafe
import           Test.Hspec.Runner
import           Test.Hspec.Core.Formatters hiding (Seconds)
import           Test.Tasty (TestTree)
import           Test.Tasty.ExpectedFailure (ignoreTestBecause, expectFailBecause)
import           Test.Tasty.HUnit (assertFailure)
import           Text.Blaze.Renderer.String (renderMarkup)
import           Text.Blaze.Internal hiding (null)

codeActionSupportCaps :: C.ClientCapabilities
codeActionSupportCaps = def { C._textDocument = Just textDocumentCaps }
  where
    textDocumentCaps = def { C._codeAction = Just codeActionCaps }
    codeActionCaps = C.CodeActionClientCapabilities (Just True) (Just literalSupport)
    literalSupport = C.CodeActionLiteralSupport def

-- ---------------------------------------------------------------------

setupBuildToolFiles :: IO ()
setupBuildToolFiles = do
  forM_ files setupDirectFilesIn

setupDirectFilesIn :: FilePath -> IO ()
setupDirectFilesIn f =
  writeFile (f ++ "hie.yaml") hieYamlCradleDirectContents


-- ---------------------------------------------------------------------

files :: [FilePath]
files =
  [  "./test/testdata/"
   -- , "./test/testdata/addPackageTest/cabal-exe/"
   -- , "./test/testdata/addPackageTest/hpack-exe/"
   -- , "./test/testdata/addPackageTest/cabal-lib/"
   -- , "./test/testdata/addPackageTest/hpack-lib/"
   -- , "./test/testdata/addPragmas/"
   -- , "./test/testdata/badProjects/cabal/"
   -- , "./test/testdata/completion/"
   -- , "./test/testdata/definition/"
   -- , "./test/testdata/gototest/"
   -- , "./test/testdata/redundantImportTest/"
   -- , "./test/testdata/wErrorTest/"
  ]

data GhcVersion
  = GHC810
  | GHC88
  | GHC86
  | GHC84
  deriving (Eq,Show)

ghcVersion :: GhcVersion
#if (defined(MIN_VERSION_GLASGOW_HASKELL) && (MIN_VERSION_GLASGOW_HASKELL(8,10,0,0)))
ghcVersion = GHC810
#elif (defined(MIN_VERSION_GLASGOW_HASKELL) && (MIN_VERSION_GLASGOW_HASKELL(8,8,0,0)))
ghcVersion = GHC88
#elif (defined(MIN_VERSION_GLASGOW_HASKELL) && (MIN_VERSION_GLASGOW_HASKELL(8,6,0,0)))
ghcVersion = GHC86
#elif (defined(MIN_VERSION_GLASGOW_HASKELL) && (MIN_VERSION_GLASGOW_HASKELL(8,4,0,0)))
ghcVersion = GHC84
#endif

knownBrokenForGhcVersions :: [GhcVersion] -> String -> TestTree -> TestTree
knownBrokenForGhcVersions vers reason
    | ghcVersion `elem` vers =  expectFailBecause reason
    | otherwise = id

ignoreForGhcVersions :: [GhcVersion] -> String -> TestTree -> TestTree
ignoreForGhcVersions vers reason
    | ghcVersion `elem` vers =  ignoreTestBecause reason
    | otherwise = id

logFilePath :: String
logFilePath = "hls-" ++ show ghcVersion ++ ".log"

-- | The command to execute the version of hls for the current compiler.
--
-- Both @stack test@ and @cabal new-test@ setup the environment so @hls@ is
-- on PATH. Cabal seems to respond to @build-tool-depends@ specifically while
-- stack just puts all project executables on PATH.
hlsCommand :: String
hlsCommand = unsafePerformIO $ do
  testExe <- fromMaybe "haskell-language-server" <$> lookupEnv "HLS_TEST_EXE"
  pure $ testExe ++ " --lsp -d -j2 -l test-logs/" ++ logFilePath

hlsCommandVomit :: String
hlsCommandVomit = hlsCommand ++ " --vomit"

hlsCommandExamplePlugin :: String
hlsCommandExamplePlugin = hlsCommand ++ " --example"

-- ---------------------------------------------------------------------

hieYamlCradleDirectContents :: String
hieYamlCradleDirectContents = unlines
  [ "# WARNING: THIS FILE IS AUTOGENERATED IN test/utils/TestUtils.hs. IT WILL BE OVERWRITTEN ON EVERY TEST RUN"
  , "cradle:"
  , "  direct:"
  , "    arguments:"
  , "      - -i."
  ]


-- ---------------------------------------------------------------------

getHspecFormattedConfig :: String -> IO Config
getHspecFormattedConfig name = do
  -- https://circleci.com/docs/2.0/env-vars/#built-in-environment-variables
  isCI <- isJust <$> lookupEnv "CI"

  -- Only use the xml formatter on CI since it hides console output
  if isCI
    then do
      let subdir = "test-results" </> name
      createDirectoryIfMissing True subdir

      return $ defaultConfig { configFormatter = Just xmlFormatter
                             , configOutputFile = Right $ subdir </> "results.xml"
                             }
    else return defaultConfig

-- | A Hspec formatter for CircleCI.
-- Originally from https://github.com/LeastAuthority/hspec-jenkins
xmlFormatter :: Formatter
xmlFormatter = silent {
    headerFormatter = do
      writeLine "<?xml version='1.0' encoding='UTF-8'?>"
      writeLine "<testsuite>"
  , exampleSucceeded
  , exampleFailed
  , examplePending
  , footerFormatter = writeLine "</testsuite>"
  }
  where

#if MIN_VERSION_hspec(2,5,0)
    exampleSucceeded path _ =
#else
    exampleSucceeded path =
#endif
      writeLine $ renderMarkup $ testcase path ""

#if MIN_VERSION_hspec(2,5,0)
    exampleFailed path _ err =
#else
    exampleFailed path (Left err) =
      writeLine $ renderMarkup $ testcase path $
        failure ! message (show err) $ ""
    exampleFailed path (Right err) =
#endif
      writeLine $ renderMarkup $ testcase path $
        failure ! message (reasonAsString err) $ ""

#if MIN_VERSION_hspec(2,5,0)
    examplePending path _ reason =
#else
    examplePending path reason =
#endif
      writeLine $ renderMarkup $ testcase path $
        case reason of
          Just desc -> skipped ! message desc  $ ""
          Nothing -> skipped ""

    failure, skipped :: Markup -> Markup
    failure = customParent "failure"
    skipped = customParent "skipped"

    name, className, message :: String -> Attribute
    name = customAttribute "name" . stringValue
    className = customAttribute "classname" . stringValue
    message = customAttribute "message" . stringValue

    testcase :: Path -> Markup -> Markup
    testcase (xs,x) = customParent "testcase" ! name x ! className (intercalate "." xs)

    reasonAsString :: FailureReason -> String
    reasonAsString NoReason = "no reason given"
    reasonAsString (Reason x) = x
    reasonAsString (ExpectedButGot Nothing expected got) = "Expected " ++ expected ++ " but got " ++ got
    reasonAsString (ExpectedButGot (Just src) expected got) = src ++ " expected " ++ expected ++ " but got " ++ got
#if MIN_VERSION_hspec(2,5,0)
    reasonAsString (Error Nothing err ) = show err
    reasonAsString (Error (Just s) err) = s ++ show err
#endif

-- ---------------------------------------------------------------------

flushStackEnvironment :: IO ()
flushStackEnvironment = do
  -- We need to clear these environment variables to prevent
  -- collisions with stack usages
  -- See https://github.com/commercialhaskell/stack/issues/4875
  unsetEnv "GHC_PACKAGE_PATH"
  unsetEnv "GHC_ENVIRONMENT"
  unsetEnv "HASKELL_PACKAGE_SANDBOX"
  unsetEnv "HASKELL_PACKAGE_SANDBOXES"

-- ---------------------------------------------------------------------

dummyLspFuncs :: Default a => LspFuncs a
dummyLspFuncs = LspFuncs { clientCapabilities = def
                         , config = return (Just def)
                         , sendFunc = const (return ())
                         , getVirtualFileFunc = const (return Nothing)
                         , persistVirtualFileFunc = \uri -> return (uriToFilePath (fromNormalizedUri uri))
                         , reverseFileMapFunc = return id
                         , publishDiagnosticsFunc = mempty
                         , flushDiagnosticsBySourceFunc = mempty
                         , getNextReqId = pure (IdInt 0)
                         , rootPath = Nothing
                         , getWorkspaceFolders = return Nothing
                         , withProgress = \_ _ f -> f (const (return ()))
                         , withIndefiniteProgress = \_ _ f -> f
                         }

-- | Like 'withCurrentDirectory', but will copy the directory over to the system
-- temporary directory first to avoid haskell-language-server's source tree from
-- interfering with the cradle
withCurrentDirectoryInTmp :: FilePath -> IO a -> IO a
withCurrentDirectoryInTmp dir f =
  withTempCopy dir $ \newDir ->
    withCurrentDirectory newDir f

withTempCopy :: FilePath -> (FilePath -> IO a) -> IO a
withTempCopy srcDir f = do
  withSystemTempDirectory "hls-test" $ \newDir -> do
    copyDir srcDir newDir
    f newDir

copyDir :: FilePath -> FilePath -> IO ()
copyDir src dst = do
  cnts <- listDirectory src
  forM_ cnts $ \file -> do
    unless (file `elem` ignored) $ do
      let srcFp = src </> file
          dstFp = dst </> file
      isDir <- doesDirectoryExist srcFp
      if isDir
        then createDirectory dstFp >> copyDir srcFp dstFp
        else copyFile srcFp dstFp
  where ignored = ["dist", "dist-newstyle", ".stack-work"]

fromAction :: CAResult -> CodeAction
fromAction (CACodeAction action) = action
fromAction _ = error "Not a code action"

fromCommand :: CAResult -> Command
fromCommand (CACommand command) = command
fromCommand _ = error "Not a command"

onMatch :: [a] -> (a -> Bool) -> String -> IO a
onMatch as predicate err = maybe (fail err) return (find predicate as)

inspectDiagnostic :: [Diagnostic] -> [T.Text] -> IO Diagnostic
inspectDiagnostic diags s = onMatch diags (\ca -> all (`T.isInfixOf` (ca ^. L.message)) s) err
    where err = "expected diagnostic matching '" ++ show s ++ "' but did not find one"

expectDiagnostic :: [Diagnostic] -> [T.Text] -> IO ()
expectDiagnostic diags s = void $ inspectDiagnostic diags s

inspectCodeAction :: [CAResult] -> [T.Text] -> IO CodeAction
inspectCodeAction cars s = fromAction <$> onMatch cars predicate err
    where predicate (CACodeAction ca) = all (`T.isInfixOf` (ca ^. L.title)) s
          predicate _ = False
          err = "expected code action matching '" ++ show s ++ "' but did not find one"

expectCodeAction :: [CAResult] -> [T.Text] -> IO ()
expectCodeAction cars s = void $ inspectCodeAction cars s

inspectCommand :: [CAResult] -> [T.Text] -> IO Command
inspectCommand cars s = fromCommand <$> onMatch cars predicate err
    where predicate (CACommand command) = all  (`T.isInfixOf` (command ^. L.title)) s
          predicate _ = False
          err = "expected code action matching '" ++ show s ++ "' but did not find one"

waitForDiagnosticsFrom :: TextDocumentIdentifier -> Test.Session [Diagnostic]
waitForDiagnosticsFrom doc = do
    diagsNot <- skipManyTill Test.anyMessage Test.message :: Test.Session PublishDiagnosticsNotification
    let (List diags) = diagsNot ^. L.params . L.diagnostics
    if doc ^. L.uri /= diagsNot ^. L.params . L.uri
       then waitForDiagnosticsFrom doc
       else return diags

waitForDiagnosticsFromSource :: TextDocumentIdentifier -> String -> Test.Session [Diagnostic]
waitForDiagnosticsFromSource doc src = do
    diagsNot <- skipManyTill Test.anyMessage Test.message :: Test.Session PublishDiagnosticsNotification
    let (List diags) = diagsNot ^. L.params . L.diagnostics
    let res = filter matches diags
    if doc ^. L.uri /= diagsNot ^. L.params . L.uri || null res
       then waitForDiagnosticsFromSource doc src
       else return res
  where
    matches :: Diagnostic -> Bool
    matches d = d ^. L.source == Just (T.pack src)

-- | wait for @timeout@ seconds and report an assertion failure
-- if any diagnostic messages arrive in that period
expectNoMoreDiagnostics :: Seconds -> TextDocumentIdentifier -> String -> Test.Session ()
expectNoMoreDiagnostics timeout doc src = do
    diags <- waitForDiagnosticsFromSourceWithTimeout timeout doc src
    unless (null diags) $
        liftIO $ assertFailure $
            "Got unexpected diagnostics for " <> show (doc ^. L.uri) <>
            " got " <> show diags

-- | wait for @timeout@ seconds and return diagnostics for the given @document and @source.
-- If timeout is 0 it will wait until the session timeout
waitForDiagnosticsFromSourceWithTimeout :: Seconds -> TextDocumentIdentifier -> String -> Test.Session [Diagnostic]
waitForDiagnosticsFromSourceWithTimeout timeout document source = do
    when (timeout > 0) $ do
        -- Give any further diagnostic messages time to arrive.
        liftIO $ sleep timeout
        -- Send a dummy message to provoke a response from the server.
        -- This guarantees that we have at least one message to
        -- process, so message won't block or timeout.
        void $ Test.sendRequest (CustomClientMethod "non-existent-method") ()
    handleMessages
  where
    matches :: Diagnostic -> Bool
    matches d = d ^. L.source == Just (T.pack source)

    handleMessages = handleDiagnostic <|> handleCustomMethodResponse <|> ignoreOthers
    handleDiagnostic = do
        diagsNot <- Test.message :: Test.Session PublishDiagnosticsNotification
        let fileUri = diagsNot ^. L.params . L.uri
            (List diags) = diagsNot ^. L.params . L.diagnostics
            res = filter matches diags
        if fileUri == document ^. L.uri && not (null res)
            then return diags else handleMessages
    handleCustomMethodResponse =
        -- the CustomClientMethod triggers a RspCustomServer
        -- handle that and then exit
        void (Test.satisfyMaybe responseForNonExistentMethod) >> return []

    responseForNonExistentMethod notif
        | NotLogMessage logMsg <- notif,
          "non-existent-method" `T.isInfixOf` (logMsg ^. L.params . L.message)  = Just notif
        | otherwise = Nothing

    ignoreOthers = void Test.anyMessage >> handleMessages

failIfSessionTimeout :: IO a -> IO a
failIfSessionTimeout action = action `catch` errorHandler
    where errorHandler :: Test.SessionException -> IO a
          errorHandler e@(Test.Timeout _) = assertFailure $ show e
          errorHandler e = throwIO e
