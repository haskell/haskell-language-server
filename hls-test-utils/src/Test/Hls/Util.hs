{-# LANGUAGE CPP                   #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns        #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE TypeOperators         #-}
module Test.Hls.Util
  (
      codeActionSupportCaps
    , expectCodeAction
    , expectDiagnostic
    , expectNoMoreDiagnostics
    , expectSameLocations
    , failIfSessionTimeout
    , flushStackEnvironment
    , fromAction
    , fromCommand
    , getHspecFormattedConfig
    , ghcVersion, GhcVersion(..)
    , hostOS, OS(..)
    , matchesCurrentEnv, EnvSpec(..)
    , ignoreForGhcVersions
    , ignoreInEnv
    , inspectCodeAction
    , inspectCommand
    , inspectDiagnostic
    , knownBrokenOnWindows
    , knownBrokenForGhcVersions
    , knownBrokenInEnv
    , setupBuildToolFiles
    , SymbolLocation
    , waitForDiagnosticsFrom
    , waitForDiagnosticsFromSource
    , waitForDiagnosticsFromSourceWithTimeout
    , withCurrentDirectoryInTmp
    , withCurrentDirectoryInTmp'
  )
where

import           Control.Applicative.Combinators (skipManyTill, (<|>))
import           Control.Exception               (catch, throwIO)
import           Control.Lens                    ((^.))
import           Control.Monad
import           Control.Monad.IO.Class
import qualified Data.Aeson                      as A
import           Data.Default
import           Data.List                       (intercalate)
import           Data.List.Extra                 (find)
import           Data.Maybe
import qualified Data.Set                        as Set
import qualified Data.Text                       as T
import           Development.IDE                 (GhcVersion(..), ghcVersion)
import qualified Language.LSP.Test               as Test
import           Language.LSP.Types              hiding (Reason (..))
import qualified Language.LSP.Types.Capabilities as C
import qualified Language.LSP.Types.Lens         as L
import           System.Directory
import           System.Environment
import           System.FilePath
import           System.IO.Temp
import           System.Info.Extra               (isMac, isWindows)
import           System.Time.Extra               (Seconds, sleep)
import           Test.Hspec.Core.Formatters      hiding (Seconds)
import           Test.Hspec.Runner
import           Test.Tasty                      (TestTree)
import           Test.Tasty.ExpectedFailure      (expectFailBecause,
                                                  ignoreTestBecause)
import           Test.Tasty.HUnit                (Assertion, assertFailure,
                                                  (@?=))
import           Text.Blaze.Internal             hiding (null)
import           Text.Blaze.Renderer.String      (renderMarkup)

codeActionSupportCaps :: C.ClientCapabilities
codeActionSupportCaps = def { C._textDocument = Just textDocumentCaps }
  where
    textDocumentCaps = def { C._codeAction = Just codeActionCaps }
    codeActionCaps = CodeActionClientCapabilities (Just True) (Just literalSupport) (Just True) Nothing Nothing Nothing Nothing
    literalSupport = CodeActionLiteralSupport def

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

data EnvSpec = HostOS OS | GhcVer GhcVersion
    deriving (Show, Eq)

matchesCurrentEnv :: EnvSpec -> Bool
matchesCurrentEnv (HostOS os)  = hostOS == os
matchesCurrentEnv (GhcVer ver) = ghcVersion == ver

data OS = Windows | MacOS | Linux
    deriving (Show, Eq)

hostOS :: OS
hostOS
    | isWindows = Windows
    | isMac = MacOS
    | otherwise = Linux

-- | Mark as broken if /any/ of environmental spec mathces the current environment.
knownBrokenInEnv :: [EnvSpec] -> String -> TestTree -> TestTree
knownBrokenInEnv envSpecs reason
    | any matchesCurrentEnv envSpecs = expectFailBecause reason
    | otherwise = id

knownBrokenOnWindows :: String -> TestTree -> TestTree
knownBrokenOnWindows = knownBrokenInEnv [HostOS Windows]

knownBrokenForGhcVersions :: [GhcVersion] -> String -> TestTree -> TestTree
knownBrokenForGhcVersions vers = knownBrokenInEnv (map GhcVer vers)

-- | IgnoreTest if /any/ of environmental spec mathces the current environment.
ignoreInEnv :: [EnvSpec] -> String -> TestTree -> TestTree
ignoreInEnv envSpecs reason
    | any matchesCurrentEnv envSpecs = ignoreTestBecause reason
    | otherwise = id

ignoreForGhcVersions :: [GhcVersion] -> String -> TestTree -> TestTree
ignoreForGhcVersions vers = ignoreInEnv (map GhcVer vers)

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
          Nothing   -> skipped ""

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

-- | Like 'withCurrentDirectory', but will copy the directory over to the system
-- temporary directory first to avoid haskell-language-server's source tree from
-- interfering with the cradle.
--
-- Ignores directories containing build artefacts to avoid interference and
-- provide reproducible test-behaviour.
withCurrentDirectoryInTmp :: FilePath -> IO a -> IO a
withCurrentDirectoryInTmp dir f =
  withTempCopy ignored dir $ \newDir ->
    withCurrentDirectory newDir f
  where
    ignored = ["dist", "dist-newstyle", ".stack-work"]


-- | Like 'withCurrentDirectory', but will copy the directory over to the system
-- temporary directory first to avoid haskell-language-server's source tree from
-- interfering with the cradle.
--
-- You may specify directories to ignore, but should be careful to maintain reproducibility.
withCurrentDirectoryInTmp' :: [FilePath] -> FilePath -> IO a -> IO a
withCurrentDirectoryInTmp' ignored dir f =
  withTempCopy ignored dir $ \newDir ->
    withCurrentDirectory newDir f

-- | Example call: @withTempCopy ignored src f@
--
-- Copy directory 'src' to into a temporary directory ignoring any directories
-- (and files) that are listed in 'ignored'. Pass the temporary directory
-- containing the copied sources to the continuation.
withTempCopy :: [FilePath] -> FilePath -> (FilePath -> IO a) -> IO a
withTempCopy ignored srcDir f = do
  withSystemTempDirectory "hls-test" $ \newDir -> do
    copyDir ignored srcDir newDir
    f newDir

-- | Example call: @copyDir ignored src dst@
--
-- Copy directory 'src' to 'dst' ignoring any directories (and files)
-- that are listed in 'ignored'.
copyDir :: [FilePath] -> FilePath -> FilePath -> IO ()
copyDir ignored src dst = do
  cnts <- listDirectory src
  forM_ cnts $ \file -> do
    unless (file `elem` ignored) $ do
      let srcFp = src </> file
          dstFp = dst </> file
      isDir <- doesDirectoryExist srcFp
      if isDir
        then createDirectory dstFp >> copyDir ignored srcFp dstFp
        else copyFile srcFp dstFp

fromAction :: (Command |? CodeAction) -> CodeAction
fromAction (InR action) = action
fromAction _            = error "Not a code action"

fromCommand :: (Command |? CodeAction) -> Command
fromCommand (InL command) = command
fromCommand _             = error "Not a command"

onMatch :: [a] -> (a -> Bool) -> String -> IO a
onMatch as predicate err = maybe (fail err) return (find predicate as)

inspectDiagnostic :: [Diagnostic] -> [T.Text] -> IO Diagnostic
inspectDiagnostic diags s = onMatch diags (\ca -> all (`T.isInfixOf` (ca ^. L.message)) s) err
    where err = "expected diagnostic matching '" ++ show s ++ "' but did not find one"

expectDiagnostic :: [Diagnostic] -> [T.Text] -> IO ()
expectDiagnostic diags s = void $ inspectDiagnostic diags s

inspectCodeAction :: [Command |? CodeAction] -> [T.Text] -> IO CodeAction
inspectCodeAction cars s = fromAction <$> onMatch cars predicate err
    where predicate (InR ca) = all (`T.isInfixOf` (ca ^. L.title)) s
          predicate _        = False
          err = "expected code action matching '" ++ show s ++ "' but did not find one"

expectCodeAction :: [Command |? CodeAction] -> [T.Text] -> IO ()
expectCodeAction cars s = void $ inspectCodeAction cars s

inspectCommand :: [Command |? CodeAction] -> [T.Text] -> IO Command
inspectCommand cars s = fromCommand <$> onMatch cars predicate err
    where predicate (InL command) = all  (`T.isInfixOf` (command ^. L.title)) s
          predicate _             = False
          err = "expected code action matching '" ++ show s ++ "' but did not find one"

waitForDiagnosticsFrom :: TextDocumentIdentifier -> Test.Session [Diagnostic]
waitForDiagnosticsFrom doc = do
    diagsNot <- skipManyTill Test.anyMessage (Test.message STextDocumentPublishDiagnostics)
    let (List diags) = diagsNot ^. L.params . L.diagnostics
    if doc ^. L.uri /= diagsNot ^. L.params . L.uri
       then waitForDiagnosticsFrom doc
       else return diags

waitForDiagnosticsFromSource :: TextDocumentIdentifier -> String -> Test.Session [Diagnostic]
waitForDiagnosticsFromSource doc src = do
    diagsNot <- skipManyTill Test.anyMessage (Test.message STextDocumentPublishDiagnostics)
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
        void $ Test.sendNotification (SCustomMethod "non-existent-method") A.Null
    handleMessages
  where
    matches :: Diagnostic -> Bool
    matches d = d ^. L.source == Just (T.pack source)

    handleMessages = handleDiagnostic <|> handleCustomMethodResponse <|> ignoreOthers
    handleDiagnostic = do
        diagsNot <- Test.message STextDocumentPublishDiagnostics
        let fileUri = diagsNot ^. L.params . L.uri
            (List diags) = diagsNot ^. L.params . L.diagnostics
            res = filter matches diags
        if fileUri == document ^. L.uri && not (null res)
            then return diags else handleMessages
    handleCustomMethodResponse =
        -- the CustomClientMethod triggers a RspCustomServer
        -- handle that and then exit
        void (Test.satisfyMaybe responseForNonExistentMethod) >> return []

    responseForNonExistentMethod :: FromServerMessage -> Maybe FromServerMessage
    responseForNonExistentMethod notif
        | FromServerMess SWindowLogMessage logMsg <- notif,
          "non-existent-method" `T.isInfixOf` (logMsg ^. L.params . L.message)  = Just notif
        | otherwise = Nothing

    ignoreOthers = void Test.anyMessage >> handleMessages

failIfSessionTimeout :: IO a -> IO a
failIfSessionTimeout action = action `catch` errorHandler
    where errorHandler :: Test.SessionException -> IO a
          errorHandler e@(Test.Timeout _) = assertFailure $ show e
          errorHandler e                  = throwIO e

-- | To locate a symbol, we provide a path to the file from the HLS root
-- directory, the line number, and the column number. (0 indexed.)
type SymbolLocation = (FilePath, Int, Int)

expectSameLocations :: [Location] -> [SymbolLocation] -> Assertion
actual `expectSameLocations` expected = do
    let actual' =
            Set.map (\location -> (location ^. L.uri
                                   , location ^. L.range . L.start . L.line
                                   , location ^. L.range . L.start . L.character))
            $ Set.fromList actual
    expected' <- Set.fromList <$>
        (forM expected $ \(file, l, c) -> do
                              fp <- canonicalizePath file
                              return (filePathToUri fp, l, c))
    actual' @?= expected'
