{-# LANGUAGE CPP                   #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE TypeOperators         #-}
module Test.Hls.Util
  (  -- * Test Capabilities
      codeActionSupportCaps
    , expectCodeAction
    -- * Environment specifications
    -- for ignoring tests
    , ghcVersion, GhcVersion(..)
    , hostOS, OS(..)
    , matchesCurrentEnv, EnvSpec(..)
    , ignoreForGhcVersions
    , ignoreInEnv
    , onlyRunForGhcVersions
    , knownBrokenOnWindows
    , knownBrokenForGhcVersions
    , knownBrokenInEnv
    , onlyWorkForGhcVersions
    -- * Extract code actions
    , fromAction
    , fromCommand
    -- * Session Assertion Helpers
    , dontExpectCodeAction
    , expectDiagnostic
    , expectNoMoreDiagnostics
    , expectSameLocations
    , failIfSessionTimeout
    , getCompletionByLabel
    , noLiteralCaps
    , inspectCodeAction
    , inspectCommand
    , inspectDiagnostic
    , SymbolLocation
    , waitForDiagnosticsFrom
    , waitForDiagnosticsFromSource
    , waitForDiagnosticsFromSourceWithTimeout
    -- * Temporary directories
    , withCurrentDirectoryInTmp
    , withCurrentDirectoryInTmp'
    , withCanonicalTempDir
  )
where

import           Control.Applicative.Combinators (skipManyTill, (<|>))
import           Control.Exception               (catch, throwIO)
import           Control.Lens                    ((&), (?~), (^.))
import           Control.Monad
import           Control.Monad.IO.Class
import qualified Data.Aeson                      as A
import           Data.Bool                       (bool)
import           Data.Default
import           Data.List.Extra                 (find)
import qualified Data.Set                        as Set
import qualified Data.Text                       as T
import           Development.IDE                 (GhcVersion (..), ghcVersion)
import qualified Language.LSP.Test               as Test
import           Language.LSP.Types              hiding (Reason (..))
import qualified Language.LSP.Types.Capabilities as C
import           Language.LSP.Types.Lens         (textDocument)
import qualified Language.LSP.Types.Lens         as L
import           System.Directory
import           System.FilePath
import           System.Info.Extra               (isMac, isWindows)
import qualified System.IO.Extra
import           System.IO.Temp
import           System.Time.Extra               (Seconds, sleep)
import           Test.Tasty                      (TestTree)
import           Test.Tasty.ExpectedFailure      (expectFailBecause,
                                                  ignoreTestBecause)
import           Test.Tasty.HUnit                (Assertion, assertFailure,
                                                  (@?=))

noLiteralCaps :: C.ClientCapabilities
noLiteralCaps = def & textDocument ?~ textDocumentCaps
  where
    textDocumentCaps = def { C._codeAction = Just codeActionCaps }
    codeActionCaps = CodeActionClientCapabilities (Just True) Nothing Nothing Nothing Nothing Nothing Nothing

codeActionSupportCaps :: C.ClientCapabilities
codeActionSupportCaps = def & textDocument ?~ textDocumentCaps
  where
    textDocumentCaps = def { C._codeAction = Just codeActionCaps }
    codeActionCaps = CodeActionClientCapabilities (Just True) (Just literalSupport) (Just True) Nothing Nothing Nothing Nothing
    literalSupport = CodeActionLiteralSupport def

-- ---------------------------------------------------------------------
-- Environment specification for ignoring tests
-- ---------------------------------------------------------------------

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

-- | Mark as broken if GHC does not match only work versions.
onlyWorkForGhcVersions :: (GhcVersion -> Bool) -> String -> TestTree -> TestTree
onlyWorkForGhcVersions p reason =
    if p ghcVersion
        then id
        else expectFailBecause reason

-- | Ignore the test if GHC does not match only work versions.
onlyRunForGhcVersions :: [GhcVersion] -> String -> TestTree -> TestTree
onlyRunForGhcVersions vers =
    if ghcVersion `elem` vers
    then const id
    else ignoreTestBecause

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

noMatch :: [a] -> (a -> Bool) -> String -> IO ()
noMatch [] _ _           = pure ()
noMatch as predicate err = bool (pure ()) (fail err) (any predicate as)

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

dontExpectCodeAction :: [Command |? CodeAction] -> [T.Text] -> IO ()
dontExpectCodeAction cars s =
  noMatch cars predicate err
    where predicate (InR ca) = all (`T.isInfixOf` (ca ^. L.title)) s
          predicate _        = False
          err = "didn't expected code action matching '" ++ show s ++ "' but found one anyway"


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
waitForDiagnosticsFromSource = waitForDiagnosticsFromSourceWithTimeout 5

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
    when (timeout > 0) $
        -- Give any further diagnostic messages time to arrive.
        liftIO $ sleep timeout
        -- Send a dummy message to provoke a response from the server.
        -- This guarantees that we have at least one message to
        -- process, so message won't block or timeout.
    testId <- Test.sendRequest (SCustomMethod "test") A.Null
    handleMessages testId
  where
    matches :: Diagnostic -> Bool
    matches d = d ^. L.source == Just (T.pack source)

    handleMessages testId = handleDiagnostic testId <|> handleCustomMethodResponse testId <|> ignoreOthers testId
    handleDiagnostic testId = do
        diagsNot <- Test.message STextDocumentPublishDiagnostics
        let fileUri = diagsNot ^. L.params . L.uri
            (List diags) = diagsNot ^. L.params . L.diagnostics
            res = filter matches diags
        if fileUri == document ^. L.uri && not (null res)
            then return res else handleMessages testId
    handleCustomMethodResponse testId = do
        _ <- Test.responseForId (SCustomMethod "test") testId
        pure []

    ignoreOthers testId = void Test.anyMessage >> handleMessages testId

failIfSessionTimeout :: IO a -> IO a
failIfSessionTimeout action = action `catch` errorHandler
    where errorHandler :: Test.SessionException -> IO a
          errorHandler e@(Test.Timeout _) = assertFailure $ show e
          errorHandler e                  = throwIO e

-- | To locate a symbol, we provide a path to the file from the HLS root
-- directory, the line number, and the column number. (0 indexed.)
type SymbolLocation = (FilePath, UInt, UInt)

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

-- ---------------------------------------------------------------------
getCompletionByLabel :: MonadIO m => T.Text -> [CompletionItem] -> m CompletionItem
getCompletionByLabel desiredLabel compls =
    case find (\c -> c ^. L.label == desiredLabel) compls of
        Just c -> pure c
        Nothing -> liftIO . assertFailure $
            "Completion with label " <> show desiredLabel
            <> " not found in " <> show (fmap (^. L.label) compls)

-- ---------------------------------------------------------------------
-- Run with a canonicalized temp dir
withCanonicalTempDir :: (FilePath -> IO a) -> IO a
withCanonicalTempDir f = System.IO.Extra.withTempDir $ \dir -> do
  dir' <- canonicalizePath dir
  f dir'
