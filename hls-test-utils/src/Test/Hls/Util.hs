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
    , dontExpectCodeAction
    , expectDiagnostic
    , expectNoMoreDiagnostics
    , expectSameLocations
    , failIfSessionTimeout
    , flushStackEnvironment
    , fromAction
    , fromCommand
    , getCompletionByLabel
    , ghcVersion, GhcVersion(..)
    , hostOS, OS(..)
    , matchesCurrentEnv, EnvSpec(..)
    , noLiteralCaps
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
import           Data.Bool                       (bool)
import           Data.Default
import           Data.List.Extra                 (find)
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
import           Test.Tasty                      (TestTree)
import           Test.Tasty.ExpectedFailure      (expectFailBecause,
                                                  ignoreTestBecause)
import           Test.Tasty.HUnit                (Assertion, assertFailure,
                                                  (@?=))

noLiteralCaps :: C.ClientCapabilities
noLiteralCaps = def { C._textDocument = Just textDocumentCaps }
  where
    textDocumentCaps = def { C._codeAction = Just codeActionCaps }
    codeActionCaps = CodeActionClientCapabilities (Just True) Nothing Nothing Nothing Nothing Nothing Nothing

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

noMatch :: [a] -> (a -> Bool) -> String -> IO ()
noMatch [] _ _ = pure ()
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
