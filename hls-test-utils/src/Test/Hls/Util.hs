{-# LANGUAGE CPP                   #-}
{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE OverloadedStrings     #-}
module Test.Hls.Util
  (  -- * Test Capabilities
      codeActionResolveCaps
    , codeActionNoResolveCaps
    , codeActionSupportCaps
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
    , knownBrokenInSpecificEnv
    , onlyWorkForGhcVersions
    -- * Extract code actions
    , fromAction
    , fromCommand
    -- * Session Assertion Helpers
    , dontExpectCodeAction
    , expectDiagnostic
    , expectNoMoreDiagnostics
    , failIfSessionTimeout
    , getCompletionByLabel
    , noLiteralCaps
    , inspectCodeAction
    , inspectCommand
    , inspectDiagnostic
    , waitForDiagnosticsFrom
    , waitForDiagnosticsFromSource
    , waitForDiagnosticsFromSourceWithTimeout
    -- * Temporary directories
    , withCurrentDirectoryInTmp
    , withCurrentDirectoryInTmp'
    , withCanonicalTempDir
    -- * Extract positions from input file.
    , extractCursorPositions
    , mkParameterisedLabel
    , trimming
  )
where

import           Control.Applicative.Combinators          (skipManyTill, (<|>))
import           Control.Exception                        (catch, throwIO)
import           Control.Lens                             (_Just, (&), (.~),
                                                           (?~), (^.))
import           Control.Monad
import           Control.Monad.IO.Class
import qualified Data.Aeson                               as A
import           Data.Bool                                (bool)
import           Data.Default
import           Data.List.Extra                          (find)
import           Data.Proxy
import qualified Data.Text                                as T
import           Development.IDE                          (GhcVersion (..),
                                                           ghcVersion)
import qualified Language.LSP.Protocol.Lens               as L
import           Language.LSP.Protocol.Message
import           Language.LSP.Protocol.Types
import qualified Language.LSP.Test                        as Test
import           System.Directory
import           System.FilePath
import           System.Info.Extra                        (isMac, isWindows)
import qualified System.IO.Extra
import           System.IO.Temp
import           System.Time.Extra                        (Seconds, sleep)
import           Test.Tasty                               (TestTree)
import           Test.Tasty.ExpectedFailure               (expectFailBecause,
                                                           ignoreTestBecause)
import           Test.Tasty.HUnit                         (assertFailure)

import qualified Data.List                                as List
import qualified Data.Text.Internal.Search                as T
import qualified Data.Text.Utf16.Rope.Mixed               as Rope
import           Development.IDE.Plugin.Completions.Logic (getCompletionPrefixFromRope)
import           Development.IDE.Plugin.Completions.Types (PosPrefixInfo (..))
import           NeatInterpolation                        (trimming)

noLiteralCaps :: ClientCapabilities
noLiteralCaps = def & L.textDocument ?~ textDocumentCaps
  where
    textDocumentCaps = def { _codeAction = Just codeActionCaps }
    codeActionCaps = CodeActionClientCapabilities (Just True) Nothing Nothing Nothing Nothing Nothing Nothing

codeActionSupportCaps :: ClientCapabilities
codeActionSupportCaps = def & L.textDocument ?~ textDocumentCaps
  where
    textDocumentCaps = def { _codeAction = Just codeActionCaps }
    codeActionCaps = CodeActionClientCapabilities (Just True) (Just literalSupport) (Just True) Nothing Nothing Nothing Nothing
    literalSupport = ClientCodeActionLiteralOptions (ClientCodeActionKindOptions [])

codeActionResolveCaps :: ClientCapabilities
codeActionResolveCaps = Test.fullLatestClientCaps
                          & (L.textDocument . _Just . L.codeAction . _Just . L.resolveSupport . _Just) .~ ClientCodeActionResolveOptions {_properties= ["edit"]}
                          & (L.textDocument . _Just . L.codeAction . _Just . L.dataSupport . _Just) .~ True

codeActionNoResolveCaps :: ClientCapabilities
codeActionNoResolveCaps = Test.fullLatestClientCaps
                          & (L.textDocument . _Just . L.codeAction . _Just . L.resolveSupport) .~ Nothing
                          & (L.textDocument . _Just . L.codeAction . _Just . L.dataSupport . _Just) .~ False
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

-- | Mark as broken if /any/ of the environmental specs matches the current environment.
knownBrokenInEnv :: [EnvSpec] -> String -> TestTree -> TestTree
knownBrokenInEnv envSpecs reason
    | any matchesCurrentEnv envSpecs = expectFailBecause reason
    | otherwise = id

-- | Mark as broken if /all/ environmental specs match the current environment.
knownBrokenInSpecificEnv :: [EnvSpec] -> String -> TestTree -> TestTree
knownBrokenInSpecificEnv envSpecs reason
    | all matchesCurrentEnv envSpecs = expectFailBecause reason
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
    diagsNot <- skipManyTill Test.anyMessage (Test.message SMethod_TextDocumentPublishDiagnostics)
    let diags = diagsNot ^. L.params . L.diagnostics
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
    testId <- Test.sendRequest (SMethod_CustomMethod (Proxy @"test")) A.Null
    handleMessages testId
  where
    matches :: Diagnostic -> Bool
    matches d = d ^. L.source == Just (T.pack source)

    handleMessages testId = handleDiagnostic testId <|> handleMethod_CustomMethodResponse testId <|> ignoreOthers testId
    handleDiagnostic testId = do
        diagsNot <- Test.message SMethod_TextDocumentPublishDiagnostics
        let fileUri = diagsNot ^. L.params . L.uri
            diags = diagsNot ^. L.params . L.diagnostics
            res = filter matches diags
        if fileUri == document ^. L.uri && not (null res)
            then return res else handleMessages testId
    handleMethod_CustomMethodResponse testId = do
        _ <- Test.responseForId (SMethod_CustomMethod (Proxy @"test")) testId
        pure []

    ignoreOthers testId = void Test.anyMessage >> handleMessages testId

failIfSessionTimeout :: IO a -> IO a
failIfSessionTimeout action = action `catch` errorHandler
    where errorHandler :: Test.SessionException -> IO a
          errorHandler e@(Test.Timeout _) = assertFailure $ show e
          errorHandler e                  = throwIO e

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

-- ----------------------------------------------------------------------------
-- Extract Position data from the source file itself.
-- ----------------------------------------------------------------------------

-- | Pretty labelling for tests that use the parameterised test helpers.
mkParameterisedLabel :: PosPrefixInfo -> String
mkParameterisedLabel posPrefixInfo = unlines
    [ "Full Line:       \"" <> T.unpack (fullLine posPrefixInfo) <> "\""
    , "Cursor Column:   \"" <> replicate (fromIntegral $ cursorPos posPrefixInfo ^. L.character) ' ' ++ "^" <> "\""
    , "Prefix Text:     \"" <> T.unpack (prefixText posPrefixInfo) <> "\""
    ]

-- | Given a in-memory representation of a file, where a user can specify the
-- current cursor position using a '^' in the next line.
--
-- This function allows to generate multiple tests for a single input file, without
-- the hassle of calculating by hand where there cursor is supposed to be.
--
-- Example (line number has been added for readability):
--
-- @
--   0: foo = 2
--   1:  ^
--   2: bar =
--   3:      ^
-- @
--
-- This example input file contains two cursor positions (y, x), at
--
-- * (1, 1), and
-- * (3, 5).
--
-- 'extractCursorPositions' will search for '^' characters, and determine there are
-- two cursor positions in the text.
-- First, it will normalise the text to:
--
-- @
--   0: foo = 2
--   1: bar =
-- @
--
-- stripping away the '^' characters. Then, the actual cursor positions are:
--
-- * (0, 1) and
-- * (2, 5).
--
extractCursorPositions :: T.Text -> (T.Text, [PosPrefixInfo])
extractCursorPositions t =
    let
        textLines = T.lines t
        foldState = List.foldl' go emptyFoldState textLines
        finalText = foldStateToText foldState
        reconstructCompletionPrefix pos = getCompletionPrefixFromRope pos (Rope.fromText finalText)
        cursorPositions = reverse . fmap reconstructCompletionPrefix $ foldStatePositions foldState
    in
        (finalText, cursorPositions)

    where
        go foldState l = case T.indices "^" l of
            [] -> addTextLine foldState l
            xs -> List.foldl' addTextCursor foldState xs

-- | 'FoldState' is an implementation detail used to parse some file contents,
-- extracting the cursor positions identified by '^' and producing a cleaned
-- representation of the file contents.
data FoldState = FoldState
    { foldStateRows      :: !Int
    -- ^ The row index of the cleaned file contents.
    --
    -- For example, the file contents
    --
    -- @
    --   0: foo
    --   1: ^
    --   2: bar
    -- @
    -- will report that 'bar' is actually occurring in line '1', as '^' is
    -- a cursor position.
    -- Lines containing cursor positions are removed.
    , foldStatePositions :: ![Position]
    -- ^ List of cursors positions found in the file contents.
    --
    -- List is stored in reverse for efficient 'cons'ing
    , foldStateFinalText :: ![T.Text]
    -- ^ Final file contents with all lines containing cursor positions removed.
    --
    -- List is stored in reverse for efficient 'cons'ing
    }

emptyFoldState :: FoldState
emptyFoldState = FoldState
    { foldStateRows = 0
    , foldStatePositions = []
    , foldStateFinalText = []
    }

-- | Produce the final file contents, without any lines containing cursor positions.
foldStateToText :: FoldState -> T.Text
foldStateToText state = T.unlines $ reverse $ foldStateFinalText state

-- | We found a '^' at some location! Add it to the list of known cursor positions.
--
-- If the row index is '0', we throw an error, as there can't be a cursor position above the first line.
addTextCursor :: FoldState -> Int -> FoldState
addTextCursor state col
    | foldStateRows state <= 0 = error $ "addTextCursor: Invalid '^' found at: " <> show (col, foldStateRows state)
    | otherwise = state
        { foldStatePositions = Position (fromIntegral (foldStateRows state) - 1) (fromIntegral col) : foldStatePositions state
        }

addTextLine :: FoldState -> T.Text -> FoldState
addTextLine state l = state
    { foldStateFinalText = l : foldStateFinalText state
    , foldStateRows = foldStateRows state + 1
    }
