-- Copyright (c) 2019 The DAML Authors. All rights reserved.
-- SPDX-License-Identifier: Apache-2.0

{-
 NOTE On enforcing determinism

   The tests below use two mechanisms to enforce deterministic LSP sequences:

    1. Progress reporting: waitForProgress(Begin|Done)
    2. Diagnostics: expectDiagnostics

    Either is fine, but diagnostics are generally more reliable.

    Mixing them both in the same test is NOT FINE as it will introduce race
    conditions since multiple interleavings are possible. In other words,
    the sequence of diagnostics and progress reports is not deterministic.
    For example:

    < do something >
    waitForProgressDone
    expectDiagnostics [...]

    - When the diagnostics arrive after the progress done message, as they usually do, the test will pass
    - When the diagnostics arrive before the progress done msg, when on a slow machine occasionally, the test will timeout

    Therefore, avoid mixing both progress reports and diagnostics in the same test
 -}

{-# LANGUAGE AllowAmbiguousTypes   #-}
{-# LANGUAGE CPP                   #-}
{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE ImplicitParams        #-}
{-# LANGUAGE MultiWayIf            #-}
{-# LANGUAGE PatternSynonyms       #-}
{-# LANGUAGE PolyKinds             #-}
{-# LANGUAGE TypeOperators         #-}
{-# OPTIONS_GHC -Wno-deprecations -Wno-unticked-promoted-constructors #-}

module Main (main) where

import           Control.Applicative.Combinators
import           Control.Concurrent
import           Control.Exception                        (bracket_, catch,
                                                           finally)
import qualified Control.Lens                             as Lens
import           Control.Monad
import           Control.Monad.IO.Class                   (MonadIO, liftIO)
import           Data.Aeson                               (toJSON)
import qualified Data.Aeson                               as A
import           Data.Default
import           Data.Foldable
import           Data.List.Extra
import           Data.Maybe
import qualified Data.Set                                 as Set
import qualified Data.Text                                as T
import           Data.Text.Utf16.Rope                     (Rope)
import qualified Data.Text.Utf16.Rope                     as Rope
import           Development.IDE.Core.PositionMapping     (PositionResult (..),
                                                           fromCurrent,
                                                           positionResultToMaybe,
                                                           toCurrent)
import           Development.IDE.GHC.Compat               (GhcVersion (..),
                                                           ghcVersion)
import           Development.IDE.GHC.Util
import qualified Development.IDE.Main                     as IDE
import           Development.IDE.Plugin.TypeLenses        (typeLensCommandId)
import           Development.IDE.Spans.Common
import           Development.IDE.Test                     (Cursor,
                                                           canonicalizeUri,
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
                                                           waitForAction,
                                                           waitForGC,
                                                           waitForTypecheck)
import           Development.IDE.Test.Runfiles
import qualified Development.IDE.Types.Diagnostics        as Diagnostics
import           Development.IDE.Types.Location
import           Development.Shake                        (getDirectoryFilesIO)
import           Ide.Plugin.Config
import           Language.LSP.Test
import           Language.LSP.Types                       hiding
                                                          (SemanticTokenAbsolute (length, line),
                                                           SemanticTokenRelative (length),
                                                           SemanticTokensEdit (_start),
                                                           mkRange)
import           Language.LSP.Types.Capabilities
import qualified Language.LSP.Types.Lens                  as Lens (label)
import qualified Language.LSP.Types.Lens                  as Lsp (diagnostics,
                                                                  message,
                                                                  params)
import           Language.LSP.VFS                         (VfsLog, applyChange)
import           Network.URI
import           System.Directory
import           System.Environment.Blank                 (getEnv, setEnv,
                                                           unsetEnv)
import           System.Exit                              (ExitCode (ExitSuccess))
import           System.FilePath
import           System.Info.Extra                        (isMac, isWindows)
import qualified System.IO.Extra
import           System.IO.Extra                          hiding (withTempDir)
import           System.Mem                               (performGC)
import           System.Process.Extra                     (CreateProcess (cwd),
                                                           createPipe, proc,
                                                           readCreateProcessWithExitCode)
import           Test.QuickCheck
-- import Test.QuickCheck.Instances ()
import           Control.Concurrent.Async
import           Control.Lens                             (to, (.~), (^.))
import           Control.Monad.Extra                      (whenJust)
import           Data.Function                            ((&))
import           Data.Functor.Identity                    (runIdentity)
import           Data.IORef
import           Data.IORef.Extra                         (atomicModifyIORef_)
import           Data.String                              (IsString (fromString))
import           Data.Tuple.Extra
import           Development.IDE.Core.FileStore           (getModTime)
import qualified Development.IDE.Plugin.HLS.GhcIde        as Ghcide
import           Development.IDE.Plugin.Test              (TestRequest (BlockSeconds),
                                                           WaitForIdeRuleResult (..),
                                                           blockCommandId)
import           Development.IDE.Types.Logger             (Logger (Logger),
                                                           LoggingColumn (DataColumn, PriorityColumn),
                                                           Pretty (pretty),
                                                           Priority (Debug),
                                                           Recorder (Recorder, logger_),
                                                           WithPriority (WithPriority, priority),
                                                           cfilter,
                                                           cmapWithPrio,
                                                           makeDefaultStderrRecorder,
                                                           toCologActionWithPrio)
import qualified FuzzySearch
import           GHC.Stack                                (emptyCallStack)
import qualified HieDbRetry
import           Ide.PluginUtils                          (pluginDescToIdePlugins)
import           Ide.Types
import qualified Language.LSP.Types                       as LSP
import           Language.LSP.Types.Lens                  (didChangeWatchedFiles,
                                                           workspace)
import qualified Language.LSP.Types.Lens                  as L
import qualified Progress
import           System.Time.Extra
import qualified Test.QuickCheck.Monadic                  as MonadicQuickCheck
import           Test.QuickCheck.Monadic                  (forAllM, monadicIO)
import           Test.Tasty
import           Test.Tasty.ExpectedFailure
import           Test.Tasty.HUnit
import           Test.Tasty.Ingredients.Rerun
import           Test.Tasty.QuickCheck
import           Text.Printf                              (printf)
import           Text.Regex.TDFA                          ((=~))

data Log
  = LogGhcIde Ghcide.Log
  | LogIDEMain IDE.Log
  | LogVfs VfsLog

instance Pretty Log where
  pretty = \case
    LogGhcIde log  -> pretty log
    LogIDEMain log -> pretty log
    LogVfs log     -> pretty log

-- | Wait for the next progress begin step
waitForProgressBegin :: Session ()
waitForProgressBegin = skipManyTill anyMessage $ satisfyMaybe $ \case
  FromServerMess SProgress (NotificationMessage _ _ (ProgressParams _ (Begin _))) -> Just ()
  _ -> Nothing

-- | Wait for the first progress end step
-- Also implemented in hls-test-utils Test.Hls
waitForProgressDone :: Session ()
waitForProgressDone = skipManyTill anyMessage $ satisfyMaybe $ \case
  FromServerMess SProgress (NotificationMessage _ _ (ProgressParams _ (End _))) -> Just ()
  _ -> Nothing

-- | Wait for all progress to be done
-- Needs at least one progress done notification to return
-- Also implemented in hls-test-utils Test.Hls
waitForAllProgressDone :: Session ()
waitForAllProgressDone = loop
  where
    loop = do
      ~() <- skipManyTill anyMessage $ satisfyMaybe $ \case
        FromServerMess SProgress (NotificationMessage _ _ (ProgressParams _ (End _))) -> Just ()
        _ -> Nothing
      done <- null <$> getIncompleteProgressSessions
      unless done loop

main :: IO ()
main = do
  docWithPriorityRecorder <- makeDefaultStderrRecorder (Just [PriorityColumn, DataColumn])

  let docWithFilteredPriorityRecorder@Recorder{ logger_ } =
        docWithPriorityRecorder
        & cfilter (\WithPriority{ priority } -> priority >= Debug)

  -- exists so old-style logging works. intended to be phased out
  let logger = Logger $ \p m -> logger_ (WithPriority p emptyCallStack (pretty m))

  let recorder = docWithFilteredPriorityRecorder
               & cmapWithPrio pretty

  -- We mess with env vars so run single-threaded.
  defaultMainWithRerun $ testGroup "ghcide"
    [ testSession "open close" $ do
        doc <- createDoc "Testing.hs" "haskell" ""
        void (skipManyTill anyMessage $ message SWindowWorkDoneProgressCreate)
        waitForProgressBegin
        closeDoc doc
        waitForProgressDone
    , initializeResponseTests
    , completionTests
    , cppTests
    , diagnosticTests
    , codeLensesTests
    , outlineTests
    , highlightTests
    , findDefinitionAndHoverTests
    , pluginSimpleTests
    , pluginParsedResultTests
    , preprocessorTests
    , thTests
    , symlinkTests
    , safeTests
    , unitTests recorder logger
    , haddockTests
    , positionMappingTests recorder
    , watchedFilesTests
    , cradleTests
    , dependentFileTest
    , nonLspCommandLine
    , ifaceTests
    , bootTests
    , rootUriTests
    , asyncTests
    , clientSettingsTest
    , referenceTests
    , garbageCollectionTests
    , HieDbRetry.tests
    ]

initializeResponseTests :: TestTree
initializeResponseTests = withResource acquire release tests where

  -- these tests document and monitor the evolution of the
  -- capabilities announced by the server in the initialize
  -- response. Currently the server advertises almost no capabilities
  -- at all, in some cases failing to announce capabilities that it
  -- actually does provide! Hopefully this will change ...
  tests :: IO (ResponseMessage Initialize) -> TestTree
  tests getInitializeResponse =
    testGroup "initialize response capabilities"
    [ chk "   text doc sync"             _textDocumentSync  tds
    , chk "   hover"                         _hoverProvider (Just $ InL True)
    , chk "   completion"               _completionProvider (Just $ CompletionOptions Nothing (Just ["."]) Nothing (Just True))
    , chk "NO signature help"        _signatureHelpProvider Nothing
    , chk "   goto definition"          _definitionProvider (Just $ InL True)
    , chk "   goto type definition" _typeDefinitionProvider (Just $ InL True)
    -- BUG in lsp-test, this test fails, just change the accepted response
    -- for now
    , chk "NO goto implementation"  _implementationProvider (Just $ InL False)
    , chk "   find references"          _referencesProvider (Just $ InL True)
    , chk "   doc highlight"     _documentHighlightProvider (Just $ InL True)
    , chk "   doc symbol"           _documentSymbolProvider (Just $ InL True)
    , chk "   workspace symbol"    _workspaceSymbolProvider (Just $ InL True)
    , chk "   code action"             _codeActionProvider  (Just $ InL False)
    , chk "   code lens"                 _codeLensProvider  (Just $ CodeLensOptions (Just False) (Just False))
    , chk "NO doc formatting"   _documentFormattingProvider (Just $ InL False)
    , chk "NO doc range formatting"
                           _documentRangeFormattingProvider (Just $ InL False)
    , chk "NO doc formatting on typing"
                          _documentOnTypeFormattingProvider Nothing
    , chk "NO renaming"                     _renameProvider (Just $ InL False)
    , chk "NO doc link"               _documentLinkProvider Nothing
    , chk "NO color"                   (^. L.colorProvider) (Just $ InL False)
    , chk "NO folding range"          _foldingRangeProvider (Just $ InL False)
    , che "   execute command"      _executeCommandProvider [typeLensCommandId, blockCommandId]
    , chk "   workspace"                   (^. L.workspace) (Just $ WorkspaceServerCapabilities (Just WorkspaceFoldersServerCapabilities{_supported = Just True, _changeNotifications = Just ( InR True )}))
    , chk "NO experimental"             (^. L.experimental) Nothing
    ] where

      tds = Just (InL (TextDocumentSyncOptions
                              { _openClose = Just True
                              , _change    = Just TdSyncIncremental
                              , _willSave  = Nothing
                              , _willSaveWaitUntil = Nothing
                              , _save = Just (InR $ SaveOptions {_includeText = Nothing})}))

      chk :: (Eq a, Show a) => TestName -> (ServerCapabilities -> a) -> a -> TestTree
      chk title getActual expected =
        testCase title $ getInitializeResponse >>= \ir -> expected @=? (getActual . innerCaps) ir

      che :: TestName -> (ServerCapabilities -> Maybe ExecuteCommandOptions) -> [T.Text] -> TestTree
      che title getActual expected = testCase title doTest
        where
            doTest = do
                ir <- getInitializeResponse
                let Just ExecuteCommandOptions {_commands = List commands} = getActual $ innerCaps ir
                    commandNames = (!! 2) . T.splitOn ":" <$> commands
                zipWithM_ (\e o -> T.isSuffixOf e o @? show (e,o)) (sort expected) (sort commandNames)

  innerCaps :: ResponseMessage Initialize -> ServerCapabilities
  innerCaps (ResponseMessage _ _ (Right (InitializeResult c _))) = c
  innerCaps (ResponseMessage _ _ (Left _)) = error "Initialization error"

  acquire :: IO (ResponseMessage Initialize)
  acquire = run initializeResponse

  release :: ResponseMessage Initialize -> IO ()
  release = const $ pure ()


diagnosticTests :: TestTree
diagnosticTests = testGroup "diagnostics"
  [ testSessionWait "fix syntax error" $ do
      let content = T.unlines [ "module Testing wher" ]
      doc <- createDoc "Testing.hs" "haskell" content
      expectDiagnostics [("Testing.hs", [(DsError, (0, 15), "parse error")])]
      let change = TextDocumentContentChangeEvent
            { _range = Just (Range (Position 0 15) (Position 0 19))
            , _rangeLength = Nothing
            , _text = "where"
            }
      changeDoc doc [change]
      expectDiagnostics [("Testing.hs", [])]
  , testSessionWait "introduce syntax error" $ do
      let content = T.unlines [ "module Testing where" ]
      doc <- createDoc "Testing.hs" "haskell" content
      void $ skipManyTill anyMessage (message SWindowWorkDoneProgressCreate)
      waitForProgressBegin
      let change = TextDocumentContentChangeEvent
            { _range = Just (Range (Position 0 15) (Position 0 18))
            , _rangeLength = Nothing
            , _text = "wher"
            }
      changeDoc doc [change]
      expectDiagnostics [("Testing.hs", [(DsError, (0, 15), "parse error")])]
  , testSessionWait "update syntax error" $ do
      let content = T.unlines [ "module Testing(missing) where" ]
      doc <- createDoc "Testing.hs" "haskell" content
      expectDiagnostics [("Testing.hs", [(DsError, (0, 15), "Not in scope: 'missing'")])]
      let change = TextDocumentContentChangeEvent
            { _range = Just (Range (Position 0 15) (Position 0 16))
            , _rangeLength = Nothing
            , _text = "l"
            }
      changeDoc doc [change]
      expectDiagnostics [("Testing.hs", [(DsError, (0, 15), "Not in scope: 'lissing'")])]
  , testSessionWait "variable not in scope" $ do
      let content = T.unlines
            [ "module Testing where"
            , "foo :: Int -> Int -> Int"
            , "foo a _b = a + ab"
            , "bar :: Int -> Int -> Int"
            , "bar _a b = cd + b"
            ]
      _ <- createDoc "Testing.hs" "haskell" content
      expectDiagnostics
        [ ( "Testing.hs"
          , [ (DsError, (2, 15), "Variable not in scope: ab")
            , (DsError, (4, 11), "Variable not in scope: cd")
            ]
          )
        ]
  , testSessionWait "type error" $ do
      let content = T.unlines
            [ "module Testing where"
            , "foo :: Int -> String -> Int"
            , "foo a b = a + b"
            ]
      _ <- createDoc "Testing.hs" "haskell" content
      expectDiagnostics
        [ ( "Testing.hs"
          , [(DsError, (2, 14), "Couldn't match type '[Char]' with 'Int'")]
          )
        ]
  , testSessionWait "typed hole" $ do
      let content = T.unlines
            [ "module Testing where"
            , "foo :: Int -> String"
            , "foo a = _ a"
            ]
      _ <- createDoc "Testing.hs" "haskell" content
      expectDiagnostics
        [ ( "Testing.hs"
          , [(DsError, (2, 8), "Found hole: _ :: Int -> String")]
          )
        ]

  , testGroup "deferral" $
    let sourceA a = T.unlines
          [ "module A where"
          , "a :: Int"
          , "a = " <> a]
        sourceB = T.unlines
          [ "module B where"
          , "import A ()"
          , "b :: Float"
          , "b = True"]
        bMessage = "Couldn't match expected type 'Float' with actual type 'Bool'"
        expectedDs aMessage =
          [ ("A.hs", [(DsError, (2,4), aMessage)])
          , ("B.hs", [(DsError, (3,4), bMessage)])]
        deferralTest title binding msg = testSessionWait title $ do
          _ <- createDoc "A.hs" "haskell" $ sourceA binding
          _ <- createDoc "B.hs" "haskell"   sourceB
          expectDiagnostics $ expectedDs msg
    in
    [ deferralTest "type error"          "True"    "Couldn't match expected type"
    , deferralTest "typed hole"          "_"       "Found hole"
    , deferralTest "out of scope var"    "unbound" "Variable not in scope"
    ]

  , testSessionWait "remove required module" $ do
      let contentA = T.unlines [ "module ModuleA where" ]
      docA <- createDoc "ModuleA.hs" "haskell" contentA
      let contentB = T.unlines
            [ "module ModuleB where"
            , "import ModuleA"
            ]
      _ <- createDoc "ModuleB.hs" "haskell" contentB
      let change = TextDocumentContentChangeEvent
            { _range = Just (Range (Position 0 0) (Position 0 20))
            , _rangeLength = Nothing
            , _text = ""
            }
      changeDoc docA [change]
      expectDiagnostics [("ModuleB.hs", [(DsError, (1, 0), "Could not find module")])]
  , testSessionWait "add missing module" $ do
      let contentB = T.unlines
            [ "module ModuleB where"
            , "import ModuleA ()"
            ]
      _ <- createDoc "ModuleB.hs" "haskell" contentB
      expectDiagnostics [("ModuleB.hs", [(DsError, (1, 7), "Could not find module")])]
      let contentA = T.unlines [ "module ModuleA where" ]
      _ <- createDoc "ModuleA.hs" "haskell" contentA
      expectDiagnostics [("ModuleB.hs", [])]
  , testCase "add missing module (non workspace)" $
    -- By default lsp-test sends FileWatched notifications for all files, which we don't want
    -- as non workspace modules will not be watched by the LSP server.
    -- To work around this, we tell lsp-test that our client doesn't have the
    -- FileWatched capability, which is enough to disable the notifications
    withTempDir $ \tmpDir -> runInDir'' lspTestCapsNoFileWatches tmpDir "." "." [] $ do
      let contentB = T.unlines
            [ "module ModuleB where"
            , "import ModuleA ()"
            ]
      _ <- createDoc (tmpDir </> "ModuleB.hs") "haskell" contentB
      expectDiagnostics [(tmpDir </> "ModuleB.hs", [(DsError, (1, 7), "Could not find module")])]
      let contentA = T.unlines [ "module ModuleA where" ]
      _ <- createDoc (tmpDir </> "ModuleA.hs") "haskell" contentA
      expectDiagnostics [(tmpDir </> "ModuleB.hs", [])]
  , testSessionWait "cyclic module dependency" $ do
      let contentA = T.unlines
            [ "module ModuleA where"
            , "import ModuleB"
            ]
      let contentB = T.unlines
            [ "module ModuleB where"
            , "import ModuleA"
            ]
      _ <- createDoc "ModuleA.hs" "haskell" contentA
      _ <- createDoc "ModuleB.hs" "haskell" contentB
      expectDiagnostics
        [ ( "ModuleA.hs"
          , [(DsError, (1, 7), "Cyclic module dependency between ModuleA, ModuleB")]
          )
        , ( "ModuleB.hs"
          , [(DsError, (1, 7), "Cyclic module dependency between ModuleA, ModuleB")]
          )
        ]
  , testSession' "deeply nested cyclic module dependency" $ \path -> do
      let contentA = unlines
            [ "module ModuleA where" , "import ModuleB" ]
      let contentB = unlines
            [ "module ModuleB where" , "import ModuleA" ]
      let contentC = unlines
            [ "module ModuleC where" , "import ModuleB" ]
      let contentD = T.unlines
            [ "module ModuleD where" , "import ModuleC" ]
          cradle =
            "cradle: {direct: {arguments: [ModuleA, ModuleB, ModuleC, ModuleD]}}"
      liftIO $ writeFile (path </> "ModuleA.hs") contentA
      liftIO $ writeFile (path </> "ModuleB.hs") contentB
      liftIO $ writeFile (path </> "ModuleC.hs") contentC
      liftIO $ writeFile (path </> "hie.yaml") cradle
      _ <- createDoc "ModuleD.hs" "haskell" contentD
      expectDiagnostics
        [ ( "ModuleB.hs"
          , [(DsError, (1, 7), "Cyclic module dependency between ModuleA, ModuleB")]
          )
        ]
  , testSessionWait "cyclic module dependency with hs-boot" $ do
      let contentA = T.unlines
            [ "module ModuleA where"
            , "import {-# SOURCE #-} ModuleB"
            ]
      let contentB = T.unlines
            [ "{-# OPTIONS -Wmissing-signatures#-}"
            , "module ModuleB where"
            , "import ModuleA"
            -- introduce an artificial diagnostic
            , "foo = ()"
            ]
      let contentBboot = T.unlines
            [ "module ModuleB where"
            ]
      _ <- createDoc "ModuleA.hs" "haskell" contentA
      _ <- createDoc "ModuleB.hs" "haskell" contentB
      _ <- createDoc "ModuleB.hs-boot" "haskell" contentBboot
      expectDiagnostics [("ModuleB.hs", [(DsWarning, (3,0), "Top-level binding")])]
  , testSessionWait "correct reference used with hs-boot" $ do
      let contentB = T.unlines
            [ "module ModuleB where"
            , "import {-# SOURCE #-} ModuleA()"
            ]
      let contentA = T.unlines
            [ "module ModuleA where"
            , "import ModuleB()"
            , "x = 5"
            ]
      let contentAboot = T.unlines
            [ "module ModuleA where"
            ]
      let contentC = T.unlines
            [ "{-# OPTIONS -Wmissing-signatures #-}"
            , "module ModuleC where"
            , "import ModuleA"
            -- this reference will fail if it gets incorrectly
            -- resolved to the hs-boot file
            , "y = x"
            ]
      _ <- createDoc "ModuleB.hs" "haskell" contentB
      _ <- createDoc "ModuleA.hs" "haskell" contentA
      _ <- createDoc "ModuleA.hs-boot" "haskell" contentAboot
      _ <- createDoc "ModuleC.hs" "haskell" contentC
      expectDiagnostics [("ModuleC.hs", [(DsWarning, (3,0), "Top-level binding")])]
  , testSessionWait "redundant import" $ do
      let contentA = T.unlines ["module ModuleA where"]
      let contentB = T.unlines
            [ "{-# OPTIONS_GHC -Wunused-imports #-}"
            , "module ModuleB where"
            , "import ModuleA"
            ]
      _ <- createDoc "ModuleA.hs" "haskell" contentA
      _ <- createDoc "ModuleB.hs" "haskell" contentB
      expectDiagnosticsWithTags
        [ ( "ModuleB.hs"
          , [(DsWarning, (2, 0), "The import of 'ModuleA' is redundant", Just DtUnnecessary)]
          )
        ]
  , testSessionWait "redundant import even without warning" $ do
      let contentA = T.unlines ["module ModuleA where"]
      let contentB = T.unlines
            [ "{-# OPTIONS_GHC -Wno-unused-imports -Wmissing-signatures #-}"
            , "module ModuleB where"
            , "import ModuleA"
            -- introduce an artificial warning for testing purposes
            , "foo = ()"
            ]
      _ <- createDoc "ModuleA.hs" "haskell" contentA
      _ <- createDoc "ModuleB.hs" "haskell" contentB
      expectDiagnostics [("ModuleB.hs", [(DsWarning, (3,0), "Top-level binding")])]
  , testSessionWait "package imports" $ do
      let thisDataListContent = T.unlines
            [ "module Data.List where"
            , "x :: Integer"
            , "x = 123"
            ]
      let mainContent = T.unlines
            [ "{-# LANGUAGE PackageImports #-}"
            , "module Main where"
            , "import qualified \"this\" Data.List as ThisList"
            , "import qualified \"base\" Data.List as BaseList"
            , "useThis = ThisList.x"
            , "useBase = BaseList.map"
            , "wrong1 = ThisList.map"
            , "wrong2 = BaseList.x"
            , "main = pure ()"
            ]
      _ <- createDoc "Data/List.hs" "haskell" thisDataListContent
      _ <- createDoc "Main.hs" "haskell" mainContent
      expectDiagnostics
        [ ( "Main.hs"
          , [(DsError, (6, 9),
                if ghcVersion >= GHC96 then
                  "Variable not in scope: ThisList.map"
                else if ghcVersion >= GHC94 then
                  "Variable not in scope: map" -- See https://gitlab.haskell.org/ghc/ghc/-/issues/22130
                else
                  "Not in scope: \8216ThisList.map\8217")
            ,(DsError, (7, 9),
                if ghcVersion >= GHC96 then
                  "Variable not in scope: BaseList.x"
                else if ghcVersion >= GHC94 then
                  "Variable not in scope: x" -- See https://gitlab.haskell.org/ghc/ghc/-/issues/22130
                else
                  "Not in scope: \8216BaseList.x\8217")
            ]
          )
        ]
  , testSessionWait "unqualified warnings" $ do
      let fooContent = T.unlines
            [ "{-# OPTIONS_GHC -Wredundant-constraints #-}"
            , "module Foo where"
            , "foo :: Ord a => a -> Int"
            , "foo _a = 1"
            ]
      _ <- createDoc "Foo.hs" "haskell" fooContent
      expectDiagnostics
        [ ( "Foo.hs"
      -- The test is to make sure that warnings contain unqualified names
      -- where appropriate. The warning should use an unqualified name 'Ord', not
      -- something like 'GHC.Classes.Ord'. The choice of redundant-constraints to
      -- test this is fairly arbitrary.
          , [(DsWarning, (2, if ghcVersion >= GHC94 then 7 else 0), "Redundant constraint: Ord a")
            ]
          )
        ]
    , testSessionWait "lower-case drive" $ do
          let aContent = T.unlines
                [ "module A.A where"
                , "import A.B ()"
                ]
              bContent = T.unlines
                [ "{-# OPTIONS_GHC -Wall #-}"
                , "module A.B where"
                , "import Data.List"
                ]
          uriB <- getDocUri "A/B.hs"
          Just pathB <- pure $ uriToFilePath uriB
          uriB <- pure $
              let (drive, suffix) = splitDrive pathB
              in filePathToUri (joinDrive (lower drive) suffix)
          liftIO $ createDirectoryIfMissing True (takeDirectory pathB)
          liftIO $ writeFileUTF8 pathB $ T.unpack bContent
          uriA <- getDocUri "A/A.hs"
          Just pathA <- pure $ uriToFilePath uriA
          uriA <- pure $
              let (drive, suffix) = splitDrive pathA
              in filePathToUri (joinDrive (lower drive) suffix)
          let itemA = TextDocumentItem uriA "haskell" 0 aContent
          let a = TextDocumentIdentifier uriA
          sendNotification STextDocumentDidOpen (DidOpenTextDocumentParams itemA)
          NotificationMessage{_params = PublishDiagnosticsParams fileUri _ diags} <- skipManyTill anyMessage diagnostic
          -- Check that if we put a lower-case drive in for A.A
          -- the diagnostics for A.B will also be lower-case.
          liftIO $ fileUri @?= uriB
          let msg = head (toList diags) ^. L.message
          liftIO $ unless ("redundant" `T.isInfixOf` msg) $
              assertFailure ("Expected redundant import but got " <> T.unpack msg)
          closeDoc a
  , testSessionWait "haddock parse error" $ do
      let fooContent = T.unlines
            [ "module Foo where"
            , "foo :: Int"
            , "foo = 1 {-|-}"
            ]
      _ <- createDoc "Foo.hs" "haskell" fooContent
      if ghcVersion >= GHC90 then
          -- Haddock parse errors are ignored on ghc-9.0
            pure ()
      else
        expectDiagnostics
            [ ( "Foo.hs"
              , [(DsWarning, (2, 8), "Haddock parse error on input")]
              )
            ]
  , testSessionWait "strip file path" $ do
      let
          name = "Testing"
          content = T.unlines
            [ "module " <> name <> " where"
            , "value :: Maybe ()"
            , "value = [()]"
            ]
      _ <- createDoc (T.unpack name <> ".hs") "haskell" content
      notification <- skipManyTill anyMessage diagnostic
      let
          offenders =
            Lsp.params .
            Lsp.diagnostics .
            Lens.folded .
            Lsp.message .
            Lens.filtered (T.isInfixOf ("/" <> name <> ".hs:"))
          failure msg = liftIO $ assertFailure $ "Expected file path to be stripped but got " <> T.unpack msg
      Lens.mapMOf_ offenders failure notification
  , testSession' "-Werror in cradle is ignored" $ \sessionDir -> do
      liftIO $ writeFile (sessionDir </> "hie.yaml")
        "cradle: {direct: {arguments: [\"-Wall\", \"-Werror\"]}}"
      let fooContent = T.unlines
            [ "module Foo where"
            , "foo = ()"
            ]
      _ <- createDoc "Foo.hs" "haskell" fooContent
      expectDiagnostics
        [ ( "Foo.hs"
          , [(DsWarning, (1, 0), "Top-level binding with no type signature:")
            ]
          )
        ]
  , testSessionWait "-Werror in pragma is ignored" $ do
      let fooContent = T.unlines
            [ "{-# OPTIONS_GHC -Wall -Werror #-}"
            , "module Foo() where"
            , "foo :: Int"
            , "foo = 1"
            ]
      _ <- createDoc "Foo.hs" "haskell" fooContent
      expectDiagnostics
        [ ( "Foo.hs"
          , [(DsWarning, (3, 0), "Defined but not used:")
            ]
          )
        ]
  , testCase "typecheck-all-parents-of-interest" $ runWithExtraFiles "recomp" $ \dir -> do
    let bPath = dir </> "B.hs"
        pPath = dir </> "P.hs"
        aPath = dir </> "A.hs"

    bSource <- liftIO $ readFileUtf8 bPath -- y :: Int
    pSource <- liftIO $ readFileUtf8 pPath -- bar = x :: Int
    aSource <- liftIO $ readFileUtf8 aPath -- x = y :: Int

    bdoc <- createDoc bPath "haskell" bSource
    _pdoc <- createDoc pPath "haskell" pSource
    expectDiagnostics
      [("P.hs", [(DsWarning,(4,0), "Top-level binding")])] -- So that we know P has been loaded

    -- Change y from Int to B which introduces a type error in A (imported from P)
    changeDoc bdoc [TextDocumentContentChangeEvent Nothing Nothing $
                    T.unlines ["module B where", "y :: Bool", "y = undefined"]]
    expectDiagnostics
      [("A.hs", [(DsError, (5, 4), "Couldn't match expected type 'Int' with actual type 'Bool'")])
      ]

    -- Open A and edit to fix the type error
    adoc <- createDoc aPath "haskell" aSource
    changeDoc adoc [TextDocumentContentChangeEvent Nothing Nothing $
                    T.unlines ["module A where", "import B", "x :: Bool", "x = y"]]

    expectDiagnostics
      [ ( "P.hs",
          [ (DsError, (4, 6), "Couldn't match expected type 'Int' with actual type 'Bool'"),
            (DsWarning, (4, 0), "Top-level binding")
          ]
        ),
        ("A.hs", [])
      ]
    expectNoMoreDiagnostics 1

  , testSessionWait "deduplicate missing module diagnostics" $  do
      let fooContent = T.unlines [ "module Foo() where" , "import MissingModule" ]
      doc <- createDoc "Foo.hs" "haskell" fooContent
      expectDiagnostics [("Foo.hs", [(DsError, (1,7), "Could not find module 'MissingModule'")])]

      changeDoc doc [TextDocumentContentChangeEvent Nothing Nothing "module Foo() where" ]
      expectDiagnostics []

      changeDoc doc [TextDocumentContentChangeEvent Nothing Nothing $ T.unlines
            [ "module Foo() where" , "import MissingModule" ] ]
      expectDiagnostics [("Foo.hs", [(DsError, (1,7), "Could not find module 'MissingModule'")])]

  , testGroup "Cancellation"
    [ cancellationTestGroup "edit header" editHeader yesSession noParse  noTc
    , cancellationTestGroup "edit import" editImport noSession  yesParse noTc
    , cancellationTestGroup "edit body"   editBody   yesSession yesParse yesTc
    ]
  ]
  where
      editPair x y = let p = Position x y ; p' = Position x (y+2) in
        (TextDocumentContentChangeEvent {_range=Just (Range p p), _rangeLength=Nothing, _text="fd"}
        ,TextDocumentContentChangeEvent {_range=Just (Range p p'), _rangeLength=Nothing, _text=""})
      editHeader = editPair 0 0
      editImport = editPair 2 10
      editBody   = editPair 3 10

      noParse = False
      yesParse = True

      noSession = False
      yesSession = True

      noTc = False
      yesTc = True

cancellationTestGroup :: TestName -> (TextDocumentContentChangeEvent, TextDocumentContentChangeEvent) -> Bool -> Bool -> Bool -> TestTree
cancellationTestGroup name edits sessionDepsOutcome parseOutcome tcOutcome = testGroup name
    [ cancellationTemplate edits Nothing
    , cancellationTemplate edits $ Just ("GetFileContents", True)
    , cancellationTemplate edits $ Just ("GhcSession", True)
      -- the outcome for GetModSummary is always True because parseModuleHeader never fails (!)
    , cancellationTemplate edits $ Just ("GetModSummary", True)
    , cancellationTemplate edits $ Just ("GetModSummaryWithoutTimestamps", True)
      -- getLocatedImports never fails
    , cancellationTemplate edits $ Just ("GetLocatedImports", True)
    , cancellationTemplate edits $ Just ("GhcSessionDeps", sessionDepsOutcome)
    , cancellationTemplate edits $ Just ("GetParsedModule", parseOutcome)
    , cancellationTemplate edits $ Just ("TypeCheck", tcOutcome)
    , cancellationTemplate edits $ Just ("GetHieAst", tcOutcome)
    ]

cancellationTemplate :: (TextDocumentContentChangeEvent, TextDocumentContentChangeEvent) -> Maybe (String, Bool) -> TestTree
cancellationTemplate (edit, undoEdit) mbKey = testCase (maybe "-" fst mbKey) $ runTestNoKick $ do
      doc <- createDoc "Foo.hs" "haskell" $ T.unlines
            [ "{-# OPTIONS_GHC -Wall #-}"
            , "module Foo where"
            , "import Data.List()"
            , "f0 x = (x,x)"
            ]

      -- for the example above we expect one warning
      let missingSigDiags = [(DsWarning, (3, 0), "Top-level binding") ]
      typeCheck doc >> expectCurrentDiagnostics doc missingSigDiags

      -- Now we edit the document and wait for the given key (if any)
      changeDoc doc [edit]
      whenJust mbKey $ \(key, expectedResult) -> do
        WaitForIdeRuleResult{ideResultSuccess} <- waitForAction key doc
        liftIO $ ideResultSuccess @?= expectedResult

      -- The 2nd edit cancels the active session and unbreaks the file
      -- wait for typecheck and check that the current diagnostics are accurate
      changeDoc doc [undoEdit]
      typeCheck doc >> expectCurrentDiagnostics doc missingSigDiags

      expectNoMoreDiagnostics 0.5
    where
        -- similar to run except it disables kick
        runTestNoKick s = withTempDir $ \dir -> runInDir' dir "." "." ["--test-no-kick"] s

        typeCheck doc = do
            WaitForIdeRuleResult {..} <- waitForAction "TypeCheck" doc
            liftIO $ assertBool "The file should typecheck" ideResultSuccess
            -- wait for the debouncer to publish diagnostics if the rule runs
            liftIO $ sleep 0.2
            -- flush messages to ensure current diagnostics state is updated
            flushMessages

codeLensesTests :: TestTree
codeLensesTests = testGroup "code lenses"
  [ addSigLensesTests
  ]

watchedFilesTests :: TestTree
watchedFilesTests = testGroup "watched files"
  [ testGroup "Subscriptions"
    [ testSession' "workspace files" $ \sessionDir -> do
        liftIO $ writeFile (sessionDir </> "hie.yaml") "cradle: {direct: {arguments: [\"-isrc\", \"A\", \"WatchedFilesMissingModule\"]}}"
        _doc <- createDoc "A.hs" "haskell" "{-#LANGUAGE NoImplicitPrelude #-}\nmodule A where\nimport WatchedFilesMissingModule"
        watchedFileRegs <- getWatchedFilesSubscriptionsUntil STextDocumentPublishDiagnostics

        -- Expect 2 subscriptions: one for all .hs files and one for the hie.yaml cradle
        liftIO $ length watchedFileRegs @?= 2

    , testSession' "non workspace file" $ \sessionDir -> do
        tmpDir <- liftIO getTemporaryDirectory
        let yaml = "cradle: {direct: {arguments: [\"-i" <> tail(init(show tmpDir)) <> "\", \"A\", \"WatchedFilesMissingModule\"]}}"
        liftIO $ writeFile (sessionDir </> "hie.yaml") yaml
        _doc <- createDoc "A.hs" "haskell" "{-# LANGUAGE NoImplicitPrelude#-}\nmodule A where\nimport WatchedFilesMissingModule"
        watchedFileRegs <- getWatchedFilesSubscriptionsUntil STextDocumentPublishDiagnostics

        -- Expect 2 subscriptions: one for all .hs files and one for the hie.yaml cradle
        liftIO $ length watchedFileRegs @?= 2

    -- TODO add a test for didChangeWorkspaceFolder
    ]
  , testGroup "Changes"
    [
      testSession' "workspace files" $ \sessionDir -> do
        liftIO $ writeFile (sessionDir </> "hie.yaml") "cradle: {direct: {arguments: [\"-isrc\", \"A\", \"B\"]}}"
        liftIO $ writeFile (sessionDir </> "B.hs") $ unlines
          ["module B where"
          ,"b :: Bool"
          ,"b = False"]
        _doc <- createDoc "A.hs" "haskell" $ T.unlines
          ["module A where"
          ,"import B"
          ,"a :: ()"
          ,"a = b"
          ]
        expectDiagnostics [("A.hs", [(DsError, (3, 4), "Couldn't match expected type '()' with actual type 'Bool'")])]
        -- modify B off editor
        liftIO $ writeFile (sessionDir </> "B.hs") $ unlines
          ["module B where"
          ,"b :: Int"
          ,"b = 0"]
        sendNotification SWorkspaceDidChangeWatchedFiles $ DidChangeWatchedFilesParams $
                List [FileEvent (filePathToUri $ sessionDir </> "B.hs") FcChanged ]
        expectDiagnostics [("A.hs", [(DsError, (3, 4), "Couldn't match expected type '()' with actual type 'Int'")])]
    ]
  ]

addSigLensesTests :: TestTree
addSigLensesTests =
  let pragmas = "{-# OPTIONS_GHC -Wmissing-signatures -Wmissing-pattern-synonym-signatures #-}"
      moduleH exported =
        T.unlines
          [ "{-# LANGUAGE PatternSynonyms,TypeApplications,DataKinds,RankNTypes,ScopedTypeVariables,TypeOperators,GADTs,BangPatterns #-}"
          , "module Sigs(" <> exported <> ") where"
          , "import qualified Data.Complex as C"
          , "import Data.Data (Proxy (..), type (:~:) (..), mkCharType)"
          , "data T1 a where"
          , "  MkT1 :: (Show b) => a -> b -> T1 a"
          ]
      before enableGHCWarnings exported (def, _) others =
        T.unlines $ [pragmas | enableGHCWarnings] <> [moduleH exported, def] <> others
      after' enableGHCWarnings exported (def, sig) others =
        T.unlines $ [pragmas | enableGHCWarnings] <> [moduleH exported] <> maybe [] pure sig <> [def] <> others
      createConfig mode = A.object ["haskell" A..= A.object ["plugin" A..= A.object ["ghcide-type-lenses" A..= A.object ["config" A..= A.object ["mode" A..= A.String mode]]]]]
      sigSession testName enableGHCWarnings mode exported def others = testSession testName $ do
        let originalCode = before enableGHCWarnings exported def others
        let expectedCode = after' enableGHCWarnings exported def others
        sendNotification SWorkspaceDidChangeConfiguration $ DidChangeConfigurationParams $ createConfig mode
        doc <- createDoc "Sigs.hs" "haskell" originalCode
        waitForProgressDone
        codeLenses <- getCodeLenses doc
        if not $ null $ snd def
          then do
            liftIO $ length codeLenses == 1 @? "Expected 1 code lens, but got: " <> show codeLenses
            executeCommand $ fromJust $ head codeLenses ^. L.command
            modifiedCode <- skipManyTill anyMessage (getDocumentEdit doc)
            liftIO $ expectedCode @=? modifiedCode
          else liftIO $ null codeLenses @? "Expected no code lens, but got: " <> show codeLenses
      cases =
        [ ("abc = True", "abc :: Bool")
        , ("foo a b = a + b", "foo :: Num a => a -> a -> a")
        , ("bar a b = show $ a + b", "bar :: (Show a, Num a) => a -> a -> String")
        , ("(!!!) a b = a > b", "(!!!) :: Ord a => a -> a -> Bool")
        , ("a >>>> b = a + b", "(>>>>) :: Num a => a -> a -> a")
        , ("a `haha` b = a b", "haha :: (t1 -> t2) -> t1 -> t2")
        , ("pattern Some a = Just a", "pattern Some :: a -> Maybe a")
        , ("pattern Some a <- Just a", "pattern Some :: a -> Maybe a")
        , ("pattern Some a <- Just a\n  where Some a = Just a", "pattern Some :: a -> Maybe a")
        , ("pattern Some a <- Just !a\n  where Some !a = Just a", "pattern Some :: a -> Maybe a")
        , ("pattern Point{x, y} = (x, y)", "pattern Point :: a -> b -> (a, b)")
        , ("pattern Point{x, y} <- (x, y)", "pattern Point :: a -> b -> (a, b)")
        , ("pattern Point{x, y} <- (x, y)\n  where Point x y = (x, y)", "pattern Point :: a -> b -> (a, b)")
        , ("pattern MkT1' b = MkT1 42 b", "pattern MkT1' :: (Eq a, Num a) => Show b => b -> T1 a")
        , ("pattern MkT1' b <- MkT1 42 b", "pattern MkT1' :: (Eq a, Num a) => Show b => b -> T1 a")
        , ("pattern MkT1' b <- MkT1 42 b\n  where MkT1' b = MkT1 42 b", "pattern MkT1' :: (Eq a, Num a) => Show b => b -> T1 a")
        , ("qualifiedSigTest= C.realPart", "qualifiedSigTest :: C.Complex a -> a")
        , ("head = 233", "head :: Integer")
        , ("rank2Test (k :: forall a . a -> a) = (k 233 :: Int, k \"QAQ\")", "rank2Test :: (forall a. a -> a) -> (Int, " <> listOfChar <> ")")
        , ("symbolKindTest = Proxy @\"qwq\"", "symbolKindTest :: Proxy \"qwq\"")
        , ("promotedKindTest = Proxy @Nothing", if ghcVersion >= GHC96 then "promotedKindTest :: Proxy Nothing" else "promotedKindTest :: Proxy 'Nothing")
        , ("typeOperatorTest = Refl", if ghcVersion >= GHC92 then "typeOperatorTest :: forall {k} {a :: k}. a :~: a" else "typeOperatorTest :: a :~: a")
        , ("notInScopeTest = mkCharType", "notInScopeTest :: String -> Data.Data.DataType")
        ]
   in testGroup
        "add signature"
        [ testGroup "signatures are correct" [sigSession (T.unpack $ T.replace "\n" "\\n" def) False "always" "" (def, Just sig) [] | (def, sig) <- cases]
        , sigSession "exported mode works" False "exported" "xyz" ("xyz = True", Just "xyz :: Bool") (fst <$> take 3 cases)
        , testGroup
            "diagnostics mode works"
            [ sigSession "with GHC warnings" True "diagnostics" "" (second Just $ head cases) []
            , sigSession "without GHC warnings" False "diagnostics" "" (second (const Nothing) $ head cases) []
            ]
        , testSession "keep stale lens" $ do
            let content = T.unlines
                    [ "module Stale where"
                    , "f = _"
                    ]
            doc <- createDoc "Stale.hs" "haskell" content
            oldLens <- getCodeLenses doc
            liftIO $ length oldLens @?= 1
            let edit = TextEdit (mkRange 0 4 0 5) "" -- Remove the `_`
            _ <- applyEdit doc edit
            newLens <- getCodeLenses doc
            liftIO $ newLens @?= oldLens
        ]

linkToLocation :: [LocationLink] -> [Location]
linkToLocation = map (\LocationLink{_targetUri,_targetRange} -> Location _targetUri _targetRange)

checkDefs :: [Location] |? [LocationLink] -> Session [Expect] -> Session ()
checkDefs (either id linkToLocation . toEither -> defs) mkExpectations = traverse_ check =<< mkExpectations where
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

findDefinitionAndHoverTests :: TestTree
findDefinitionAndHoverTests = let

  tst :: (TextDocumentIdentifier -> Position -> Session a, a -> Session [Expect] -> Session ()) -> Position -> String -> Session [Expect] -> String -> TestTree
  tst (get, check) pos sfp targetRange title = testSessionWithExtraFiles "hover" title $ \dir -> do

    -- Dirty the cache to check that definitions work even in the presence of iface files
    liftIO $ runInDir dir $ do
      let fooPath = dir </> "Foo.hs"
      fooSource <- liftIO $ readFileUtf8 fooPath
      fooDoc <- createDoc fooPath "haskell" fooSource
      _ <- getHover fooDoc $ Position 4 3
      closeDoc fooDoc

    doc <- openTestDataDoc (dir </> sfp)
    waitForProgressDone
    found <- get doc pos
    check found targetRange



  checkHover :: Maybe Hover -> Session [Expect] -> Session ()
  checkHover hover expectations = traverse_ check =<< expectations where

    check expected =
      case hover of
        Nothing -> unless (expected == ExpectNoHover) $ liftIO $ assertFailure "no hover found"
        Just Hover{_contents = (HoverContents MarkupContent{_value = standardizeQuotes -> msg})
                  ,_range    = rangeInHover } ->
          case expected of
            ExpectRange  expectedRange -> checkHoverRange expectedRange rangeInHover msg
            ExpectHoverRange expectedRange -> checkHoverRange expectedRange rangeInHover msg
            ExpectHoverText snippets -> liftIO $ traverse_ (`assertFoundIn` msg) snippets
            ExpectHoverExcludeText snippets -> liftIO $ traverse_ (`assertNotFoundIn` msg) snippets
            ExpectHoverTextRegex re -> liftIO $ assertBool ("Regex not found in " <> T.unpack msg) (msg =~ re :: Bool)
            ExpectNoHover -> liftIO $ assertFailure $ "Expected no hover but got " <> show hover
            _ -> pure () -- all other expectations not relevant to hover
        _ -> liftIO $ assertFailure $ "test not expecting this kind of hover info" <> show hover

  extractLineColFromHoverMsg :: T.Text -> [T.Text]
  extractLineColFromHoverMsg = T.splitOn ":" . head . T.splitOn "*" . last . T.splitOn (sourceFileName <> ":")

  checkHoverRange :: Range -> Maybe Range -> T.Text -> Session ()
  checkHoverRange expectedRange rangeInHover msg =
    let
      lineCol = extractLineColFromHoverMsg msg
      -- looks like hovers use 1-based numbering while definitions use 0-based
      -- turns out that they are stored 1-based in RealSrcLoc by GHC itself.
      adjust Position{_line = l, _character = c} =
        Position{_line = l + 1, _character = c + 1}
    in
    case map (read . T.unpack) lineCol of
      [l,c] -> liftIO $ adjust (_start expectedRange) @=? Position l c
      _     -> liftIO $ assertFailure $
        "expected: " <> show ("[...]" <> sourceFileName <> ":<LINE>:<COL>**[...]", Just expectedRange) <>
        "\n but got: " <> show (msg, rangeInHover)

  assertFoundIn :: T.Text -> T.Text -> Assertion
  assertFoundIn part whole = assertBool
    (T.unpack $ "failed to find: `" <> part <> "` in hover message:\n" <> whole)
    (part `T.isInfixOf` whole)

  assertNotFoundIn :: T.Text -> T.Text -> Assertion
  assertNotFoundIn part whole = assertBool
    (T.unpack $ "found unexpected: `" <> part <> "` in hover message:\n" <> whole)
    (not . T.isInfixOf part $ whole)

  sourceFilePath = T.unpack sourceFileName
  sourceFileName = "GotoHover.hs"

  mkFindTests tests = testGroup "get"
    [ testGroup "definition" $ mapMaybe fst tests
    , testGroup "hover"      $ mapMaybe snd tests
    , checkFileCompiles sourceFilePath $
        expectDiagnostics
          [ ( "GotoHover.hs", [(DsError, (62, 7), "Found hole: _")])
          , ( "GotoHover.hs", [(DsError, (65, 8), "Found hole: _")])
          ]
    , testGroup "type-definition" typeDefinitionTests
    , testGroup "hover-record-dot-syntax" recordDotSyntaxTests ]

  typeDefinitionTests = [ tst (getTypeDefinitions, checkDefs) aaaL14 sourceFilePath (pure tcData) "Saturated data con"
                        , tst (getTypeDefinitions, checkDefs) aL20 sourceFilePath (pure [ExpectNoDefinitions]) "Polymorphic variable"]

  recordDotSyntaxTests
    | ghcVersion >= GHC92 =
        [ tst (getHover, checkHover) (Position 19 24) (T.unpack "RecordDotSyntax.hs") (pure [ExpectHoverText ["x :: MyRecord"]]) "hover over parent"
        , tst (getHover, checkHover) (Position 19 25) (T.unpack "RecordDotSyntax.hs") (pure [ExpectHoverText ["_ :: MyChild"]]) "hover over dot shows child"
        , tst (getHover, checkHover) (Position 19 26) (T.unpack "RecordDotSyntax.hs") (pure [ExpectHoverText ["_ :: MyChild"]]) "hover over child"
        ]
    | otherwise = []

  test runDef runHover look expect = testM runDef runHover look (return expect)

  testM runDef runHover look expect title =
    ( runDef   $ tst def   look sourceFilePath expect title
    , runHover $ tst hover look sourceFilePath expect title ) where
      def   = (getDefinitions, checkDefs)
      hover = (getHover      , checkHover)

  -- search locations            expectations on results
  fffL4  = _start fffR     ;  fffR = mkRange 8  4    8  7 ; fff  = [ExpectRange fffR]
  fffL8  = Position 12  4  ;
  fffL14 = Position 18  7  ;
  aL20   = Position 19 15
  aaaL14 = Position 18 20  ;  aaa    = [mkR  11  0   11  3]
  dcL7   = Position 11 11  ;  tcDC   = [mkR   7 23    9 16]
  dcL12  = Position 16 11  ;
  xtcL5  = Position  9 11  ;  xtc    = [ExpectExternFail,   ExpectHoverText ["Int", "Defined in ", "GHC.Types", "ghc-prim"]]
  tcL6   = Position 10 11  ;  tcData = [mkR   7  0    9 16, ExpectHoverText ["TypeConstructor", "GotoHover.hs:8:1"]]
  vvL16  = Position 20 12  ;  vv     = [mkR  20  4   20  6]
  opL16  = Position 20 15  ;  op     = [mkR  21  2   21  4]
  opL18  = Position 22 22  ;  opp    = [mkR  22 13   22 17]
  aL18   = Position 22 20  ;  apmp   = [mkR  22 10   22 11]
  b'L19  = Position 23 13  ;  bp     = [mkR  23  6   23  7]
  xvL20  = Position 24  8  ;  xvMsg  = [ExpectExternFail,   ExpectHoverText ["pack", ":: String -> Text", "Data.Text", "text"]]
  clL23  = Position 27 11  ;  cls    = [mkR  25  0   26 20, ExpectHoverText ["MyClass", "GotoHover.hs:26:1"]]
  clL25  = Position 29  9
  eclL15 = Position 19  8  ;  ecls   = [ExpectExternFail, ExpectHoverText ["Num", "Defined in ", "GHC.Num", "base"]]
  dnbL29 = Position 33 18  ;  dnb    = [ExpectHoverText [":: ()"],   mkR  33 12   33 21]
  dnbL30 = Position 34 23
  lcbL33 = Position 37 26  ;  lcb    = [ExpectHoverText [":: Char"], mkR  37 26   37 27]
  lclL33 = Position 37 22
  mclL36 = Position 40  1  ;  mcl    = [mkR  40  0   40 14]
  mclL37 = Position 41  1
  spaceL37 = Position 41  24 ; space = [ExpectNoDefinitions, ExpectHoverText [":: Char"]]
  docL41 = Position 45  1  ;  doc    = [ExpectHoverText ["Recognizable docs: kpqz"]]
                           ;  constr = [ExpectHoverText ["Monad m"]]
  eitL40 = Position 44 28  ;  kindE  = [ExpectHoverText [if ghcVersion >= GHC92 then ":: Type -> Type -> Type\n" else ":: * -> * -> *\n"]]
  intL40 = Position 44 34  ;  kindI  = [ExpectHoverText [if ghcVersion >= GHC92 then ":: Type\n" else ":: *\n"]]
  tvrL40 = Position 44 37  ;  kindV  = [ExpectHoverText [":: * -> *\n"]]
  intL41 = Position 45 20  ;  litI   = [ExpectHoverText ["7518"]]
  chrL36 = Position 41 24  ;  litC   = [ExpectHoverText ["'f'"]]
  txtL8  = Position 12 14  ;  litT   = [ExpectHoverText ["\"dfgy\""]]
  lstL43 = Position 47 12  ;  litL   = [ExpectHoverText ["[8391 :: Int, 6268]"]]
  outL45 = Position 49  3  ;  outSig = [ExpectHoverText ["outer", "Bool"], mkR 50 0 50 5]
  innL48 = Position 52  5  ;  innSig = [ExpectHoverText ["inner", "Char"], mkR 49 2 49 7]
  holeL60 = Position 62 7  ;  hleInfo = [ExpectHoverText ["_ ::"]]
  holeL65 = Position 65 8  ;  hleInfo2 = [ExpectHoverText ["_ :: a -> Maybe a"]]
  cccL17 = Position 17 16  ;  docLink = [ExpectHoverTextRegex "\\*Defined in 'GHC.Types'\\* \\*\\(ghc-prim-[0-9.]+\\)\\*\n\n"]
  imported = Position 56 13 ; importedSig = getDocUri "Foo.hs" >>= \foo -> return [ExpectHoverText ["foo", "Foo", "Haddock"], mkL foo 5 0 5 3]
  reexported = Position 55 14 ; reexportedSig = getDocUri "Bar.hs" >>= \bar -> return [ExpectHoverText ["Bar", "Bar", "Haddock"], mkL bar 3 (if ghcVersion >= GHC94 then 5 else 0) 3 (if ghcVersion >= GHC94 then 8 else 14)]
  thLocL57 = Position 59 10 ; thLoc = [ExpectHoverText ["Identity"]]
  cmtL68 = Position 67  0  ;  lackOfdEq = [ExpectHoverExcludeText ["$dEq"]]
  in
  mkFindTests
  --      def    hover  look       expect
  [
    if ghcVersion >= GHC90 then
        -- It suggests either going to the constructor or to the field
        test  broken yes    fffL4      fff           "field in record definition"
    else
        test  yes    yes    fffL4      fff           "field in record definition"
  , test  yes    yes    fffL8      fff           "field in record construction    #1102"
  , test  yes    yes    fffL14     fff           "field name used as accessor"           -- https://github.com/haskell/ghcide/pull/120 in Calculate.hs
  , test  yes    yes    aaaL14     aaa           "top-level name"                        -- https://github.com/haskell/ghcide/pull/120
  , test  yes    yes    dcL7       tcDC          "data constructor record         #1029"
  , test  yes    yes    dcL12      tcDC          "data constructor plain"                -- https://github.com/haskell/ghcide/pull/121
  , test  yes    yes    tcL6       tcData        "type constructor                #1028" -- https://github.com/haskell/ghcide/pull/147
  , test  broken yes    xtcL5      xtc           "type constructor external   #717,1028"
  , test  broken yes    xvL20      xvMsg         "value external package           #717" -- https://github.com/haskell/ghcide/pull/120
  , test  yes    yes    vvL16      vv            "plain parameter"                       -- https://github.com/haskell/ghcide/pull/120
  , test  yes    yes    aL18       apmp          "pattern match name"                    -- https://github.com/haskell/ghcide/pull/120
  , test  yes    yes    opL16      op            "top-level operator               #713" -- https://github.com/haskell/ghcide/pull/120
  , test  yes    yes    opL18      opp           "parameter operator"                    -- https://github.com/haskell/ghcide/pull/120
  , test  yes    yes    b'L19      bp            "name in backticks"                     -- https://github.com/haskell/ghcide/pull/120
  , test  yes    yes    clL23      cls           "class in instance declaration   #1027"
  , test  yes    yes    clL25      cls           "class in signature              #1027" -- https://github.com/haskell/ghcide/pull/147
  , test  broken yes    eclL15     ecls          "external class in signature #717,1027"
  , test  yes    yes    dnbL29     dnb           "do-notation   bind              #1073"
  , test  yes    yes    dnbL30     dnb           "do-notation lookup"
  , test  yes    yes    lcbL33     lcb           "listcomp   bind                 #1073"
  , test  yes    yes    lclL33     lcb           "listcomp lookup"
  , test  yes    yes    mclL36     mcl           "top-level fn 1st clause"
  , test  yes    yes    mclL37     mcl           "top-level fn 2nd clause         #1030"
  , if ghcVersion >= GHC810 then
        test  yes    yes    spaceL37   space         "top-level fn on space           #1002"
    else
        test  yes    broken spaceL37   space         "top-level fn on space           #1002"
  , test  no     yes    docL41     doc           "documentation                   #1129"
  , test  no     yes    eitL40     kindE         "kind of Either                  #1017"
  , test  no     yes    intL40     kindI         "kind of Int                     #1017"
  , test  no     broken tvrL40     kindV         "kind of (* -> *) type variable  #1017"
  , test  no     broken intL41     litI          "literal Int  in hover info      #1016"
  , test  no     broken chrL36     litC          "literal Char in hover info      #1016"
  , test  no     broken txtL8      litT          "literal Text in hover info      #1016"
  , test  no     broken lstL43     litL          "literal List in hover info      #1016"
  , test  yes    yes    cmtL68     lackOfdEq     "no Core symbols                 #3280"
  , if ghcVersion >= GHC90 then
        test  no     yes    docL41     constr        "type constraint in hover info   #1012"
    else
        test  no     broken docL41     constr        "type constraint in hover info   #1012"
  , test  no     yes    outL45     outSig        "top-level signature              #767"
  , test  broken broken innL48     innSig        "inner     signature              #767"
  , test  no     yes    holeL60    hleInfo       "hole without internal name       #831"
  , test  no     yes    holeL65    hleInfo2      "hole with variable"
  , test  no     yes    cccL17     docLink       "Haddock html links"
  , testM yes    yes    imported   importedSig   "Imported symbol"
  , if | isWindows ->
        -- Flaky on Windows: https://github.com/haskell/haskell-language-server/issues/2997
        testM no     yes    reexported reexportedSig "Imported symbol (reexported)"
       | otherwise ->
        testM yes    yes    reexported reexportedSig "Imported symbol (reexported)"
  , if | ghcVersion == GHC90 && isWindows ->
        test  no     broken    thLocL57   thLoc         "TH Splice Hover"
       | otherwise ->
        test  no     yes       thLocL57   thLoc         "TH Splice Hover"
  ]
  where yes, broken :: (TestTree -> Maybe TestTree)
        yes    = Just -- test should run and pass
        broken = Just . (`xfail` "known broken")
        no = const Nothing -- don't run this test at all
        skip = const Nothing -- unreliable, don't run

checkFileCompiles :: FilePath -> Session () -> TestTree
checkFileCompiles fp diag =
  testSessionWithExtraFiles "hover" ("Does " ++ fp ++ " compile") $ \dir -> do
    void (openTestDataDoc (dir </> fp))
    diag


pluginSimpleTests :: TestTree
pluginSimpleTests =
  ignoreInWindowsForGHC810 $
  -- Build profile: -w ghc-9.4.2 -O1
  -- In order, the following will be built (use -v for more details):
  -- - ghc-typelits-natnormalise-0.7.7 (lib) (requires build)
  -- - ghc-typelits-knownnat-0.7.7 (lib) (requires build)
  -- - plugin-1.0.0 (lib) (first run)
  -- Starting     ghc-typelits-natnormalise-0.7.7 (lib)
  -- Building     ghc-typelits-natnormalise-0.7.7 (lib)

  -- Failed to build ghc-typelits-natnormalise-0.7.7.
  -- Build log (
  -- C:\cabal\logs\ghc-9.4.2\ghc-typelits-_-0.7.7-3f036a52a0d9bfc3389d1852a87da2e87c6de2e4.log
  -- ):
  -- Preprocessing library for ghc-typelits-natnormalise-0.7.7..
  -- Building library for ghc-typelits-natnormalise-0.7.7..
  -- [1 of 3] Compiling GHC.TypeLits.Normalise.SOP ( src\GHC\TypeLits\Normalise\SOP.hs, dist\build\GHC\TypeLits\Normalise\SOP.o )
  -- [2 of 3] Compiling GHC.TypeLits.Normalise.Unify ( src\GHC\TypeLits\Normalise\Unify.hs, dist\build\GHC\TypeLits\Normalise\Unify.o )
  -- [3 of 3] Compiling GHC.TypeLits.Normalise ( src-ghc-9.4\GHC\TypeLits\Normalise.hs, dist\build\GHC\TypeLits\Normalise.o )
  -- C:\tools\ghc-9.4.2\lib\../mingw/bin/llvm-ar.exe: error: dist\build\objs-5156\libHSghc-typelits-_-0.7.7-3f036a52a0d9bfc3389d1852a87da2e87c6de2e4.a: No such file or directory

  -- Error: cabal: Failed to build ghc-typelits-natnormalise-0.7.7 (which is
  -- required by plugin-1.0.0). See the build log above for details.
  ignoreFor (BrokenForGHC [GHC96]) "fragile, frequently times out" $
  ignoreFor (BrokenSpecific Windows [GHC94]) "ghc-typelist-natnormalise fails to build on GHC 9.4.2 for windows only" $
  testSessionWithExtraFiles "plugin-knownnat" "simple plugin" $ \dir -> do
    _ <- openDoc (dir </> "KnownNat.hs") "haskell"
    liftIO $ writeFile (dir</>"hie.yaml")
      "cradle: {cabal: [{path: '.', component: 'lib:plugin'}]}"

    expectDiagnostics
      [ ( "KnownNat.hs",
          [(DsError, (9, 15), "Variable not in scope: c")]
          )
      ]

pluginParsedResultTests :: TestTree
pluginParsedResultTests =
  ignoreInWindowsForGHC810 $
  ignoreForGHC92Plus "No need for this plugin anymore!" $
  testSessionWithExtraFiles "plugin-recorddot" "parsedResultAction plugin" $ \dir -> do
    _ <- openDoc (dir</> "RecordDot.hs") "haskell"
    expectNoMoreDiagnostics 2

cppTests :: TestTree
cppTests =
  testGroup "cpp"
    [ ignoreInWindowsBecause "Throw a lsp session time out in windows for ghc-8.8 and is broken for other versions" $ testCase "cpp-error" $ do
        let content =
              T.unlines
                [ "{-# LANGUAGE CPP #-}",
                  "module Testing where",
                  "#ifdef FOO",
                  "foo = 42"
                ]
        -- The error locations differ depending on which C-preprocessor is used.
        -- Some give the column number and others don't (hence maxBound == -1 unsigned). Assert either
        -- of them.
        (run $ expectError content (2, maxBound))
          `catch` ( \e -> do
                      let _ = e :: HUnitFailure
                      run $ expectError content (2, 1)
                  )
    , testSessionWait "cpp-ghcide" $ do
        _ <- createDoc "A.hs" "haskell" $ T.unlines
          ["{-# LANGUAGE CPP #-}"
          ,"main ="
          ,"#ifdef __GHCIDE__"
          ,"  worked"
          ,"#else"
          ,"  failed"
          ,"#endif"
          ]
        expectDiagnostics [("A.hs", [(DsError, (3, 2), "Variable not in scope: worked")])]
    ]
  where
    expectError :: T.Text -> Cursor -> Session ()
    expectError content cursor = do
      _ <- createDoc "Testing.hs" "haskell" content
      expectDiagnostics
        [ ( "Testing.hs",
            [(DsError, cursor, "error: unterminated")]
          )
        ]
      expectNoMoreDiagnostics 0.5

preprocessorTests :: TestTree
preprocessorTests = testSessionWait "preprocessor" $ do
  let content =
        T.unlines
          [ "{-# OPTIONS_GHC -F -pgmF=ghcide-test-preprocessor #-}"
          , "module Testing where"
          , "y = x + z" -- plugin replaces x with y, making this have only one diagnostic
          ]
  _ <- createDoc "Testing.hs" "haskell" content
  expectDiagnostics
    [ ( "Testing.hs",
        [(DsError, (2, 8), "Variable not in scope: z")]
      )
    ]


safeTests :: TestTree
safeTests =
  testGroup
    "SafeHaskell"
    [ -- Test for https://github.com/haskell/ghcide/issues/424
      testSessionWait "load" $ do
        let sourceA =
              T.unlines
                ["{-# LANGUAGE Trustworthy #-}"
                ,"module A where"
                ,"import System.IO.Unsafe"
                ,"import System.IO ()"
                ,"trustWorthyId :: a -> a"
                ,"trustWorthyId i = unsafePerformIO $ do"
                ,"  putStrLn \"I'm safe\""
                ,"  return i"]
            sourceB =
              T.unlines
                ["{-# LANGUAGE Safe #-}"
                ,"module B where"
                ,"import A"
                ,"safeId :: a -> a"
                ,"safeId = trustWorthyId"
                ]

        _ <- createDoc "A.hs" "haskell" sourceA
        _ <- createDoc "B.hs" "haskell" sourceB
        expectNoMoreDiagnostics 1 ]

thTests :: TestTree
thTests =
  testGroup
    "TemplateHaskell"
    [ -- Test for https://github.com/haskell/ghcide/pull/212
      testSessionWait "load" $ do
        let sourceA =
              T.unlines
                [ "{-# LANGUAGE PackageImports #-}",
                  "{-# LANGUAGE TemplateHaskell #-}",
                  "module A where",
                  "import \"template-haskell\" Language.Haskell.TH",
                  "a :: Integer",
                  "a = $(litE $ IntegerL 3)"
                ]
            sourceB =
              T.unlines
                [ "{-# LANGUAGE PackageImports #-}",
                  "{-# LANGUAGE TemplateHaskell #-}",
                  "module B where",
                  "import A",
                  "import \"template-haskell\" Language.Haskell.TH",
                  "b :: Integer",
                  "b = $(litE $ IntegerL $ a) + n"
                ]
        _ <- createDoc "A.hs" "haskell" sourceA
        _ <- createDoc "B.hs" "haskell" sourceB
        expectDiagnostics [ ( "B.hs", [(DsError, (6, 29), "Variable not in scope: n")] ) ]
    , testSessionWait "newtype-closure" $ do
        let sourceA =
              T.unlines
                [ "{-# LANGUAGE DeriveDataTypeable #-}"
                  ,"{-# LANGUAGE TemplateHaskell #-}"
                  ,"module A (a) where"
                  ,"import Data.Data"
                  ,"import Language.Haskell.TH"
                  ,"newtype A = A () deriving (Data)"
                  ,"a :: ExpQ"
                  ,"a = [| 0 |]"]
        let sourceB =
              T.unlines
                [ "{-# LANGUAGE TemplateHaskell #-}"
                ,"module B where"
                ,"import A"
                ,"b :: Int"
                ,"b = $( a )" ]
        _ <- createDoc "A.hs" "haskell" sourceA
        _ <- createDoc "B.hs" "haskell" sourceB
        return ()
    , thReloadingTest False
    , thLoadingTest
    , thCoreTest
    , ignoreInWindowsBecause "Broken in windows" $ thReloadingTest True
    -- Regression test for https://github.com/haskell/haskell-language-server/issues/891
    , thLinkingTest False
    , ignoreInWindowsBecause "Broken in windows" $ thLinkingTest True
    , testSessionWait "findsTHIdentifiers" $ do
        let sourceA =
              T.unlines
                [ "{-# LANGUAGE TemplateHaskell #-}"
                , "module A (a) where"
                , "import Language.Haskell.TH (ExpQ)"
                , "a :: ExpQ" -- TH 2.17 requires an explicit type signature since splices are polymorphic
                , "a = [| glorifiedID |]"
                , "glorifiedID :: a -> a"
                , "glorifiedID = id" ]
        let sourceB =
              T.unlines
                [ "{-# OPTIONS_GHC -Wall #-}"
                , "{-# LANGUAGE TemplateHaskell #-}"
                , "module B where"
                , "import A"
                , "main = $a (putStrLn \"success!\")"]
        _ <- createDoc "A.hs" "haskell" sourceA
        _ <- createDoc "B.hs" "haskell" sourceB
        expectDiagnostics [ ( "B.hs", [(DsWarning, (4, 0), "Top-level binding with no type signature: main :: IO ()")] ) ]
    , testCase "findsTHnewNameConstructor" $ runWithExtraFiles "THNewName" $ \dir -> do

    -- This test defines a TH value with the meaning "data A = A" in A.hs
    -- Loads and export the template in B.hs
    -- And checks wether the constructor A can be loaded in C.hs
    -- This test does not fail when either A and B get manually loaded before C.hs
    -- or when we remove the seemingly unnecessary TH pragma from C.hs

    let cPath = dir </> "C.hs"
    _ <- openDoc cPath "haskell"
    expectDiagnostics [ ( cPath, [(DsWarning, (3, 0), "Top-level binding with no type signature: a :: A")] ) ]
    ]

-- | Tests for projects that use symbolic links one way or another
symlinkTests :: TestTree
symlinkTests =
  testGroup "Projects using Symlinks"
    [ testCase "Module is symlinked" $ runWithExtraFiles "symlink" $ \dir -> do
        liftIO $ createFileLink (dir </> "some_loc" </> "Sym.hs") (dir </> "other_loc" </> "Sym.hs")
        let fooPath = dir </> "src" </> "Foo.hs"
        _ <- openDoc fooPath "haskell"
        expectDiagnosticsWithTags  [("src" </> "Foo.hs", [(DsWarning, (2, 0), "The import of 'Sym' is redundant", Just DtUnnecessary)])]
        pure ()
    ]

-- | Test that all modules have linkables
thLoadingTest :: TestTree
thLoadingTest = testCase "Loading linkables" $ runWithExtraFiles "THLoading" $ \dir -> do
    let thb = dir </> "THB.hs"
    _ <- openDoc thb "haskell"
    expectNoMoreDiagnostics 1

thCoreTest :: TestTree
thCoreTest = testCase "Verifying TH core files" $ runWithExtraFiles "THCoreFile" $ \dir -> do
    let thc = dir </> "THC.hs"
    _ <- openDoc thc "haskell"
    expectNoMoreDiagnostics 1

-- | test that TH is reevaluated on typecheck
thReloadingTest :: Bool -> TestTree
thReloadingTest unboxed = testCase name $ runWithExtraFiles dir $ \dir -> do

    let aPath = dir </> "THA.hs"
        bPath = dir </> "THB.hs"
        cPath = dir </> "THC.hs"

    aSource <- liftIO $ readFileUtf8 aPath --  th = [d|a = ()|]
    bSource <- liftIO $ readFileUtf8 bPath --  $th
    cSource <- liftIO $ readFileUtf8 cPath --  c = a :: ()

    adoc <- createDoc aPath "haskell" aSource
    bdoc <- createDoc bPath "haskell" bSource
    cdoc <- createDoc cPath "haskell" cSource

    expectDiagnostics [("THB.hs", [(DsWarning, (4,thDollarIdx), "Top-level binding")])]

    -- Change th from () to Bool
    let aSource' = T.unlines $ init (T.lines aSource) ++ ["th_a = [d| a = False|]"]
    changeDoc adoc [TextDocumentContentChangeEvent Nothing Nothing aSource']
    -- generate an artificial warning to avoid timing out if the TH change does not propagate
    changeDoc cdoc [TextDocumentContentChangeEvent Nothing Nothing $ cSource <> "\nfoo=()"]

    -- Check that the change propagates to C
    expectDiagnostics
        [("THC.hs", [(DsError, (4, 4), "Couldn't match expected type '()' with actual type 'Bool'")])
        ,("THC.hs", [(DsWarning, (6,0), "Top-level binding")])
        ,("THB.hs", [(DsWarning, (4,thDollarIdx), "Top-level bindin")])
        ]

    closeDoc adoc
    closeDoc bdoc
    closeDoc cdoc
  where
    name = "reloading-th-test" <> if unboxed then "-unboxed" else ""
    dir | unboxed = "THUnboxed"
        | otherwise = "TH"

thLinkingTest :: Bool -> TestTree
thLinkingTest unboxed = testCase name $ runWithExtraFiles dir $ \dir -> do

    let aPath = dir </> "THA.hs"
        bPath = dir </> "THB.hs"

    aSource <- liftIO $ readFileUtf8 aPath --  th_a = [d|a :: ()|]
    bSource <- liftIO $ readFileUtf8 bPath --  $th_a

    adoc <- createDoc aPath "haskell" aSource
    bdoc <- createDoc bPath "haskell" bSource

    expectDiagnostics [("THB.hs", [(DsWarning, (4,thDollarIdx), "Top-level binding")])]

    let aSource' = T.unlines $ init (init (T.lines aSource)) ++ ["th :: DecsQ", "th = [d| a = False|]"]
    changeDoc adoc [TextDocumentContentChangeEvent Nothing Nothing aSource']

    -- modify b too
    let bSource' = T.unlines $ init (T.lines bSource) ++ ["$th"]
    changeDoc bdoc [TextDocumentContentChangeEvent Nothing Nothing bSource']
    waitForProgressBegin
    waitForAllProgressDone

    expectCurrentDiagnostics bdoc [(DsWarning, (4,thDollarIdx), "Top-level binding")]

    closeDoc adoc
    closeDoc bdoc
  where
    name = "th-linking-test" <> if unboxed then "-unboxed" else ""
    dir | unboxed = "THUnboxed"
        | otherwise = "TH"

completionTests :: TestTree
completionTests
  = testGroup "completion"
    [
    testGroup "non local" nonLocalCompletionTests
    , testGroup "topLevel" topLevelCompletionTests
    , testGroup "local" localCompletionTests
    , testGroup "package" packageCompletionTests
    , testGroup "project" projectCompletionTests
    , testGroup "other" otherCompletionTests
    , testGroup "doc" completionDocTests
    ]

completionTest :: HasCallStack => String -> [T.Text] -> Position -> [(T.Text, CompletionItemKind, T.Text, Bool, Bool, Maybe (List TextEdit))] -> TestTree
completionTest name src pos expected = testSessionWait name $ do
    docId <- createDoc "A.hs" "haskell" (T.unlines src)
    _ <- waitForDiagnostics
    compls <- getCompletions docId pos
    let compls' = [ (_label, _kind, _insertText, _additionalTextEdits) | CompletionItem{..} <- compls]
    let emptyToMaybe x = if T.null x then Nothing else Just x
    liftIO $ sortOn (Lens.view Lens._1) (take (length expected) compls') @?=
        sortOn (Lens.view Lens._1)
          [ (l, Just k, emptyToMaybe t, at) | (l,k,t,_,_,at) <- expected]
    forM_ (zip compls expected) $ \(item, (_,_,_,expectedSig, expectedDocs, _)) -> do
        CompletionItem{..} <-
          if expectedSig || expectedDocs
          then do
            rsp <- request SCompletionItemResolve item
            case rsp ^. L.result of
              Left err -> liftIO $ assertFailure ("completionItem/resolve failed with: " <> show err)
              Right x -> pure x
          else pure item
        when expectedSig $
            liftIO $ assertBool ("Missing type signature: " <> T.unpack _label) (isJust _detail)
        when expectedDocs $
            liftIO $ assertBool ("Missing docs: " <> T.unpack _label) (isJust _documentation)


topLevelCompletionTests :: [TestTree]
topLevelCompletionTests = [
    completionTest
        "variable"
        ["bar = xx", "-- | haddock", "xxx :: ()", "xxx = ()", "-- | haddock", "data Xxx = XxxCon"]
        (Position 0 8)
        [("xxx", CiFunction, "xxx", True, True, Nothing)
        ],
    completionTest
        "constructor"
        ["bar = xx", "-- | haddock", "xxx :: ()", "xxx = ()", "-- | haddock", "data Xxx = XxxCon"]
        (Position 0 8)
        [("xxx", CiFunction, "xxx", True, True, Nothing)
        ],
    completionTest
        "class method"
        ["bar = xx", "class Xxx a where", "-- | haddock", "xxx :: ()", "xxx = ()"]
        (Position 0 8)
        [("xxx", CiFunction, "xxx", True, True, Nothing)],
    completionTest
        "type"
        ["bar :: Xz", "zzz = ()", "-- | haddock", "data Xzz = XzzCon"]
        (Position 0 9)
        [("Xzz", CiStruct, "Xzz", False, True, Nothing)],
    completionTest
        "class"
        ["bar :: Xz", "zzz = ()", "-- | haddock", "class Xzz a"]
        (Position 0 9)
        [("Xzz", CiInterface, "Xzz", False, True, Nothing)],
    completionTest
        "records"
        ["data Person = Person { _personName:: String, _personAge:: Int}", "bar = Person { _pers }" ]
        (Position 1 19)
        [("_personName", CiFunction, "_personName", False, True, Nothing),
         ("_personAge", CiFunction, "_personAge", False, True, Nothing)],
    completionTest
        "recordsConstructor"
        ["data XxRecord = XyRecord { x:: String, y:: Int}", "bar = Xy" ]
        (Position 1 19)
        [("XyRecord", CiConstructor, "XyRecord", False, True, Nothing),
         ("XyRecord", CiSnippet, "XyRecord {x=${1:_x}, y=${2:_y}}", False, True, Nothing)]
    ]

localCompletionTests :: [TestTree]
localCompletionTests = [
    completionTest
        "argument"
        ["bar (Just abcdef) abcdefg = abcd"]
        (Position 0 32)
        [("abcdef", CiFunction, "abcdef", True, False, Nothing),
         ("abcdefg", CiFunction , "abcdefg", True, False, Nothing)
        ],
    completionTest
        "let"
        ["bar = let (Just abcdef) = undefined"
        ,"          abcdefg = let abcd = undefined in undefined"
        ,"        in abcd"
        ]
        (Position 2 15)
        [("abcdef", CiFunction, "abcdef", True, False, Nothing),
         ("abcdefg", CiFunction , "abcdefg", True, False, Nothing)
        ],
    completionTest
        "where"
        ["bar = abcd"
        ,"  where (Just abcdef) = undefined"
        ,"        abcdefg = let abcd = undefined in undefined"
        ]
        (Position 0 10)
        [("abcdef", CiFunction, "abcdef", True, False, Nothing),
         ("abcdefg", CiFunction , "abcdefg", True, False, Nothing)
        ],
    completionTest
        "do/1"
        ["bar = do"
        ,"  Just abcdef <- undefined"
        ,"  abcd"
        ,"  abcdefg <- undefined"
        ,"  pure ()"
        ]
        (Position 2 6)
        [("abcdef", CiFunction, "abcdef", True, False, Nothing)
        ],
    completionTest
        "do/2"
        ["bar abcde = do"
        ,"    Just [(abcdef,_)] <- undefined"
        ,"    abcdefg <- undefined"
        ,"    let abcdefgh = undefined"
        ,"        (Just [abcdefghi]) = undefined"
        ,"    abcd"
        ,"  where"
        ,"    abcdefghij = undefined"
        ]
        (Position 5 8)
        [("abcde", CiFunction, "abcde", True, False, Nothing)
        ,("abcdefghij", CiFunction, "abcdefghij", True, False, Nothing)
        ,("abcdef", CiFunction, "abcdef", True, False, Nothing)
        ,("abcdefg", CiFunction, "abcdefg", True, False, Nothing)
        ,("abcdefgh", CiFunction, "abcdefgh", True, False, Nothing)
        ,("abcdefghi", CiFunction, "abcdefghi", True, False, Nothing)
        ],
    completionTest
        "type family"
        ["{-# LANGUAGE DataKinds, TypeFamilies #-}"
        ,"type family Bar a"
        ,"a :: Ba"
        ]
        (Position 2 7)
        [("Bar", CiStruct, "Bar", True, False, Nothing)
        ],
    completionTest
        "class method"
        [
          "class Test a where"
        , "    abcd :: a -> ()"
        , "    abcde :: a -> Int"
        , "instance Test Int where"
        , "    abcd = abc"
        ]
        (Position 4 14)
        [("abcd", CiFunction, "abcd", True, False, Nothing)
        ,("abcde", CiFunction, "abcde", True, False, Nothing)
        ],
    testSessionWait "incomplete entries" $ do
        let src a = "data Data = " <> a
        doc <- createDoc "A.hs" "haskell" $ src "AAA"
        void $ waitForTypecheck doc
        let editA rhs =
                changeDoc doc [TextDocumentContentChangeEvent
                    { _range=Nothing
                    , _rangeLength=Nothing
                    , _text=src rhs}]

        editA "AAAA"
        void $ waitForTypecheck doc
        editA "AAAAA"
        void $ waitForTypecheck doc

        compls <- getCompletions doc (Position 0 15)
        liftIO $ filter ("AAA" `T.isPrefixOf`) (mapMaybe _insertText compls) @?= ["AAAAA"]
        pure ()
    ]

nonLocalCompletionTests :: [TestTree]
nonLocalCompletionTests =
  [ brokenForWinGhc $ completionTest
      "variable"
      ["module A where", "f = hea"]
      (Position 1 7)
      [("head", CiFunction, "head", True, True, Nothing)],
    completionTest
      "constructor"
      ["{-# OPTIONS_GHC -Wall #-}", "module A where", "f = True"]
      (Position 2 8)
      [ ("True", CiConstructor, "True", True, True, Nothing)
      ],
    brokenForWinGhc $ completionTest
      "type"
      ["{-# OPTIONS_GHC -Wall #-}", "module A () where", "f :: Boo", "f = True"]
      (Position 2 8)
      [ ("Bool", CiStruct, "Bool", True, True, Nothing)
      ],
    completionTest
      "qualified"
      ["{-# OPTIONS_GHC -Wunused-binds #-}", "module A () where", "f = Prelude.hea"]
      (Position 2 15)
      [ ("head", CiFunction, "head", True, True, Nothing)
      ],
    completionTest
      "duplicate import"
      ["module A where", "import Data.List", "import Data.List", "f = permu"]
      (Position 3 9)
      [ ("permutations", CiFunction, "permutations", False, False, Nothing)
      ],
    completionTest
       "dont show hidden items"
       [ "{-# LANGUAGE NoImplicitPrelude #-}",
         "module A where",
         "import Control.Monad hiding (join)",
         "f = joi"
       ]
       (Position 3 6)
       [],
    testGroup "ordering"
      [completionTest "qualified has priority"
        ["module A where"
        ,"import qualified Data.ByteString as BS"
        ,"f = BS.read"
        ]
        (Position 2 10)
        [("readFile", CiFunction, "readFile", True, True, Nothing)]
        ],
      -- we need this test to make sure the ghcide completions module does not return completions for language pragmas. this functionality is turned on in hls
     completionTest
      "do not show pragma completions"
      [ "{-# LANGUAGE  ",
        "{module A where}",
        "main = return ()"
      ]
      (Position 0 13)
      []
  ]
  where
    brokenForWinGhc = knownBrokenFor (BrokenSpecific Windows [GHC810, GHC90, GHC92, GHC94, GHC96]) "Windows has strange things in scope for some reason"

otherCompletionTests :: [TestTree]
otherCompletionTests = [
    completionTest
      "keyword"
      ["module A where", "f = newty"]
      (Position 1 9)
      [("newtype", CiKeyword, "", False, False, Nothing)],
    completionTest
      "type context"
      [ "{-# OPTIONS_GHC -Wunused-binds #-}",
        "module A () where",
        "f = f",
        "g :: Intege"
      ]
      -- At this point the module parses but does not typecheck.
      -- This should be sufficient to detect that we are in a
      -- type context and only show the completion to the type.
      (Position 3 11)
      [("Integer", CiStruct, "Integer", True, True, Nothing)],

    testSession "duplicate record fields" $ do
      void $
        createDoc "B.hs" "haskell" $
          T.unlines
            [ "{-# LANGUAGE DuplicateRecordFields #-}",
              "module B where",
              "newtype Foo = Foo { member :: () }",
              "newtype Bar = Bar { member :: () }"
            ]
      docA <-
        createDoc "A.hs" "haskell" $
          T.unlines
            [ "module A where",
              "import B",
              "memb"
            ]
      _ <- waitForDiagnostics
      compls <- getCompletions docA $ Position 2 4
      let compls' = [txt | CompletionItem {_insertText = Just txt, ..} <- compls, _label == "member"]
      liftIO $ take 2 compls' @?= ["member"],

    testSessionWait "maxCompletions" $ do
        doc <- createDoc "A.hs" "haskell" $ T.unlines
            [ "{-# OPTIONS_GHC -Wunused-binds #-}",
                "module A () where",
                "a = Prelude."
            ]
        _ <- waitForDiagnostics
        compls <- getCompletions  doc (Position 3 13)
        liftIO $ length compls @?= maxCompletions def
  ]

packageCompletionTests :: [TestTree]
packageCompletionTests =
  [ testSession' "fromList" $ \dir -> do
        liftIO $ writeFile (dir </> "hie.yaml")
            "cradle: {direct: {arguments: [-hide-all-packages, -package, base, A]}}"
        doc <- createDoc "A.hs" "haskell" $ T.unlines
            [ "{-# OPTIONS_GHC -Wunused-binds #-}",
                "module A () where",
                "a = fromList"
            ]
        _ <- waitForDiagnostics
        compls <- getCompletions doc (Position 2 12)
        let compls' =
              [T.drop 1 $ T.dropEnd 3 d
              | CompletionItem {_documentation = Just (CompletionDocMarkup (MarkupContent MkMarkdown d)), _label}
                <- compls
              , _label == "fromList"
              ]
        liftIO $ take 3 (sort compls') @?=
          map ("Defined in "<>) (
              [ "'Data.List.NonEmpty"
              , "'GHC.Exts"
              ] ++ if ghcVersion >= GHC94 then [ "'GHC.IsList" ] else [])

  , testSessionWait "Map" $ do
        doc <- createDoc "A.hs" "haskell" $ T.unlines
            [ "{-# OPTIONS_GHC -Wunused-binds #-}",
                "module A () where",
                "a :: Map"
            ]
        _ <- waitForDiagnostics
        compls <- getCompletions doc (Position 2 7)
        let compls' =
              [T.drop 1 $ T.dropEnd 3 d
              | CompletionItem {_documentation = Just (CompletionDocMarkup (MarkupContent MkMarkdown d)), _label}
                <- compls
              , _label == "Map"
              ]
        liftIO $ take 3 (sort compls') @?=
          map ("Defined in "<>)
              [ "'Data.Map"
              , "'Data.Map.Lazy"
              , "'Data.Map.Strict"
              ]
  , testSessionWait "no duplicates" $ do
        doc <- createDoc "A.hs" "haskell" $ T.unlines
            [ "{-# OPTIONS_GHC -Wunused-binds #-}",
                "module A () where",
                "import GHC.Exts(fromList)",
                "a = fromList"
            ]
        _ <- waitForDiagnostics
        compls <- getCompletions doc (Position 3 13)
        let duplicate =
              filter
                (\case
                  CompletionItem
                    { _insertText = Just "fromList"
                    , _documentation =
                      Just (CompletionDocMarkup (MarkupContent MkMarkdown d))
                    } ->
                    "GHC.Exts" `T.isInfixOf` d
                  _ -> False
                ) compls
        liftIO $ length duplicate @?= 1

  , testSessionWait "non-local before global" $ do
    -- non local completions are more specific
        doc <- createDoc "A.hs" "haskell" $ T.unlines
            [ "{-# OPTIONS_GHC -Wunused-binds #-}",
                "module A () where",
                "import GHC.Exts(fromList)",
                "a = fromList"
            ]
        _ <- waitForDiagnostics
        compls <- getCompletions doc (Position 3 13)
        let compls' =
              [_insertText
              | CompletionItem {_label, _insertText} <- compls
              , _label == "fromList"
              ]
        liftIO $ take 3 compls' @?=
          map Just ["fromList"]
  ]

projectCompletionTests :: [TestTree]
projectCompletionTests =
    [ testSession' "from hiedb" $ \dir-> do
        liftIO $ writeFile (dir </> "hie.yaml")
            "cradle: {direct: {arguments: [\"-Wmissing-signatures\", \"A\", \"B\"]}}"
        _ <- createDoc "A.hs" "haskell" $ T.unlines
            [  "module A (anidentifier) where",
               "anidentifier = ()"
            ]
        _ <- waitForDiagnostics
        -- Note that B does not import A
        doc <- createDoc "B.hs" "haskell" $ T.unlines
            [ "module B where",
              "b = anidenti"
            ]
        compls <- getCompletions doc (Position 1 10)
        let compls' =
              [T.drop 1 $ T.dropEnd 3 d
              | CompletionItem {_documentation = Just (CompletionDocMarkup (MarkupContent MkMarkdown d)), _label}
                <- compls
              , _label == "anidentifier"
              ]
        liftIO $ compls' @?= ["Defined in 'A"],
      testSession' "auto complete project imports" $ \dir-> do
        liftIO $ writeFile (dir </> "hie.yaml")
            "cradle: {direct: {arguments: [\"-Wmissing-signatures\", \"ALocalModule\", \"B\"]}}"
        _ <- createDoc "ALocalModule.hs" "haskell" $ T.unlines
            [  "module ALocalModule (anidentifier) where",
               "anidentifier = ()"
            ]
        _ <- waitForDiagnostics
        -- Note that B does not import A
        doc <- createDoc "B.hs" "haskell" $ T.unlines
            [ "module B where",
              "import ALocal"
            ]
        compls <- getCompletions doc (Position 1 13)
        let item = head $ filter ((== "ALocalModule") . (^. Lens.label)) compls
        liftIO $ do
          item ^. Lens.label @?= "ALocalModule",
      testSession' "auto complete functions from qualified imports without alias" $ \dir-> do
        liftIO $ writeFile (dir </> "hie.yaml")
            "cradle: {direct: {arguments: [\"-Wmissing-signatures\", \"A\", \"B\"]}}"
        _ <- createDoc "A.hs" "haskell" $ T.unlines
            [  "module A (anidentifier) where",
               "anidentifier = ()"
            ]
        _ <- waitForDiagnostics
        doc <- createDoc "B.hs" "haskell" $ T.unlines
            [ "module B where",
              "import qualified A",
              "A."
            ]
        compls <- getCompletions doc (Position 2 2)
        let item = head compls
        liftIO $ do
          item ^. L.label @?= "anidentifier",
      testSession' "auto complete functions from qualified imports with alias" $ \dir-> do
        liftIO $ writeFile (dir </> "hie.yaml")
            "cradle: {direct: {arguments: [\"-Wmissing-signatures\", \"A\", \"B\"]}}"
        _ <- createDoc "A.hs" "haskell" $ T.unlines
            [  "module A (anidentifier) where",
               "anidentifier = ()"
            ]
        _ <- waitForDiagnostics
        doc <- createDoc "B.hs" "haskell" $ T.unlines
            [ "module B where",
              "import qualified A as Alias",
              "foo = Alias."
            ]
        compls <- getCompletions doc (Position 2 12)
        let item = head compls
        liftIO $ do
          item ^. L.label @?= "anidentifier"
    ]

completionDocTests :: [TestTree]
completionDocTests =
  [ testSession "local define" $ do
      doc <- createDoc "A.hs" "haskell" $ T.unlines
        [ "module A where"
        , "foo = ()"
        , "bar = fo"
        ]
      let expected = "*Defined at line 2, column 1 in this module*\n"
      test doc (Position 2 8) "foo" Nothing [expected]
  , testSession "local empty doc" $ do
      doc <- createDoc "A.hs" "haskell" $ T.unlines
        [ "module A where"
        , "foo = ()"
        , "bar = fo"
        ]
      test doc (Position 2 8) "foo" Nothing ["*Defined at line 2, column 1 in this module*\n"]
  , testSession "local single line doc without newline" $ do
      doc <- createDoc "A.hs" "haskell" $ T.unlines
        [ "module A where"
        , "-- |docdoc"
        , "foo = ()"
        , "bar = fo"
        ]
      test doc (Position 3 8) "foo" Nothing ["*Defined at line 3, column 1 in this module*\n* * *\n\n\ndocdoc\n"]
  , testSession "local multi line doc with newline" $ do
      doc <- createDoc "A.hs" "haskell" $ T.unlines
        [ "module A where"
        , "-- | abcabc"
        , "--"
        , "foo = ()"
        , "bar = fo"
        ]
      test doc (Position 4 8) "foo" Nothing ["*Defined at line 4, column 1 in this module*\n* * *\n\n\nabcabc\n"]
  , testSession "local multi line doc without newline" $ do
      doc <- createDoc "A.hs" "haskell" $ T.unlines
        [ "module A where"
        , "-- |     abcabc"
        , "--"
        , "--def"
        , "foo = ()"
        , "bar = fo"
        ]
      test doc (Position 5 8) "foo" Nothing ["*Defined at line 5, column 1 in this module*\n* * *\n\n\nabcabc \n\ndef\n"]
  , testSession "extern empty doc" $ do
      doc <- createDoc "A.hs" "haskell" $ T.unlines
        [ "module A where"
        , "foo = od"
        ]
      let expected = "*Imported from 'Prelude'*\n"
      test doc (Position 1 8) "odd" (Just $ T.length expected) [expected]
  , brokenForMacGhc9 $ brokenForWinGhc90 $ testSession "extern single line doc without '\\n'" $ do
      doc <- createDoc "A.hs" "haskell" $ T.unlines
        [ "module A where"
        , "foo = no"
        ]
      let expected = "*Imported from 'Prelude'*\n* * *\n\n\nBoolean \"not\"\n"
      test doc (Position 1 8) "not" (Just $ T.length expected) [expected]
  , brokenForMacGhc9 $ brokenForWinGhc90 $ testSession "extern mulit line doc" $ do
      doc <- createDoc "A.hs" "haskell" $ T.unlines
        [ "module A where"
        , "foo = i"
        ]
      let expected = "*Imported from 'Prelude'*\n* * *\n\n\nIdentity function. \n```haskell\nid x = x\n```\n"
      test doc (Position 1 7) "id" (Just $ T.length expected) [expected]
  , testSession "extern defined doc" $ do
      doc <- createDoc "A.hs" "haskell" $ T.unlines
        [ "module A where"
        , "foo = i"
        ]
      let expected = "*Imported from 'Prelude'*\n"
      test doc (Position 1 7) "id" (Just $ T.length expected) [expected]
  ]
  where
    brokenForGhc9 = knownBrokenFor (BrokenForGHC [GHC90, GHC92, GHC94, GHC96]) "Completion doc doesn't support ghc9"
    brokenForWinGhc90 = knownBrokenFor (BrokenSpecific Windows [GHC90]) "Extern doc doesn't support Windows for ghc9.2"
    -- https://gitlab.haskell.org/ghc/ghc/-/issues/20903
    brokenForMacGhc9 = knownBrokenFor (BrokenSpecific MacOS [GHC90, GHC92, GHC94, GHC96]) "Extern doc doesn't support MacOS for ghc9"
    test doc pos label mn expected = do
      _ <- waitForDiagnostics
      compls <- getCompletions doc pos
      rcompls <- forM compls $ \item -> do
        rsp <- request SCompletionItemResolve item
        case rsp ^. L.result of
          Left err -> liftIO $ assertFailure ("completionItem/resolve failed with: " <> show err)
          Right x -> pure x
      let compls' = [
            -- We ignore doc uris since it points to the local path which determined by specific machines
            case mn of
                Nothing -> txt
                Just n  -> T.take n txt
            | CompletionItem {_documentation = Just (CompletionDocMarkup (MarkupContent MkMarkdown txt)), ..} <- rcompls
            , _label == label
            ]
      liftIO $ compls' @?= expected

highlightTests :: TestTree
highlightTests = testGroup "highlight"
  [ testSessionWait "value" $ do
    doc <- createDoc "A.hs" "haskell" source
    _ <- waitForDiagnostics
    highlights <- getHighlights doc (Position 3 2)
    liftIO $ highlights @?= List
            [ DocumentHighlight (R 2 0 2 3) (Just HkRead)
            , DocumentHighlight (R 3 0 3 3) (Just HkWrite)
            , DocumentHighlight (R 4 6 4 9) (Just HkRead)
            , DocumentHighlight (R 5 22 5 25) (Just HkRead)
            ]
  , testSessionWait "type" $ do
    doc <- createDoc "A.hs" "haskell" source
    _ <- waitForDiagnostics
    highlights <- getHighlights doc (Position 2 8)
    liftIO $ highlights @?= List
            [ DocumentHighlight (R 2 7 2 10) (Just HkRead)
            , DocumentHighlight (R 3 11 3 14) (Just HkRead)
            ]
  , testSessionWait "local" $ do
    doc <- createDoc "A.hs" "haskell" source
    _ <- waitForDiagnostics
    highlights <- getHighlights doc (Position 6 5)
    liftIO $ highlights @?= List
            [ DocumentHighlight (R 6 4 6 7) (Just HkWrite)
            , DocumentHighlight (R 6 10 6 13) (Just HkRead)
            , DocumentHighlight (R 7 12 7 15) (Just HkRead)
            ]
  , knownBrokenForGhcVersions [GHC90, GHC92, GHC94, GHC96] "Ghc9 highlights the constructor and not just this field" $
        testSessionWait "record" $ do
        doc <- createDoc "A.hs" "haskell" recsource
        _ <- waitForDiagnostics
        highlights <- getHighlights doc (Position 4 15)
        liftIO $ highlights @?= List
          -- Span is just the .. on 8.10, but Rec{..} before
          [ if ghcVersion >= GHC810
            then DocumentHighlight (R 4 8 4 10) (Just HkWrite)
            else DocumentHighlight (R 4 4 4 11) (Just HkWrite)
          , DocumentHighlight (R 4 14 4 20) (Just HkRead)
          ]
        highlights <- getHighlights doc (Position 3 17)
        liftIO $ highlights @?= List
          [ DocumentHighlight (R 3 17 3 23) (Just HkWrite)
          -- Span is just the .. on 8.10, but Rec{..} before
          , if ghcVersion >= GHC810
              then DocumentHighlight (R 4 8 4 10) (Just HkRead)
              else DocumentHighlight (R 4 4 4 11) (Just HkRead)
          ]
  ]
  where
    source = T.unlines
      ["{-# OPTIONS_GHC -Wunused-binds #-}"
      ,"module Highlight () where"
      ,"foo :: Int"
      ,"foo = 3 :: Int"
      ,"bar = foo"
      ,"  where baz = let x = foo in x"
      ,"baz arg = arg + x"
      ,"  where x = arg"
      ]
    recsource = T.unlines
      ["{-# LANGUAGE RecordWildCards #-}"
      ,"{-# OPTIONS_GHC -Wunused-binds #-}"
      ,"module Highlight () where"
      ,"data Rec = Rec { field1 :: Int, field2 :: Char }"
      ,"foo Rec{..} = field2 + field1"
      ]

outlineTests :: TestTree
outlineTests = testGroup
  "outline"
  [ testSessionWait "type class" $ do
    let source = T.unlines ["module A where", "class A a where a :: a -> Bool"]
    docId   <- createDoc "A.hs" "haskell" source
    symbols <- getDocumentSymbols docId
    liftIO $ symbols @?= Left
      [ moduleSymbol
          "A"
          (R 0 7 0 8)
          [ classSymbol "A a"
                        (R 1 0 1 30)
                        [docSymbol' "a" SkMethod (R 1 16 1 30) (R 1 16 1 17)]
          ]
      ]
  , testSessionWait "type class instance " $ do
    let source = T.unlines ["class A a where", "instance A () where"]
    docId   <- createDoc "A.hs" "haskell" source
    symbols <- getDocumentSymbols docId
    liftIO $ symbols @?= Left
      [ classSymbol "A a" (R 0 0 0 15) []
      , docSymbol "A ()" SkInterface (R 1 0 1 19)
      ]
  , testSessionWait "type family" $ do
    let source = T.unlines ["{-# language TypeFamilies #-}", "type family A"]
    docId   <- createDoc "A.hs" "haskell" source
    symbols <- getDocumentSymbols docId
    liftIO $ symbols @?= Left [docSymbolD "A" "type family" SkFunction (R 1 0 1 13)]
  , testSessionWait "type family instance " $ do
    let source = T.unlines
          [ "{-# language TypeFamilies #-}"
          , "type family A a"
          , "type instance A () = ()"
          ]
    docId   <- createDoc "A.hs" "haskell" source
    symbols <- getDocumentSymbols docId
    liftIO $ symbols @?= Left
      [ docSymbolD "A a"   "type family" SkFunction     (R 1 0 1 15)
      , docSymbol "A ()" SkInterface (R 2 0 2 23)
      ]
  , testSessionWait "data family" $ do
    let source = T.unlines ["{-# language TypeFamilies #-}", "data family A"]
    docId   <- createDoc "A.hs" "haskell" source
    symbols <- getDocumentSymbols docId
    liftIO $ symbols @?= Left [docSymbolD "A" "data family" SkFunction (R 1 0 1 11)]
  , testSessionWait "data family instance " $ do
    let source = T.unlines
          [ "{-# language TypeFamilies #-}"
          , "data family A a"
          , "data instance A () = A ()"
          ]
    docId   <- createDoc "A.hs" "haskell" source
    symbols <- getDocumentSymbols docId
    liftIO $ symbols @?= Left
      [ docSymbolD "A a"   "data family" SkFunction     (R 1 0 1 11)
      , docSymbol "A ()" SkInterface (R 2 0 2 25)
      ]
  , testSessionWait "constant" $ do
    let source = T.unlines ["a = ()"]
    docId   <- createDoc "A.hs" "haskell" source
    symbols <- getDocumentSymbols docId
    liftIO $ symbols @?= Left
      [docSymbol "a" SkFunction (R 0 0 0 6)]
  , testSessionWait "pattern" $ do
    let source = T.unlines ["Just foo = Just 21"]
    docId   <- createDoc "A.hs" "haskell" source
    symbols <- getDocumentSymbols docId
    liftIO $ symbols @?= Left
      [docSymbol "Just foo" SkFunction (R 0 0 0 18)]
  , testSessionWait "pattern with type signature" $ do
    let source = T.unlines ["{-# language ScopedTypeVariables #-}", "a :: () = ()"]
    docId   <- createDoc "A.hs" "haskell" source
    symbols <- getDocumentSymbols docId
    liftIO $ symbols @?= Left
      [docSymbol "a :: ()" SkFunction (R 1 0 1 12)]
  , testSessionWait "function" $ do
    let source = T.unlines ["a _x = ()"]
    docId   <- createDoc "A.hs" "haskell" source
    symbols <- getDocumentSymbols docId
    liftIO $ symbols @?= Left [docSymbol "a" SkFunction (R 0 0 0 9)]
  , testSessionWait "type synonym" $ do
    let source = T.unlines ["type A = Bool"]
    docId   <- createDoc "A.hs" "haskell" source
    symbols <- getDocumentSymbols docId
    liftIO $ symbols @?= Left
      [docSymbol' "A" SkTypeParameter (R 0 0 0 13) (R 0 5 0 6)]
  , testSessionWait "datatype" $ do
    let source = T.unlines ["data A = C"]
    docId   <- createDoc "A.hs" "haskell" source
    symbols <- getDocumentSymbols docId
    liftIO $ symbols @?= Left
      [ docSymbolWithChildren "A"
                              SkStruct
                              (R 0 0 0 10)
                              [docSymbol "C" SkConstructor (R 0 9 0 10)]
      ]
  , testSessionWait "record fields" $ do
    let source = T.unlines ["data A = B {", "  x :: Int", "  , y :: Int}"]
    docId   <- createDoc "A.hs" "haskell" source
    symbols <- getDocumentSymbols docId
    liftIO $ symbols @?= Left
      [ docSymbolWithChildren "A" SkStruct (R 0 0 2 13)
          [ docSymbolWithChildren' "B" SkConstructor (R 0 9 2 13) (R 0 9 0 10)
            [ docSymbol "x" SkField (R 1 2 1 3)
            , docSymbol "y" SkField (R 2 4 2 5)
            ]
          ]
      ]
  , testSessionWait "import" $ do
    let source = T.unlines ["import Data.Maybe ()"]
    docId   <- createDoc "A.hs" "haskell" source
    symbols <- getDocumentSymbols docId
    liftIO $ symbols @?= Left
      [docSymbolWithChildren "imports"
                             SkModule
                             (R 0 0 0 20)
                             [ docSymbol "import Data.Maybe" SkModule (R 0 0 0 20)
                             ]
      ]
  , testSessionWait "multiple import" $ do
    let source = T.unlines ["", "import Data.Maybe ()", "", "import Control.Exception ()", ""]
    docId   <- createDoc "A.hs" "haskell" source
    symbols <- getDocumentSymbols docId
    liftIO $ symbols @?= Left
      [docSymbolWithChildren "imports"
                             SkModule
                             (R 1 0 3 27)
                             [ docSymbol "import Data.Maybe" SkModule (R 1 0 1 20)
                             , docSymbol "import Control.Exception" SkModule (R 3 0 3 27)
                             ]
      ]
  , testSessionWait "foreign import" $ do
    let source = T.unlines
          [ "{-# language ForeignFunctionInterface #-}"
          , "foreign import ccall \"a\" a :: Int"
          ]
    docId   <- createDoc "A.hs" "haskell" source
    symbols <- getDocumentSymbols docId
    liftIO $ symbols @?= Left [docSymbolD "a" "import" SkObject (R 1 0 1 33)]
  , testSessionWait "foreign export" $ do
    let source = T.unlines
          [ "{-# language ForeignFunctionInterface #-}"
          , "foreign export ccall odd :: Int -> Bool"
          ]
    docId   <- createDoc "A.hs" "haskell" source
    symbols <- getDocumentSymbols docId
    liftIO $ symbols @?= Left [docSymbolD "odd" "export" SkObject (R 1 0 1 39)]
  ]
 where
  docSymbol name kind loc =
    DocumentSymbol name Nothing kind Nothing Nothing loc loc Nothing
  docSymbol' name kind loc selectionLoc =
    DocumentSymbol name Nothing kind Nothing Nothing loc selectionLoc Nothing
  docSymbolD name detail kind loc =
    DocumentSymbol name (Just detail) kind Nothing Nothing loc loc Nothing
  docSymbolWithChildren name kind loc cc =
    DocumentSymbol name Nothing kind Nothing Nothing loc loc (Just $ List cc)
  docSymbolWithChildren' name kind loc selectionLoc cc =
    DocumentSymbol name Nothing kind Nothing Nothing loc selectionLoc (Just $ List cc)
  moduleSymbol name loc cc = DocumentSymbol name
                                            Nothing
                                            SkFile
                                            Nothing
                                            Nothing
                                            (R 0 0 maxBound 0)
                                            loc
                                            (Just $ List cc)
  classSymbol name loc cc = DocumentSymbol name
                                           (Just "class")
                                           SkInterface
                                           Nothing
                                           Nothing
                                           loc
                                           loc
                                           (Just $ List cc)

pattern R :: UInt -> UInt -> UInt -> UInt -> Range
pattern R x y x' y' = Range (Position x y) (Position x' y')

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

haddockTests :: TestTree
haddockTests
  = testGroup "haddock"
      [ testCase "Num" $ checkHaddock
          (unlines
             [ "However, '(+)' and '(*)' are"
             , "customarily expected to define a ring and have the following properties:"
             , ""
             , "[__Associativity of (+)__]: @(x + y) + z@ = @x + (y + z)@"
             , "[__Commutativity of (+)__]: @x + y@ = @y + x@"
             , "[__@fromInteger 0@ is the additive identity__]: @x + fromInteger 0@ = @x@"
             ]
          )
          (unlines
             [ ""
             , ""
             , "However,  `(+)`  and  `(*)`  are"
             , "customarily expected to define a ring and have the following properties: "
             , "+ ****Associativity of (+)****: `(x + y) + z`  =  `x + (y + z)`"
             , "+ ****Commutativity of (+)****: `x + y`  =  `y + x`"
             , "+ ****`fromInteger 0`  is the additive identity****: `x + fromInteger 0`  =  `x`"
             ]
          )
      , testCase "unsafePerformIO" $ checkHaddock
          (unlines
             [ "may require"
             , "different precautions:"
             , ""
             , "  * Use @{\\-\\# NOINLINE foo \\#-\\}@ as a pragma on any function @foo@"
             , "        that calls 'unsafePerformIO'.  If the call is inlined,"
             , "        the I\\/O may be performed more than once."
             , ""
             , "  * Use the compiler flag @-fno-cse@ to prevent common sub-expression"
             , "        elimination being performed on the module."
             , ""
             ]
          )
          (unlines
             [ ""
             , ""
             , "may require"
             , "different precautions: "
             , "+ Use  `{-# NOINLINE foo #-}`  as a pragma on any function  `foo` "
             , "  that calls  `unsafePerformIO` .  If the call is inlined,"
             , "  the I/O may be performed more than once."
             , ""
             , "+ Use the compiler flag  `-fno-cse`  to prevent common sub-expression"
             , "  elimination being performed on the module."
             , ""
             ]
          )
      , testCase "ordered list" $ checkHaddock
          (unlines
             [ "may require"
             , "different precautions:"
             , ""
             , "  1. Use @{\\-\\# NOINLINE foo \\#-\\}@ as a pragma on any function @foo@"
             , "        that calls 'unsafePerformIO'.  If the call is inlined,"
             , "        the I\\/O may be performed more than once."
             , ""
             , "  2. Use the compiler flag @-fno-cse@ to prevent common sub-expression"
             , "        elimination being performed on the module."
             , ""
             ]
          )
          (unlines
             [ ""
             , ""
             , "may require"
             , "different precautions: "
             , "1. Use  `{-# NOINLINE foo #-}`  as a pragma on any function  `foo` "
             , "  that calls  `unsafePerformIO` .  If the call is inlined,"
             , "  the I/O may be performed more than once."
             , ""
             , "2. Use the compiler flag  `-fno-cse`  to prevent common sub-expression"
             , "  elimination being performed on the module."
             , ""
             ]
          )
      ]
  where
    checkHaddock s txt = spanDocToMarkdownForTest s @?= txt

cradleTests :: TestTree
cradleTests = testGroup "cradle"
    [testGroup "dependencies" [sessionDepsArePickedUp]
    ,testGroup "ignore-fatal" [ignoreFatalWarning]
    ,testGroup "loading" [loadCradleOnlyonce, retryFailedCradle]
    ,testGroup "multi"   [simpleMultiTest, simpleMultiTest2, simpleMultiTest3, simpleMultiDefTest, simpleMultiUnitTest]
    ,testGroup "sub-directory"   [simpleSubDirectoryTest]
    ]

loadCradleOnlyonce :: TestTree
loadCradleOnlyonce = testGroup "load cradle only once"
    [ testSession' "implicit" implicit
    , testSession' "direct"   direct
    ]
    where
        direct dir = do
            liftIO $ writeFileUTF8 (dir </> "hie.yaml")
                "cradle: {direct: {arguments: []}}"
            test dir
        implicit dir = test dir
        test _dir = do
            doc <- createDoc "B.hs" "haskell" "module B where\nimport Data.Foo"
            msgs <- someTill (skipManyTill anyMessage cradleLoadedMessage) (skipManyTill anyMessage (message STextDocumentPublishDiagnostics))
            liftIO $ length msgs @?= 1
            changeDoc doc [TextDocumentContentChangeEvent Nothing Nothing "module B where\nimport Data.Maybe"]
            msgs <- manyTill (skipManyTill anyMessage cradleLoadedMessage) (skipManyTill anyMessage (message STextDocumentPublishDiagnostics))
            liftIO $ length msgs @?= 0
            _ <- createDoc "A.hs" "haskell" "module A where\nimport LoadCradleBar"
            msgs <- manyTill (skipManyTill anyMessage cradleLoadedMessage) (skipManyTill anyMessage (message STextDocumentPublishDiagnostics))
            liftIO $ length msgs @?= 0

retryFailedCradle :: TestTree
retryFailedCradle = testSession' "retry failed" $ \dir -> do
  -- The false cradle always fails
  let hieContents = "cradle: {bios: {shell: \"false\"}}"
      hiePath = dir </> "hie.yaml"
  liftIO $ writeFile hiePath hieContents
  let aPath = dir </> "A.hs"
  doc <- createDoc aPath "haskell" "main = return ()"
  WaitForIdeRuleResult {..} <- waitForAction "TypeCheck" doc
  liftIO $ "Test assumption failed: cradle should error out" `assertBool` not ideResultSuccess

  -- Fix the cradle and typecheck again
  let validCradle = "cradle: {bios: {shell: \"echo A.hs\"}}"
  liftIO $ writeFileUTF8 hiePath $ T.unpack validCradle
  sendNotification SWorkspaceDidChangeWatchedFiles $ DidChangeWatchedFilesParams $
          List [FileEvent (filePathToUri $ dir </> "hie.yaml") FcChanged ]

  WaitForIdeRuleResult {..} <- waitForAction "TypeCheck" doc
  liftIO $ "No joy after fixing the cradle" `assertBool` ideResultSuccess


dependentFileTest :: TestTree
dependentFileTest = testGroup "addDependentFile"
    [testGroup "file-changed" [testSession' "test" test]
    ]
    where
      test dir = do
        -- If the file contains B then no type error
        -- otherwise type error
        let depFilePath = dir </> "dep-file.txt"
        liftIO $ writeFile depFilePath "A"
        let fooContent = T.unlines
              [ "{-# LANGUAGE TemplateHaskell #-}"
              , "module Foo where"
              , "import Language.Haskell.TH.Syntax"
              , "foo :: Int"
              , "foo = 1 + $(do"
              , "               qAddDependentFile \"dep-file.txt\""
              , "               f <- qRunIO (readFile \"dep-file.txt\")"
              , "               if f == \"B\" then [| 1 |] else lift f)"
              ]
        let bazContent = T.unlines ["module Baz where", "import Foo ()"]
        _ <- createDoc "Foo.hs" "haskell" fooContent
        doc <- createDoc "Baz.hs" "haskell" bazContent
        expectDiagnostics $
            if ghcVersion >= GHC90
                -- String vs [Char] causes this change in error message
                then [("Foo.hs", [(DsError, if ghcVersion >= GHC92 then (4,11) else (4, 6), "Couldn't match type")])]
                else [("Foo.hs", [(DsError, (4, 6), "Couldn't match expected type")])]
        -- Now modify the dependent file
        liftIO $ writeFile depFilePath "B"
        sendNotification SWorkspaceDidChangeWatchedFiles $ DidChangeWatchedFilesParams $
          List [FileEvent (filePathToUri "dep-file.txt") FcChanged ]

        -- Modifying Baz will now trigger Foo to be rebuilt as well
        let change = TextDocumentContentChangeEvent
              { _range = Just (Range (Position 2 0) (Position 2 6))
              , _rangeLength = Nothing
              , _text = "f = ()"
              }
        changeDoc doc [change]
        expectDiagnostics [("Foo.hs", [])]


cradleLoadedMessage :: Session FromServerMessage
cradleLoadedMessage = satisfy $ \case
        FromServerMess (SCustomMethod m) (NotMess _) -> m == cradleLoadedMethod
        _                                            -> False

cradleLoadedMethod :: T.Text
cradleLoadedMethod = "ghcide/cradle/loaded"

ignoreFatalWarning :: TestTree
ignoreFatalWarning = testCase "ignore-fatal-warning" $ runWithExtraFiles "ignore-fatal" $ \dir -> do
    let srcPath = dir </> "IgnoreFatal.hs"
    src <- liftIO $ readFileUtf8 srcPath
    _ <- createDoc srcPath "haskell" src
    expectNoMoreDiagnostics 5

simpleSubDirectoryTest :: TestTree
simpleSubDirectoryTest =
  testCase "simple-subdirectory" $ runWithExtraFiles "cabal-exe" $ \dir -> do
    let mainPath = dir </> "a/src/Main.hs"
    mainSource <- liftIO $ readFileUtf8 mainPath
    _mdoc <- createDoc mainPath "haskell" mainSource
    expectDiagnosticsWithTags
      [("a/src/Main.hs", [(DsWarning,(2,0), "Top-level binding", Nothing)]) -- So that we know P has been loaded
      ]
    expectNoMoreDiagnostics 0.5

simpleMultiTest :: TestTree
simpleMultiTest = testCase "simple-multi-test" $ withLongTimeout $ runWithExtraFiles "multi" $ \dir -> do
    let aPath = dir </> "a/A.hs"
        bPath = dir </> "b/B.hs"
    adoc <- openDoc aPath "haskell"
    bdoc <- openDoc bPath "haskell"
    WaitForIdeRuleResult {..} <- waitForAction "TypeCheck" adoc
    liftIO $ assertBool "A should typecheck" ideResultSuccess
    WaitForIdeRuleResult {..} <- waitForAction "TypeCheck" bdoc
    liftIO $ assertBool "B should typecheck" ideResultSuccess
    locs <- getDefinitions bdoc (Position 2 7)
    let fooL = mkL (adoc ^. L.uri) 2 0 2 3
    checkDefs locs (pure [fooL])
    expectNoMoreDiagnostics 0.5

-- Test support for loading multiple components as -unit flags as
-- implemented in GHC 9.4
simpleMultiUnitTest :: TestTree
simpleMultiUnitTest = testCase "simple-multi-unit-test" $ withLongTimeout $ runWithExtraFiles "multi-unit" $ \dir -> do
    let aPath = dir </> "a/A.hs"
        bPath = dir </> "b/B.hs"
        cPath = dir </> "c/C.hs"
    bdoc <- openDoc bPath "haskell"
    WaitForIdeRuleResult {} <- waitForAction "TypeCheck" bdoc
    TextDocumentIdentifier auri <- openDoc aPath "haskell"
    skipManyTill anyMessage $ isReferenceReady aPath
    cdoc <- openDoc cPath "haskell"
    WaitForIdeRuleResult {} <- waitForAction "TypeCheck" cdoc
    locs <- getDefinitions cdoc (Position 2 7)
    let fooL = mkL auri 2 0 2 3
    checkDefs locs (pure [fooL])
    expectNoMoreDiagnostics 0.5

-- Like simpleMultiTest but open the files in the other order
simpleMultiTest2 :: TestTree
simpleMultiTest2 = testCase "simple-multi-test2" $ runWithExtraFiles "multi" $ \dir -> do
    let aPath = dir </> "a/A.hs"
        bPath = dir </> "b/B.hs"
    bdoc <- openDoc bPath "haskell"
    WaitForIdeRuleResult {} <- waitForAction "TypeCheck" bdoc
    TextDocumentIdentifier auri <- openDoc aPath "haskell"
    skipManyTill anyMessage $ isReferenceReady aPath
    locs <- getDefinitions bdoc (Position 2 7)
    let fooL = mkL auri 2 0 2 3
    checkDefs locs (pure [fooL])
    expectNoMoreDiagnostics 0.5

-- Now with 3 components
simpleMultiTest3 :: TestTree
simpleMultiTest3 =
  testCase "simple-multi-test3" $ runWithExtraFiles "multi" $ \dir -> do
    let aPath = dir </> "a/A.hs"
        bPath = dir </> "b/B.hs"
        cPath = dir </> "c/C.hs"
    bdoc <- openDoc bPath "haskell"
    WaitForIdeRuleResult {} <- waitForAction "TypeCheck" bdoc
    TextDocumentIdentifier auri <- openDoc aPath "haskell"
    skipManyTill anyMessage $ isReferenceReady aPath
    cdoc <- openDoc cPath "haskell"
    WaitForIdeRuleResult {} <- waitForAction "TypeCheck" cdoc
    locs <- getDefinitions cdoc (Position 2 7)
    let fooL = mkL auri 2 0 2 3
    checkDefs locs (pure [fooL])
    expectNoMoreDiagnostics 0.5

-- Like simpleMultiTest but open the files in component 'a' in a separate session
simpleMultiDefTest :: TestTree
simpleMultiDefTest = testCase "simple-multi-def-test" $ runWithExtraFiles "multi" $ \dir -> do
    let aPath = dir </> "a/A.hs"
        bPath = dir </> "b/B.hs"
    adoc <- liftIO $ runInDir dir $ do
      aSource <- liftIO $ readFileUtf8 aPath
      adoc <- createDoc aPath "haskell" aSource
      skipManyTill anyMessage $ isReferenceReady aPath
      closeDoc adoc
      pure adoc
    bSource <- liftIO $ readFileUtf8 bPath
    bdoc <- createDoc bPath "haskell" bSource
    locs <- getDefinitions bdoc (Position 2 7)
    let fooL = mkL (adoc ^. L.uri) 2 0 2 3
    checkDefs locs (pure [fooL])
    expectNoMoreDiagnostics 0.5

ifaceTests :: TestTree
ifaceTests = testGroup "Interface loading tests"
    [ -- https://github.com/haskell/ghcide/pull/645/
      ifaceErrorTest
    , ifaceErrorTest2
    , ifaceErrorTest3
    , ifaceTHTest
    ]

bootTests :: TestTree
bootTests = testGroup "boot"
  [ testCase "boot-def-test" $ runWithExtraFiles "boot" $ \dir -> do
        let cPath = dir </> "C.hs"
        cSource <- liftIO $ readFileUtf8 cPath
        -- Dirty the cache
        liftIO $ runInDir dir $ do
            cDoc <- createDoc cPath "haskell" cSource
            -- We send a hover request then wait for either the hover response or
            -- `ghcide/reference/ready` notification.
            -- Once we receive one of the above, we wait for the other that we
            -- haven't received yet.
            -- If we don't wait for the `ready` notification it is possible
            -- that the `getDefinitions` request/response in the outer ghcide
            -- session will find no definitions.
            let hoverParams = HoverParams cDoc (Position 4 3) Nothing
            hoverRequestId <- sendRequest STextDocumentHover hoverParams
            let parseReadyMessage = isReferenceReady cPath
            let parseHoverResponse = responseForId STextDocumentHover hoverRequestId
            hoverResponseOrReadyMessage <- skipManyTill anyMessage ((Left <$> parseHoverResponse) <|> (Right <$> parseReadyMessage))
            _ <- skipManyTill anyMessage $
              case hoverResponseOrReadyMessage of
                Left _  -> void parseReadyMessage
                Right _ -> void parseHoverResponse
            closeDoc cDoc
        cdoc <- createDoc cPath "haskell" cSource
        locs <- getDefinitions cdoc (Position 7 4)
        let floc = mkR 9 0 9 1
        checkDefs locs (pure [floc])
  , testCase "graph with boot modules" $ runWithExtraFiles "boot2" $ \dir -> do
      _ <- openDoc (dir </> "A.hs") "haskell"
      expectNoMoreDiagnostics 2
  ]

-- | test that TH reevaluates across interfaces
ifaceTHTest :: TestTree
ifaceTHTest = testCase "iface-th-test" $ runWithExtraFiles "TH" $ \dir -> do
    let aPath = dir </> "THA.hs"
        bPath = dir </> "THB.hs"
        cPath = dir </> "THC.hs"

    aSource <- liftIO $ readFileUtf8 aPath -- [TH] a :: ()
    _bSource <- liftIO $ readFileUtf8 bPath -- a :: ()
    cSource <- liftIO $ readFileUtf8 cPath -- c = a :: ()

    cdoc <- createDoc cPath "haskell" cSource

    -- Change [TH]a from () to Bool
    liftIO $ writeFileUTF8 aPath (unlines $ init (lines $ T.unpack aSource) ++ ["th_a = [d| a = False|]"])

    -- Check that the change propagates to C
    changeDoc cdoc [TextDocumentContentChangeEvent Nothing Nothing cSource]
    expectDiagnostics
      [("THC.hs", [(DsError, (4, 4), "Couldn't match expected type '()' with actual type 'Bool'")])
      ,("THB.hs", [(DsWarning, (4,thDollarIdx), "Top-level binding")])]
    closeDoc cdoc

ifaceErrorTest :: TestTree
ifaceErrorTest = testCase "iface-error-test-1" $ runWithExtraFiles "recomp" $ \dir -> do
    configureCheckProject True
    let bPath = dir </> "B.hs"
        pPath = dir </> "P.hs"

    bSource <- liftIO $ readFileUtf8 bPath -- y :: Int
    pSource <- liftIO $ readFileUtf8 pPath -- bar = x :: Int

    bdoc <- createDoc bPath "haskell" bSource
    expectDiagnostics
      [("P.hs", [(DsWarning,(4,0), "Top-level binding")])] -- So what we know P has been loaded

    -- Change y from Int to B
    changeDoc bdoc [TextDocumentContentChangeEvent Nothing Nothing $ T.unlines ["module B where", "y :: Bool", "y = undefined"]]
    -- save so that we can that the error propagates to A
    sendNotification STextDocumentDidSave (DidSaveTextDocumentParams bdoc Nothing)


    -- Check that the error propagates to A
    expectDiagnostics
      [("A.hs", [(DsError, (5, 4), "Couldn't match expected type 'Int' with actual type 'Bool'")])]

    -- Check that we wrote the interfaces for B when we saved
    hidir <- getInterfaceFilesDir bdoc
    hi_exists <- liftIO $ doesFileExist $ hidir </> "B.hi"
    liftIO $ assertBool ("Couldn't find B.hi in " ++ hidir) hi_exists

    pdoc <- openDoc pPath "haskell"
    expectDiagnostics
      [("P.hs", [(DsWarning,(4,0), "Top-level binding")])
      ]
    changeDoc pdoc [TextDocumentContentChangeEvent Nothing Nothing $ pSource <> "\nfoo = y :: Bool" ]
    -- Now in P we have
    -- bar = x :: Int
    -- foo = y :: Bool
    -- HOWEVER, in A...
    -- x = y  :: Int
    -- This is clearly inconsistent, and the expected outcome a bit surprising:
    --   - The diagnostic for A has already been received. Ghcide does not repeat diagnostics
    --   - P is being typechecked with the last successful artifacts for A.
    expectDiagnostics
      [("P.hs", [(DsWarning,(4,0), "Top-level binding")])
      ,("P.hs", [(DsWarning,(6,0), "Top-level binding")])
      ]
    expectNoMoreDiagnostics 2

ifaceErrorTest2 :: TestTree
ifaceErrorTest2 = testCase "iface-error-test-2" $ runWithExtraFiles "recomp" $ \dir -> do
    let bPath = dir </> "B.hs"
        pPath = dir </> "P.hs"

    bSource <- liftIO $ readFileUtf8 bPath -- y :: Int
    pSource <- liftIO $ readFileUtf8 pPath -- bar = x :: Int

    bdoc <- createDoc bPath "haskell" bSource
    pdoc <- createDoc pPath "haskell" pSource
    expectDiagnostics
      [("P.hs", [(DsWarning,(4,0), "Top-level binding")])] -- So that we know P has been loaded

    -- Change y from Int to B
    changeDoc bdoc [TextDocumentContentChangeEvent Nothing Nothing $ T.unlines ["module B where", "y :: Bool", "y = undefined"]]

    -- Add a new definition to P
    changeDoc pdoc [TextDocumentContentChangeEvent Nothing Nothing $ pSource <> "\nfoo = y :: Bool" ]
    -- Now in P we have
    -- bar = x :: Int
    -- foo = y :: Bool
    -- HOWEVER, in A...
    -- x = y  :: Int
    expectDiagnostics
    -- As in the other test, P is being typechecked with the last successful artifacts for A
    -- (ot thanks to -fdeferred-type-errors)
      [("A.hs", [(DsError, (5, 4), "Couldn't match expected type 'Int' with actual type 'Bool'")])
      ,("P.hs", [(DsWarning, (4, 0), "Top-level binding")])
      ,("P.hs", [(DsWarning, (6, 0), "Top-level binding")])
      ]

    expectNoMoreDiagnostics 2

ifaceErrorTest3 :: TestTree
ifaceErrorTest3 = testCase "iface-error-test-3" $ runWithExtraFiles "recomp" $ \dir -> do
    let bPath = dir </> "B.hs"
        pPath = dir </> "P.hs"

    bSource <- liftIO $ readFileUtf8 bPath -- y :: Int
    pSource <- liftIO $ readFileUtf8 pPath -- bar = x :: Int

    bdoc <- createDoc bPath "haskell" bSource

    -- Change y from Int to B
    changeDoc bdoc [TextDocumentContentChangeEvent Nothing Nothing $ T.unlines ["module B where", "y :: Bool", "y = undefined"]]

    -- P should not typecheck, as there are no last valid artifacts for A
    _pdoc <- createDoc pPath "haskell" pSource

    -- In this example the interface file for A should not exist (modulo the cache folder)
    -- Despite that P still type checks, as we can generate an interface file for A thanks to -fdeferred-type-errors
    expectDiagnostics
      [("A.hs", [(DsError, (5, 4), "Couldn't match expected type 'Int' with actual type 'Bool'")])
      ,("P.hs", [(DsWarning,(4,0), "Top-level binding")])
      ]
    expectNoMoreDiagnostics 2

sessionDepsArePickedUp :: TestTree
sessionDepsArePickedUp = testSession'
  "session-deps-are-picked-up"
  $ \dir -> do
    liftIO $
      writeFileUTF8
        (dir </> "hie.yaml")
        "cradle: {direct: {arguments: []}}"
    -- Open without OverloadedStrings and expect an error.
    doc <- createDoc "Foo.hs" "haskell" fooContent
    expectDiagnostics $
        if ghcVersion >= GHC90
            -- String vs [Char] causes this change in error message
            then [("Foo.hs", [(DsError, (3, 6), "Couldn't match type")])]
            else [("Foo.hs", [(DsError, (3, 6), "Couldn't match expected type")])]
    -- Update hie.yaml to enable OverloadedStrings.
    liftIO $
      writeFileUTF8
        (dir </> "hie.yaml")
        "cradle: {direct: {arguments: [-XOverloadedStrings]}}"
    sendNotification SWorkspaceDidChangeWatchedFiles $ DidChangeWatchedFilesParams $
          List [FileEvent (filePathToUri $ dir </> "hie.yaml") FcChanged ]
    -- Send change event.
    let change =
          TextDocumentContentChangeEvent
            { _range = Just (Range (Position 4 0) (Position 4 0)),
              _rangeLength = Nothing,
              _text = "\n"
            }
    changeDoc doc [change]
    -- Now no errors.
    expectDiagnostics [("Foo.hs", [])]
  where
    fooContent =
      T.unlines
        [ "module Foo where",
          "import Data.Text",
          "foo :: Text",
          "foo = \"hello\""
        ]

-- A test to ensure that the command line ghcide workflow stays working
nonLspCommandLine :: TestTree
nonLspCommandLine = testGroup "ghcide command line"
  [ testCase "works" $ withTempDir $ \dir -> do
        ghcide <- locateGhcideExecutable
        copyTestDataFiles dir "multi"
        let cmd = (proc ghcide ["a/A.hs"]){cwd = Just dir}

        setEnv "HOME" "/homeless-shelter" False

        (ec, _, _) <- readCreateProcessWithExitCode cmd ""

        ec @?= ExitSuccess
  ]

-- | checks if we use InitializeParams.rootUri for loading session
rootUriTests :: TestTree
rootUriTests = testCase "use rootUri" . runTest "dirA" "dirB" $ \dir -> do
  let bPath = dir </> "dirB/Foo.hs"
  liftIO $ copyTestDataFiles dir "rootUri"
  bSource <- liftIO $ readFileUtf8 bPath
  _ <- createDoc "Foo.hs" "haskell" bSource
  expectNoMoreDiagnostics 0.5
  where
    -- similar to run' except we can configure where to start ghcide and session
    runTest :: FilePath -> FilePath -> (FilePath -> Session ()) -> IO ()
    runTest dir1 dir2 s = withTempDir $ \dir -> runInDir' dir dir1 dir2 [] (s dir)

-- | Test if ghcide asynchronously handles Commands and user Requests
asyncTests :: TestTree
asyncTests = testGroup "async"
    [
      testSession "command" $ do
            -- Execute a command that will block forever
            let req = ExecuteCommandParams Nothing blockCommandId Nothing
            void $ sendRequest SWorkspaceExecuteCommand req
            -- Load a file and check for code actions. Will only work if the command is run asynchronously
            doc <- createDoc "A.hs" "haskell" $ T.unlines
              [ "{-# OPTIONS -Wmissing-signatures #-}"
              , "foo = id"
              ]
            void waitForDiagnostics
            codeLenses <- getCodeLenses doc
            liftIO $ [ _title | CodeLens{_command = Just Command{_title}} <- codeLenses] @=?
              [ "foo :: a -> a" ]
    , testSession "request" $ do
            -- Execute a custom request that will block for 1000 seconds
            void $ sendRequest (SCustomMethod "test") $ toJSON $ BlockSeconds 1000
            -- Load a file and check for code actions. Will only work if the request is run asynchronously
            doc <- createDoc "A.hs" "haskell" $ T.unlines
              [ "{-# OPTIONS -Wmissing-signatures #-}"
              , "foo = id"
              ]
            void waitForDiagnostics
            codeLenses <- getCodeLenses doc
            liftIO $ [ _title | CodeLens{_command = Just Command{_title}} <- codeLenses] @=?
              [ "foo :: a -> a" ]
    ]


clientSettingsTest :: TestTree
clientSettingsTest = testGroup "client settings handling"
    [ testSession "ghcide restarts shake session on config changes" $ do
            void $ skipManyTill anyMessage $ message SClientRegisterCapability
            void $ createDoc "A.hs" "haskell" "module A where"
            waitForProgressDone
            sendNotification SWorkspaceDidChangeConfiguration
                (DidChangeConfigurationParams (toJSON (mempty :: A.Object)))
            skipManyTill anyMessage restartingBuildSession

    ]
  where
    restartingBuildSession :: Session ()
    restartingBuildSession = do
        FromServerMess SWindowLogMessage NotificationMessage{_params = LogMessageParams{..}} <- loggingNotification
        guard $ "Restarting build session" `T.isInfixOf` _message

referenceTests :: TestTree
referenceTests = testGroup "references"
    [ testGroup "can get references to FOIs"
          [ referenceTest "can get references to symbols"
                          ("References.hs", 4, 7)
                          YesIncludeDeclaration
                          [ ("References.hs", 4, 6)
                          , ("References.hs", 6, 0)
                          , ("References.hs", 6, 14)
                          , ("References.hs", 9, 7)
                          , ("References.hs", 10, 11)
                          ]

          , referenceTest "can get references to data constructor"
                          ("References.hs", 13, 2)
                          YesIncludeDeclaration
                          [ ("References.hs", 13, 2)
                          , ("References.hs", 16, 14)
                          , ("References.hs", 19, 21)
                          ]

          , referenceTest "getting references works in the other module"
                          ("OtherModule.hs", 6, 0)
                          YesIncludeDeclaration
                          [ ("OtherModule.hs", 6, 0)
                          , ("OtherModule.hs", 8, 16)
                          ]

          , referenceTest "getting references works in the Main module"
                          ("Main.hs", 9, 0)
                          YesIncludeDeclaration
                          [ ("Main.hs", 9, 0)
                          , ("Main.hs", 10, 4)
                          ]

          , referenceTest "getting references to main works"
                          ("Main.hs", 5, 0)
                          YesIncludeDeclaration
                          [ ("Main.hs", 4, 0)
                          , ("Main.hs", 5, 0)
                          ]

          , referenceTest "can get type references"
                          ("Main.hs", 9, 9)
                          YesIncludeDeclaration
                          [ ("Main.hs", 9, 0)
                          , ("Main.hs", 9, 9)
                          , ("Main.hs", 10, 0)
                          ]

          , expectFailBecause "references provider does not respect includeDeclaration parameter" $
 referenceTest "works when we ask to exclude declarations"
                          ("References.hs", 4, 7)
                          NoExcludeDeclaration
                          [ ("References.hs", 6, 0)
                          , ("References.hs", 6, 14)
                          , ("References.hs", 9, 7)
                          , ("References.hs", 10, 11)
                          ]

          , referenceTest "INCORRECTLY returns declarations when we ask to exclude them"
                          ("References.hs", 4, 7)
                          NoExcludeDeclaration
                          [ ("References.hs", 4, 6)
                          , ("References.hs", 6, 0)
                          , ("References.hs", 6, 14)
                          , ("References.hs", 9, 7)
                          , ("References.hs", 10, 11)
                          ]
          ]

    , testGroup "can get references to non FOIs"
          [ referenceTest "can get references to symbol defined in a module we import"
                          ("References.hs", 22, 4)
                          YesIncludeDeclaration
                          [ ("References.hs", 22, 4)
                          , ("OtherModule.hs", 0, 20)
                          , ("OtherModule.hs", 4, 0)
                          ]

          , referenceTest "can get references in modules that import us to symbols we define"
                          ("OtherModule.hs", 4, 0)
                          YesIncludeDeclaration
                          [ ("References.hs", 22, 4)
                          , ("OtherModule.hs", 0, 20)
                          , ("OtherModule.hs", 4, 0)
                          ]

          , referenceTest "can get references to symbol defined in a module we import transitively"
                          ("References.hs", 24, 4)
                          YesIncludeDeclaration
                          [ ("References.hs", 24, 4)
                          , ("OtherModule.hs", 0, 48)
                          , ("OtherOtherModule.hs", 2, 0)
                          ]

          , referenceTest "can get references in modules that import us transitively to symbols we define"
                          ("OtherOtherModule.hs", 2, 0)
                          YesIncludeDeclaration
                          [ ("References.hs", 24, 4)
                          , ("OtherModule.hs", 0, 48)
                          , ("OtherOtherModule.hs", 2, 0)
                          ]

          , referenceTest "can get type references to other modules"
                          ("Main.hs", 12, 10)
                          YesIncludeDeclaration
                          [ ("Main.hs", 12, 7)
                          , ("Main.hs", 13, 0)
                          , ("References.hs", 12, 5)
                          , ("References.hs", 16, 0)
                          ]
          ]
    ]

-- | When we ask for all references to symbol "foo", should the declaration "foo
-- = 2" be among the references returned?
data IncludeDeclaration =
    YesIncludeDeclaration
    | NoExcludeDeclaration

getReferences' :: SymbolLocation -> IncludeDeclaration -> Session (List Location)
getReferences' (file, l, c) includeDeclaration = do
    doc <- openDoc file "haskell"
    getReferences doc (Position l c) $ toBool includeDeclaration
    where toBool YesIncludeDeclaration = True
          toBool NoExcludeDeclaration  = False

referenceTestSession :: String -> FilePath -> [FilePath] -> (FilePath -> Session ()) -> TestTree
referenceTestSession name thisDoc docs' f = testSessionWithExtraFiles "references" name $ \dir -> do
  -- needed to build whole project indexing
  configureCheckProject True
  let docs = map (dir </>) $ delete thisDoc $ nubOrd docs'
  -- Initial Index
  docid <- openDoc thisDoc "haskell"
  let
    loop :: [FilePath] -> Session ()
    loop [] = pure ()
    loop docs = do
      doc <- skipManyTill anyMessage $ referenceReady (`elem` docs)
      loop (delete doc docs)
  loop docs
  f dir
  closeDoc docid

-- | Given a location, lookup the symbol and all references to it. Make sure
-- they are the ones we expect.
referenceTest :: String -> SymbolLocation -> IncludeDeclaration -> [SymbolLocation] -> TestTree
referenceTest name loc includeDeclaration expected =
    referenceTestSession name (fst3 loc) docs $ \dir -> do
        List actual <- getReferences' loc includeDeclaration
        liftIO $ actual `expectSameLocations` map (first3 (dir </>)) expected
  where
    docs = map fst3 expected

type SymbolLocation = (FilePath, UInt, UInt)

expectSameLocations :: [Location] -> [SymbolLocation] -> Assertion
expectSameLocations actual expected = do
    let actual' =
            Set.map (\location -> (location ^. L.uri
                                   , location ^. L.range . L.start . L.line . to fromIntegral
                                   , location ^. L.range . L.start . L.character . to fromIntegral))
            $ Set.fromList actual
    expected' <- Set.fromList <$>
        (forM expected $ \(file, l, c) -> do
                              fp <- canonicalizePath file
                              return (filePathToUri fp, l, c))
    actual' @?= expected'

----------------------------------------------------------------------
-- Utils
----------------------------------------------------------------------

testSession :: String -> Session () -> TestTree
testSession name = testCase name . run

testSessionWithExtraFiles :: FilePath -> String -> (FilePath -> Session ()) -> TestTree
testSessionWithExtraFiles prefix name = testCase name . runWithExtraFiles prefix

testSession' :: String -> (FilePath -> Session ()) -> TestTree
testSession' name = testCase name . run'

testSessionWait :: HasCallStack => String -> Session () -> TestTree
testSessionWait name = testSession name .
      -- Check that any diagnostics produced were already consumed by the test case.
      --
      -- If in future we add test cases where we don't care about checking the diagnostics,
      -- this could move elsewhere.
      --
      -- Experimentally, 0.5s seems to be long enough to wait for any final diagnostics to appear.
      ( >> expectNoMoreDiagnostics 0.5)

mkRange :: UInt -> UInt -> UInt -> UInt -> Range
mkRange a b c d = Range (Position a b) (Position c d)

run :: Session a -> IO a
run s = run' (const s)

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

run' :: (FilePath -> Session a) -> IO a
run' s = withTempDir $ \dir -> runInDir dir (s dir)

runInDir :: FilePath -> Session a -> IO a
runInDir dir = runInDir' dir "." "." []

withLongTimeout :: IO a -> IO a
withLongTimeout = bracket_ (setEnv "LSP_TIMEOUT" "120" True) (unsetEnv "LSP_TIMEOUT")

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

lspTestCaps :: ClientCapabilities
lspTestCaps = fullCaps { _window = Just $ WindowClientCapabilities (Just True) Nothing Nothing }

lspTestCapsNoFileWatches :: ClientCapabilities
lspTestCapsNoFileWatches = lspTestCaps & workspace . Lens._Just . didChangeWatchedFiles .~ Nothing

openTestDataDoc :: FilePath -> Session TextDocumentIdentifier
openTestDataDoc path = do
  source <- liftIO $ readFileUtf8 $ "test/data" </> path
  createDoc path "haskell" source

unitTests :: Recorder (WithPriority Log) -> Logger -> TestTree
unitTests recorder logger = do
  testGroup "Unit"
     [ testCase "empty file path does NOT work with the empty String literal" $
         uriToFilePath' (fromNormalizedUri $ filePathToUri' "") @?= Just "."
     , testCase "empty file path works using toNormalizedFilePath'" $
         uriToFilePath' (fromNormalizedUri $ filePathToUri' (toNormalizedFilePath' "")) @?= Just ""
     , testCase "empty path URI" $ do
         Just URI{..} <- pure $ parseURI (T.unpack $ getUri $ fromNormalizedUri emptyPathUri)
         uriScheme @?= "file:"
         uriPath @?= ""
     , testCase "from empty path URI" $ do
         let uri = Uri "file://"
         uriToFilePath' uri @?= Just ""
     , testCase "showDiagnostics prints ranges 1-based (like vscode)" $ do
         let diag = ("", Diagnostics.ShowDiag, Diagnostic
               { _range = Range
                   { _start = Position{_line = 0, _character = 1}
                   , _end = Position{_line = 2, _character = 3}
                   }
               , _severity = Nothing
               , _code = Nothing
               , _source = Nothing
               , _message = ""
               , _relatedInformation = Nothing
               , _tags = Nothing
               })
         let shown = T.unpack (Diagnostics.showDiagnostics [diag])
         let expected = "1:2-3:4"
         assertBool (unwords ["expected to find range", expected, "in diagnostic", shown]) $
             expected `isInfixOf` shown
     , testCase "notification handlers run in priority order" $ do
        orderRef <- newIORef []
        let plugins = pluginDescToIdePlugins $
                [ (priorityPluginDescriptor i)
                    { pluginNotificationHandlers = mconcat
                        [ mkPluginNotificationHandler LSP.STextDocumentDidOpen $ \_ _ _ _ ->
                            liftIO $ atomicModifyIORef_ orderRef (i:)
                        ]
                    }
                    | i <- [1..20]
                ] ++ Ghcide.descriptors (cmapWithPrio LogGhcIde recorder)
            priorityPluginDescriptor i = (defaultPluginDescriptor $ fromString $ show i){pluginPriority = i}

        testIde recorder (IDE.testing (cmapWithPrio LogIDEMain recorder) logger plugins) $ do
            _ <- createDoc "A.hs" "haskell" "module A where"
            waitForProgressDone
            actualOrder <- liftIO $ reverse <$> readIORef orderRef

            -- Handlers are run in priority descending order
            liftIO $ actualOrder @?= [20, 19 .. 1]
     , ignoreTestBecause "The test fails sometimes showing 10000us" $
         testCase "timestamps have millisecond resolution" $ do
           resolution_us <- findResolution_us 1
           let msg = printf "Timestamps do not have millisecond resolution: %dus" resolution_us
           assertBool msg (resolution_us <= 1000)
     , Progress.tests
     , FuzzySearch.tests
     ]

garbageCollectionTests :: TestTree
garbageCollectionTests = testGroup "garbage collection"
  [ testGroup "dirty keys"
        [ testSession' "are collected" $ \dir -> do
            liftIO $ writeFile (dir </> "hie.yaml") "cradle: {direct: {arguments: [A]}}"
            doc <- generateGarbage "A" dir
            closeDoc doc
            garbage <- waitForGC
            liftIO $ assertBool "no garbage was found" $ not $ null garbage

        , testSession' "are deleted from the state" $ \dir -> do
            liftIO $ writeFile (dir </> "hie.yaml") "cradle: {direct: {arguments: [A]}}"
            docA <- generateGarbage "A" dir
            keys0 <- getStoredKeys
            closeDoc docA
            garbage <- waitForGC
            liftIO $ assertBool "something is wrong with this test - no garbage found" $ not $ null garbage
            keys1 <- getStoredKeys
            liftIO $ assertBool "keys were not deleted from the state" (length keys1 < length keys0)

        , testSession' "are not regenerated unless needed" $ \dir -> do
            liftIO $ writeFile (dir </> "hie.yaml") "cradle: {direct: {arguments: [A.hs, B.hs]}}"
            docA <- generateGarbage "A" dir
            _docB <- generateGarbage "B" dir

            -- garbage collect A keys
            keysBeforeGC <- getStoredKeys
            closeDoc docA
            garbage <- waitForGC
            liftIO $ assertBool "something is wrong with this test - no garbage found" $ not $ null garbage
            keysAfterGC <- getStoredKeys
            liftIO $ assertBool "something is wrong with this test - keys were not deleted from the state"
                (length keysAfterGC < length keysBeforeGC)

            -- re-typecheck B and check that the keys for A have not materialized back
            _docB <- generateGarbage "B" dir
            keysB <- getStoredKeys
            let regeneratedKeys = Set.filter (not . isExpected) $
                    Set.intersection (Set.fromList garbage) (Set.fromList keysB)
            liftIO $ regeneratedKeys @?= mempty

        , testSession' "regenerate successfully" $ \dir -> do
            liftIO $ writeFile (dir </> "hie.yaml") "cradle: {direct: {arguments: [A]}}"
            docA <- generateGarbage "A" dir
            closeDoc docA
            garbage <- waitForGC
            liftIO $ assertBool "no garbage was found" $ not $ null garbage
            let edit = T.unlines
                        [ "module A where"
                        , "a :: Bool"
                        , "a = ()"
                        ]
            doc <- generateGarbage "A" dir
            changeDoc doc [TextDocumentContentChangeEvent Nothing Nothing edit]
            builds <- waitForTypecheck doc
            liftIO $ assertBool "it still builds" builds
            expectCurrentDiagnostics doc [(DsError, (2,4), "Couldn't match expected type")]
        ]
  ]
  where
    isExpected k = any (`T.isPrefixOf` k) ["GhcSessionIO"]

    generateGarbage :: String -> FilePath -> Session TextDocumentIdentifier
    generateGarbage modName dir = do
        let fp = modName <> ".hs"
            body = printf "module %s where" modName
        doc <- createDoc fp "haskell" (T.pack body)
        liftIO $ writeFile (dir </> fp) body
        builds <- waitForTypecheck doc
        liftIO $ assertBool "something is wrong with this test" builds
        return doc

findResolution_us :: Int -> IO Int
findResolution_us delay_us | delay_us >= 1000000 = error "Unable to compute timestamp resolution"
findResolution_us delay_us = withTempFile $ \f -> withTempFile $ \f' -> do
    performGC
    writeFile f ""
    threadDelay delay_us
    writeFile f' ""
    t <- getModTime f
    t' <- getModTime f'
    if t /= t' then return delay_us else findResolution_us (delay_us * 10)


testIde :: Recorder (WithPriority Log) -> IDE.Arguments -> Session () -> IO ()
testIde recorder arguments session = do
    config <- getConfigFromEnv
    cwd <- getCurrentDirectory
    (hInRead, hInWrite) <- createPipe
    (hOutRead, hOutWrite) <- createPipe
    let projDir = "."
    let server = IDE.defaultMain (cmapWithPrio LogIDEMain recorder) arguments
            { IDE.argsHandleIn = pure hInRead
            , IDE.argsHandleOut = pure hOutWrite
            }

    flip finally (setCurrentDirectory cwd) $ withAsync server $ \_ ->
        runSessionWithHandles hInWrite hOutRead config lspTestCaps projDir session

positionMappingTests :: Recorder (WithPriority Log) -> TestTree
positionMappingTests recorder =
    testGroup "position mapping"
        [ testGroup "toCurrent"
              [ testCase "before" $
                toCurrent
                    (Range (Position 0 1) (Position 0 3))
                    "ab"
                    (Position 0 0) @?= PositionExact (Position 0 0)
              , testCase "after, same line, same length" $
                toCurrent
                    (Range (Position 0 1) (Position 0 3))
                    "ab"
                    (Position 0 3) @?= PositionExact (Position 0 3)
              , testCase "after, same line, increased length" $
                toCurrent
                    (Range (Position 0 1) (Position 0 3))
                    "abc"
                    (Position 0 3) @?= PositionExact (Position 0 4)
              , testCase "after, same line, decreased length" $
                toCurrent
                    (Range (Position 0 1) (Position 0 3))
                    "a"
                    (Position 0 3) @?= PositionExact (Position 0 2)
              , testCase "after, next line, no newline" $
                toCurrent
                    (Range (Position 0 1) (Position 0 3))
                    "abc"
                    (Position 1 3) @?= PositionExact (Position 1 3)
              , testCase "after, next line, newline" $
                toCurrent
                    (Range (Position 0 1) (Position 0 3))
                    "abc\ndef"
                    (Position 1 0) @?= PositionExact (Position 2 0)
              , testCase "after, same line, newline" $
                toCurrent
                    (Range (Position 0 1) (Position 0 3))
                    "abc\nd"
                    (Position 0 4) @?= PositionExact (Position 1 2)
              , testCase "after, same line, newline + newline at end" $
                toCurrent
                    (Range (Position 0 1) (Position 0 3))
                    "abc\nd\n"
                    (Position 0 4) @?= PositionExact (Position 2 1)
              , testCase "after, same line, newline + newline at end" $
                toCurrent
                    (Range (Position 0 1) (Position 0 1))
                    "abc"
                    (Position 0 1) @?= PositionExact (Position 0 4)
              ]
        , testGroup "fromCurrent"
              [ testCase "before" $
                fromCurrent
                    (Range (Position 0 1) (Position 0 3))
                    "ab"
                    (Position 0 0) @?= PositionExact (Position 0 0)
              , testCase "after, same line, same length" $
                fromCurrent
                    (Range (Position 0 1) (Position 0 3))
                    "ab"
                    (Position 0 3) @?= PositionExact (Position 0 3)
              , testCase "after, same line, increased length" $
                fromCurrent
                    (Range (Position 0 1) (Position 0 3))
                    "abc"
                    (Position 0 4) @?= PositionExact (Position 0 3)
              , testCase "after, same line, decreased length" $
                fromCurrent
                    (Range (Position 0 1) (Position 0 3))
                    "a"
                    (Position 0 2) @?= PositionExact (Position 0 3)
              , testCase "after, next line, no newline" $
                fromCurrent
                    (Range (Position 0 1) (Position 0 3))
                    "abc"
                    (Position 1 3) @?= PositionExact (Position 1 3)
              , testCase "after, next line, newline" $
                fromCurrent
                    (Range (Position 0 1) (Position 0 3))
                    "abc\ndef"
                    (Position 2 0) @?= PositionExact (Position 1 0)
              , testCase "after, same line, newline" $
                fromCurrent
                    (Range (Position 0 1) (Position 0 3))
                    "abc\nd"
                    (Position 1 2) @?= PositionExact (Position 0 4)
              , testCase "after, same line, newline + newline at end" $
                fromCurrent
                    (Range (Position 0 1) (Position 0 3))
                    "abc\nd\n"
                    (Position 2 1) @?= PositionExact (Position 0 4)
              , testCase "after, same line, newline + newline at end" $
                fromCurrent
                    (Range (Position 0 1) (Position 0 1))
                    "abc"
                    (Position 0 4) @?= PositionExact (Position 0 1)
              ]
        , adjustOption (\(QuickCheckTests i) -> QuickCheckTests (max 1000 i)) $ testGroup "properties"
              [ testProperty "fromCurrent r t <=< toCurrent r t" $ do
                -- Note that it is important to use suchThatMap on all values at once
                -- instead of only using it on the position. Otherwise you can get
                -- into situations where there is no position that can be mapped back
                -- for the edit which will result in QuickCheck looping forever.
                let gen = do
                        rope <- genRope
                        range <- genRange rope
                        PrintableText replacement <- arbitrary
                        oldPos <- genPosition rope
                        pure (range, replacement, oldPos)
                forAll
                    (suchThatMap gen
                        (\(range, replacement, oldPos) -> positionResultToMaybe $ (range, replacement, oldPos,) <$> toCurrent range replacement oldPos)) $
                    \(range, replacement, oldPos, newPos) ->
                    fromCurrent range replacement newPos === PositionExact oldPos
              , testProperty "toCurrent r t <=< fromCurrent r t" $ do
                let gen = do
                        rope <- genRope
                        range <- genRange rope
                        PrintableText replacement <- arbitrary
                        let newRope = runIdentity $ applyChange mempty rope
                                (TextDocumentContentChangeEvent (Just range) Nothing replacement)
                        newPos <- genPosition newRope
                        pure (range, replacement, newPos)
                forAll
                    (suchThatMap gen
                        (\(range, replacement, newPos) -> positionResultToMaybe $ (range, replacement, newPos,) <$> fromCurrent range replacement newPos)) $
                    \(range, replacement, newPos, oldPos) ->
                    toCurrent range replacement oldPos === PositionExact newPos
              ]
        ]

newtype PrintableText = PrintableText { getPrintableText :: T.Text }
    deriving Show

instance Arbitrary PrintableText where
    arbitrary = PrintableText . T.pack . getPrintableString <$> arbitrary


genRope :: Gen Rope
genRope = Rope.fromText . getPrintableText <$> arbitrary

genPosition :: Rope -> Gen Position
genPosition r = do
    let rows :: Int = fromIntegral $ Rope.lengthInLines r
    row <- choose (0, max 0 $ rows - 1) `suchThat` inBounds @UInt
    let columns = T.length (nthLine (fromIntegral row) r)
    column <- choose (0, max 0 $ columns - 1) `suchThat` inBounds @UInt
    pure $ Position (fromIntegral row) (fromIntegral column)

genRange :: Rope -> Gen Range
genRange r = do
    let rows :: Int = fromIntegral $ Rope.lengthInLines r
    startPos@(Position startLine startColumn) <- genPosition r
    let maxLineDiff = max 0 $ rows - 1 - fromIntegral startLine
    endLine <- choose (fromIntegral startLine, fromIntegral startLine + maxLineDiff) `suchThat` inBounds @UInt
    let columns = T.length (nthLine (fromIntegral endLine) r)
    endColumn <-
        if fromIntegral startLine == endLine
            then choose (fromIntegral startColumn, columns)
            else choose (0, max 0 $ columns - 1)
        `suchThat` inBounds @UInt
    pure $ Range startPos (Position (fromIntegral endLine) (fromIntegral endColumn))

inBounds :: forall b a . (Integral a, Integral b, Bounded b) => a -> Bool
inBounds a = let i = toInteger a in i <= toInteger (maxBound @b) && i >= toInteger (minBound @b)

-- | Get the ith line of a rope, starting from 0. Trailing newline not included.
nthLine :: Int -> Rope -> T.Text
nthLine i r
    | Rope.null r = ""
    | otherwise = Rope.lines r !! i

getWatchedFilesSubscriptionsUntil :: forall m. SServerMethod m -> Session [DidChangeWatchedFilesRegistrationOptions]
getWatchedFilesSubscriptionsUntil m = do
      msgs <- manyTill (Just <$> message SClientRegisterCapability <|> Nothing <$ anyMessage) (message m)
      return
            [ args
            | Just RequestMessage{_params = RegistrationParams (List regs)} <- msgs
            , SomeRegistration (Registration _id SWorkspaceDidChangeWatchedFiles args) <- regs
            ]

-- | Version of 'System.IO.Extra.withTempDir' that canonicalizes the path
-- Which we need to do on macOS since the $TMPDIR can be in @/private/var@ or
-- @/var@
withTempDir :: (FilePath -> IO a) -> IO a
withTempDir f = System.IO.Extra.withTempDir $ \dir -> do
  dir' <- canonicalizePath dir
  f dir'


-- | Before ghc9, lists of Char is displayed as [Char], but with ghc9 and up, it's displayed as String
listOfChar :: T.Text
listOfChar | ghcVersion >= GHC90 = "String"
           | otherwise = "[Char]"

-- | Ghc 9 doesn't include the $-sign in TH warnings like earlier versions did
thDollarIdx :: UInt
thDollarIdx | ghcVersion >= GHC90 = 1
            | otherwise = 0
