-- Copyright (c) 2019 The DAML Authors. All rights reserved.
-- SPDX-License-Identifier: Apache-2.0

{-# LANGUAGE AllowAmbiguousTypes   #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE ImplicitParams  #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE CPP #-}
#include "ghc-api-version.h"

module Main (main) where

import Control.Applicative.Combinators
import Control.Exception (catch)
import qualified Control.Lens as Lens
import Control.Monad
import Control.Monad.IO.Class (liftIO)
import Data.Aeson (FromJSON, Value, toJSON)
import qualified Data.Binary as Binary
import Data.Foldable
import Data.List.Extra
import Data.Maybe
import Data.Rope.UTF16 (Rope)
import qualified Data.Rope.UTF16 as Rope
import Development.IDE.Core.PositionMapping (fromCurrent, toCurrent, PositionResult(..), positionResultToMaybe)
import Development.IDE.Core.Shake (Q(..))
import Development.IDE.GHC.Util
import qualified Data.Text as T
import Data.Typeable
import Development.IDE.Spans.Common
import Development.IDE.Test
import Development.IDE.Test.Runfiles
import qualified Development.IDE.Types.Diagnostics as Diagnostics
import Development.IDE.Types.Location
import Development.Shake (getDirectoryFilesIO)
import qualified Experiments as Bench
import Language.Haskell.LSP.Test
import Language.Haskell.LSP.Messages
import Language.Haskell.LSP.Types
import Language.Haskell.LSP.Types.Capabilities
import qualified Language.Haskell.LSP.Types.Lens as Lsp (diagnostics, params, message)
import Language.Haskell.LSP.VFS (applyChange)
import Network.URI
import System.Environment.Blank (getEnv, setEnv)
import System.FilePath
import System.IO.Extra hiding (withTempDir)
import qualified System.IO.Extra
import System.Directory
import System.Exit (ExitCode(ExitSuccess))
import System.Process.Extra (readCreateProcessWithExitCode, CreateProcess(cwd), proc)
import System.Info.Extra (isWindows)
import Test.QuickCheck
import Test.QuickCheck.Instances ()
import Test.Tasty
import Test.Tasty.ExpectedFailure
import Test.Tasty.Ingredients.Rerun
import Test.Tasty.HUnit
import Test.Tasty.QuickCheck
import System.Time.Extra
import Development.IDE.Plugin.CodeAction (typeSignatureCommandId, blockCommandId, matchRegExMultipleImports)
import Development.IDE.Plugin.Test (WaitForIdeRuleResult(..), TestRequest(WaitForIdeRule, BlockSeconds,GetInterfaceFilesDir))
import Control.Monad.Extra (whenJust)

main :: IO ()
main = do
  -- We mess with env vars so run single-threaded.
  defaultMainWithRerun $ testGroup "ghcide"
    [ testSession "open close" $ do
        doc <- createDoc "Testing.hs" "haskell" ""
        void (skipManyTill anyMessage message :: Session WorkDoneProgressCreateRequest)
        void (skipManyTill anyMessage message :: Session WorkDoneProgressBeginNotification)
        closeDoc doc
        void (skipManyTill anyMessage message :: Session WorkDoneProgressEndNotification)
    , initializeResponseTests
    , completionTests
    , cppTests
    , diagnosticTests
    , codeActionTests
    , codeLensesTests
    , outlineTests
    , highlightTests
    , findDefinitionAndHoverTests
    , pluginSimpleTests
    , pluginParsedResultTests
    , preprocessorTests
    , thTests
    , safeTests
    , unitTests
    , haddockTests
    , positionMappingTests
    , watchedFilesTests
    , cradleTests
    , dependentFileTest
    , nonLspCommandLine
    , benchmarkTests
    , ifaceTests
    , bootTests
    , rootUriTests
    , asyncTests
    , clientSettingsTest
    , codeActionHelperFunctionTests
    ]

initializeResponseTests :: TestTree
initializeResponseTests = withResource acquire release tests where

  -- these tests document and monitor the evolution of the
  -- capabilities announced by the server in the initialize
  -- response. Currently the server advertises almost no capabilities
  -- at all, in some cases failing to announce capabilities that it
  -- actually does provide! Hopefully this will change ...
  tests :: IO InitializeResponse -> TestTree
  tests getInitializeResponse =
    testGroup "initialize response capabilities"
    [ chk "   text doc sync"             _textDocumentSync  tds
    , chk "   hover"                         _hoverProvider (Just True)
    , chk "   completion"               _completionProvider (Just $ CompletionOptions (Just False) (Just ["."]) Nothing)
    , chk "NO signature help"        _signatureHelpProvider  Nothing
    , chk "   goto definition"          _definitionProvider (Just True)
    , chk "   goto type definition" _typeDefinitionProvider (Just $ GotoOptionsStatic True)
    -- BUG in lsp-test, this test fails, just change the accepted response
    -- for now
    , chk "NO goto implementation"  _implementationProvider (Just $ GotoOptionsStatic True)
    , chk "NO find references"          _referencesProvider  Nothing
    , chk "   doc highlight"     _documentHighlightProvider  (Just True)
    , chk "   doc symbol"           _documentSymbolProvider  (Just True)
    , chk "NO workspace symbol"    _workspaceSymbolProvider  Nothing
    , chk "   code action"             _codeActionProvider $ Just $ CodeActionOptionsStatic True
    , chk "   code lens"                 _codeLensProvider $ Just $ CodeLensOptions Nothing
    , chk "NO doc formatting"   _documentFormattingProvider  Nothing
    , chk "NO doc range formatting"
                           _documentRangeFormattingProvider  Nothing
    , chk "NO doc formatting on typing"
                          _documentOnTypeFormattingProvider  Nothing
    , chk "NO renaming"                     _renameProvider (Just $ RenameOptionsStatic False)
    , chk "NO doc link"               _documentLinkProvider  Nothing
    , chk "NO color"                         _colorProvider (Just $ ColorOptionsStatic False)
    , chk "NO folding range"          _foldingRangeProvider (Just $ FoldingRangeOptionsStatic False)
    , che "   execute command"      _executeCommandProvider (Just $ ExecuteCommandOptions $ List [typeSignatureCommandId, blockCommandId])
    , chk "   workspace"                         _workspace (Just $ WorkspaceOptions (Just WorkspaceFolderOptions{_supported = Just True, _changeNotifications = Just ( WorkspaceFolderChangeNotificationsBool True )}))
    , chk "NO experimental"                   _experimental  Nothing
    ] where

      tds = Just (TDSOptions (TextDocumentSyncOptions
                              { _openClose = Just True
                              , _change    = Just TdSyncIncremental
                              , _willSave  = Nothing
                              , _willSaveWaitUntil = Nothing
                              , _save = Just (SaveOptions {_includeText = Nothing})}))

      chk :: (Eq a, Show a) => TestName -> (InitializeResponseCapabilitiesInner -> a) -> a -> TestTree
      chk title getActual expected =
        testCase title $ getInitializeResponse >>= \ir -> expected @=? (getActual . innerCaps) ir

      che :: TestName -> (InitializeResponseCapabilitiesInner -> Maybe ExecuteCommandOptions) -> Maybe ExecuteCommandOptions -> TestTree
      che title getActual _expected = testCase title doTest
        where
            doTest = do
                ir <- getInitializeResponse
                let Just ExecuteCommandOptions {_commands = List [command]} = getActual $ innerCaps ir
                True @=? T.isSuffixOf "typesignature.add" command


  innerCaps :: InitializeResponse -> InitializeResponseCapabilitiesInner
  innerCaps (ResponseMessage _ _ (Right (InitializeResponseCapabilities c))) = c
  innerCaps  _ = error "this test only expects inner capabilities"

  acquire :: IO InitializeResponse
  acquire = run initializeResponse

  release :: InitializeResponse -> IO ()
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
      void $ skipManyTill anyMessage (message :: Session WorkDoneProgressCreateRequest)
      void $ skipManyTill anyMessage (message :: Session WorkDoneProgressBeginNotification)
      let change = TextDocumentContentChangeEvent
            { _range = Just (Range (Position 0 15) (Position 0 18))
            , _rangeLength = Nothing
            , _text = "wher"
            }
      changeDoc doc [change]
      expectDiagnostics [("Testing.hs", [(DsError, (0, 15), "parse error")])]
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
  , ignoreInWindowsBecause "Broken in windows" $ testSessionWait "add missing module (non workspace)" $ do
      -- need to canonicalize in Mac Os
      tmpDir <- liftIO $ canonicalizePath =<< getTemporaryDirectory
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
            ]
      _ <- createDoc "Data/List.hs" "haskell" thisDataListContent
      _ <- createDoc "Main.hs" "haskell" mainContent
      expectDiagnostics
        [ ( "Main.hs"
          , [(DsError, (6, 9), "Not in scope: \8216ThisList.map\8217")
            ,(DsError, (7, 9), "Not in scope: \8216BaseList.x\8217")
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
      -- sometihng like 'GHC.Classes.Ord'. The choice of redundant-constraints to
      -- test this is fairly arbitrary.
          , [(DsWarning, (2, 0), "Redundant constraint: Ord a")
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
          sendNotification TextDocumentDidOpen (DidOpenTextDocumentParams itemA)
          diagsNot <- skipManyTill anyMessage diagnostic
          let PublishDiagnosticsParams fileUri diags = _params (diagsNot :: PublishDiagnosticsNotification)
          -- Check that if we put a lower-case drive in for A.A
          -- the diagnostics for A.B will also be lower-case.
          liftIO $ fileUri @?= uriB
          let msg = _message (head (toList diags) :: Diagnostic)
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
      expectDiagnostics
        [ ( "Foo.hs"
          , [(DsWarning, (2, 8), "Haddock parse error on input")
            ]
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

    bSource <- liftIO $ readFileUtf8 bPath -- y :: Int
    pSource <- liftIO $ readFileUtf8 pPath -- bar = x :: Int

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
    expectNoMoreDiagnostics 2

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
    [ cancellationTestGroup "edit header" editHeader yesDepends yesSession noParse  noTc
    , cancellationTestGroup "edit import" editImport noDepends  noSession  yesParse noTc
    , cancellationTestGroup "edit body"   editBody   yesDepends yesSession yesParse yesTc
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

      noDepends = False
      yesDepends = True

      noSession = False
      yesSession = True

      noTc = False
      yesTc = True

cancellationTestGroup :: TestName -> (TextDocumentContentChangeEvent, TextDocumentContentChangeEvent) -> Bool -> Bool -> Bool -> Bool -> TestTree
cancellationTestGroup name edits dependsOutcome sessionDepsOutcome parseOutcome tcOutcome = testGroup name
    [ cancellationTemplate edits Nothing
    , cancellationTemplate edits $ Just ("GetFileContents", True)
    , cancellationTemplate edits $ Just ("GhcSession", True)
      -- the outcome for GetModSummary is always True because parseModuleHeader never fails (!)
    , cancellationTemplate edits $ Just ("GetModSummary", True)
    , cancellationTemplate edits $ Just ("GetModSummaryWithoutTimestamps", True)
      -- getLocatedImports never fails
    , cancellationTemplate edits $ Just ("GetLocatedImports", True)
    , cancellationTemplate edits $ Just ("GetDependencies", dependsOutcome)
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
        Right WaitForIdeRuleResult{ideResultSuccess} <- waitForAction key doc
        liftIO $ ideResultSuccess @?= expectedResult

      -- The 2nd edit cancels the active session and unbreaks the file
      -- wait for typecheck and check that the current diagnostics are accurate
      changeDoc doc [undoEdit]
      typeCheck doc >> expectCurrentDiagnostics doc missingSigDiags

      expectNoMoreDiagnostics 0.5
    where
        -- similar to run except it disables kick
        runTestNoKick s = withTempDir $ \dir -> runInDir' dir "." "." ["--test-no-kick"] s

        waitForAction key TextDocumentIdentifier{_uri} = do
            waitId <- sendRequest (CustomClientMethod "test") (WaitForIdeRule key _uri)
            ResponseMessage{_result} <- skipManyTill anyMessage $ responseForId waitId
            return _result

        typeCheck doc = do
            Right WaitForIdeRuleResult {..} <- waitForAction "TypeCheck" doc
            liftIO $ assertBool "The file should typecheck" ideResultSuccess
            -- wait for the debouncer to publish diagnostics if the rule runs
            liftIO $ sleep 0.2
            -- flush messages to ensure current diagnostics state is updated
            flushMessages

codeActionTests :: TestTree
codeActionTests = testGroup "code actions"
  [ renameActionTests
  , typeWildCardActionTests
  , removeImportTests
  , extendImportTests
  , suggestImportTests
  , fixConstructorImportTests
  , importRenameActionTests
  , fillTypedHoleTests
  , addSigActionTests
  , insertNewDefinitionTests
  , deleteUnusedDefinitionTests
  , addInstanceConstraintTests
  , addFunctionConstraintTests
  , removeRedundantConstraintsTests
  , addTypeAnnotationsToLiteralsTest
  , exportUnusedTests
  ]

codeActionHelperFunctionTests :: TestTree
codeActionHelperFunctionTests = testGroup "code action helpers"
    [
    extendImportTestsRegEx
    ]


codeLensesTests :: TestTree
codeLensesTests = testGroup "code lenses"
  [ addSigLensesTests
  ]

watchedFilesTests :: TestTree
watchedFilesTests = testGroup "watched files"
  [ testSession' "workspace files" $ \sessionDir -> do
      liftIO $ writeFile (sessionDir </> "hie.yaml") "cradle: {direct: {arguments: [\"-isrc\", \"A\", \"WatchedFilesMissingModule\"]}}"
      _doc <- createDoc "A.hs" "haskell" "{-#LANGUAGE NoImplicitPrelude #-}\nmodule A where\nimport WatchedFilesMissingModule"
      watchedFileRegs <- getWatchedFilesSubscriptionsUntil @PublishDiagnosticsNotification

      -- Expect 1 subscription: we only ever send one
      liftIO $ length watchedFileRegs @?= 1

  , testSession' "non workspace file" $ \sessionDir -> do
      tmpDir <- liftIO getTemporaryDirectory
      liftIO $ writeFile (sessionDir </> "hie.yaml") ("cradle: {direct: {arguments: [\"-i" <> tmpDir <> "\", \"A\", \"WatchedFilesMissingModule\"]}}")
      _doc <- createDoc "A.hs" "haskell" "{-# LANGUAGE NoImplicitPrelude#-}\nmodule A where\nimport WatchedFilesMissingModule"
      watchedFileRegs <- getWatchedFilesSubscriptionsUntil @PublishDiagnosticsNotification

      -- Expect 1 subscription: we only ever send one
      liftIO $ length watchedFileRegs @?= 1

  -- TODO add a test for didChangeWorkspaceFolder
  ]

renameActionTests :: TestTree
renameActionTests = testGroup "rename actions"
  [ testSession "change to local variable name" $ do
      let content = T.unlines
            [ "module Testing where"
            , "foo :: Int -> Int"
            , "foo argName = argNme"
            ]
      doc <- createDoc "Testing.hs" "haskell" content
      _ <- waitForDiagnostics
      action <- findCodeAction doc (Range (Position 2 14) (Position 2 20)) "Replace with ‘argName’"
      executeCodeAction action
      contentAfterAction <- documentContents doc
      let expectedContentAfterAction = T.unlines
            [ "module Testing where"
            , "foo :: Int -> Int"
            , "foo argName = argName"
            ]
      liftIO $ expectedContentAfterAction @=? contentAfterAction
  , testSession "change to name of imported function" $ do
      let content = T.unlines
            [ "module Testing where"
            , "import Data.Maybe (maybeToList)"
            , "foo :: Maybe a -> [a]"
            , "foo = maybToList"
            ]
      doc <- createDoc "Testing.hs" "haskell" content
      _ <- waitForDiagnostics
      action <- findCodeAction doc (Range (Position 3 6) (Position 3 16))  "Replace with ‘maybeToList’"
      executeCodeAction action
      contentAfterAction <- documentContents doc
      let expectedContentAfterAction = T.unlines
            [ "module Testing where"
            , "import Data.Maybe (maybeToList)"
            , "foo :: Maybe a -> [a]"
            , "foo = maybeToList"
            ]
      liftIO $ expectedContentAfterAction @=? contentAfterAction
  , testSession "suggest multiple local variable names" $ do
      let content = T.unlines
            [ "module Testing where"
            , "foo :: Char -> Char -> Char -> Char"
            , "foo argument1 argument2 argument3 = argumentX"
            ]
      doc <- createDoc "Testing.hs" "haskell" content
      _ <- waitForDiagnostics
      _ <- findCodeActions doc (Range (Position 2 36) (Position 2 45))
                           ["Replace with ‘argument1’", "Replace with ‘argument2’", "Replace with ‘argument3’"]
      return()
  , testSession "change infix function" $ do
      let content = T.unlines
            [ "module Testing where"
            , "monus :: Int -> Int"
            , "monus x y = max 0 (x - y)"
            , "foo x y = x `monnus` y"
            ]
      doc <- createDoc "Testing.hs" "haskell" content
      _ <- waitForDiagnostics
      actionsOrCommands <- getCodeActions doc (Range (Position 3 12) (Position 3 20))
      [fixTypo] <- pure [action | CACodeAction action@CodeAction{ _title = actionTitle } <- actionsOrCommands, "monus" `T.isInfixOf` actionTitle ]
      executeCodeAction fixTypo
      contentAfterAction <- documentContents doc
      let expectedContentAfterAction = T.unlines
            [ "module Testing where"
            , "monus :: Int -> Int"
            , "monus x y = max 0 (x - y)"
            , "foo x y = x `monus` y"
            ]
      liftIO $ expectedContentAfterAction @=? contentAfterAction
  ]

typeWildCardActionTests :: TestTree
typeWildCardActionTests = testGroup "type wildcard actions"
  [ testSession "global signature" $ do
      let content = T.unlines
            [ "module Testing where"
            , "func :: _"
            , "func x = x"
            ]
      doc <- createDoc "Testing.hs" "haskell" content
      _ <- waitForDiagnostics
      actionsOrCommands <- getCodeActions doc (Range (Position 2 1) (Position 2 10))
      let [addSignature] = [action | CACodeAction action@CodeAction { _title = actionTitle } <- actionsOrCommands
                                   , "Use type signature" `T.isInfixOf` actionTitle
                           ]
      executeCodeAction addSignature
      contentAfterAction <- documentContents doc
      let expectedContentAfterAction = T.unlines
            [ "module Testing where"
            , "func :: (p -> p)"
            , "func x = x"
            ]
      liftIO $ expectedContentAfterAction @=? contentAfterAction
  , testSession "multi-line message" $ do
      let content = T.unlines
            [ "module Testing where"
            , "func :: _"
            , "func x y = x + y"
            ]
      doc <- createDoc "Testing.hs" "haskell" content
      _ <- waitForDiagnostics
      actionsOrCommands <- getCodeActions doc (Range (Position 2 1) (Position 2 10))
      let [addSignature] = [action | CACodeAction action@CodeAction { _title = actionTitle } <- actionsOrCommands
                                    , "Use type signature" `T.isInfixOf` actionTitle
                              ]
      executeCodeAction addSignature
      contentAfterAction <- documentContents doc
      let expectedContentAfterAction = T.unlines
            [ "module Testing where"
            , "func :: (Integer -> Integer -> Integer)"
            , "func x y = x + y"
            ]
      liftIO $ expectedContentAfterAction @=? contentAfterAction
  , testSession "local signature" $ do
      let content = T.unlines
            [ "module Testing where"
            , "func :: Int -> Int"
            , "func x ="
            , "  let y :: _"
            , "      y = x * 2"
            , "  in y"
            ]
      doc <- createDoc "Testing.hs" "haskell" content
      _ <- waitForDiagnostics
      actionsOrCommands <- getCodeActions doc (Range (Position 4 1) (Position 4 10))
      let [addSignature] = [action | CACodeAction action@CodeAction { _title = actionTitle } <- actionsOrCommands
                                    , "Use type signature" `T.isInfixOf` actionTitle
                              ]
      executeCodeAction addSignature
      contentAfterAction <- documentContents doc
      let expectedContentAfterAction = T.unlines
            [ "module Testing where"
            , "func :: Int -> Int"
            , "func x ="
            , "  let y :: (Int)"
            , "      y = x * 2"
            , "  in y"
            ]
      liftIO $ expectedContentAfterAction @=? contentAfterAction
  ]

removeImportTests :: TestTree
removeImportTests = testGroup "remove import actions"
  [ testSession "redundant" $ do
      let contentA = T.unlines
            [ "module ModuleA where"
            ]
      _docA <- createDoc "ModuleA.hs" "haskell" contentA
      let contentB = T.unlines
            [ "{-# OPTIONS_GHC -Wunused-imports #-}"
            , "module ModuleB where"
            , "import ModuleA"
            , "stuffB :: Integer"
            , "stuffB = 123"
            ]
      docB <- createDoc "ModuleB.hs" "haskell" contentB
      _ <- waitForDiagnostics
      [CACodeAction action@CodeAction { _title = actionTitle }, _]
          <- getCodeActions docB (Range (Position 2 0) (Position 2 5))
      liftIO $ "Remove import" @=? actionTitle
      executeCodeAction action
      contentAfterAction <- documentContents docB
      let expectedContentAfterAction = T.unlines
            [ "{-# OPTIONS_GHC -Wunused-imports #-}"
            , "module ModuleB where"
            , "stuffB :: Integer"
            , "stuffB = 123"
            ]
      liftIO $ expectedContentAfterAction @=? contentAfterAction
  , testSession "qualified redundant" $ do
      let contentA = T.unlines
            [ "module ModuleA where"
            ]
      _docA <- createDoc "ModuleA.hs" "haskell" contentA
      let contentB = T.unlines
            [ "{-# OPTIONS_GHC -Wunused-imports #-}"
            , "module ModuleB where"
            , "import qualified ModuleA"
            , "stuffB :: Integer"
            , "stuffB = 123"
            ]
      docB <- createDoc "ModuleB.hs" "haskell" contentB
      _ <- waitForDiagnostics
      [CACodeAction action@CodeAction { _title = actionTitle }, _]
          <- getCodeActions docB (Range (Position 2 0) (Position 2 5))
      liftIO $ "Remove import" @=? actionTitle
      executeCodeAction action
      contentAfterAction <- documentContents docB
      let expectedContentAfterAction = T.unlines
            [ "{-# OPTIONS_GHC -Wunused-imports #-}"
            , "module ModuleB where"
            , "stuffB :: Integer"
            , "stuffB = 123"
            ]
      liftIO $ expectedContentAfterAction @=? contentAfterAction
  , testSession "redundant binding" $ do
      let contentA = T.unlines
            [ "module ModuleA where"
            , "stuffA = False"
            , "stuffB :: Integer"
            , "stuffB = 123"
            , "stuffC = ()"
            ]
      _docA <- createDoc "ModuleA.hs" "haskell" contentA
      let contentB = T.unlines
            [ "{-# OPTIONS_GHC -Wunused-imports #-}"
            , "module ModuleB where"
            , "import ModuleA (stuffA, stuffB, stuffC, stuffA)"
            , "main = print stuffB"
            ]
      docB <- createDoc "ModuleB.hs" "haskell" contentB
      _ <- waitForDiagnostics
      [CACodeAction action@CodeAction { _title = actionTitle }, _]
          <- getCodeActions docB (Range (Position 2 0) (Position 2 5))
      liftIO $ "Remove stuffA, stuffC from import" @=? actionTitle
      executeCodeAction action
      contentAfterAction <- documentContents docB
      let expectedContentAfterAction = T.unlines
            [ "{-# OPTIONS_GHC -Wunused-imports #-}"
            , "module ModuleB where"
            , "import ModuleA (stuffB)"
            , "main = print stuffB"
            ]
      liftIO $ expectedContentAfterAction @=? contentAfterAction
  , testSession "redundant operator" $ do
      let contentA = T.unlines
            [ "module ModuleA where"
            , "a !! _b = a"
            , "a <?> _b = a"
            , "stuffB :: Integer"
            , "stuffB = 123"
            ]
      _docA <- createDoc "ModuleA.hs" "haskell" contentA
      let contentB = T.unlines
            [ "{-# OPTIONS_GHC -Wunused-imports #-}"
            , "module ModuleB where"
            , "import qualified ModuleA as A ((<?>), stuffB, (!!))"
            , "main = print A.stuffB"
            ]
      docB <- createDoc "ModuleB.hs" "haskell" contentB
      _ <- waitForDiagnostics
      [CACodeAction action@CodeAction { _title = actionTitle }, _]
          <- getCodeActions docB (Range (Position 2 0) (Position 2 5))
      liftIO $ "Remove !!, <?> from import" @=? actionTitle
      executeCodeAction action
      contentAfterAction <- documentContents docB
      let expectedContentAfterAction = T.unlines
            [ "{-# OPTIONS_GHC -Wunused-imports #-}"
            , "module ModuleB where"
            , "import qualified ModuleA as A (stuffB)"
            , "main = print A.stuffB"
            ]
      liftIO $ expectedContentAfterAction @=? contentAfterAction
  , testSession "redundant all import" $ do
      let contentA = T.unlines
            [ "module ModuleA where"
            , "data A = A"
            , "stuffB :: Integer"
            , "stuffB = 123"
            ]
      _docA <- createDoc "ModuleA.hs" "haskell" contentA
      let contentB = T.unlines
            [ "{-# OPTIONS_GHC -Wunused-imports #-}"
            , "module ModuleB where"
            , "import ModuleA (A(..), stuffB)"
            , "main = print stuffB"
            ]
      docB <- createDoc "ModuleB.hs" "haskell" contentB
      _ <- waitForDiagnostics
      [CACodeAction action@CodeAction { _title = actionTitle }, _]
          <- getCodeActions docB (Range (Position 2 0) (Position 2 5))
      liftIO $ "Remove A from import" @=? actionTitle
      executeCodeAction action
      contentAfterAction <- documentContents docB
      let expectedContentAfterAction = T.unlines
            [ "{-# OPTIONS_GHC -Wunused-imports #-}"
            , "module ModuleB where"
            , "import ModuleA (stuffB)"
            , "main = print stuffB"
            ]
      liftIO $ expectedContentAfterAction @=? contentAfterAction
  , testSession "redundant constructor import" $ do
      let contentA = T.unlines
            [ "module ModuleA where"
            , "data D = A | B"
            , "data E = F"
            ]
      _docA <- createDoc "ModuleA.hs" "haskell" contentA
      let contentB = T.unlines
            [ "{-# OPTIONS_GHC -Wunused-imports #-}"
            , "module ModuleB where"
            , "import ModuleA (D(A,B), E(F))"
            , "main = B"
            ]
      docB <- createDoc "ModuleB.hs" "haskell" contentB
      _ <- waitForDiagnostics
      [CACodeAction action@CodeAction { _title = actionTitle }, _]
          <- getCodeActions docB (Range (Position 2 0) (Position 2 5))
      liftIO $ "Remove A, E, F from import" @=? actionTitle
      executeCodeAction action
      contentAfterAction <- documentContents docB
      let expectedContentAfterAction = T.unlines
            [ "{-# OPTIONS_GHC -Wunused-imports #-}"
            , "module ModuleB where"
            , "import ModuleA (D(B))"
            , "main = B"
            ]
      liftIO $ expectedContentAfterAction @=? contentAfterAction
  , testSession "import containing the identifier Strict" $ do
      let contentA = T.unlines
            [ "module Strict where"
            ]
      _docA <- createDoc "Strict.hs" "haskell" contentA
      let contentB = T.unlines
            [ "{-# OPTIONS_GHC -Wunused-imports #-}"
            , "module ModuleB where"
            , "import Strict"
            ]
      docB <- createDoc "ModuleB.hs" "haskell" contentB
      _ <- waitForDiagnostics
      [CACodeAction action@CodeAction { _title = actionTitle }, _]
          <- getCodeActions docB (Range (Position 2 0) (Position 2 5))
      liftIO $ "Remove import" @=? actionTitle
      executeCodeAction action
      contentAfterAction <- documentContents docB
      let expectedContentAfterAction = T.unlines
            [ "{-# OPTIONS_GHC -Wunused-imports #-}"
            , "module ModuleB where"
            ]
      liftIO $ expectedContentAfterAction @=? contentAfterAction
  , testSession "remove all" $ do
      let content = T.unlines
            [ "{-# OPTIONS_GHC -Wunused-imports #-}"
            , "module ModuleA where"
            , "import Data.Function (fix, (&))"
            , "import qualified Data.Functor.Const"
            , "import Data.Functor.Identity"
            , "import Data.Functor.Sum (Sum (InL, InR))"
            , "import qualified Data.Kind as K (Constraint, Type)"
            , "x = InL (Identity 123)"
            , "y = fix id"
            , "type T = K.Type"
            ]
      doc <- createDoc "ModuleC.hs" "haskell" content
      _ <- waitForDiagnostics
      [_, _, _, _, CACodeAction action@CodeAction { _title = actionTitle }]
          <- getCodeActions doc (Range (Position 2 0) (Position 2 5))
      liftIO $ "Remove all redundant imports" @=? actionTitle
      executeCodeAction action
      contentAfterAction <- documentContents doc
      let expectedContentAfterAction = T.unlines
            [ "{-# OPTIONS_GHC -Wunused-imports #-}"
            , "module ModuleA where"
            , "import Data.Function (fix)"
            , "import Data.Functor.Identity"
            , "import Data.Functor.Sum (Sum (InL))"
            , "import qualified Data.Kind as K (Type)"
            , "x = InL (Identity 123)"
            , "y = fix id"
            , "type T = K.Type"
            ]
      liftIO $ expectedContentAfterAction @=? contentAfterAction
  ]

extendImportTests :: TestTree
extendImportTests = testGroup "extend import actions"
  [ testSession "extend single line import with value" $ template
      [("ModuleA.hs", T.unlines
            [ "module ModuleA where"
            , "stuffA :: Double"
            , "stuffA = 0.00750"
            , "stuffB :: Integer"
            , "stuffB = 123"
            ])]
      ("ModuleB.hs", T.unlines
            [ "module ModuleB where"
            , "import ModuleA as A (stuffB)"
            , "main = print (stuffA, stuffB)"
            ])
      (Range (Position 3 17) (Position 3 18))
      ["Add stuffA to the import list of ModuleA"]
      (T.unlines
            [ "module ModuleB where"
            , "import ModuleA as A (stuffA, stuffB)"
            , "main = print (stuffA, stuffB)"
            ])
  , testSession "extend single line import with operator" $ template
      [("ModuleA.hs", T.unlines
            [ "module ModuleA where"
            , "(.*) :: Integer -> Integer -> Integer"
            , "x .* y = x * y"
            , "stuffB :: Integer"
            , "stuffB = 123"
            ])]
      ("ModuleB.hs", T.unlines
            [ "module ModuleB where"
            , "import ModuleA as A (stuffB)"
            , "main = print (stuffB .* stuffB)"
            ])
      (Range (Position 3 17) (Position 3 18))
      ["Add (.*) to the import list of ModuleA"]
      (T.unlines
            [ "module ModuleB where"
            , "import ModuleA as A ((.*), stuffB)"
            , "main = print (stuffB .* stuffB)"
            ])
  , testSession "extend single line import with type" $ template
      [("ModuleA.hs", T.unlines
            [ "module ModuleA where"
            , "type A = Double"
            ])]
      ("ModuleB.hs", T.unlines
            [ "module ModuleB where"
            , "import ModuleA ()"
            , "b :: A"
            , "b = 0"
            ])
      (Range (Position 2 5) (Position 2 5))
      ["Add A to the import list of ModuleA"]
      (T.unlines
            [ "module ModuleB where"
            , "import ModuleA (A)"
            , "b :: A"
            , "b = 0"
            ])
  , testSession "extend single line import with constructor" $ template
      [("ModuleA.hs", T.unlines
            [ "module ModuleA where"
            , "data A = Constructor"
            ])]
      ("ModuleB.hs", T.unlines
            [ "module ModuleB where"
            , "import ModuleA (A)"
            , "b :: A"
            , "b = Constructor"
            ])
      (Range (Position 2 5) (Position 2 5))
      ["Add A(Constructor) to the import list of ModuleA"]
      (T.unlines
            [ "module ModuleB where"
            , "import ModuleA (A(Constructor))"
            , "b :: A"
            , "b = Constructor"
            ])
  , testSession "extend single line import with mixed constructors" $ template
      [("ModuleA.hs", T.unlines
            [ "module ModuleA where"
            , "data A = ConstructorFoo | ConstructorBar"
            , "a = 1"
            ])]
      ("ModuleB.hs", T.unlines
            [ "module ModuleB where"
            , "import ModuleA (A(ConstructorBar), a)"
            , "b :: A"
            , "b = ConstructorFoo"
            ])
      (Range (Position 2 5) (Position 2 5))
      ["Add A(ConstructorFoo) to the import list of ModuleA"]
      (T.unlines
            [ "module ModuleB where"
            , "import ModuleA (A(ConstructorFoo, ConstructorBar), a)"
            , "b :: A"
            , "b = ConstructorFoo"
            ])
  , testSession "extend single line qualified import with value" $ template
      [("ModuleA.hs", T.unlines
            [ "module ModuleA where"
            , "stuffA :: Double"
            , "stuffA = 0.00750"
            , "stuffB :: Integer"
            , "stuffB = 123"
            ])]
      ("ModuleB.hs", T.unlines
            [ "module ModuleB where"
            , "import qualified ModuleA as A (stuffB)"
            , "main = print (A.stuffA, A.stuffB)"
            ])
      (Range (Position 3 17) (Position 3 18))
      ["Add stuffA to the import list of ModuleA"]
      (T.unlines
            [ "module ModuleB where"
            , "import qualified ModuleA as A (stuffA, stuffB)"
            , "main = print (A.stuffA, A.stuffB)"
            ])
  , testSession "extend multi line import with value" $ template
      [("ModuleA.hs", T.unlines
            [ "module ModuleA where"
            , "stuffA :: Double"
            , "stuffA = 0.00750"
            , "stuffB :: Integer"
            , "stuffB = 123"
            ])]
      ("ModuleB.hs", T.unlines
            [ "module ModuleB where"
            , "import ModuleA (stuffB"
            , "               )"
            , "main = print (stuffA, stuffB)"
            ])
      (Range (Position 3 17) (Position 3 18))
      ["Add stuffA to the import list of ModuleA"]
      (T.unlines
            [ "module ModuleB where"
            , "import ModuleA (stuffA, stuffB"
            , "               )"
            , "main = print (stuffA, stuffB)"
            ])
  , testSession "extend import list with multiple choices" $ template
      [("ModuleA.hs", T.unlines
            --  this is just a dummy module to help the arguments needed for this test
            [  "module ModuleA (bar) where"
             , "bar = 10"
               ]),
      ("ModuleB.hs", T.unlines
            --  this is just a dummy module to help the arguments needed for this test
            [  "module ModuleB (bar) where"
             , "bar = 10"
               ])]
      ("ModuleC.hs", T.unlines
            [ "module ModuleC where"
            , "import ModuleB ()"
            , "import ModuleA ()"
            , "foo = bar"
            ])
      (Range (Position 3 17) (Position 3 18))
      ["Add bar to the import list of ModuleA",
       "Add bar to the import list of ModuleB"]
      (T.unlines
            [ "module ModuleC where"
            , "import ModuleB ()"
            , "import ModuleA (bar)"
            , "foo = bar"
            ])
  ]
  where
    template setUpModules moduleUnderTest range expectedActions expectedContentB = do
      mapM_ (\x -> createDoc (fst x) "haskell" (snd x)) setUpModules
      docB <- createDoc (fst moduleUnderTest) "haskell" (snd moduleUnderTest)
      _  <- waitForDiagnostics
      void (skipManyTill anyMessage message :: Session WorkDoneProgressEndNotification)
      codeActions <- filter (\(CACodeAction CodeAction{_title=x}) -> T.isPrefixOf "Add" x)
          <$>  getCodeActions docB range
      let expectedTitles = (\(CACodeAction CodeAction{_title=x}) ->x) <$> codeActions
      liftIO $ expectedActions @=? expectedTitles

      -- Get the first action and execute the first action
      let CACodeAction action :  _
                  = sortOn (\(CACodeAction CodeAction{_title=x}) -> x) codeActions
      executeCodeAction action
      contentAfterAction <- documentContents docB
      liftIO $ expectedContentB @=? contentAfterAction

extendImportTestsRegEx :: TestTree
extendImportTestsRegEx = testGroup "regex parsing"
    [
      testCase "parse invalid multiple imports" $ template "foo bar foo" Nothing
    , testCase "parse malformed import list" $ template
                  "\n\8226 Perhaps you want to add \8216fromList\8217 to one of these import lists:\n    \8216Data.Map\8217)"
                  Nothing
    , testCase "parse multiple imports" $ template
                 "\n\8226 Perhaps you want to add \8216fromList\8217 to one of these import lists:\n    \8216Data.Map\8217 (app/testlsp.hs:7:1-18)\n    \8216Data.HashMap.Strict\8217 (app/testlsp.hs:8:1-29)"
                 $ Just ("fromList",[("Data.Map","app/testlsp.hs:7:1-18"),("Data.HashMap.Strict","app/testlsp.hs:8:1-29")])
    ]
    where
        template message expected = do
            liftIO $ matchRegExMultipleImports message @=? expected



suggestImportTests :: TestTree
suggestImportTests = testGroup "suggest import actions"
  [ testGroup "Dont want suggestion"
    [ -- extend import
      test False ["Data.List.NonEmpty ()"] "f = nonEmpty" []                "import Data.List.NonEmpty (nonEmpty)"
      -- data constructor
    , test False []                        "f = First"    []                "import Data.Monoid (First)"
      -- internal module
    , test False []         "f :: Typeable a => a"        ["f = undefined"] "import Data.Typeable.Internal (Typeable)"
      -- package not in scope
    , test False []         "f = quickCheck"              []                "import Test.QuickCheck (quickCheck)"
    ]
  , testGroup "want suggestion"
    [ wantWait  []          "f = foo"                     []                "import Foo (foo)"
    , wantWait  []          "f = Bar"                     []                "import Bar (Bar(Bar))"
    , wantWait  []          "f :: Bar"                    []                "import Bar (Bar)"
    , test True []          "f = nonEmpty"                []                "import Data.List.NonEmpty (nonEmpty)"
    , test True []          "f = (:|)"                    []                "import Data.List.NonEmpty (NonEmpty((:|)))"
    , test True []          "f :: Natural"                ["f = undefined"] "import Numeric.Natural (Natural)"
    , test True []          "f :: Natural"                ["f = undefined"] "import Numeric.Natural"
    , test True []          "f :: NonEmpty ()"            ["f = () :| []"]  "import Data.List.NonEmpty (NonEmpty)"
    , test True []          "f :: NonEmpty ()"            ["f = () :| []"]  "import Data.List.NonEmpty"
    , test True []          "f = First"                   []                "import Data.Monoid (First(First))"
    , test True []          "f = Endo"                    []                "import Data.Monoid (Endo(Endo))"
    , test True []          "f = Version"                 []                "import Data.Version (Version(Version))"
    , test True []          "f ExitSuccess = ()"          []                "import System.Exit (ExitCode(ExitSuccess))"
    , test True []          "f = AssertionFailed"         []                "import Control.Exception (AssertionFailed(AssertionFailed))"
    , test True ["Prelude"] "f = nonEmpty"                []                "import Data.List.NonEmpty (nonEmpty)"
    , test True []          "f :: Alternative f => f ()"  ["f = undefined"] "import Control.Applicative (Alternative)"
    , test True []          "f :: Alternative f => f ()"  ["f = undefined"] "import Control.Applicative"
    , test True []          "f = empty"                   []                "import Control.Applicative (Alternative(empty))"
    , test True []          "f = empty"                   []                "import Control.Applicative"
    , test True []          "f = (&)"                     []                "import Data.Function ((&))"
    , test True []          "f = NE.nonEmpty"             []                "import qualified Data.List.NonEmpty as NE"
    , test True []          "f = Data.List.NonEmpty.nonEmpty" []            "import qualified Data.List.NonEmpty"
    , test True []          "f :: Typeable a => a"        ["f = undefined"] "import Data.Typeable (Typeable)"
    , test True []          "f = pack"                    []                "import Data.Text (pack)"
    , test True []          "f :: Text"                   ["f = undefined"] "import Data.Text (Text)"
    , test True []          "f = [] & id"                 []                "import Data.Function ((&))"
    , test True []          "f = (&) [] id"               []                "import Data.Function ((&))"
    , test True []          "f = (.|.)"                   []                "import Data.Bits (Bits((.|.)))"
    ]
  ]
  where
    test = test' False
    wantWait = test' True True
    test' waitForCheckProject wanted imps def other newImp = testSessionWithExtraFiles "hover" (T.unpack def) $ \dir -> do
      let before = T.unlines $ "module A where" : ["import " <> x | x <- imps] ++ def : other
          after  = T.unlines $ "module A where" : ["import " <> x | x <- imps] ++ [newImp] ++ def : other
          cradle = "cradle: {direct: {arguments: [-hide-all-packages, -package, base, -package, text, -package-env, -, A, Bar, Foo]}}"
      liftIO $ writeFileUTF8 (dir </> "hie.yaml") cradle
      doc <- createDoc "Test.hs" "haskell" before
      void (skipManyTill anyMessage message :: Session WorkDoneProgressEndNotification)
      _diags <- waitForDiagnostics
      -- there isn't a good way to wait until the whole project is checked atm
      when waitForCheckProject $ liftIO $ sleep 0.5
      let defLine = length imps + 1
          range = Range (Position defLine 0) (Position defLine maxBound)
      actions <- getCodeActions doc range
      if wanted
         then do
             action <- liftIO $ pickActionWithTitle newImp actions
             executeCodeAction action
             contentAfterAction <- documentContents doc
             liftIO $ after @=? contentAfterAction
          else
              liftIO $ [_title | CACodeAction CodeAction{_title} <- actions, _title == newImp ] @?= []

insertNewDefinitionTests :: TestTree
insertNewDefinitionTests = testGroup "insert new definition actions"
  [ testSession "insert new function definition" $ do
      let txtB =
            ["foo True = select [True]"
            , ""
            ,"foo False = False"
            ]
          txtB' =
            [""
            ,"someOtherCode = ()"
            ]
      docB <- createDoc "ModuleB.hs" "haskell" (T.unlines $ txtB ++ txtB')
      _ <- waitForDiagnostics
      CACodeAction action@CodeAction { _title = actionTitle } : _
                  <- sortOn (\(CACodeAction CodeAction{_title=x}) -> x) <$>
                     getCodeActions docB (R 1 0 1 50)
      liftIO $ actionTitle @?= "Define select :: [Bool] -> Bool"
      executeCodeAction action
      contentAfterAction <- documentContents docB
      liftIO $ contentAfterAction @?= T.unlines (txtB ++
        [ ""
        , "select :: [Bool] -> Bool"
        , "select = error \"not implemented\""
        ]
        ++ txtB')
  , testSession "define a hole" $ do
      let txtB =
            ["foo True = _select [True]"
            , ""
            ,"foo False = False"
            ]
          txtB' =
            [""
            ,"someOtherCode = ()"
            ]
      docB <- createDoc "ModuleB.hs" "haskell" (T.unlines $ txtB ++ txtB')
      _ <- waitForDiagnostics
      CACodeAction action@CodeAction { _title = actionTitle } : _
                  <- sortOn (\(CACodeAction CodeAction{_title=x}) -> x) <$>
                     getCodeActions docB (R 1 0 1 50)
      liftIO $ actionTitle @?= "Define select :: [Bool] -> Bool"
      executeCodeAction action
      contentAfterAction <- documentContents docB
      liftIO $ contentAfterAction @?= T.unlines (
        ["foo True = select [True]"
        , ""
        ,"foo False = False"
        , ""
        , "select :: [Bool] -> Bool"
        , "select = error \"not implemented\""
        ]
        ++ txtB')
  ]


deleteUnusedDefinitionTests :: TestTree
deleteUnusedDefinitionTests = testGroup "delete unused definition action"
  [ testSession "delete unused top level binding" $
    testFor
    (T.unlines [ "{-# OPTIONS_GHC -Wunused-top-binds #-}"
               , "module A (some) where"
               , ""
               , "f :: Int -> Int"
               , "f 1 = let a = 1"
               , "      in a"
               , "f 2 = 2"
               , ""
               , "some = ()"
               ])
    (4, 0)
    "Delete ‘f’"
    (T.unlines [
        "{-# OPTIONS_GHC -Wunused-top-binds #-}"
        , "module A (some) where"
        , ""
        , "some = ()"
        ])

  , testSession "delete unused top level binding defined in infix form" $
    testFor
    (T.unlines [ "{-# OPTIONS_GHC -Wunused-top-binds #-}"
               , "module A (some) where"
               , ""
               , "myPlus :: Int -> Int -> Int"
               , "a `myPlus` b = a + b"
               , ""
               , "some = ()"
               ])
    (4, 2)
    "Delete ‘myPlus’"
    (T.unlines [
        "{-# OPTIONS_GHC -Wunused-top-binds #-}"
        , "module A (some) where"
        , ""
        , "some = ()"
      ])
  , testSession "delete unused binding in where clause" $
    testFor
    (T.unlines [ "{-# OPTIONS_GHC -Wunused-binds #-}"
               , "module A (h, g) where"
               , ""
               , "h :: Int"
               , "h = 3"
               , ""
               , "g :: Int"
               , "g = 6"
               , "  where"
               , "    h :: Int"
               , "    h = 4"
               , ""
               ])
    (10, 4)
    "Delete ‘h’"
    (T.unlines [ "{-# OPTIONS_GHC -Wunused-binds #-}"
               , "module A (h, g) where"
               , ""
               , "h :: Int"
               , "h = 3"
               , ""
               , "g :: Int"
               , "g = 6"
               , "  where"
               , ""
               ])
  , testSession "delete unused binding with multi-oneline signatures front" $
    testFor
    (T.unlines [ "{-# OPTIONS_GHC -Wunused-binds #-}"
               , "module A (b, c) where"
               , ""
               , "a, b, c :: Int"
               , "a = 3"
               , "b = 4"
               , "c = 5"
               ])
    (4, 0)
    "Delete ‘a’"
    (T.unlines [ "{-# OPTIONS_GHC -Wunused-binds #-}"
               , "module A (b, c) where"
               , ""
               , "b, c :: Int"
               , "b = 4"
               , "c = 5"
               ])
  , testSession "delete unused binding with multi-oneline signatures mid" $
    testFor
    (T.unlines [ "{-# OPTIONS_GHC -Wunused-binds #-}"
               , "module A (a, c) where"
               , ""
               , "a, b, c :: Int"
               , "a = 3"
               , "b = 4"
               , "c = 5"
               ])
    (5, 0)
    "Delete ‘b’"
    (T.unlines [ "{-# OPTIONS_GHC -Wunused-binds #-}"
               , "module A (a, c) where"
               , ""
               , "a, c :: Int"
               , "a = 3"
               , "c = 5"
               ])
  , testSession "delete unused binding with multi-oneline signatures end" $
    testFor
    (T.unlines [ "{-# OPTIONS_GHC -Wunused-binds #-}"
               , "module A (a, b) where"
               , ""
               , "a, b, c :: Int"
               , "a = 3"
               , "b = 4"
               , "c = 5"
               ])
    (6, 0)
    "Delete ‘c’"
    (T.unlines [ "{-# OPTIONS_GHC -Wunused-binds #-}"
               , "module A (a, b) where"
               , ""
               , "a, b :: Int"
               , "a = 3"
               , "b = 4"
               ])
  ]
  where
    testFor source pos expectedTitle expectedResult = do
      docId <- createDoc "A.hs" "haskell" source
      expectDiagnostics [ ("A.hs", [(DsWarning, pos, "not used")]) ]

      (action, title) <- extractCodeAction docId "Delete"

      liftIO $ title @?= expectedTitle
      executeCodeAction action
      contentAfterAction <- documentContents docId
      liftIO $ contentAfterAction @?= expectedResult

    extractCodeAction docId actionPrefix = do
      [action@CodeAction { _title = actionTitle }]  <- findCodeActionsByPrefix docId (R 0 0 0 0) [actionPrefix]
      return (action, actionTitle)

addTypeAnnotationsToLiteralsTest :: TestTree
addTypeAnnotationsToLiteralsTest = testGroup "add type annotations to literals to satisfy contraints"
  [
    testSession "add default type to satisfy one contraint" $
    testFor
    (T.unlines [ "{-# OPTIONS_GHC -Wtype-defaults #-}"
               , "module A (f) where"
               , ""
               , "f = 1"
               ])
    [ (DsWarning, (3, 4), "Defaulting the following constraint") ]
    "Add type annotation ‘Integer’ to ‘1’"
    (T.unlines [ "{-# OPTIONS_GHC -Wtype-defaults #-}"
               , "module A (f) where"
               , ""
               , "f = (1 :: Integer)"
               ])

  , testSession "add default type to satisfy one contraint with duplicate literals" $
    testFor
    (T.unlines [ "{-# OPTIONS_GHC -Wtype-defaults #-}"
               , "{-# LANGUAGE OverloadedStrings #-}"
               , "module A (f) where"
               , ""
               , "import Debug.Trace"
               , ""
               , "f = seq \"debug\" traceShow \"debug\""
               ])
    [ (DsWarning, (6, 8), "Defaulting the following constraint")
    , (DsWarning, (6, 16), "Defaulting the following constraint")
    ]
    "Add type annotation ‘[Char]’ to ‘\"debug\"’"
    (T.unlines [ "{-# OPTIONS_GHC -Wtype-defaults #-}"
               , "{-# LANGUAGE OverloadedStrings #-}"
               , "module A (f) where"
               , ""
               , "import Debug.Trace"
               , ""
               , "f = seq (\"debug\" :: [Char]) traceShow \"debug\""
               ])
  , testSession "add default type to satisfy two contraints" $
    testFor
    (T.unlines [ "{-# OPTIONS_GHC -Wtype-defaults #-}"
               , "{-# LANGUAGE OverloadedStrings #-}"
               , "module A (f) where"
               , ""
               , "import Debug.Trace"
               , ""
               , "f a = traceShow \"debug\" a"
               ])
    [ (DsWarning, (6, 6), "Defaulting the following constraint") ]
    "Add type annotation ‘[Char]’ to ‘\"debug\"’"
    (T.unlines [ "{-# OPTIONS_GHC -Wtype-defaults #-}"
               , "{-# LANGUAGE OverloadedStrings #-}"
               , "module A (f) where"
               , ""
               , "import Debug.Trace"
               , ""
               , "f a = traceShow (\"debug\" :: [Char]) a"
               ])
  , testSession "add default type to satisfy two contraints with duplicate literals" $
    testFor
    (T.unlines [ "{-# OPTIONS_GHC -Wtype-defaults #-}"
               , "{-# LANGUAGE OverloadedStrings #-}"
               , "module A (f) where"
               , ""
               , "import Debug.Trace"
               , ""
               , "f = seq (\"debug\" :: [Char]) (seq (\"debug\" :: [Char]) (traceShow \"debug\"))"
               ])
    [ (DsWarning, (6, 54), "Defaulting the following constraint") ]
    "Add type annotation ‘[Char]’ to ‘\"debug\"’"
    (T.unlines [ "{-# OPTIONS_GHC -Wtype-defaults #-}"
               , "{-# LANGUAGE OverloadedStrings #-}"
               , "module A (f) where"
               , ""
               , "import Debug.Trace"
               , ""
               , "f = seq (\"debug\" :: [Char]) (seq (\"debug\" :: [Char]) (traceShow (\"debug\" :: [Char])))"
               ])
  ]
  where
    testFor source diag expectedTitle expectedResult = do
      docId <- createDoc "A.hs" "haskell" source
      expectDiagnostics [ ("A.hs", diag) ]

      (action, title) <- extractCodeAction docId "Add type annotation"

      liftIO $ title @?= expectedTitle
      executeCodeAction action
      contentAfterAction <- documentContents docId
      liftIO $ contentAfterAction @?= expectedResult

    extractCodeAction docId actionPrefix = do
      [action@CodeAction { _title = actionTitle }]  <- findCodeActionsByPrefix docId (R 0 0 0 0) [actionPrefix]
      return (action, actionTitle)


fixConstructorImportTests :: TestTree
fixConstructorImportTests = testGroup "fix import actions"
  [ testSession "fix constructor import" $ template
      (T.unlines
            [ "module ModuleA where"
            , "data A = Constructor"
            ])
      (T.unlines
            [ "module ModuleB where"
            , "import ModuleA(Constructor)"
            ])
      (Range (Position 1 10) (Position 1 11))
      "Fix import of A(Constructor)"
      (T.unlines
            [ "module ModuleB where"
            , "import ModuleA(A(Constructor))"
            ])
  ]
  where
    template contentA contentB range expectedAction expectedContentB = do
      _docA <- createDoc "ModuleA.hs" "haskell" contentA
      docB  <- createDoc "ModuleB.hs" "haskell" contentB
      _diags <- waitForDiagnostics
      CACodeAction action@CodeAction { _title = actionTitle } : _
                  <- sortOn (\(CACodeAction CodeAction{_title=x}) -> x) <$>
                     getCodeActions docB range
      liftIO $ expectedAction @=? actionTitle
      executeCodeAction action
      contentAfterAction <- documentContents docB
      liftIO $ expectedContentB @=? contentAfterAction

importRenameActionTests :: TestTree
importRenameActionTests = testGroup "import rename actions"
  [ testSession "Data.Mape -> Data.Map"   $ check "Map"
  , testSession "Data.Mape -> Data.Maybe" $ check "Maybe" ] where
  check modname = do
      let content = T.unlines
            [ "module Testing where"
            , "import Data.Mape"
            ]
      doc <- createDoc "Testing.hs" "haskell" content
      _ <- waitForDiagnostics
      actionsOrCommands <- getCodeActions doc (Range (Position 2 8) (Position 2 16))
      let [changeToMap] = [action | CACodeAction action@CodeAction{ _title = actionTitle } <- actionsOrCommands, ("Data." <> modname) `T.isInfixOf` actionTitle ]
      executeCodeAction changeToMap
      contentAfterAction <- documentContents doc
      let expectedContentAfterAction = T.unlines
            [ "module Testing where"
            , "import Data." <> modname
            ]
      liftIO $ expectedContentAfterAction @=? contentAfterAction

fillTypedHoleTests :: TestTree
fillTypedHoleTests = let

  sourceCode :: T.Text -> T.Text -> T.Text -> T.Text
  sourceCode a b c = T.unlines
    [ "module Testing where"
      , ""
      , "globalConvert :: Int -> String"
      , "globalConvert = undefined"
      , ""
      , "globalInt :: Int"
      , "globalInt = 3"
      , ""
      , "bar :: Int -> Int -> String"
      , "bar n parameterInt = " <> a <> " (n + " <> b <> " + " <> c <> ")  where"
      , "  localConvert = (flip replicate) 'x'"
      , ""
      , "foo :: () -> Int -> String"
      , "foo = undefined"

    ]

  check :: T.Text -> T.Text -> T.Text -> T.Text -> T.Text -> T.Text -> T.Text -> TestTree
  check actionTitle
        oldA oldB oldC
        newA newB newC = testSession (T.unpack actionTitle) $ do
    let originalCode = sourceCode oldA oldB oldC
    let expectedCode = sourceCode newA newB newC
    doc <- createDoc "Testing.hs" "haskell" originalCode
    _ <- waitForDiagnostics
    actionsOrCommands <- getCodeActions doc (Range (Position 9 0) (Position 9 maxBound))
    chosenAction <- liftIO $ pickActionWithTitle actionTitle actionsOrCommands
    executeCodeAction chosenAction
    modifiedCode <- documentContents doc
    liftIO $ expectedCode @=? modifiedCode
  in
  testGroup "fill typed holes"
  [ check "replace _ with show"
          "_"    "n" "n"
          "show" "n" "n"

  , check "replace _ with globalConvert"
          "_"             "n" "n"
          "globalConvert" "n" "n"

  , check "replace _convertme with localConvert"
          "_convertme"   "n" "n"
          "localConvert" "n" "n"

  , check "replace _b with globalInt"
          "_a" "_b"        "_c"
          "_a" "globalInt" "_c"

  , check "replace _c with globalInt"
          "_a" "_b"        "_c"
          "_a" "_b" "globalInt"

  , check "replace _c with parameterInt"
          "_a" "_b" "_c"
          "_a" "_b"  "parameterInt"
  , check "replace _ with foo _"
          "_" "n" "n"
          "(foo _)" "n" "n"
  , testSession "replace _toException with E.toException" $ do
      let mkDoc x = T.unlines
            [ "module Testing where"
            , "import qualified Control.Exception as E"
            , "ioToSome :: E.IOException -> E.SomeException"
            , "ioToSome = " <> x ]
      doc <- createDoc "Test.hs" "haskell" $ mkDoc "_toException"
      _ <- waitForDiagnostics
      actions <- getCodeActions doc (Range (Position 3 0) (Position 3 maxBound))
      chosen <- liftIO $ pickActionWithTitle "replace _toException with E.toException" actions
      executeCodeAction chosen
      modifiedCode <- documentContents doc
      liftIO $ mkDoc "E.toException" @=? modifiedCode
  ]

addInstanceConstraintTests :: TestTree
addInstanceConstraintTests = let
  missingConstraintSourceCode :: Maybe T.Text -> T.Text
  missingConstraintSourceCode mConstraint =
    let constraint = maybe "" (<> " => ") mConstraint
     in T.unlines
    [ "module Testing where"
    , ""
    , "data Wrap a = Wrap a"
    , ""
    , "instance " <> constraint <> "Eq (Wrap a) where"
    , "  (Wrap x) == (Wrap y) = x == y"
    ]

  incompleteConstraintSourceCode :: Maybe T.Text -> T.Text
  incompleteConstraintSourceCode mConstraint =
    let constraint = maybe "Eq a" (\c -> "(Eq a, " <> c <> ")")  mConstraint
     in T.unlines
    [ "module Testing where"
    , ""
    , "data Pair a b = Pair a b"
    , ""
    , "instance " <> constraint <> " => Eq (Pair a b) where"
    , "  (Pair x y) == (Pair x' y') = x == x' && y == y'"
    ]

  incompleteConstraintSourceCode2 :: Maybe T.Text -> T.Text
  incompleteConstraintSourceCode2 mConstraint =
    let constraint = maybe "(Eq a, Eq b)" (\c -> "(Eq a, Eq b, " <> c <> ")")  mConstraint
     in T.unlines
    [ "module Testing where"
    , ""
    , "data Three a b c = Three a b c"
    , ""
    , "instance " <> constraint <> " => Eq (Three a b c) where"
    , "  (Three x y z) == (Three x' y' z') = x == x' && y == y' && z == z'"
    ]

  check :: T.Text -> T.Text -> T.Text -> TestTree
  check actionTitle originalCode expectedCode = testSession (T.unpack actionTitle) $ do
    doc <- createDoc "Testing.hs" "haskell" originalCode
    _ <- waitForDiagnostics
    actionsOrCommands <- getCodeActions doc (Range (Position 6 0) (Position 6 68))
    chosenAction <- liftIO $ pickActionWithTitle actionTitle actionsOrCommands
    executeCodeAction chosenAction
    modifiedCode <- documentContents doc
    liftIO $ expectedCode @=? modifiedCode

  in testGroup "add instance constraint"
  [ check
    "Add `Eq a` to the context of the instance declaration"
    (missingConstraintSourceCode Nothing)
    (missingConstraintSourceCode $ Just "Eq a")
  , check
    "Add `Eq b` to the context of the instance declaration"
    (incompleteConstraintSourceCode Nothing)
    (incompleteConstraintSourceCode $ Just "Eq b")
  , check
    "Add `Eq c` to the context of the instance declaration"
    (incompleteConstraintSourceCode2 Nothing)
    (incompleteConstraintSourceCode2 $ Just "Eq c")
  ]

addFunctionConstraintTests :: TestTree
addFunctionConstraintTests = let
  missingConstraintSourceCode :: T.Text -> T.Text
  missingConstraintSourceCode constraint =
    T.unlines
    [ "module Testing where"
    , ""
    , "eq :: " <> constraint <> "a -> a -> Bool"
    , "eq x y = x == y"
    ]

  incompleteConstraintSourceCode :: T.Text -> T.Text
  incompleteConstraintSourceCode constraint =
    T.unlines
    [ "module Testing where"
    , ""
    , "data Pair a b = Pair a b"
    , ""
    , "eq :: " <> constraint <> " => Pair a b -> Pair a b -> Bool"
    , "eq (Pair x y) (Pair x' y') = x == x' && y == y'"
    ]

  incompleteConstraintSourceCode2 :: T.Text -> T.Text
  incompleteConstraintSourceCode2 constraint =
    T.unlines
    [ "module Testing where"
    , ""
    , "data Three a b c = Three a b c"
    , ""
    , "eq :: " <> constraint <> " => Three a b c -> Three a b c -> Bool"
    , "eq (Three x y z) (Three x' y' z') = x == x' && y == y' && z == z'"
    ]

  incompleteConstraintSourceCodeWithExtraCharsInContext :: T.Text -> T.Text
  incompleteConstraintSourceCodeWithExtraCharsInContext constraint =
    T.unlines
    [ "module Testing where"
    , ""
    , "data Pair a b = Pair a b"
    , ""
    , "eq :: " <> constraint <> " => Pair a b -> Pair a b -> Bool"
    , "eq (Pair x y) (Pair x' y') = x == x' && y == y'"
    ]

  incompleteConstraintSourceCodeWithNewlinesInTypeSignature :: T.Text -> T.Text
  incompleteConstraintSourceCodeWithNewlinesInTypeSignature constraint =
    T.unlines
    [ "module Testing where"
    , "data Pair a b = Pair a b"
    , "eq "
    , "    :: " <> constraint
    , "    => Pair a b -> Pair a b -> Bool"
    , "eq (Pair x y) (Pair x' y') = x == x' && y == y'"
    ]

  check :: T.Text -> T.Text -> T.Text -> TestTree
  check actionTitle originalCode expectedCode = testSession (T.unpack actionTitle) $ do
    doc <- createDoc "Testing.hs" "haskell" originalCode
    _ <- waitForDiagnostics
    actionsOrCommands <- getCodeActions doc (Range (Position 6 0) (Position 6 maxBound))
    chosenAction <- liftIO $ pickActionWithTitle actionTitle actionsOrCommands
    executeCodeAction chosenAction
    modifiedCode <- documentContents doc
    liftIO $ expectedCode @=? modifiedCode

  in testGroup "add function constraint"
  [ check
    "Add `Eq a` to the context of the type signature for `eq`"
    (missingConstraintSourceCode "")
    (missingConstraintSourceCode "Eq a => ")
  , check
    "Add `Eq b` to the context of the type signature for `eq`"
    (incompleteConstraintSourceCode "Eq a")
    (incompleteConstraintSourceCode "(Eq a, Eq b)")
  , check
    "Add `Eq c` to the context of the type signature for `eq`"
    (incompleteConstraintSourceCode2 "(Eq a, Eq b)")
    (incompleteConstraintSourceCode2 "(Eq a, Eq b, Eq c)")
  , check
    "Add `Eq b` to the context of the type signature for `eq`"
    (incompleteConstraintSourceCodeWithExtraCharsInContext "( Eq a )")
    (incompleteConstraintSourceCodeWithExtraCharsInContext "(Eq a, Eq b)")
  , check
    "Add `Eq b` to the context of the type signature for `eq`"
    (incompleteConstraintSourceCodeWithNewlinesInTypeSignature "(Eq a)")
    (incompleteConstraintSourceCodeWithNewlinesInTypeSignature "(Eq a, Eq b)")
  ]

removeRedundantConstraintsTests :: TestTree
removeRedundantConstraintsTests = let
  header =
    [ "{-# OPTIONS_GHC -Wredundant-constraints #-}"
    , "module Testing where"
    , ""
    ]

  redundantConstraintsCode :: Maybe T.Text -> T.Text
  redundantConstraintsCode mConstraint =
    let constraint = maybe "" (\c -> "" <> c <> " => ") mConstraint
      in T.unlines $ header <>
        [ "foo :: " <> constraint <> "a -> a"
        , "foo = id"
        ]

  redundantMixedConstraintsCode :: Maybe T.Text -> T.Text
  redundantMixedConstraintsCode mConstraint =
    let constraint = maybe "(Num a, Eq a)" (\c -> "(Num a, Eq a, " <> c <> ")") mConstraint
      in T.unlines $ header <>
        [ "foo :: " <> constraint <> " => a -> Bool"
        , "foo x = x == 1"
        ]

  typeSignatureSpaces :: T.Text
  typeSignatureSpaces = T.unlines $ header <>
    [ "foo ::  (Num a, Eq a, Monoid a)  => a -> Bool"
    , "foo x = x == 1"
    ]

  typeSignatureMultipleLines :: T.Text
  typeSignatureMultipleLines = T.unlines $ header <>
    [ "foo :: (Num a, Eq a, Monoid a)"
    , "=> a -> Bool"
    , "foo x = x == 1"
    ]

  check :: T.Text -> T.Text -> T.Text -> TestTree
  check actionTitle originalCode expectedCode = testSession (T.unpack actionTitle) $ do
    doc <- createDoc "Testing.hs" "haskell" originalCode
    _ <- waitForDiagnostics
    actionsOrCommands <- getCodeActions doc (Range (Position 4 0) (Position 4 maxBound))
    chosenAction <- liftIO $ pickActionWithTitle actionTitle actionsOrCommands
    executeCodeAction chosenAction
    modifiedCode <- documentContents doc
    liftIO $ expectedCode @=? modifiedCode

  checkPeculiarFormatting :: String -> T.Text -> TestTree
  checkPeculiarFormatting title code = testSession title $ do
    doc <- createDoc "Testing.hs" "haskell" code
    _ <- waitForDiagnostics
    actionsOrCommands <- getCodeActions doc (Range (Position 4 0) (Position 4 maxBound))
    liftIO $ assertBool "Found some actions" (null actionsOrCommands)

  in testGroup "remove redundant function constraints"
  [ check
    "Remove redundant constraint `Eq a` from the context of the type signature for `foo`"
    (redundantConstraintsCode $ Just "Eq a")
    (redundantConstraintsCode Nothing)
  , check
    "Remove redundant constraints `(Eq a, Monoid a)` from the context of the type signature for `foo`"
    (redundantConstraintsCode $ Just "(Eq a, Monoid a)")
    (redundantConstraintsCode Nothing)
  , check
    "Remove redundant constraints `(Monoid a, Show a)` from the context of the type signature for `foo`"
    (redundantMixedConstraintsCode $ Just "Monoid a, Show a")
    (redundantMixedConstraintsCode Nothing)
  , checkPeculiarFormatting
    "should do nothing when constraints contain an arbitrary number of spaces"
    typeSignatureSpaces
  , checkPeculiarFormatting
    "should do nothing when constraints contain line feeds"
    typeSignatureMultipleLines
  ]

addSigActionTests :: TestTree
addSigActionTests = let
  header = "{-# OPTIONS_GHC -Wmissing-signatures -Wmissing-pattern-synonym-signatures #-}"
  moduleH = "{-# LANGUAGE PatternSynonyms #-}\nmodule Sigs where"
  before def     = T.unlines [header, moduleH,      def]
  after' def sig = T.unlines [header, moduleH, sig, def]

  def >:: sig = testSession (T.unpack def) $ do
    let originalCode = before def
    let expectedCode = after' def sig
    doc <- createDoc "Sigs.hs" "haskell" originalCode
    _ <- waitForDiagnostics
    actionsOrCommands <- getCodeActions doc (Range (Position 3 1) (Position 3 maxBound))
    chosenAction <- liftIO $ pickActionWithTitle ("add signature: " <> sig) actionsOrCommands
    executeCodeAction chosenAction
    modifiedCode <- documentContents doc
    liftIO $ expectedCode @=? modifiedCode
  in
  testGroup "add signature"
    [ "abc = True"              >:: "abc :: Bool"
    , "foo a b = a + b"         >:: "foo :: Num a => a -> a -> a"
    , "bar a b = show $ a + b"  >:: "bar :: (Show a, Num a) => a -> a -> String"
    , "(!!!) a b = a > b"       >:: "(!!!) :: Ord a => a -> a -> Bool"
    , "a >>>> b = a + b"        >:: "(>>>>) :: Num a => a -> a -> a"
    , "a `haha` b = a b"        >:: "haha :: (t1 -> t2) -> t1 -> t2"
    , "pattern Some a = Just a" >:: "pattern Some :: a -> Maybe a"
    ]

exportUnusedTests :: TestTree
exportUnusedTests = testGroup "export unused actions"
  [ testGroup "don't want suggestion"
    [ testSession "implicit exports" $ template
        (T.unlines
              [ "{-# OPTIONS_GHC -Wunused-top-binds #-}"
              , "{-# OPTIONS_GHC -Wmissing-signatures #-}"
              , "module A where"
              , "foo = id"])
        (R 3 0 3 3)
        "Export ‘foo’"
        Nothing -- codeaction should not be available
    , testSession "not top-level" $ template
        (T.unlines
              [ "{-# OPTIONS_GHC -Wunused-top-binds #-}"
              , "{-# OPTIONS_GHC -Wunused-binds #-}"
              , "module A (foo,bar) where"
              , "foo = ()"
              , "  where bar = ()"
              , "bar = ()"])
        (R 2 0 2 11)
        "Export ‘bar’"
        Nothing
    , testSession "type is exported but not the constructor of same name" $ template
        (T.unlines
              [ "{-# OPTIONS_GHC -Wunused-top-binds #-}"
              , "module A (Foo) where"
              , "data Foo = Foo"])
        (R 2 0 2 8)
        "Export ‘Foo’"
        Nothing -- codeaction should not be available
    , testSession "unused data field" $ template
        (T.unlines
              [ "{-# OPTIONS_GHC -Wunused-top-binds #-}"
              , "module A (Foo(Foo)) where"
              , "data Foo = Foo {foo :: ()}"])
        (R 2 0 2 20)
        "Export ‘foo’"
        Nothing -- codeaction should not be available
    ]
  , testGroup "want suggestion"
    [ testSession "empty exports" $ template
        (T.unlines
              [ "{-# OPTIONS_GHC -Wunused-top-binds #-}"
              , "module A ("
              , ") where"
              , "foo = id"])
        (R 3 0 3 3)
        "Export ‘foo’"
        (Just $ T.unlines
              [ "{-# OPTIONS_GHC -Wunused-top-binds #-}"
              , "module A ("
              , "foo) where"
              , "foo = id"])
    , testSession "single line explicit exports" $ template
        (T.unlines
              [ "{-# OPTIONS_GHC -Wunused-top-binds #-}"
              , "module A (foo) where"
              , "foo = id"
              , "bar = foo"])
        (R 3 0 3 3)
        "Export ‘bar’"
        (Just $ T.unlines
              [ "{-# OPTIONS_GHC -Wunused-top-binds #-}"
              , "module A (foo,bar) where"
              , "foo = id"
              , "bar = foo"])
    , testSession "multi line explicit exports" $ template
        (T.unlines
              [ "{-# OPTIONS_GHC -Wunused-top-binds #-}"
              , "module A"
              , "  ("
              , "    foo) where"
              , "foo = id"
              , "bar = foo"])
        (R 5 0 5 3)
        "Export ‘bar’"
        (Just $ T.unlines
              [ "{-# OPTIONS_GHC -Wunused-top-binds #-}"
              , "module A"
              , "  ("
              , "    foo,bar) where"
              , "foo = id"
              , "bar = foo"])
    , testSession "export list ends in comma" $ template
        (T.unlines
              [ "{-# OPTIONS_GHC -Wunused-top-binds #-}"
              , "module A"
              , "  (foo,"
              , "  ) where"
              , "foo = id"
              , "bar = foo"])
        (R 4 0 4 3)
        "Export ‘bar’"
        (Just $ T.unlines
              [ "{-# OPTIONS_GHC -Wunused-top-binds #-}"
              , "module A"
              , "  (foo,"
              , "  bar) where"
              , "foo = id"
              , "bar = foo"])
    , testSession "unused pattern synonym" $ template
        (T.unlines
              [ "{-# OPTIONS_GHC -Wunused-top-binds #-}"
              , "{-# LANGUAGE PatternSynonyms #-}"
              , "module A () where"
              , "pattern Foo a <- (a, _)"])
        (R 3 0 3 10)
        "Export ‘Foo’"
        (Just $ T.unlines
              [ "{-# OPTIONS_GHC -Wunused-top-binds #-}"
              , "{-# LANGUAGE PatternSynonyms #-}"
              , "module A (pattern Foo) where"
              , "pattern Foo a <- (a, _)"])
    , testSession "unused data type" $ template
        (T.unlines
              [ "{-# OPTIONS_GHC -Wunused-top-binds #-}"
              , "module A () where"
              , "data Foo = Foo"])
        (R 2 0 2 7)
        "Export ‘Foo’"
        (Just $ T.unlines
              [ "{-# OPTIONS_GHC -Wunused-top-binds #-}"
              , "module A (Foo(..)) where"
              , "data Foo = Foo"])
    , testSession "unused newtype" $ template
        (T.unlines
              [ "{-# OPTIONS_GHC -Wunused-top-binds #-}"
              , "module A () where"
              , "newtype Foo = Foo ()"])
        (R 2 0 2 10)
        "Export ‘Foo’"
        (Just $ T.unlines
              [ "{-# OPTIONS_GHC -Wunused-top-binds #-}"
              , "module A (Foo(..)) where"
              , "newtype Foo = Foo ()"])
    , testSession "unused type synonym" $ template
        (T.unlines
              [ "{-# OPTIONS_GHC -Wunused-top-binds #-}"
              , "module A () where"
              , "type Foo = ()"])
        (R 2 0 2 7)
        "Export ‘Foo’"
        (Just $ T.unlines
              [ "{-# OPTIONS_GHC -Wunused-top-binds #-}"
              , "module A (Foo) where"
              , "type Foo = ()"])
    , testSession "unused type family" $ template
        (T.unlines
              [ "{-# OPTIONS_GHC -Wunused-top-binds #-}"
              , "{-# LANGUAGE TypeFamilies #-}"
              , "module A () where"
              , "type family Foo p"])
        (R 3 0 3 15)
        "Export ‘Foo’"
        (Just $ T.unlines
              [ "{-# OPTIONS_GHC -Wunused-top-binds #-}"
              , "{-# LANGUAGE TypeFamilies #-}"
              , "module A (Foo(..)) where"
              , "type family Foo p"])
    , testSession "unused typeclass" $ template
        (T.unlines
              [ "{-# OPTIONS_GHC -Wunused-top-binds #-}"
              , "module A () where"
              , "class Foo a"])
        (R 2 0 2 8)
        "Export ‘Foo’"
        (Just $ T.unlines
              [ "{-# OPTIONS_GHC -Wunused-top-binds #-}"
              , "module A (Foo(..)) where"
              , "class Foo a"])
    , testSession "infix" $ template
        (T.unlines
              [ "{-# OPTIONS_GHC -Wunused-top-binds #-}"
              , "module A () where"
              , "a `f` b = ()"])
        (R 2 0 2 11)
        "Export ‘f’"
        (Just $ T.unlines
              [ "{-# OPTIONS_GHC -Wunused-top-binds #-}"
              , "module A (f) where"
              , "a `f` b = ()"])
   , testSession "function operator" $ template
        (T.unlines
              [ "{-# OPTIONS_GHC -Wunused-top-binds #-}"
              , "module A () where"
              , "(<|) = ($)"])
        (R 2 0 2 9)
        "Export ‘<|’"
        (Just $ T.unlines
              [ "{-# OPTIONS_GHC -Wunused-top-binds #-}"
              , "module A ((<|)) where"
              , "(<|) = ($)"])
    , testSession "type synonym operator" $ template
        (T.unlines
              [ "{-# OPTIONS_GHC -Wunused-top-binds #-}"
              , "{-# LANGUAGE TypeOperators #-}"
              , "module A () where"
              , "type (:<) = ()"])
        (R 3 0 3 13)
        "Export ‘:<’"
        (Just $ T.unlines
              [ "{-# OPTIONS_GHC -Wunused-top-binds #-}"
              , "{-# LANGUAGE TypeOperators #-}"
              , "module A ((:<)) where"
              , "type (:<) = ()"])
    , testSession "type family operator" $ template
        (T.unlines
              [ "{-# OPTIONS_GHC -Wunused-top-binds #-}"
              , "{-# LANGUAGE TypeFamilies #-}"
              , "{-# LANGUAGE TypeOperators #-}"
              , "module A () where"
              , "type family (:<)"])
        (R 4 0 4 15)
        "Export ‘:<’"
        (Just $ T.unlines
              [ "{-# OPTIONS_GHC -Wunused-top-binds #-}"
              , "{-# LANGUAGE TypeFamilies #-}"
              , "{-# LANGUAGE TypeOperators #-}"
              , "module A (type (:<)(..)) where"
              , "type family (:<)"])
    , testSession "typeclass operator" $ template
        (T.unlines
              [ "{-# OPTIONS_GHC -Wunused-top-binds #-}"
              , "{-# LANGUAGE TypeOperators #-}"
              , "module A () where"
              , "class (:<) a"])
        (R 3 0 3 11)
        "Export ‘:<’"
        (Just $ T.unlines
              [ "{-# OPTIONS_GHC -Wunused-top-binds #-}"
              , "{-# LANGUAGE TypeOperators #-}"
              , "module A (type (:<)(..)) where"
              , "class (:<) a"])
    , testSession "newtype operator" $ template
        (T.unlines
              [ "{-# OPTIONS_GHC -Wunused-top-binds #-}"
              , "{-# LANGUAGE TypeOperators #-}"
              , "module A () where"
              , "newtype (:<) = Foo ()"])
        (R 3 0 3 20)
        "Export ‘:<’"
        (Just $ T.unlines
              [ "{-# OPTIONS_GHC -Wunused-top-binds #-}"
              , "{-# LANGUAGE TypeOperators #-}"
              , "module A (type (:<)(..)) where"
              , "newtype (:<) = Foo ()"])
    , testSession "data type operator" $ template
        (T.unlines
              [ "{-# OPTIONS_GHC -Wunused-top-binds #-}"
              , "{-# LANGUAGE TypeOperators #-}"
              , "module A () where"
              , "data (:<) = Foo ()"])
        (R 3 0 3 17)
        "Export ‘:<’"
        (Just $ T.unlines
              [ "{-# OPTIONS_GHC -Wunused-top-binds #-}"
              , "{-# LANGUAGE TypeOperators #-}"
              , "module A (type (:<)(..)) where"
              , "data (:<) = Foo ()"])
    ]
  ]
    where
      template initialContent range expectedAction expectedContents = do
        doc <- createDoc "A.hs" "haskell" initialContent
        _ <- waitForDiagnostics
        actions <- getCodeActions doc range
        case expectedContents of
          Just content -> do
            action <- liftIO $ pickActionWithTitle expectedAction actions
            executeCodeAction action
            contentAfterAction <- documentContents doc
            liftIO $ content @=? contentAfterAction
          Nothing ->
            liftIO $ [_title | CACodeAction CodeAction{_title} <- actions, _title == expectedAction ] @?= []

addSigLensesTests :: TestTree
addSigLensesTests = let
  missing = "{-# OPTIONS_GHC -Wmissing-signatures -Wmissing-pattern-synonym-signatures -Wunused-matches #-}"
  notMissing = "{-# OPTIONS_GHC -Wunused-matches #-}"
  moduleH = "{-# LANGUAGE PatternSynonyms #-}\nmodule Sigs where\nimport qualified Data.Complex as C"
  other = T.unlines ["f :: Integer -> Integer", "f x = 3"]
  before  withMissing def
    = T.unlines $ (if withMissing then (missing :) else (notMissing :)) [moduleH, def, other]
  after'  withMissing def sig
    = T.unlines $ (if withMissing then (missing :) else (notMissing :)) [moduleH, sig, def, other]

  sigSession withMissing def sig = testSession (T.unpack def) $ do
    let originalCode = before withMissing def
    let expectedCode = after' withMissing def sig
    doc <- createDoc "Sigs.hs" "haskell" originalCode
    [CodeLens {_command = Just c}] <- getCodeLenses doc
    executeCommand c
    modifiedCode <- getDocumentEdit doc
    liftIO $ expectedCode @=? modifiedCode
  in
  testGroup "add signature"
    [ testGroup title
      [ sigSession enableWarnings "abc = True"              "abc :: Bool"
      , sigSession enableWarnings "foo a b = a + b"         "foo :: Num a => a -> a -> a"
      , sigSession enableWarnings "bar a b = show $ a + b"  "bar :: (Show a, Num a) => a -> a -> String"
      , sigSession enableWarnings "(!!!) a b = a > b"       "(!!!) :: Ord a => a -> a -> Bool"
      , sigSession enableWarnings "a >>>> b = a + b"        "(>>>>) :: Num a => a -> a -> a"
      , sigSession enableWarnings "a `haha` b = a b"        "haha :: (t1 -> t2) -> t1 -> t2"
      , sigSession enableWarnings "pattern Some a = Just a" "pattern Some :: a -> Maybe a"
      , sigSession enableWarnings "qualifiedSigTest= C.realPart" "qualifiedSigTest :: C.Complex a -> a"
      ]
      | (title, enableWarnings) <-
        [("with warnings enabled", True)
        ,("with warnings disabled", False)
        ]
    ]

checkDefs :: [Location] -> Session [Expect] -> Session ()
checkDefs defs mkExpectations = traverse_ check =<< mkExpectations where

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

  tst (get, check) pos targetRange title = testSessionWithExtraFiles "hover" title $ \dir -> do

    -- Dirty the cache to check that definitions work even in the presence of iface files
    liftIO $ runInDir dir $ do
      let fooPath = dir </> "Foo.hs"
      fooSource <- liftIO $ readFileUtf8 fooPath
      fooDoc <- createDoc fooPath "haskell" fooSource
      _ <- getHover fooDoc $ Position 4 3
      closeDoc fooDoc

    doc <- openTestDataDoc (dir </> sourceFilePath)
    void (skipManyTill anyMessage message :: Session WorkDoneProgressEndNotification)
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
      [l,c] -> liftIO $ (adjust $ _start expectedRange) @=? Position l c
      _     -> liftIO $ assertFailure $
        "expected: " <> show ("[...]" <> sourceFileName <> ":<LINE>:<COL>**[...]", Just expectedRange) <>
        "\n but got: " <> show (msg, rangeInHover)

  assertFoundIn :: T.Text -> T.Text -> Assertion
  assertFoundIn part whole = assertBool
    (T.unpack $ "failed to find: `" <> part <> "` in hover message:\n" <> whole)
    (part `T.isInfixOf` whole)

  sourceFilePath = T.unpack sourceFileName
  sourceFileName = "GotoHover.hs"

  mkFindTests tests = testGroup "get"
    [ testGroup "definition" $ mapMaybe fst tests
    , testGroup "hover"      $ mapMaybe snd tests
    , checkFileCompiles sourceFilePath $
        expectDiagnostics
          [ ( "GotoHover.hs", [(DsError, (59, 7), "Found hole: _")]) ]
    , testGroup "type-definition" typeDefinitionTests ]

  typeDefinitionTests = [ tst (getTypeDefinitions, checkDefs) aaaL14 (pure tcData) "Saturated data con"
                        , tst (getTypeDefinitions, checkDefs) opL16 (pure [ExpectNoDefinitions]) "Polymorphic variable"]

  test runDef runHover look expect = testM runDef runHover look (return expect)

  testM runDef runHover look expect title =
    ( runDef   $ tst def   look expect title
    , runHover $ tst hover look expect title ) where
      def   = (getDefinitions, checkDefs)
      hover = (getHover      , checkHover)

  -- search locations            expectations on results
  fffL4  = _start fffR     ;  fffR = mkRange 8  4    8  7 ; fff  = [ExpectRange fffR]
  fffL8  = Position 12  4  ;
  fffL14 = Position 18  7  ;
  aaaL14 = Position 18 20  ;  aaa    = [mkR  11  0   11  3]
  dcL7   = Position 11 11  ;  tcDC   = [mkR   7 23    9 16]
  dcL12  = Position 16 11  ;
  xtcL5  = Position  9 11  ;  xtc    = [ExpectExternFail,   ExpectHoverText ["Int", "Defined in ", "GHC.Types"]]
  tcL6   = Position 10 11  ;  tcData = [mkR   7  0    9 16, ExpectHoverText ["TypeConstructor", "GotoHover.hs:8:1"]]
  vvL16  = Position 20 12  ;  vv     = [mkR  20  4   20  6]
  opL16  = Position 20 15  ;  op     = [mkR  21  2   21  4]
  opL18  = Position 22 22  ;  opp    = [mkR  22 13   22 17]
  aL18   = Position 22 20  ;  apmp   = [mkR  22 10   22 11]
  b'L19  = Position 23 13  ;  bp     = [mkR  23  6   23  7]
  xvL20  = Position 24  8  ;  xvMsg  = [ExpectExternFail,   ExpectHoverText ["pack", ":: String -> Text", "Data.Text"]]
  clL23  = Position 27 11  ;  cls    = [mkR  25  0   26 20, ExpectHoverText ["MyClass", "GotoHover.hs:26:1"]]
  clL25  = Position 29  9
  eclL15 = Position 19  8  ;  ecls   = [ExpectExternFail, ExpectHoverText ["Num", "Defined in ", "GHC.Num"]]
  dnbL29 = Position 33 18  ;  dnb    = [ExpectHoverText [":: ()"],   mkR  33 12   33 21]
  dnbL30 = Position 34 23
  lcbL33 = Position 37 26  ;  lcb    = [ExpectHoverText [":: Char"], mkR  37 26   37 27]
  lclL33 = Position 37 22
  mclL36 = Position 40  1  ;  mcl    = [mkR  40  0   40 14]
  mclL37 = Position 41  1
  spaceL37 = Position 41  24 ; space = [ExpectNoDefinitions, ExpectHoverText [":: Char"]]
  docL41 = Position 45  1  ;  doc    = [ExpectHoverText ["Recognizable docs: kpqz"]]
                           ;  constr = [ExpectHoverText ["Monad m"]]
  eitL40 = Position 44 28  ;  kindE  = [ExpectHoverText [":: * -> * -> *\n"]]
  intL40 = Position 44 34  ;  kindI  = [ExpectHoverText [":: *\n"]]
  tvrL40 = Position 44 37  ;  kindV  = [ExpectHoverText [":: * -> *\n"]]
  intL41 = Position 45 20  ;  litI   = [ExpectHoverText ["7518"]]
  chrL36 = Position 41 24  ;  litC   = [ExpectHoverText ["'f'"]]
  txtL8  = Position 12 14  ;  litT   = [ExpectHoverText ["\"dfgy\""]]
  lstL43 = Position 47 12  ;  litL   = [ExpectHoverText ["[8391 :: Int, 6268]"]]
  outL45 = Position 49  3  ;  outSig = [ExpectHoverText ["outer", "Bool"], mkR 46 0 46 5]
  innL48 = Position 52  5  ;  innSig = [ExpectHoverText ["inner", "Char"], mkR 49 2 49 7]
  holeL60 = Position 59 7  ;  hleInfo = [ExpectHoverText ["_ ::"]]
  cccL17 = Position 17 16  ;  docLink = [ExpectHoverText ["[Documentation](file:///"]]
  imported = Position 56 13 ; importedSig = getDocUri "Foo.hs" >>= \foo -> return [ExpectHoverText ["foo", "Foo", "Haddock"], mkL foo 5 0 5 3]
  reexported = Position 55 14 ; reexportedSig = getDocUri "Bar.hs" >>= \bar -> return [ExpectHoverText ["Bar", "Bar", "Haddock"], mkL bar 3 0 3 14]
  in
  mkFindTests
  --      def    hover  look       expect
  [ test  yes    yes    fffL4      fff           "field in record definition"
  , test  yes    yes    fffL8      fff           "field in record construction     #71"
  , test  yes    yes    fffL14     fff           "field name used as accessor"          -- 120 in Calculate.hs
  , test  yes    yes    aaaL14     aaa           "top-level name"                       -- 120
  , test  yes    yes    dcL7       tcDC          "data constructor record         #247"
  , test  yes    yes    dcL12      tcDC          "data constructor plain"               -- 121
  , test  yes    yes    tcL6       tcData        "type constructor                #248" -- 147
  , test  broken yes    xtcL5      xtc           "type constructor external   #248,249"
  , test  broken yes    xvL20      xvMsg         "value external package          #249" -- 120
  , test  yes    yes    vvL16      vv            "plain parameter"                      -- 120
  , test  yes    yes    aL18       apmp          "pattern match name"                   -- 120
  , test  yes    yes    opL16      op            "top-level operator"                   -- 120, 123
  , test  yes    yes    opL18      opp           "parameter operator"                   -- 120
  , test  yes    yes    b'L19      bp            "name in backticks"                    -- 120
  , test  yes    yes    clL23      cls           "class in instance declaration   #250"
  , test  yes    yes    clL25      cls           "class in signature              #250" -- 147
  , test  broken yes    eclL15     ecls          "external class in signature #249,250"
  , test  yes    yes    dnbL29     dnb           "do-notation   bind"                   -- 137
  , test  yes    yes    dnbL30     dnb           "do-notation lookup"
  , test  yes    yes    lcbL33     lcb           "listcomp   bind"                      -- 137
  , test  yes    yes    lclL33     lcb           "listcomp lookup"
  , test  yes    yes    mclL36     mcl           "top-level fn 1st clause"
  , test  yes    yes    mclL37     mcl           "top-level fn 2nd clause         #246"
#if MIN_GHC_API_VERSION(8,10,0)
  , test  yes    yes    spaceL37   space         "top-level fn on space #315"
#else
  , test  yes    broken spaceL37   space         "top-level fn on space #315"
#endif
  , test  no     yes    docL41     doc           "documentation                     #7"
  , test  no     yes    eitL40     kindE         "kind of Either                  #273"
  , test  no     yes    intL40     kindI         "kind of Int                     #273"
  , test  no     broken tvrL40     kindV         "kind of (* -> *) type variable  #273"
  , test  no     broken intL41     litI          "literal Int  in hover info      #274"
  , test  no     broken chrL36     litC          "literal Char in hover info      #274"
  , test  no     broken txtL8      litT          "literal Text in hover info      #274"
  , test  no     broken lstL43     litL          "literal List in hover info      #274"
  , test  no     broken docL41     constr        "type constraint in hover info   #283"
  , test  broken broken outL45     outSig        "top-level signature             #310"
  , test  broken broken innL48     innSig        "inner     signature             #310"
  , test  no     yes    holeL60    hleInfo       "hole without internal name      #847"
  , test  no     skip   cccL17     docLink       "Haddock html links"
  , testM yes    yes    imported   importedSig   "Imported symbol"
  , testM yes    yes    reexported reexportedSig "Imported symbol (reexported)"
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
  ignoreTest8101 "GHC #18070" $
  ignoreInWindowsForGHC88And810 $
  testSessionWithExtraFiles "plugin" "simple plugin" $ \dir -> do
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
  ignoreTest8101 "GHC #18070" $
  ignoreInWindowsForGHC88And810 $
  testSessionWithExtraFiles "plugin" "parsedResultAction plugin" $ \dir -> do
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
        -- Some give the column number and others don't (hence -1). Assert either
        -- of them.
        (run $ expectError content (2, -1))
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
    , thReloadingTest
    -- Regression test for https://github.com/haskell/ghcide/issues/614
    , thLinkingTest
    , testSessionWait "findsTHIdentifiers" $ do
        let sourceA =
              T.unlines
                [ "{-# LANGUAGE TemplateHaskell #-}"
                , "module A (a) where"
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
    , ignoreInWindowsForGHC88 $ testCase "findsTHnewNameConstructor" $ runWithExtraFiles "THNewName" $ \dir -> do

    -- This test defines a TH value with the meaning "data A = A" in A.hs
    -- Loads and export the template in B.hs
    -- And checks wether the constructor A can be loaded in C.hs
    -- This test does not fail when either A and B get manually loaded before C.hs
    -- or when we remove the seemingly unnecessary TH pragma from C.hs

    let cPath = dir </> "C.hs"
    _ <- openDoc cPath "haskell"
    expectDiagnostics [ ( cPath, [(DsWarning, (3, 0), "Top-level binding with no type signature: a :: A")] ) ]
    ]

-- | test that TH is reevaluated on typecheck
thReloadingTest :: TestTree
thReloadingTest = testCase "reloading-th-test" $ runWithExtraFiles "TH" $ \dir -> do

    let aPath = dir </> "THA.hs"
        bPath = dir </> "THB.hs"
        cPath = dir </> "THC.hs"

    aSource <- liftIO $ readFileUtf8 aPath --  th = [d|a = ()|]
    bSource <- liftIO $ readFileUtf8 bPath --  $th
    cSource <- liftIO $ readFileUtf8 cPath --  c = a :: ()

    adoc <- createDoc aPath "haskell" aSource
    bdoc <- createDoc bPath "haskell" bSource
    cdoc <- createDoc cPath "haskell" cSource

    expectDiagnostics [("THB.hs", [(DsWarning, (4,0), "Top-level binding")])]

    -- Change th from () to Bool
    let aSource' = T.unlines $ init (T.lines aSource) ++ ["th_a = [d| a = False|]"]
    changeDoc adoc [TextDocumentContentChangeEvent Nothing Nothing aSource']
    -- generate an artificial warning to avoid timing out if the TH change does not propagate
    changeDoc cdoc [TextDocumentContentChangeEvent Nothing Nothing $ cSource <> "\nfoo=()"]

    -- Check that the change propagates to C
    expectDiagnostics
        [("THC.hs", [(DsError, (4, 4), "Couldn't match expected type '()' with actual type 'Bool'")])
        ,("THC.hs", [(DsWarning, (6,0), "Top-level binding")])
        ,("THB.hs", [(DsWarning, (4,0), "Top-level binding")])
        ]

    closeDoc adoc
    closeDoc bdoc
    closeDoc cdoc

thLinkingTest :: TestTree
thLinkingTest = testCase "th-linking-test" $ runWithExtraFiles "TH" $ \dir -> do

    let aPath = dir </> "THA.hs"
        bPath = dir </> "THB.hs"

    aSource <- liftIO $ readFileUtf8 aPath --  th_a = [d|a :: ()|]
    bSource <- liftIO $ readFileUtf8 bPath --  $th_a

    adoc <- createDoc aPath "haskell" aSource
    bdoc <- createDoc bPath "haskell" bSource

    expectDiagnostics [("THB.hs", [(DsWarning, (4,0), "Top-level binding")])]

    let aSource' = T.unlines $ init (init (T.lines aSource)) ++ ["th :: DecsQ", "th = [d| a = False|]"]
    changeDoc adoc [TextDocumentContentChangeEvent Nothing Nothing aSource']

    -- modify b too
    let bSource' = T.unlines $ init (T.lines bSource) ++ ["$th"]
    changeDoc bdoc [TextDocumentContentChangeEvent Nothing Nothing bSource']

    expectDiagnostics [("THB.hs", [(DsWarning, (4,0), "Top-level binding")])]

    closeDoc adoc
    closeDoc bdoc


completionTests :: TestTree
completionTests
  = testGroup "completion"
    [ testGroup "non local" nonLocalCompletionTests
    , testGroup "topLevel" topLevelCompletionTests
    , testGroup "local" localCompletionTests
    , testGroup "other" otherCompletionTests
    ]

completionTest :: String -> [T.Text] -> Position -> [(T.Text, CompletionItemKind, T.Text, Bool, Bool, Maybe (List TextEdit))] -> TestTree
completionTest name src pos expected = testSessionWait name $ do
    docId <- createDoc "A.hs" "haskell" (T.unlines src)
    _ <- waitForDiagnostics
    compls <- getCompletions docId pos
    let compls' = [ (_label, _kind, _insertText, _additionalTextEdits) | CompletionItem{..} <- compls]
    liftIO $ do
        let emptyToMaybe x = if T.null x then Nothing else Just x
        compls' @?= [ (l, Just k, emptyToMaybe t, at) | (l,k,t,_,_,at) <- expected]
        forM_ (zip compls expected) $ \(CompletionItem{..}, (_,_,_,expectedSig, expectedDocs, _)) -> do
            when expectedSig $
                assertBool ("Missing type signature: " <> T.unpack _label) (isJust _detail)
            when expectedDocs $
                assertBool ("Missing docs: " <> T.unpack _label) (isJust _documentation)

topLevelCompletionTests :: [TestTree]
topLevelCompletionTests = [
    completionTest
        "variable"
        ["bar = xx", "-- | haddock", "xxx :: ()", "xxx = ()", "-- | haddock", "data Xxx = XxxCon"]
        (Position 0 8)
        [("xxx", CiFunction, "xxx", True, True, Nothing),
         ("XxxCon", CiConstructor, "XxxCon", False, True, Nothing)
        ],
    completionTest
        "constructor"
        ["bar = xx", "-- | haddock", "xxx :: ()", "xxx = ()", "-- | haddock", "data Xxx = XxxCon"]
        (Position 0 8)
        [("xxx", CiFunction, "xxx", True, True, Nothing),
         ("XxxCon", CiConstructor, "XxxCon", False, True, Nothing)
        ],
    completionTest
        "class method"
        ["bar = xx", "class Xxx a where", "-- | haddock", "xxx :: ()", "xxx = ()"]
        (Position 0 8)
        [("xxx", CiFunction, "xxx", True, True, Nothing)],
    completionTest
        "type"
        ["bar :: Xx", "xxx = ()", "-- | haddock", "data Xxx = XxxCon"]
        (Position 0 9)
        [("Xxx", CiStruct, "Xxx", False, True, Nothing)],
    completionTest
        "class"
        ["bar :: Xx", "xxx = ()", "-- | haddock", "class Xxx a"]
        (Position 0 9)
        [("Xxx", CiClass, "Xxx", False, True, Nothing)],
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
        ]
    ]

nonLocalCompletionTests :: [TestTree]
nonLocalCompletionTests =
  [ completionTest
      "variable"
      ["module A where", "f = hea"]
      (Position 1 7)
      [("head", CiFunction, "head ${1:[a]}", True, True, Nothing)],
    completionTest
      "constructor"
      ["module A where", "f = Tru"]
      (Position 1 7)
      [ ("True", CiConstructor, "True ", True, True, Nothing),
        ("truncate", CiFunction, "truncate ${1:a}", True, True, Nothing)
      ],
    completionTest
      "type"
      ["{-# OPTIONS_GHC -Wall #-}", "module A () where", "f :: Bo", "f = True"]
      (Position 2 7)
      [ ("Bounded", CiClass, "Bounded ${1:*}", True, True, Nothing),
        ("Bool", CiStruct, "Bool ", True, True, Nothing)
      ],
    completionTest
      "qualified"
      ["{-# OPTIONS_GHC -Wunused-binds #-}", "module A () where", "f = Prelude.hea"]
      (Position 2 15)
      [ ("head", CiFunction, "head ${1:[a]}", True, True, Nothing)
      ],
    completionTest
      "duplicate import"
      ["module A where", "import Data.List", "import Data.List", "f = perm"]
      (Position 3 8)
      [ ("permutations", CiFunction, "permutations ${1:[a]}", False, False, Nothing)
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
    expectFailBecause "Auto import completion snippets were disabled in v0.6.0.2" $
      testGroup "auto import snippets"
      [ completionTest
        "show imports not in list - simple"
        ["{-# LANGUAGE NoImplicitPrelude #-}",
        "module A where", "import Control.Monad (msum)", "f = joi"]
        (Position 3 6)
        [("join", CiFunction, "join ${1:m (m a)}", False, False,
            Just (List [TextEdit {_range = Range {_start = Position {_line = 2, _character = 26}, _end = Position {_line = 2, _character = 26}}, _newText = "join, "}]))]
      , completionTest
        "show imports not in list - multi-line"
        ["{-# LANGUAGE NoImplicitPrelude #-}",
        "module A where", "import Control.Monad (\n    msum)", "f = joi"]
        (Position 4 6)
        [("join", CiFunction, "join ${1:m (m a)}", False, False,
            Just (List [TextEdit {_range = Range {_start = Position {_line = 3, _character = 8}, _end = Position {_line = 3, _character = 8}}, _newText = "join, "}]))]
      , completionTest
        "show imports not in list - names with _"
        ["{-# LANGUAGE NoImplicitPrelude #-}",
        "module A where", "import qualified Control.Monad as M (msum)", "f = M.mapM_"]
        (Position 3 11)
        [("mapM_", CiFunction, "mapM_ ${1:a -> m b} ${2:t a}", False, False,
            Just (List [TextEdit {_range = Range {_start = Position {_line = 2, _character = 41}, _end = Position {_line = 2, _character = 41}}, _newText = "mapM_, "}]))]
      , completionTest
        "show imports not in list - initial empty list"
        ["{-# LANGUAGE NoImplicitPrelude #-}",
        "module A where", "import qualified Control.Monad as M ()", "f = M.joi"]
        (Position 3 10)
        [("join", CiFunction, "join ${1:m (m a)}", False, False,
            Just (List [TextEdit {_range = Range {_start = Position {_line = 2, _character = 37}, _end = Position {_line = 2, _character = 37}}, _newText = "join, "}]))]
      , completionTest
        "record snippet on import"
        ["module A where", "import Text.Printf (FormatParse(FormatParse))", "FormatParse"]
        (Position 2 10)
        [("FormatParse", CiStruct, "FormatParse ", False, False,
        Just (List [TextEdit {_range = Range {_start = Position {_line = 1, _character = 44}, _end = Position {_line = 1, _character = 44}}, _newText = "FormatParse, "}])),
        ("FormatParse", CiConstructor, "FormatParse ${1:String} ${2:Char} ${3:String}", False, False,
        Just (List [TextEdit {_range = Range {_start = Position {_line = 1, _character = 44}, _end = Position {_line = 1, _character = 44}}, _newText = "FormatParse, "}])),
        ("FormatParse", CiSnippet, "FormatParse {fpModifiers=${1:_fpModifiers}, fpChar=${2:_fpChar}, fpRest=${3:_fpRest}}", False, False,
        Just (List [TextEdit {_range = Range {_start = Position {_line = 1, _character = 44}, _end = Position {_line = 1, _character = 44}}, _newText = "FormatParse, "}]))
        ]
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
      [("Integer", CiStruct, "Integer ", True, True, Nothing)]
  ]

highlightTests :: TestTree
highlightTests = testGroup "highlight"
  [ testSessionWait "value" $ do
    doc <- createDoc "A.hs" "haskell" source
    _ <- waitForDiagnostics
    highlights <- getHighlights doc (Position 3 2)
    liftIO $ highlights @?=
            [ DocumentHighlight (R 2 0 2 3) (Just HkRead)
            , DocumentHighlight (R 3 0 3 3) (Just HkWrite)
            , DocumentHighlight (R 4 6 4 9) (Just HkRead)
            , DocumentHighlight (R 5 22 5 25) (Just HkRead)
            ]
  , testSessionWait "type" $ do
    doc <- createDoc "A.hs" "haskell" source
    _ <- waitForDiagnostics
    highlights <- getHighlights doc (Position 2 8)
    liftIO $ highlights @?=
            [ DocumentHighlight (R 2 7 2 10) (Just HkRead)
            , DocumentHighlight (R 3 11 3 14) (Just HkRead)
            ]
  , testSessionWait "local" $ do
    doc <- createDoc "A.hs" "haskell" source
    _ <- waitForDiagnostics
    highlights <- getHighlights doc (Position 6 5)
    liftIO $ highlights @?=
            [ DocumentHighlight (R 6 4 6 7) (Just HkWrite)
            , DocumentHighlight (R 6 10 6 13) (Just HkRead)
            , DocumentHighlight (R 7 12 7 15) (Just HkRead)
            ]
  , testSessionWait "record" $ do
    doc <- createDoc "A.hs" "haskell" recsource
    _ <- waitForDiagnostics
    highlights <- getHighlights doc (Position 4 15)
    liftIO $ highlights @?=
      -- Span is just the .. on 8.10, but Rec{..} before
#if MIN_GHC_API_VERSION(8,10,0)
            [ DocumentHighlight (R 4 8 4 10) (Just HkWrite)
#else
            [ DocumentHighlight (R 4 4 4 11) (Just HkWrite)
#endif
            , DocumentHighlight (R 4 14 4 20) (Just HkRead)
            ]
    highlights <- getHighlights doc (Position 3 17)
    liftIO $ highlights @?=
            [ DocumentHighlight (R 3 17 3 23) (Just HkWrite)
      -- Span is just the .. on 8.10, but Rec{..} before
#if MIN_GHC_API_VERSION(8,10,0)
            , DocumentHighlight (R 4 8 4 10) (Just HkRead)
#else
            , DocumentHighlight (R 4 4 4 11) (Just HkRead)
#endif
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
    liftIO $ symbols @?= Left [docSymbolD "A" "type family" SkClass (R 1 0 1 13)]
  , testSessionWait "type family instance " $ do
    let source = T.unlines
          [ "{-# language TypeFamilies #-}"
          , "type family A a"
          , "type instance A () = ()"
          ]
    docId   <- createDoc "A.hs" "haskell" source
    symbols <- getDocumentSymbols docId
    liftIO $ symbols @?= Left
      [ docSymbolD "A a"   "type family" SkClass     (R 1 0 1 15)
      , docSymbol "A ()" SkInterface (R 2 0 2 23)
      ]
  , testSessionWait "data family" $ do
    let source = T.unlines ["{-# language TypeFamilies #-}", "data family A"]
    docId   <- createDoc "A.hs" "haskell" source
    symbols <- getDocumentSymbols docId
    liftIO $ symbols @?= Left [docSymbolD "A" "data family" SkClass (R 1 0 1 11)]
  , testSessionWait "data family instance " $ do
    let source = T.unlines
          [ "{-# language TypeFamilies #-}"
          , "data family A a"
          , "data instance A () = A ()"
          ]
    docId   <- createDoc "A.hs" "haskell" source
    symbols <- getDocumentSymbols docId
    liftIO $ symbols @?= Left
      [ docSymbolD "A a"   "data family" SkClass     (R 1 0 1 11)
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
    liftIO $ symbols @=? Left
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
    DocumentSymbol name Nothing kind Nothing loc loc Nothing
  docSymbol' name kind loc selectionLoc =
    DocumentSymbol name Nothing kind Nothing loc selectionLoc Nothing
  docSymbolD name detail kind loc =
    DocumentSymbol name (Just detail) kind Nothing loc loc Nothing
  docSymbolWithChildren name kind loc cc =
    DocumentSymbol name Nothing kind Nothing loc loc (Just $ List cc)
  docSymbolWithChildren' name kind loc selectionLoc cc =
    DocumentSymbol name Nothing kind Nothing loc selectionLoc (Just $ List cc)
  moduleSymbol name loc cc = DocumentSymbol name
                                            Nothing
                                            SkFile
                                            Nothing
                                            (R 0 0 maxBound 0)
                                            loc
                                            (Just $ List cc)
  classSymbol name loc cc = DocumentSymbol name
                                           (Just "class")
                                           SkClass
                                           Nothing
                                           loc
                                           loc
                                           (Just $ List cc)

pattern R :: Int -> Int -> Int -> Int -> Range
pattern R x y x' y' = Range (Position x y) (Position x' y')

xfail :: TestTree -> String -> TestTree
xfail = flip expectFailBecause

ignoreTest8101 :: String -> TestTree -> TestTree
ignoreTest8101
  | GHC_API_VERSION == ("8.10.1" :: String) = ignoreTestBecause
  | otherwise = const id

ignoreInWindowsBecause :: String -> TestTree -> TestTree
ignoreInWindowsBecause = if isWindows then ignoreTestBecause else (\_ x -> x)

ignoreInWindowsForGHC88And810 :: TestTree -> TestTree
#if MIN_GHC_API_VERSION(8,8,1) && !MIN_GHC_API_VERSION(9,0,0)
ignoreInWindowsForGHC88And810 =
    ignoreInWindowsBecause "tests are unreliable in windows for ghc 8.8 and 8.10"
#else
ignoreInWindowsForGHC88And810 = id
#endif

ignoreInWindowsForGHC88 :: TestTree -> TestTree
#if MIN_GHC_API_VERSION(8,8,1) && !MIN_GHC_API_VERSION(8,10,1)
ignoreInWindowsForGHC88 =
    ignoreInWindowsBecause "tests are unreliable in windows for ghc 8.8"
#else
ignoreInWindowsForGHC88 = id
#endif

data Expect
  = ExpectRange Range -- Both gotoDef and hover should report this range
  | ExpectLocation Location
--  | ExpectDefRange Range -- Only gotoDef should report this range
  | ExpectHoverRange Range -- Only hover should report this range
  | ExpectHoverText [T.Text] -- the hover message must contain these snippets
  | ExpectExternFail -- definition lookup in other file expected to fail
  | ExpectNoDefinitions
  | ExpectNoHover
--  | ExpectExtern -- TODO: as above, but expected to succeed: need some more info in here, once we have some working examples
  deriving Eq

mkR :: Int -> Int -> Int -> Int -> Expect
mkR startLine startColumn endLine endColumn = ExpectRange $ mkRange startLine startColumn endLine endColumn

mkL :: Uri -> Int -> Int -> Int -> Int -> Expect
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
      ]
  where
    checkHaddock s txt = spanDocToMarkdownForTest s @?= txt

cradleTests :: TestTree
cradleTests = testGroup "cradle"
    [testGroup "dependencies" [sessionDepsArePickedUp]
    ,testGroup "ignore-fatal" [ignoreFatalWarning]
    ,testGroup "loading" [loadCradleOnlyonce]
    ,testGroup "multi"   [simpleMultiTest, simpleMultiTest2]
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
            msgs <- someTill (skipManyTill anyMessage cradleLoadedMessage) (skipManyTill anyMessage (message @PublishDiagnosticsNotification))
            liftIO $ length msgs @?= 1
            changeDoc doc [TextDocumentContentChangeEvent Nothing Nothing "module B where\nimport Data.Maybe"]
            msgs <- manyTill (skipManyTill anyMessage cradleLoadedMessage) (skipManyTill anyMessage (message @PublishDiagnosticsNotification))
            liftIO $ length msgs @?= 0
            _ <- createDoc "A.hs" "haskell" "module A where\nimport LoadCradleBar"
            msgs <- manyTill (skipManyTill anyMessage cradleLoadedMessage) (skipManyTill anyMessage (message @PublishDiagnosticsNotification))
            liftIO $ length msgs @?= 0


dependentFileTest :: TestTree
dependentFileTest = testGroup "addDependentFile"
    [testGroup "file-changed" [ignoreInWindowsForGHC88 $ testSession' "test" test]
    ]
    where
      test dir = do
        -- If the file contains B then no type error
        -- otherwise type error
        liftIO $ writeFile (dir </> "dep-file.txt") "A"
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
        _ <-createDoc "Foo.hs" "haskell" fooContent
        doc <- createDoc "Baz.hs" "haskell" bazContent
        expectDiagnostics
          [("Foo.hs", [(DsError, (4, 6), "Couldn't match expected type")])]
        -- Now modify the dependent file
        liftIO $ writeFile (dir </> "dep-file.txt") "B"
        let change = TextDocumentContentChangeEvent
              { _range = Just (Range (Position 2 0) (Position 2 6))
              , _rangeLength = Nothing
              , _text = "f = ()"
              }
        -- Modifying Baz will now trigger Foo to be rebuilt as well
        changeDoc doc [change]
        expectDiagnostics [("Foo.hs", [])]


cradleLoadedMessage :: Session FromServerMessage
cradleLoadedMessage = satisfy $ \case
        NotCustomServer (NotificationMessage _ (CustomServerMethod m) _) -> m == cradleLoadedMethod
        _ -> False

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
simpleMultiTest = testCase "simple-multi-test" $ runWithExtraFiles "multi" $ \dir -> do
    let aPath = dir </> "a/A.hs"
        bPath = dir </> "b/B.hs"
    aSource <- liftIO $ readFileUtf8 aPath
    (TextDocumentIdentifier adoc) <- createDoc aPath "haskell" aSource
    expectNoMoreDiagnostics 0.5
    bSource <- liftIO $ readFileUtf8 bPath
    bdoc <- createDoc bPath "haskell" bSource
    expectNoMoreDiagnostics 0.5
    locs <- getDefinitions bdoc (Position 2 7)
    let fooL = mkL adoc 2 0 2 3
    checkDefs locs (pure [fooL])
    expectNoMoreDiagnostics 0.5

-- Like simpleMultiTest but open the files in the other order
simpleMultiTest2 :: TestTree
simpleMultiTest2 = testCase "simple-multi-test2" $ runWithExtraFiles "multi" $ \dir -> do
    let aPath = dir </> "a/A.hs"
        bPath = dir </> "b/B.hs"
    bSource <- liftIO $ readFileUtf8 bPath
    bdoc <- createDoc bPath "haskell" bSource
    expectNoMoreDiagnostics 10
    aSource <- liftIO $ readFileUtf8 aPath
    (TextDocumentIdentifier adoc) <- createDoc aPath "haskell" aSource
    -- Need to have some delay here or the test fails
    expectNoMoreDiagnostics 10
    locs <- getDefinitions bdoc (Position 2 7)
    let fooL = mkL adoc 2 0 2 3
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
bootTests = testCase "boot-def-test" $ runWithExtraFiles "boot" $ \dir -> do
  let cPath = dir </> "C.hs"
  cSource <- liftIO $ readFileUtf8 cPath

  -- Dirty the cache
  liftIO $ runInDir dir $ do
    cDoc <- createDoc cPath "haskell" cSource
    _ <- getHover cDoc $ Position 4 3
    closeDoc cDoc

  cdoc <- createDoc cPath "haskell" cSource
  locs <- getDefinitions cdoc (Position 7 4)
  let floc = mkR 7 0 7 1
  checkDefs locs (pure [floc])

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

    -- Check that the change propogates to C
    changeDoc cdoc [TextDocumentContentChangeEvent Nothing Nothing cSource]
    expectDiagnostics
      [("THC.hs", [(DsError, (4, 4), "Couldn't match expected type '()' with actual type 'Bool'")])
      ,("THB.hs", [(DsWarning, (4,0), "Top-level binding")])]
    closeDoc cdoc

ifaceErrorTest :: TestTree
ifaceErrorTest = testCase "iface-error-test-1" $ runWithExtraFiles "recomp" $ \dir -> do
    let bPath = dir </> "B.hs"
        pPath = dir </> "P.hs"

    bSource <- liftIO $ readFileUtf8 bPath -- y :: Int
    pSource <- liftIO $ readFileUtf8 pPath -- bar = x :: Int

    bdoc <- createDoc bPath "haskell" bSource
    expectDiagnostics
      [("P.hs", [(DsWarning,(4,0), "Top-level binding")])] -- So what we know P has been loaded

    -- Change y from Int to B
    changeDoc bdoc [TextDocumentContentChangeEvent Nothing Nothing $ T.unlines ["module B where", "y :: Bool", "y = undefined"]]
    -- save so that we can that the error propogates to A
    sendNotification TextDocumentDidSave (DidSaveTextDocumentParams bdoc)

    -- Check that the error propogates to A
    expectDiagnostics
      [("A.hs", [(DsError, (5, 4), "Couldn't match expected type 'Int' with actual type 'Bool'")])]


    -- Check that we wrote the interfaces for B when we saved
    lid <- sendRequest (CustomClientMethod "hidir") $ GetInterfaceFilesDir bPath
    res <- skipManyTill anyMessage $ responseForId lid
    liftIO $ case res of
      ResponseMessage{_result=Right hidir} -> do
        hi_exists <- doesFileExist $ hidir </> "B.hi"
        assertBool ("Couldn't find B.hi in " ++ hidir) hi_exists
      _ -> assertFailure $ "Got malformed response for CustomMessage hidir: " ++ show res

    pdoc <- createDoc pPath "haskell" pSource
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
    expectDiagnostics
      [("Foo.hs", [(DsError, (3, 6), "Couldn't match expected type")])]
    -- Update hie.yaml to enable OverloadedStrings.
    liftIO $
      writeFileUTF8
        (dir </> "hie.yaml")
        "cradle: {direct: {arguments: [-XOverloadedStrings]}}"
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

        ec @=? ExitSuccess
  ]

benchmarkTests :: TestTree
benchmarkTests =
    let ?config = Bench.defConfig
            { Bench.verbosity = Bench.Quiet
            , Bench.repetitions = Just 3
            , Bench.buildTool = Bench.Cabal
            } in
    withResource Bench.setup Bench.cleanUp $ \getResource -> testGroup "benchmark experiments"
    [ testCase (Bench.name e) $ do
        Bench.SetupResult{Bench.benchDir} <- getResource
        res <- Bench.runBench (runInDir benchDir) e
        assertBool "did not successfully complete 5 repetitions" $ Bench.success res
        | e <- Bench.experiments
        , Bench.name e /= "edit" -- the edit experiment does not ever fail
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
            let req = ExecuteCommandParams blockCommandId Nothing Nothing
            void $ sendRequest WorkspaceExecuteCommand req
            -- Load a file and check for code actions. Will only work if the command is run asynchronously
            doc <- createDoc "A.hs" "haskell" $ T.unlines
              [ "{-# OPTIONS -Wmissing-signatures #-}"
              , "foo = id"
              ]
            void waitForDiagnostics
            actions <- getCodeActions doc (Range (Position 1 0) (Position 1 0))
            liftIO $ [ _title | CACodeAction CodeAction{_title} <- actions] @=? ["add signature: foo :: a -> a"]
    , testSession "request" $ do
            -- Execute a custom request that will block for 1000 seconds
            void $ sendRequest (CustomClientMethod "test") $ BlockSeconds 1000
            -- Load a file and check for code actions. Will only work if the request is run asynchronously
            doc <- createDoc "A.hs" "haskell" $ T.unlines
              [ "{-# OPTIONS -Wmissing-signatures #-}"
              , "foo = id"
              ]
            void waitForDiagnostics
            actions <- getCodeActions doc (Range (Position 0 0) (Position 0 0))
            liftIO $ [ _title | CACodeAction CodeAction{_title} <- actions] @=? ["add signature: foo :: a -> a"]
    ]


clientSettingsTest :: TestTree
clientSettingsTest = testGroup "client settings handling"
    [
        testSession "ghcide does not support update config" $ do
            sendNotification WorkspaceDidChangeConfiguration (DidChangeConfigurationParams (toJSON ("" :: String)))
            logNot <- skipManyTill anyMessage loggingNotification
            isMessagePresent "Updating Not supported" [getLogMessage logNot]
    ,   testSession "ghcide restarts shake session on config changes" $ do
            void $ skipManyTill anyMessage $ message @RegisterCapabilityRequest
            sendNotification WorkspaceDidChangeConfiguration (DidChangeConfigurationParams (toJSON ("" :: String)))
            nots <- skipManyTill anyMessage $ count 3 loggingNotification
            isMessagePresent "Restarting build session" (map getLogMessage nots)

    ]
  where getLogMessage (NotLogMessage (NotificationMessage _ _ (LogMessageParams _ msg))) = msg
        getLogMessage _ = ""

        isMessagePresent expectedMsg actualMsgs = liftIO $
            assertBool ("\"" ++ expectedMsg ++ "\" is not present in: " ++ show actualMsgs)
                       (any ((expectedMsg `isSubsequenceOf`) . show) actualMsgs)
----------------------------------------------------------------------
-- Utils
----------------------------------------------------------------------

testSession :: String -> Session () -> TestTree
testSession name = testCase name . run

testSessionWithExtraFiles :: FilePath -> String -> (FilePath -> Session ()) -> TestTree
testSessionWithExtraFiles prefix name = testCase name . runWithExtraFiles prefix

testSession' :: String -> (FilePath -> Session ()) -> TestTree
testSession' name = testCase name . run'

testSessionWait :: String -> Session () -> TestTree
testSessionWait name = testSession name .
      -- Check that any diagnostics produced were already consumed by the test case.
      --
      -- If in future we add test cases where we don't care about checking the diagnostics,
      -- this could move elsewhere.
      --
      -- Experimentally, 0.5s seems to be long enough to wait for any final diagnostics to appear.
      ( >> expectNoMoreDiagnostics 0.5)

pickActionWithTitle :: T.Text -> [CAResult] -> IO CodeAction
pickActionWithTitle title actions = do
  assertBool ("Found no matching actions for " <> show title <> " in " <> show titles) (not $ null matches)
  return $ head matches
  where
    titles =
        [ actionTitle
        | CACodeAction CodeAction { _title = actionTitle } <- actions
        ]
    matches =
        [ action
        | CACodeAction action@CodeAction { _title = actionTitle } <- actions
        , title == actionTitle
        ]

mkRange :: Int -> Int -> Int -> Int -> Range
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

-- | Takes a directory as well as relative paths to where we should launch the executable as well as the session root.
runInDir' :: FilePath -> FilePath -> FilePath -> [String] -> Session a -> IO a
runInDir' dir startExeIn startSessionIn extraOptions s = do
  ghcideExe <- locateGhcideExecutable
  let startDir = dir </> startExeIn
  let projDir = dir </> startSessionIn

  createDirectoryIfMissing True startDir
  createDirectoryIfMissing True projDir
  -- Temporarily hack around https://github.com/mpickering/hie-bios/pull/56
  -- since the package import test creates "Data/List.hs", which otherwise has no physical home
  createDirectoryIfMissing True $ projDir ++ "/Data"

  let cmd = unwords $
       [ghcideExe, "--lsp", "--test", "--verbose", "--cwd", startDir] ++ extraOptions
  -- HIE calls getXgdDirectory which assumes that HOME is set.
  -- Only sets HOME if it wasn't already set.
  setEnv "HOME" "/homeless-shelter" False
  let lspTestCaps = fullCaps { _window = Just $ WindowClientCapabilities $ Just True }
  logColor <- fromMaybe True <$> checkEnv "LSP_TEST_LOG_COLOR"
  runSessionWithConfig conf{logColor} cmd lspTestCaps projDir s
  where
    checkEnv :: String -> IO (Maybe Bool)
    checkEnv s = fmap convertVal <$> getEnv s
    convertVal "0" = False
    convertVal _ = True

    conf = defaultConfig
      -- uncomment this or set LSP_TEST_LOG_STDERR=1 to see all logging
    --   { logStdErr = True }
    --   uncomment this or set LSP_TEST_LOG_MESSAGES=1 to see all messages
    --   { logMessages = True }

openTestDataDoc :: FilePath -> Session TextDocumentIdentifier
openTestDataDoc path = do
  source <- liftIO $ readFileUtf8 $ "test/data" </> path
  createDoc path "haskell" source

findCodeActions :: TextDocumentIdentifier -> Range -> [T.Text] -> Session [CodeAction]
findCodeActions = findCodeActions' (==) "is not a superset of"

findCodeActionsByPrefix :: TextDocumentIdentifier -> Range -> [T.Text] -> Session [CodeAction]
findCodeActionsByPrefix = findCodeActions' T.isPrefixOf "is not prefix of"

findCodeActions' :: (T.Text -> T.Text -> Bool) -> String -> TextDocumentIdentifier -> Range -> [T.Text] -> Session [CodeAction]
findCodeActions' op errMsg doc range expectedTitles = do
  actions <- getCodeActions doc range
  let matches = sequence
        [ listToMaybe
          [ action
          | CACodeAction action@CodeAction { _title = actionTitle } <- actions
          , expectedTitle `op` actionTitle]
        | expectedTitle <- expectedTitles]
  let msg = show
            [ actionTitle
            | CACodeAction CodeAction { _title = actionTitle } <- actions
            ]
            ++ " " <> errMsg <> " "
            ++ show expectedTitles
  liftIO $ case matches of
    Nothing -> assertFailure msg
    Just _ -> pure ()
  return (fromJust matches)

findCodeAction :: TextDocumentIdentifier -> Range -> T.Text -> Session CodeAction
findCodeAction doc range t = head <$> findCodeActions doc range [t]

unitTests :: TestTree
unitTests = do
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
     , testCase "Key with empty file path roundtrips via Binary"  $
         Binary.decode (Binary.encode (Q ((), emptyFilePath))) @?= Q ((), emptyFilePath)
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
     ]

positionMappingTests :: TestTree
positionMappingTests =
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
                        let newRope = applyChange rope (TextDocumentContentChangeEvent (Just range) Nothing replacement)
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
    row <- choose (0, max 0 $ rows - 1)
    let columns = Rope.columns (nthLine row r)
    column <- choose (0, max 0 $ columns - 1)
    pure $ Position row column
    where rows = Rope.rows r

genRange :: Rope -> Gen Range
genRange r = do
    startPos@(Position startLine startColumn) <- genPosition r
    let maxLineDiff = max 0 $ rows - 1 - startLine
    endLine <- choose (startLine, startLine + maxLineDiff)
    let columns = Rope.columns (nthLine endLine r)
    endColumn <-
        if startLine == endLine
            then choose (startColumn, columns)
            else choose (0, max 0 $ columns - 1)
    pure $ Range startPos (Position endLine endColumn)
    where rows = Rope.rows r

-- | Get the ith line of a rope, starting from 0. Trailing newline not included.
nthLine :: Int -> Rope -> Rope
nthLine i r
    | i < 0 = error $ "Negative line number: " <> show i
    | i == 0 && Rope.rows r == 0 = r
    | i >= Rope.rows r = error $ "Row number out of bounds: " <> show i <> "/" <> show (Rope.rows r)
    | otherwise = Rope.takeWhile (/= '\n') $ fst $ Rope.splitAtLine 1 $ snd $ Rope.splitAtLine (i - 1) r

getWatchedFilesSubscriptionsUntil :: forall end . (FromJSON end, Typeable end) => Session [Maybe Value]
getWatchedFilesSubscriptionsUntil = do
      msgs <- manyTill (Just <$> message @RegisterCapabilityRequest <|> Nothing <$ anyMessage) (message @end)
      return
            [ args
            | Just RequestMessage{_params = RegistrationParams (List regs)} <- msgs
            , Registration _id WorkspaceDidChangeWatchedFiles args <- regs
            ]

-- | Version of 'System.IO.Extra.withTempDir' that canonicalizes the path
-- Which we need to do on macOS since the $TMPDIR can be in @/private/var@ or
-- @/var@
withTempDir :: (FilePath -> IO a) -> IO a
withTempDir f = System.IO.Extra.withTempDir $ \dir -> do
  dir' <- canonicalizePath dir
  f dir'
