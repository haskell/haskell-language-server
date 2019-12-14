-- Copyright (c) 2019 The DAML Authors. All rights reserved.
-- SPDX-License-Identifier: Apache-2.0

{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE CPP #-}
#include "ghc-api-version.h"

module Main (main) where

import Control.Applicative.Combinators
import Control.Monad
import Control.Monad.IO.Class (liftIO)
import Data.Char (toLower)
import Data.Foldable
import Development.IDE.GHC.Util
import qualified Data.Text as T
import Development.IDE.Test
import Development.IDE.Test.Runfiles
import Language.Haskell.LSP.Test
import Language.Haskell.LSP.Types
import Language.Haskell.LSP.Types.Capabilities
import System.Environment.Blank (setEnv)
import System.FilePath
import System.IO.Extra
import System.Directory
import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.ExpectedFailure
import Data.Maybe

main :: IO ()
main = defaultMain $ testGroup "HIE"
  [ testSession "open close" $ do
      doc <- openDoc' "Testing.hs" "haskell" ""
      void (message :: Session WorkDoneProgressCreateRequest)
      void (message :: Session WorkDoneProgressBeginNotification)
      closeDoc doc
      void (message :: Session WorkDoneProgressEndNotification)
  , initializeResponseTests
  , diagnosticTests
  , codeActionTests
  , findDefinitionAndHoverTests
  , pluginTests
  , thTests
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
    , chk "NO completion"               _completionProvider  Nothing
    , chk "NO signature help"        _signatureHelpProvider  Nothing
    , chk "   goto definition"          _definitionProvider (Just True)
    , chk "NO goto type definition" _typeDefinitionProvider (Just $ GotoOptionsStatic False)
    , chk "NO goto implementation"  _implementationProvider (Just $ GotoOptionsStatic False)
    , chk "NO find references"          _referencesProvider  Nothing
    , chk "NO doc highlight"     _documentHighlightProvider  Nothing
    , chk "NO doc symbol"           _documentSymbolProvider  Nothing
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
    , chk "   execute command"      _executeCommandProvider (Just $ ExecuteCommandOptions $ List ["typesignature.add"])
    , chk "NO workspace"                         _workspace  nothingWorkspace
    , chk "NO experimental"                   _experimental  Nothing
    ] where

      tds = Just (TDSOptions (TextDocumentSyncOptions
                              { _openClose = Just True
                              , _change    = Just TdSyncIncremental
                              , _willSave  = Nothing
                              , _willSaveWaitUntil = Nothing
                              , _save = Just (SaveOptions {_includeText = Nothing})}))

      nothingWorkspace = Just (WorkspaceOptions {_workspaceFolders = Nothing})

      chk :: (Eq a, Show a) => TestName -> (InitializeResponseCapabilitiesInner -> a) -> a -> TestTree
      chk title getActual expected =
        testCase title $ getInitializeResponse >>= \ir -> expected @=? (getActual . innerCaps) ir

  innerCaps :: InitializeResponse -> InitializeResponseCapabilitiesInner
  innerCaps (ResponseMessage _ _ (Just (InitializeResponseCapabilities c)) _) = c
  innerCaps  _ = error "this test only expects inner capabilities"

  acquire :: IO InitializeResponse
  acquire = run initializeResponse

  release :: InitializeResponse -> IO ()
  release = const $ pure ()


diagnosticTests :: TestTree
diagnosticTests = testGroup "diagnostics"
  [ testSessionWait "fix syntax error" $ do
      let content = T.unlines [ "module Testing wher" ]
      doc <- openDoc' "Testing.hs" "haskell" content
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
      doc <- openDoc' "Testing.hs" "haskell" content
      void (message :: Session WorkDoneProgressCreateRequest)
      void (message :: Session WorkDoneProgressBeginNotification)
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
            , "foo a b = a + ab"
            , "bar :: Int -> Int -> Int"
            , "bar a b = cd + b"
            ]
      _ <- openDoc' "Testing.hs" "haskell" content
      expectDiagnostics
        [ ( "Testing.hs"
          , [ (DsError, (2, 14), "Variable not in scope: ab")
            , (DsError, (4, 10), "Variable not in scope: cd")
            ]
          )
        ]
  , testSessionWait "type error" $ do
      let content = T.unlines
            [ "module Testing where"
            , "foo :: Int -> String -> Int"
            , "foo a b = a + b"
            ]
      _ <- openDoc' "Testing.hs" "haskell" content
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
      _ <- openDoc' "Testing.hs" "haskell" content
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
          , "import A"
          , "b :: Float"
          , "b = True"]
        bMessage = "Couldn't match expected type 'Float' with actual type 'Bool'"
        expectedDs aMessage =
          [ ("A.hs", [(DsError, (2,4), aMessage)])
          , ("B.hs", [(DsError, (3,4), bMessage)])]
        deferralTest title binding msg = testSessionWait title $ do
          _ <- openDoc' "A.hs" "haskell" $ sourceA binding
          _ <- openDoc' "B.hs" "haskell"   sourceB
          expectDiagnostics $ expectedDs msg
    in
    [ deferralTest "type error"          "True"    "Couldn't match expected type"
    , deferralTest "typed hole"          "_"       "Found hole"
    , deferralTest "out of scope var"    "unbound" "Variable not in scope"
    , deferralTest "message shows error" "True"    "A.hs:3:5: error:"
    ]

  , testSessionWait "remove required module" $ do
      let contentA = T.unlines [ "module ModuleA where" ]
      docA <- openDoc' "ModuleA.hs" "haskell" contentA
      let contentB = T.unlines
            [ "module ModuleB where"
            , "import ModuleA"
            ]
      _ <- openDoc' "ModuleB.hs" "haskell" contentB
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
            , "import ModuleA"
            ]
      _ <- openDoc' "ModuleB.hs" "haskell" contentB
      expectDiagnostics [("ModuleB.hs", [(DsError, (1, 7), "Could not find module")])]
      let contentA = T.unlines [ "module ModuleA where" ]
      _ <- openDoc' "ModuleA.hs" "haskell" contentA
      expectDiagnostics [("ModuleB.hs", [])]
  , testSessionWait "cyclic module dependency" $ do
      let contentA = T.unlines
            [ "module ModuleA where"
            , "import ModuleB"
            ]
      let contentB = T.unlines
            [ "module ModuleB where"
            , "import ModuleA"
            ]
      _ <- openDoc' "ModuleA.hs" "haskell" contentA
      _ <- openDoc' "ModuleB.hs" "haskell" contentB
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
            [ "module ModuleB where"
            , "import ModuleA"
            ]
      let contentBboot = T.unlines
            [ "module ModuleB where"
            ]
      _ <- openDoc' "ModuleA.hs" "haskell" contentA
      _ <- openDoc' "ModuleB.hs" "haskell" contentB
      _ <- openDoc' "ModuleB.hs-boot" "haskell" contentBboot
      expectDiagnostics []
  , testSessionWait "correct reference used with hs-boot" $ do
      let contentB = T.unlines
            [ "module ModuleB where"
            , "import {-# SOURCE #-} ModuleA"
            ]
      let contentA = T.unlines
            [ "module ModuleA where"
            , "import ModuleB"
            , "x = 5"
            ]
      let contentAboot = T.unlines
            [ "module ModuleA where"
            ]
      let contentC = T.unlines
            [ "module ModuleC where"
            , "import ModuleA"
            -- this reference will fail if it gets incorrectly
            -- resolved to the hs-boot file
            , "y = x"
            ]
      _ <- openDoc' "ModuleB.hs" "haskell" contentB
      _ <- openDoc' "ModuleA.hs" "haskell" contentA
      _ <- openDoc' "ModuleA.hs-boot" "haskell" contentAboot
      _ <- openDoc' "ModuleC.hs" "haskell" contentC
      expectDiagnostics []
  , testSessionWait "redundant import" $ do
      let contentA = T.unlines ["module ModuleA where"]
      let contentB = T.unlines
            [ "{-# OPTIONS_GHC -Wunused-imports #-}"
            , "module ModuleB where"
            , "import ModuleA"
            ]
      _ <- openDoc' "ModuleA.hs" "haskell" contentA
      _ <- openDoc' "ModuleB.hs" "haskell" contentB
      expectDiagnostics
        [ ( "ModuleB.hs"
          , [(DsWarning, (2, 0), "The import of 'ModuleA' is redundant")]
          )
        ]
  , testSessionWait "package imports" $ do
      let thisDataListContent = T.unlines
            [ "module Data.List where"
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
      _ <- openDoc' "Data/List.hs" "haskell" thisDataListContent
      _ <- openDoc' "Main.hs" "haskell" mainContent
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
            , "foo a = 1"
            ]
      _ <- openDoc' "Foo.hs" "haskell" fooContent
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
              in filePathToUri (joinDrive (map toLower drive ) suffix)
          liftIO $ createDirectoryIfMissing True (takeDirectory pathB)
          liftIO $ writeFileUTF8 pathB $ T.unpack bContent
          uriA <- getDocUri "A/A.hs"
          Just pathA <- pure $ uriToFilePath uriA
          uriA <- pure $
              let (drive, suffix) = splitDrive pathA
              in filePathToUri (joinDrive (map toLower drive ) suffix)
          let itemA = TextDocumentItem uriA "haskell" 0 aContent
          let a = TextDocumentIdentifier uriA
          sendNotification TextDocumentDidOpen (DidOpenTextDocumentParams itemA)
          diagsNot <- skipManyTill anyMessage message :: Session PublishDiagnosticsNotification
          let PublishDiagnosticsParams fileUri diags = _params (diagsNot :: PublishDiagnosticsNotification)
          -- Check that if we put a lower-case drive in for A.A
          -- the diagnostics for A.B will also be lower-case.
          liftIO $ fileUri @?= uriB
          let msg = _message (head (toList diags) :: Diagnostic)
          liftIO $ unless ("redundant" `T.isInfixOf` msg) $
              assertFailure ("Expected redundant import but got " <> T.unpack msg)
          closeDoc a
  ]

codeActionTests :: TestTree
codeActionTests = testGroup "code actions"
  [ renameActionTests
  , typeWildCardActionTests
  , removeImportTests
  , importRenameActionTests
  , fillTypedHoleTests
  , addSigActionTests
  ]

renameActionTests :: TestTree
renameActionTests = testGroup "rename actions"
  [ testSession "change to local variable name" $ do
      let content = T.unlines
            [ "module Testing where"
            , "foo :: Int -> Int"
            , "foo argName = argNme"
            ]
      doc <- openDoc' "Testing.hs" "haskell" content
      _ <- waitForDiagnostics
      [CACodeAction action@CodeAction { _title = actionTitle }]
          <- getCodeActions doc (Range (Position 2 14) (Position 2 20))
      liftIO $ "Replace with ‘argName’" @=? actionTitle
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
      doc <- openDoc' "Testing.hs" "haskell" content
      _ <- waitForDiagnostics
      [CACodeAction action@CodeAction { _title = actionTitle }]
          <- getCodeActions doc (Range (Position 3 6) (Position 3 16))
      liftIO $ "Replace with ‘maybeToList’" @=? actionTitle
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
      doc <- openDoc' "Testing.hs" "haskell" content
      _ <- waitForDiagnostics
      actionsOrCommands <- getCodeActions doc (Range (Position 2 36) (Position 2 45))
      let actionTitles = [ actionTitle | CACodeAction CodeAction{ _title = actionTitle } <- actionsOrCommands ]
          expectedActionTitles = ["Replace with ‘argument1’", "Replace with ‘argument2’", "Replace with ‘argument3’"]
      liftIO $ expectedActionTitles @=? actionTitles
  , testSession "change infix function" $ do
      let content = T.unlines
            [ "module Testing where"
            , "monus :: Int -> Int"
            , "monus x y = max 0 (x - y)"
            , "foo x y = x `monnus` y"
            ]
      doc <- openDoc' "Testing.hs" "haskell" content
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
      doc <- openDoc' "Testing.hs" "haskell" content
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
      doc <- openDoc' "Testing.hs" "haskell" content
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
      doc <- openDoc' "Testing.hs" "haskell" content
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
      _docA <- openDoc' "ModuleA.hs" "haskell" contentA
      let contentB = T.unlines
            [ "{-# OPTIONS_GHC -Wunused-imports #-}"
            , "module ModuleB where"
            , "import ModuleA"
            , "stuffB = 123"
            ]
      docB <- openDoc' "ModuleB.hs" "haskell" contentB
      _ <- waitForDiagnostics
      [CACodeAction action@CodeAction { _title = actionTitle }]
          <- getCodeActions docB (Range (Position 2 0) (Position 2 5))
      liftIO $ "Remove import" @=? actionTitle
      executeCodeAction action
      contentAfterAction <- documentContents docB
      let expectedContentAfterAction = T.unlines
            [ "{-# OPTIONS_GHC -Wunused-imports #-}"
            , "module ModuleB where"
            , "stuffB = 123"
            ]
      liftIO $ expectedContentAfterAction @=? contentAfterAction
  , testSession "qualified redundant" $ do
      let contentA = T.unlines
            [ "module ModuleA where"
            ]
      _docA <- openDoc' "ModuleA.hs" "haskell" contentA
      let contentB = T.unlines
            [ "{-# OPTIONS_GHC -Wunused-imports #-}"
            , "module ModuleB where"
            , "import qualified ModuleA"
            , "stuffB = 123"
            ]
      docB <- openDoc' "ModuleB.hs" "haskell" contentB
      _ <- waitForDiagnostics
      [CACodeAction action@CodeAction { _title = actionTitle }]
          <- getCodeActions docB (Range (Position 2 0) (Position 2 5))
      liftIO $ "Remove import" @=? actionTitle
      executeCodeAction action
      contentAfterAction <- documentContents docB
      let expectedContentAfterAction = T.unlines
            [ "{-# OPTIONS_GHC -Wunused-imports #-}"
            , "module ModuleB where"
            , "stuffB = 123"
            ]
      liftIO $ expectedContentAfterAction @=? contentAfterAction
  ]

importRenameActionTests :: TestTree
importRenameActionTests = testGroup "import rename actions"
  [ testSession "Data.Mape -> Data.Map"   $ check "Map"
  , testSession "Data.Mape -> Data.Maybe" $ check "Maybe" ] where
  check modname = do
      let content = T.unlines
            [ "module Testing where"
            , "import Data.Mape"
            ]
      doc <- openDoc' "Testing.hs" "haskell" content
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

    ]

  check :: T.Text -> T.Text -> T.Text -> T.Text -> T.Text -> T.Text -> T.Text -> TestTree
  check actionTitle
        oldA oldB oldC
        newA newB newC = testSession (T.unpack actionTitle) $ do
    let originalCode = sourceCode oldA oldB oldC
    let expectedCode = sourceCode newA newB newC
    doc <- openDoc' "Testing.hs" "haskell" originalCode
    _ <- waitForDiagnostics
    actionsOrCommands <- getCodeActions doc (Range (Position 9 0) (Position 9 maxBound))
    let chosenAction = pickActionWithTitle actionTitle actionsOrCommands
    executeCodeAction chosenAction
    modifiedCode <- documentContents doc
    liftIO $ expectedCode @=? modifiedCode
  in
  testGroup "fill typed holes"
  [ check "replace hole `_` with show"
          "_"    "n" "n"
          "show" "n" "n"

  , check "replace hole `_` with globalConvert"
          "_"             "n" "n"
          "globalConvert" "n" "n"

#if MIN_GHC_API_VERSION(8,6,0)
  , check "replace hole `_convertme` with localConvert"
          "_convertme"   "n" "n"
          "localConvert" "n" "n"
#endif

  , check "replace hole `_b` with globalInt"
          "_a" "_b"        "_c"
          "_a" "globalInt" "_c"

  , check "replace hole `_c` with globalInt"
          "_a" "_b"        "_c"
          "_a" "_b" "globalInt"

#if MIN_GHC_API_VERSION(8,6,0)
  , check "replace hole `_c` with parameterInt"
          "_a" "_b" "_c"
          "_a" "_b"  "parameterInt"
#endif
  ]

addSigActionTests :: TestTree
addSigActionTests = let
  header = T.unlines [ "{-# OPTIONS_GHC -Wmissing-signatures #-}"
                     , "module Sigs where"]
  before  def     = T.unlines [header,      def]
  after'  def sig = T.unlines [header, sig, def]

  def >:: sig = testSession (T.unpack def) $ do
    let originalCode = before def
    let expectedCode = after'  def sig
    doc <- openDoc' "Sigs.hs" "haskell" originalCode
    _ <- waitForDiagnostics
    actionsOrCommands <- getCodeActions doc (Range (Position 3 1) (Position 3 maxBound))
    let chosenAction = pickActionWithTitle ("add signature: " <> sig) actionsOrCommands
    executeCodeAction chosenAction
    modifiedCode <- documentContents doc
    liftIO $ expectedCode @=? modifiedCode
  in
  testGroup "add signature"
  [ "abc = True"             >:: "abc :: Bool"
  , "foo a b = a + b"        >:: "foo :: Num a => a -> a -> a"
  , "bar a b = show $ a + b" >:: "bar :: (Show a, Num a) => a -> a -> String"
  , "(!!!) a b = a > b"      >:: "(!!!) :: Ord a => a -> a -> Bool"
  , "a >>>> b = a + b"       >:: "(>>>>) :: Num a => a -> a -> a"
  , "a `haha` b = a b"       >:: "haha :: (t1 -> t2) -> t1 -> t2"
  ]

findDefinitionAndHoverTests :: TestTree
findDefinitionAndHoverTests = let

  tst (get, check) pos targetRange title = testSession title $ do
    doc <- openTestDataDoc sourceFilePath
    found <- get doc pos
    check found targetRange

  checkDefs :: [Location] -> [Expect] -> Session ()
  checkDefs defs expectations = traverse_ check expectations where

    check (ExpectRange expectedRange) = do
      assertNDefinitionsFound 1 defs
      assertRangeCorrect (head defs) expectedRange
    check ExpectExternFail = liftIO $ assertFailure "Expecting to fail to find in external file"
    check _ = pure () -- all other expectations not relevant to getDefinition

  assertNDefinitionsFound :: Int -> [a] -> Session ()
  assertNDefinitionsFound n defs = liftIO $ assertEqual "number of definitions" n (length defs)

  assertRangeCorrect Location{_range = foundRange} expectedRange =
    liftIO $ expectedRange @=? foundRange

  checkHover :: Maybe Hover -> [Expect] -> Session ()
  checkHover hover expectations = traverse_ check expectations where

    check expected =
      case hover of
        Nothing -> liftIO $ assertFailure "no hover found"
        Just Hover{_contents = (HoverContents MarkupContent{_value = msg})
                  ,_range    = rangeInHover } ->
          case expected of
            ExpectRange  expectedRange -> checkHoverRange expectedRange rangeInHover msg
            ExpectHoverRange expectedRange -> checkHoverRange expectedRange rangeInHover msg
            ExpectHoverText snippets -> liftIO $ traverse_ (`assertFoundIn` msg) snippets
            _ -> pure () -- all other expectations not relevant to hover
        _ -> liftIO $ assertFailure $ "test not expecting this kind of hover info" <> show hover

  extractLineColFromHoverMsg :: T.Text -> [T.Text]
  extractLineColFromHoverMsg = T.splitOn ":" . head . T.splitOn "**" . last . T.splitOn (sourceFileName <> ":")

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
    , testGroup "hover"      $ mapMaybe snd tests ]

  test runDef runHover look expect title =
    ( runDef   $ tst def   look expect title
    , runHover $ tst hover look expect title ) where
      def   = (getDefinitions, checkDefs)
      hover = (getHover      , checkHover)
      --type_ = (getTypeDefinitions, checkTDefs) -- getTypeDefinitions always times out

  -- search locations            expectations on results
  fffL4  = _start fffR     ;  fffR = mkRange 4  4    4  7 ; fff  = [ExpectRange fffR]
  fffL8  = Position  8  4  ;
  fffL14 = Position 14  7  ;
  aaaL14 = Position 14 20  ;  aaa    = [mkR   7  0    7  3]
  dcL7   = Position  7 11  ;  tcDC   = [mkR   3 23    5 16]
  dcL12  = Position 12 11  ;
  xtcL5  = Position  5 11  ;  xtc    = [ExpectExternFail]
  tcL6   = Position  6 11  ;  tcData = [mkR   3  0    5 16]
  vvL16  = Position 16 12  ;  vv     = [mkR  16  4   16  6]
  opL16  = Position 16 15  ;  op     = [mkR  17  2   17  4]
  opL18  = Position 18 22  ;  opp    = [mkR  18 13   18 17]
  aL18   = Position 18 20  ;  apmp   = [mkR  18 10   18 11]
  b'L19  = Position 19 13  ;  bp     = [mkR  19  6   19  7]
  xvL20  = Position 20  8  ;  xvMsg  = [ExpectHoverText ["Data.Text.pack", ":: String -> Text"], ExpectExternFail]
  clL23  = Position 23 11  ;  cls    = [mkR  21  0   22 20]
  clL25  = Position 25  9
  eclL15 = Position 15  8  ;  ecls   = [ExpectHoverText ["Num"], ExpectExternFail]
  dnbL29 = Position 29 18  ;  dnb    = [ExpectHoverText [":: ()"],   mkR  29 12   29 21]
  dnbL30 = Position 30 23
  lcbL33 = Position 33 26  ;  lcb    = [ExpectHoverText [":: Char"], mkR  33 26   33 27]
  lclL33 = Position 33 22
  in
  mkFindTests
  --     def    hover  look   expect
  [ test yes    yes    fffL4  fff    "field in record definition"
  , test broken broken fffL8  fff    "field in record construction"
  , test yes    yes    fffL14 fff    "field name used as accessor"   -- 120 in Calculate.hs
  , test yes    yes    aaaL14 aaa    "top-level name"                -- 120
  , test broken broken dcL7   tcDC   "record data constructor"
  , test yes    yes    dcL12  tcDC   "plain  data constructor"       -- 121
  , test yes    broken tcL6   tcData "type constructor"              -- 147
  , test broken broken xtcL5  xtc    "type constructor from other package"
  , test broken yes    xvL20  xvMsg  "value from other package"      -- 120
  , test yes    yes    vvL16  vv     "plain parameter"               -- 120
  , test yes    yes    aL18   apmp   "pattern match name"            -- 120
  , test yes    yes    opL16  op     "top-level operator"            -- 120, 123
  , test yes    yes    opL18  opp    "parameter operator"            -- 120
  , test yes    yes    b'L19  bp     "name in backticks"             -- 120
  , test yes    broken clL23  cls    "class in instance declaration"
  , test yes    broken clL25  cls    "class in signature"            -- 147
  , test broken broken eclL15 ecls   "external class in signature"
  , test yes    yes    dnbL29 dnb    "do-notation   bind"            -- 137
  , test yes    yes    dnbL30 dnb    "do-notation lookup"
  , test yes    yes    lcbL33 lcb    "listcomp   bind"               -- 137
  , test yes    yes    lclL33 lcb    "listcomp lookup"
  ]
  where yes, broken :: (TestTree -> Maybe TestTree)
        yes    = Just -- test should run and pass
        broken = Just . (`xfail` "known broken")
        -- no = const Nothing -- don't run this test at all

pluginTests :: TestTree
pluginTests = testSessionWait "plugins" $ do
  let content =
        T.unlines
          [ "{-# OPTIONS_GHC -fplugin GHC.TypeLits.KnownNat.Solver #-}"
          , "{-# LANGUAGE DataKinds, ScopedTypeVariables, TypeOperators #-}"
          , "module Testing where"
          , "import Data.Proxy"
          , "import GHC.TypeLits"
          -- This function fails without plugins being initialized.
          , "f :: forall n. KnownNat n => Proxy n -> Integer"
          , "f _ = natVal (Proxy :: Proxy n) + natVal (Proxy :: Proxy (n+2))"
          , "foo :: Int -> Int -> Int"
          , "foo a b = a + c"
          ]
  _ <- openDoc' "Testing.hs" "haskell" content
  expectDiagnostics
    [ ( "Testing.hs",
        [(DsError, (8, 14), "Variable not in scope: c")]
      )
    ]

thTests :: TestTree
thTests =
  testGroup
    "TemplateHaskell"
    [ -- Test for https://github.com/digital-asset/ghcide/pull/212
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
        _ <- openDoc' "A.hs" "haskell" sourceA
        _ <- openDoc' "B.hs" "haskell" sourceB
        expectDiagnostics [ ( "B.hs", [(DsError, (6, 29), "Variable not in scope: n")] ) ]
    ]

xfail :: TestTree -> String -> TestTree
xfail = flip expectFailBecause

data Expect
  = ExpectRange Range -- Both gotoDef and hover should report this range
--  | ExpectDefRange Range -- Only gotoDef should report this range
  | ExpectHoverRange Range -- Only hover should report this range
  | ExpectHoverText [T.Text] -- the hover message must contain these snippets
  | ExpectExternFail -- definition lookup in other file expected to fail
--  | ExpectExtern -- TODO: as above, but expected to succeed: need some more info in here, once we have some working examples

mkR :: Int -> Int -> Int -> Int -> Expect
mkR startLine startColumn endLine endColumn = ExpectRange $ mkRange startLine startColumn endLine endColumn
----------------------------------------------------------------------
-- Utils


testSession :: String -> Session () -> TestTree
testSession name = testCase name . run

testSessionWait :: String -> Session () -> TestTree
testSessionWait name = testSession name .
      -- Check that any diagnostics produced were already consumed by the test case.
      --
      -- If in future we add test cases where we don't care about checking the diagnostics,
      -- this could move elsewhere.
      --
      -- Experimentally, 0.5s seems to be long enough to wait for any final diagnostics to appear.
      ( >> expectNoMoreDiagnostics 0.5)

pickActionWithTitle :: T.Text -> [CAResult] -> CodeAction
pickActionWithTitle title actions = head
  [ action
  | CACodeAction action@CodeAction{ _title = actionTitle } <- actions
  , title == actionTitle ]

mkRange :: Int -> Int -> Int -> Int -> Range
mkRange a b c d = Range (Position a b) (Position c d)

run :: Session a -> IO a
run s = withTempDir $ \dir -> do
  ghcideExe <- locateGhcideExecutable

  -- Temporarily hack around https://github.com/mpickering/hie-bios/pull/56
  -- since the package import test creates "Data/List.hs", which otherwise has no physical home
  createDirectoryIfMissing True $ dir ++ "/Data"

  let cmd = unwords [ghcideExe, "--lsp", "--cwd", dir]
  -- HIE calls getXgdDirectory which assumes that HOME is set.
  -- Only sets HOME if it wasn't already set.
  setEnv "HOME" "/homeless-shelter" False
  runSessionWithConfig conf cmd fullCaps { _window = Just $ WindowClientCapabilities $ Just True } dir s
  where
    conf = defaultConfig
      -- If you uncomment this you can see all messages
      -- which can be quite useful for debugging.
      -- { logMessages = True, logColor = False, logStdErr = True }

openTestDataDoc :: FilePath -> Session TextDocumentIdentifier
openTestDataDoc path = do
  source <- liftIO $ readFileUtf8 $ "test/data" </> path
  openDoc' path "haskell" source
