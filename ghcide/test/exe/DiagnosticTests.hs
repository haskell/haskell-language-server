
{-# LANGUAGE GADTs #-}

module DiagnosticTests (tests) where

import           Control.Applicative.Combinators
import qualified Control.Lens                    as Lens
import           Control.Monad
import           Control.Monad.IO.Class          (liftIO)
import           Data.List.Extra
import qualified Data.Text                       as T
import           Development.IDE.GHC.Compat      (GhcVersion (..), ghcVersion)
import           Development.IDE.GHC.Util
import           Development.IDE.Test            (diagnostic,
                                                  expectCurrentDiagnostics,
                                                  expectDiagnostics,
                                                  expectDiagnosticsWithTags,
                                                  expectNoMoreDiagnostics,
                                                  flushMessages, waitForAction)
import           Development.IDE.Types.Location
import qualified Language.LSP.Protocol.Lens      as L
import           Language.LSP.Protocol.Message
import           Language.LSP.Protocol.Types     hiding
                                                 (SemanticTokenAbsolute (..),
                                                  SemanticTokenRelative (..),
                                                  SemanticTokensEdit (..),
                                                  mkRange)
import           Language.LSP.Test
import           System.Directory
import           System.FilePath
import           System.IO.Extra                 hiding (withTempDir)

import           Config
import           Control.Lens                    ((^.))
import           Control.Monad.Extra             (whenJust)
import           Data.Default                    (def)
import           Development.IDE.Plugin.Test     (WaitForIdeRuleResult (..))
import           System.Time.Extra
import           Test.Hls                        (TestConfig (testConfigCaps, testDirLocation, testDisableKick, testPluginDescriptor),
                                                  runSessionWithTestConfig,
                                                  waitForProgressBegin)
import           Test.Hls.FileSystem             (directCradle, file, text)
import           Test.Tasty
import           Test.Tasty.HUnit

tests :: TestTree
tests = testGroup "diagnostics"
  [ testWithDummyPluginEmpty "fix syntax error" $ do
      let content = T.unlines [ "module Testing wher" ]
      doc <- createDoc "Testing.hs" "haskell" content
      expectDiagnostics [("Testing.hs", [(DiagnosticSeverity_Error, (0, 15), "parse error")])]
      let change = TextDocumentContentChangeEvent $ InL TextDocumentContentChangePartial
              { _range = Range (Position 0 15) (Position 0 19)
              , _rangeLength = Nothing
              , _text = "where"
              }
      changeDoc doc [change]
      expectDiagnostics [("Testing.hs", [])]
  , testWithDummyPluginEmpty "introduce syntax error" $ do
      let content = T.unlines [ "module Testing where" ]
      doc <- createDoc "Testing.hs" "haskell" content
      void $ skipManyTill anyMessage (message SMethod_WindowWorkDoneProgressCreate)
      waitForProgressBegin
      let change = TextDocumentContentChangeEvent$ InL TextDocumentContentChangePartial
              { _range = Range (Position 0 15) (Position 0 18)
              , _rangeLength = Nothing
              , _text = "wher"
              }
      changeDoc doc [change]
      expectDiagnostics [("Testing.hs", [(DiagnosticSeverity_Error, (0, 15), "parse error")])]
  , testWithDummyPluginEmpty "update syntax error" $ do
      let content = T.unlines [ "module Testing(missing) where" ]
      doc <- createDoc "Testing.hs" "haskell" content
      expectDiagnostics [("Testing.hs", [(DiagnosticSeverity_Error, (0, 15), "Not in scope: 'missing'")])]
      let change = TextDocumentContentChangeEvent $ InL TextDocumentContentChangePartial
              { _range = Range (Position 0 15) (Position 0 16)
              , _rangeLength = Nothing
              , _text = "l"
              }
      changeDoc doc [change]
      expectDiagnostics [("Testing.hs", [(DiagnosticSeverity_Error, (0, 15), "Not in scope: 'lissing'")])]
  , testWithDummyPluginEmpty "variable not in scope" $ do
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
          , [ (DiagnosticSeverity_Error, (2, 15), "Variable not in scope: ab")
            , (DiagnosticSeverity_Error, (4, 11), "Variable not in scope: cd")
            ]
          )
        ]
  , testWithDummyPluginEmpty "type error" $ do
      let content = T.unlines
            [ "module Testing where"
            , "foo :: Int -> String -> Int"
            , "foo a b = a + b"
            ]
      _ <- createDoc "Testing.hs" "haskell" content
      expectDiagnostics
        [ ( "Testing.hs"
          , [(DiagnosticSeverity_Error, (2, 14), "Couldn't match type '[Char]' with 'Int'")]
          )
        ]
  , testWithDummyPluginEmpty "typed hole" $ do
      let content = T.unlines
            [ "module Testing where"
            , "foo :: Int -> String"
            , "foo a = _ a"
            ]
      _ <- createDoc "Testing.hs" "haskell" content
      expectDiagnostics
        [ ( "Testing.hs"
          , [(DiagnosticSeverity_Error, (2, 8), "Found hole: _ :: Int -> String")]
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
          [ ("A.hs", [(DiagnosticSeverity_Error, (2,4), aMessage)])
          , ("B.hs", [(DiagnosticSeverity_Error, (3,4), bMessage)])]
        deferralTest title binding msg = testWithDummyPluginEmpty title $ do
          _ <- createDoc "A.hs" "haskell" $ sourceA binding
          _ <- createDoc "B.hs" "haskell"   sourceB
          expectDiagnostics $ expectedDs msg
    in
    [ deferralTest "type error"          "True"    "Couldn't match expected type"
    , deferralTest "typed hole"          "_"       "Found hole"
    , deferralTest "out of scope var"    "unbound" "Variable not in scope"
    ]

  , testWithDummyPluginEmpty "remove required module" $ do
      let contentA = T.unlines [ "module ModuleA where" ]
      docA <- createDoc "ModuleA.hs" "haskell" contentA
      let contentB = T.unlines
            [ "module ModuleB where"
            , "import ModuleA"
            ]
      _ <- createDoc "ModuleB.hs" "haskell" contentB
      let change = TextDocumentContentChangeEvent $ InL TextDocumentContentChangePartial
              { _range = Range (Position 0 0) (Position 0 20)
              , _rangeLength = Nothing
              , _text = ""
              }
      changeDoc docA [change]
      expectDiagnostics [("ModuleB.hs", [(DiagnosticSeverity_Error, (1, 0), "Could not find module")])]
  , testWithDummyPluginEmpty "add missing module" $ do
      let contentB = T.unlines
            [ "module ModuleB where"
            , "import ModuleA ()"
            ]
      _ <- createDoc "ModuleB.hs" "haskell" contentB
      expectDiagnostics [("ModuleB.hs", [(DiagnosticSeverity_Error, (1, 7), "Could not find module")])]
      let contentA = T.unlines [ "module ModuleA where" ]
      _ <- createDoc "ModuleA.hs" "haskell" contentA
      expectDiagnostics [("ModuleB.hs", [])]
  , testCase "add missing module (non workspace)" $
    runSessionWithTestConfig def {
        testPluginDescriptor = dummyPlugin
        , testConfigCaps = lspTestCapsNoFileWatches
        , testDirLocation = Right (mkIdeTestFs [])
    }
    $ \tmpDir -> do
    -- By default lsp-test sends FileWatched notifications for all files, which we don't want
    -- as non workspace modules will not be watched by the LSP server.
    -- To work around this, we tell lsp-test that our client doesn't have the
    -- FileWatched capability, which is enough to disable the notifications
      let contentB = T.unlines
            [ "module ModuleB where"
            , "import ModuleA ()"
            ]
      _ <- createDoc (tmpDir </> "ModuleB.hs") "haskell" contentB
      expectDiagnostics [(tmpDir </> "ModuleB.hs", [(DiagnosticSeverity_Error, (1, 7), "Could not find module")])]
      let contentA = T.unlines [ "module ModuleA where" ]
      _ <- createDoc (tmpDir </> "ModuleA.hs") "haskell" contentA
      expectDiagnostics [(tmpDir </> "ModuleB.hs", [])]
  , testWithDummyPluginEmpty "cyclic module dependency" $ do
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
          , [(DiagnosticSeverity_Error, (1, 7), "Cyclic module dependency between ModuleA, ModuleB")]
          )
        , ( "ModuleB.hs"
          , [(DiagnosticSeverity_Error, (1, 7), "Cyclic module dependency between ModuleA, ModuleB")]
          )
        ]
  , let contentA = T.unlines [ "module ModuleA where" , "import ModuleB" ]
        contentB = T.unlines [ "module ModuleB where" , "import ModuleA" ]
        contentC = T.unlines [ "module ModuleC where" , "import ModuleB" ]
        contentD = T.unlines [ "module ModuleD where" , "import ModuleC" ]
        cradle = directCradle ["ModuleA", "ModuleB", "ModuleC", "ModuleD"]
    in testWithDummyPlugin "deeply nested cyclic module dependency"
        (mkIdeTestFs [
            file "ModuleA.hs" (text contentA)
            ,file "ModuleB.hs" (text contentB)
            ,file "ModuleC.hs" (text contentC)
            ,cradle
        ]) $ do
      _ <- createDoc "ModuleD.hs" "haskell" contentD
      expectDiagnostics
        [ ( "ModuleB.hs" , [(DiagnosticSeverity_Error, (1, 7), "Cyclic module dependency between ModuleA, ModuleB")])
        , ( "ModuleA.hs" , [(DiagnosticSeverity_Error, (1, 7), "Cyclic module dependency between ModuleA, ModuleB")])
        ]
  , testWithDummyPluginEmpty "cyclic module dependency with hs-boot" $ do
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
      expectDiagnostics [("ModuleB.hs", [(DiagnosticSeverity_Warning, (3,0), "Top-level binding")])]
  , testWithDummyPlugin "bidirectional module dependency with hs-boot"
        (mkIdeTestFs [directCradle ["ModuleA", "ModuleB"]])
        $ do
      let contentA = T.unlines
            [ "module ModuleA where"
            , "import {-# SOURCE #-} ModuleB"
            ]
      let contentB = T.unlines
            [ "{-# OPTIONS -Wmissing-signatures#-}"
            , "module ModuleB where"
            , "import {-# SOURCE #-} ModuleA"
            -- introduce an artificial diagnostic
            , "foo = ()"
            ]
      let contentBboot = T.unlines
            [ "module ModuleB where"
            ]
      let contentAboot = T.unlines
            [ "module ModuleA where"
            ]
      _ <- createDoc "ModuleA.hs" "haskell" contentA
      _ <- createDoc "ModuleA.hs-boot" "haskell" contentAboot
      _ <- createDoc "ModuleB.hs" "haskell" contentB
      _ <- createDoc "ModuleB.hs-boot" "haskell" contentBboot
      expectDiagnostics [("ModuleB.hs", [(DiagnosticSeverity_Warning, (3,0), "Top-level binding")])]
  , testWithDummyPluginEmpty "correct reference used with hs-boot" $ do
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
      expectDiagnostics [("ModuleC.hs", [(DiagnosticSeverity_Warning, (3,0), "Top-level binding")])]
  , testWithDummyPluginEmpty "redundant import" $ do
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
          , [(DiagnosticSeverity_Warning, (2, 0), "The import of 'ModuleA' is redundant", Just DiagnosticTag_Unnecessary)]
          )
        ]
  , testWithDummyPluginEmpty "redundant import even without warning" $ do
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
      expectDiagnostics [("ModuleB.hs", [(DiagnosticSeverity_Warning, (3,0), "Top-level binding")])]
  , testWithDummyPluginEmpty "package imports" $ do
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
          , [(DiagnosticSeverity_Error, (6, 9),
                if ghcVersion >= GHC96 then
                  "Variable not in scope: ThisList.map"
                else if ghcVersion >= GHC94 then
                  "Variable not in scope: map" -- See https://gitlab.haskell.org/ghc/ghc/-/issues/22130
                else
                  "Not in scope: \8216ThisList.map\8217")
            ,(DiagnosticSeverity_Error, (7, 9),
                if ghcVersion >= GHC96 then
                  "Variable not in scope: BaseList.x"
                else if ghcVersion >= GHC94 then
                  "Variable not in scope: x" -- See https://gitlab.haskell.org/ghc/ghc/-/issues/22130
                else
                  "Not in scope: \8216BaseList.x\8217")
            ]
          )
        ]
  , testWithDummyPluginEmpty "unqualified warnings" $ do
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
          , [(DiagnosticSeverity_Warning, (2, if ghcVersion >= GHC94 then 7 else 0), "Redundant constraint: Ord a")
            ]
          )
        ]
    , testWithDummyPluginEmpty "lower-case drive" $ do
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
          sendNotification SMethod_TextDocumentDidOpen (DidOpenTextDocumentParams itemA)
          TNotificationMessage{_params = PublishDiagnosticsParams fileUri _ diags} <- skipManyTill anyMessage diagnostic
          -- Check that if we put a lower-case drive in for A.A
          -- the diagnostics for A.B will also be lower-case.
          liftIO $ fileUri @?= uriB
          let msg :: T.Text = head diags ^. L.message
          liftIO $ unless ("redundant" `T.isInfixOf` msg) $
              assertFailure ("Expected redundant import but got " <> T.unpack msg)
          closeDoc a
  , testWithDummyPluginEmpty "strip file path" $ do
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
            L.params .
            L.diagnostics .
            Lens.folded .
            L.message .
            Lens.filtered (T.isInfixOf ("/" <> name <> ".hs:"))
          failure msg = liftIO $ assertFailure $ "Expected file path to be stripped but got " <> T.unpack msg
      Lens.mapMOf_ offenders failure notification
  , testWithDummyPlugin "-Werror in cradle is ignored"
        (mkIdeTestFs [directCradle ["-Wall", "-Werror"]])
        $ do
      let fooContent = T.unlines
            [ "module Foo where"
            , "foo = ()"
            ]
      _ <- createDoc "Foo.hs" "haskell" fooContent
      expectDiagnostics
        [ ( "Foo.hs"
          , [(DiagnosticSeverity_Warning, (1, 0), "Top-level binding with no type signature:")
            ]
          )
        ]
  , testWithDummyPluginEmpty "-Werror in pragma is ignored" $ do
      let fooContent = T.unlines
            [ "{-# OPTIONS_GHC -Wall -Werror #-}"
            , "module Foo() where"
            , "foo :: Int"
            , "foo = 1"
            ]
      _ <- createDoc "Foo.hs" "haskell" fooContent
      expectDiagnostics
        [ ( "Foo.hs"
          , [(DiagnosticSeverity_Warning, (3, 0), "Defined but not used:")
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
      [("P.hs", [(DiagnosticSeverity_Warning,(4,0), "Top-level binding")])] -- So that we know P has been loaded

    -- Change y from Int to B which introduces a type error in A (imported from P)
    changeDoc bdoc [TextDocumentContentChangeEvent . InR . TextDocumentContentChangeWholeDocument $
                    T.unlines ["module B where", "y :: Bool", "y = undefined"]]
    expectDiagnostics
      [("A.hs", [(DiagnosticSeverity_Error, (5, 4), "Couldn't match expected type 'Int' with actual type 'Bool'")])
      ]

    -- Open A and edit to fix the type error
    adoc <- createDoc aPath "haskell" aSource
    changeDoc adoc [TextDocumentContentChangeEvent . InR . TextDocumentContentChangeWholeDocument $
                    T.unlines ["module A where", "import B", "x :: Bool", "x = y"]]

    expectDiagnostics
      [ ( "P.hs",
          [ (DiagnosticSeverity_Error, (4, 6), "Couldn't match expected type 'Int' with actual type 'Bool'"),
            (DiagnosticSeverity_Warning, (4, 0), "Top-level binding")
          ]
        ),
        ("A.hs", [])
      ]
    expectNoMoreDiagnostics 1

  , testWithDummyPluginEmpty "deduplicate missing module diagnostics" $  do
      let fooContent = T.unlines [ "module Foo() where" , "import MissingModule" ]
      doc <- createDoc "Foo.hs" "haskell" fooContent
      expectDiagnostics [("Foo.hs", [(DiagnosticSeverity_Error, (1,7), "Could not find module 'MissingModule'")])]

      changeDoc doc [TextDocumentContentChangeEvent . InR . TextDocumentContentChangeWholeDocument $ "module Foo() where" ]
      expectDiagnostics []

      changeDoc doc [TextDocumentContentChangeEvent . InR . TextDocumentContentChangeWholeDocument $ T.unlines
            [ "module Foo() where" , "import MissingModule" ] ]
      expectDiagnostics [("Foo.hs", [(DiagnosticSeverity_Error, (1,7), "Could not find module 'MissingModule'")])]

  , testGroup "Cancellation"
    [ cancellationTestGroup "edit header" editHeader yesSession noParse  noTc
    , cancellationTestGroup "edit import" editImport noSession  yesParse noTc
    , cancellationTestGroup "edit body"   editBody   yesSession yesParse yesTc
    ]
  ]
  where
      editPair x y = let p = Position x y ; p' = Position x (y+2) in
        (TextDocumentContentChangeEvent $ InL TextDocumentContentChangePartial
              { _range = Range p p
              , _rangeLength = Nothing
              , _text = "fd"
              }

        ,TextDocumentContentChangeEvent $ InL TextDocumentContentChangePartial
              { _range = Range p p'
              , _rangeLength = Nothing
              , _text = ""
              }
        )
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
      let missingSigDiags = [(DiagnosticSeverity_Warning, (3, 0), "Top-level binding") ]
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
        runTestNoKick s =
            runSessionWithTestConfig def {
                testPluginDescriptor = dummyPlugin
                , testDirLocation = Right (mkIdeTestFs [])
                , testDisableKick = True
                } $ const s

        typeCheck doc = do
            WaitForIdeRuleResult {..} <- waitForAction "TypeCheck" doc
            liftIO $ assertBool "The file should typecheck" ideResultSuccess
            -- wait for the debouncer to publish diagnostics if the rule runs
            liftIO $ sleep 0.2
            -- flush messages to ensure current diagnostics state is updated
            flushMessages
