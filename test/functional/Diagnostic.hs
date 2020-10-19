{-# LANGUAGE OverloadedStrings #-}

module Diagnostic (tests) where

import Control.Applicative.Combinators
import           Control.Lens hiding (List)
import           Control.Monad.IO.Class
import           Data.Aeson (toJSON)
import qualified Data.Text as T
import qualified Data.Default
import           Ide.Logger
import           Ide.Plugin.Config
import           Language.Haskell.LSP.Test hiding (message)
import           Language.Haskell.LSP.Types
import qualified Language.Haskell.LSP.Types.Lens as LSP
import           Test.Hls.Util
import           Test.Tasty
import           Test.Tasty.ExpectedFailure (ignoreTestBecause)
import           Test.Tasty.HUnit

-- ---------------------------------------------------------------------

tests :: TestTree
tests = testGroup "diagnostics providers" [
        saveTests
        , triggerTests
        , errorTests
        , warningTests
    ]


triggerTests :: TestTree
triggerTests = testGroup "diagnostics triggers" [
    ignoreTestBecause "Broken" $
    ignoreTestBecause "Broken" $ testCase "runs diagnostics on save" $
        runSession hlsCommandExamplePlugin codeActionSupportCaps "test/testdata" $ do
            logm "starting DiagnosticSpec.runs diagnostic on save"
            doc <- openDoc "ApplyRefact2.hs" "haskell"

            diags@(reduceDiag:_) <- waitForDiagnostics

            liftIO $ do
                length diags @?= 2
                reduceDiag ^. LSP.range @?= Range (Position 1 0) (Position 1 12)
                reduceDiag ^. LSP.severity @?= Just DsInfo
                reduceDiag ^. LSP.code @?= Just (StringValue "Eta reduce")
                reduceDiag ^. LSP.source @?= Just "hlint"

            diags2a <- waitForDiagnostics

            liftIO $ length diags2a @?= 2

            sendNotification TextDocumentDidSave (DidSaveTextDocumentParams doc)

            diags3@(d:_) <- waitForDiagnosticsSource "eg2"

            liftIO $ do
                length diags3 @?= 1
                d ^. LSP.range @?= Range (Position 0 0) (Position 1 0)
                d ^. LSP.severity @?= Nothing
                d ^. LSP.code @?= Nothing
                d ^. LSP.message @?= T.pack "Example plugin diagnostic, triggered byDiagnosticOnSave"
    ]

errorTests :: TestTree
errorTests = testGroup  "typed hole errors" [
    ignoreTestBecause "Broken" $ testCase "is deferred" $
        runSession hlsCommand fullCaps "test/testdata" $ do
            _ <- openDoc "TypedHoles.hs" "haskell"
            [diag] <- waitForDiagnosticsSource "bios"
            liftIO $ diag ^. LSP.severity @?= Just DsWarning
    ]

warningTests :: TestTree
warningTests = testGroup  "Warnings are warnings" [
    ignoreTestBecause "Broken" $ testCase "Overrides -Werror" $
        runSession hlsCommand fullCaps "test/testdata/wErrorTest" $ do
            _ <- openDoc "src/WError.hs" "haskell"
            [diag] <- waitForDiagnosticsSource "bios"
            liftIO $ diag ^. LSP.severity @?= Just DsWarning
    ]

saveTests :: TestTree
saveTests = testGroup  "only diagnostics on save" [
    ignoreTestBecause "Broken" $ testCase "Respects diagnosticsOnChange setting" $
        runSession hlsCommandExamplePlugin codeActionSupportCaps "test/testdata" $ do
            let config = Data.Default.def { diagnosticsOnChange = False } :: Config
            sendNotification WorkspaceDidChangeConfiguration (DidChangeConfigurationParams (toJSON config))
            doc <- openDoc "Hover.hs" "haskell"
            diags <- waitForDiagnostics

            liftIO $ do
                length diags @?= 0

            let te = TextEdit (Range (Position 0 0) (Position 0 13)) ""
            _ <- applyEdit doc te
            skipManyTill loggingNotification noDiagnostics

            sendNotification TextDocumentDidSave (DidSaveTextDocumentParams doc)
            diags2 <- waitForDiagnostics
            liftIO $
                length diags2 @?= 1
    ]
