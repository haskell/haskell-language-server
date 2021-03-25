{-# LANGUAGE OverloadedStrings #-}

module Diagnostic (tests) where

import           Control.Lens            hiding (List)
import           Data.Aeson              (toJSON)
import qualified Data.Default
import           Ide.Plugin.Config
import qualified Language.LSP.Types.Lens as LSP
import           Test.Hls
import           Test.Hls.Command

-- ---------------------------------------------------------------------

tests :: TestTree
tests = testGroup "diagnostics providers" [
        basicTests
        , saveTests
        , warningTests
    ]

basicTests :: TestTree
basicTests = testGroup "Diagnostics work" [
    testCase "hlint produces diagnostics" $
        runSession hlsCommand fullCaps "test/testdata/hlint" $ do
            doc <- openDoc "ApplyRefact2.hs" "haskell"
            diags <- waitForDiagnosticsFromSource doc "hlint"
            reduceDiag <- liftIO $ inspectDiagnostic diags ["Eta reduce"]
            redundantID <- liftIO $ inspectDiagnostic diags ["Redundant id"]
            liftIO $ do
                length diags @?= 2
                reduceDiag ^. LSP.range @?= Range (Position 1 0) (Position 1 12)
                reduceDiag ^. LSP.severity @?= Just DsInfo
                redundantID ^. LSP.severity @?= Just DsInfo

    , testCase "example plugin produces diagnostics" $
        runSession hlsCommandExamplePlugin fullCaps "test/testdata/hlint" $ do
            doc <- openDoc "ApplyRefact2.hs" "haskell"
            diags <- waitForDiagnosticsFromSource doc "example2"
            reduceDiag <- liftIO $ inspectDiagnostic diags ["example2 diagnostic, hello world"]
            liftIO $ do
                length diags @?= 1
                reduceDiag ^. LSP.range @?= Range (Position 0 0) (Position 1 0)
                reduceDiag ^. LSP.severity @?= Just DsError
    ]

warningTests :: TestTree
warningTests = testGroup  "Warnings are warnings" [
    testCase "Overrides -Werror" $
        runSession hlsCommand fullCaps "test/testdata/wErrorTest" $ do
            doc <- openDoc "src/WError.hs" "haskell"
            [diag] <- waitForDiagnosticsFrom doc
            liftIO $ diag ^. LSP.severity @?= Just DsWarning
    ]

saveTests :: TestTree
saveTests = testGroup  "only diagnostics on save" [
    ignoreTestBecause "diagnosticsOnChange parameter is not supported right now" $ testCase "Respects diagnosticsOnChange setting" $
        runSession hlsCommandExamplePlugin codeActionSupportCaps "test/testdata" $ do
            let config = Data.Default.def { diagnosticsOnChange = False } :: Config
            sendNotification SWorkspaceDidChangeConfiguration (DidChangeConfigurationParams (toJSON config))
            doc <- openDoc "Hover.hs" "haskell"
            diags <- waitForDiagnosticsFrom doc

            liftIO $ do
                length diags @?= 0

            let te = TextEdit (Range (Position 0 0) (Position 0 13)) ""
            _ <- applyEdit doc te
            skipManyTill loggingNotification noDiagnostics

            sendNotification STextDocumentDidSave (DidSaveTextDocumentParams doc Nothing)
            diags2 <- waitForDiagnosticsFrom doc
            liftIO $
                length diags2 @?= 1
    ]
