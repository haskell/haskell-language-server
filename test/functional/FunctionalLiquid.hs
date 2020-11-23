{-# LANGUAGE OverloadedStrings #-}

module FunctionalLiquid (tests) where

import           Control.Lens hiding (List)
import           Control.Monad.IO.Class
import           Data.Aeson
import           Data.Default
import qualified Data.Text as T
import           Language.Haskell.LSP.Test hiding (message)
import           Language.Haskell.LSP.Types as LSP
import           Language.Haskell.LSP.Types.Lens as LSP hiding (contents)
import           Ide.Plugin.Config
import           Test.Hls.Util
import           Test.Tasty
import           Test.Tasty.ExpectedFailure (ignoreTestBecause)
import           Test.Tasty.HUnit

-- ---------------------------------------------------------------------

tests :: TestTree
tests = testGroup "liquid haskell diagnostics" [
    ignoreTestBecause "Broken" $ testCase "runs diagnostics on save, no liquid" $
        runSession hlsCommandExamplePlugin codeActionSupportCaps "test/testdata" $ do
            doc <- openDoc "liquid/Evens.hs" "haskell"

            diags@(reduceDiag:_) <- waitForDiagnostics

            liftIO $ do
                length diags @?= 2
                reduceDiag ^. range @?= Range (Position 5 18) (Position 5 22)
                reduceDiag ^. severity @?= Just DsHint
                reduceDiag ^. code @?= Just (StringValue "Use negate")
                reduceDiag ^. source @?= Just "hlint"

            diags2hlint <- waitForDiagnostics

            liftIO $ length diags2hlint @?= 2

            sendNotification TextDocumentDidSave (DidSaveTextDocumentParams doc)

            diags3@(d:_) <- waitForDiagnosticsSource "eg2"

            liftIO $ do
                length diags3 @?= 1
                d ^. LSP.range @?= Range (Position 0 0) (Position 1 0)
                d ^. LSP.severity @?= Nothing
                d ^. LSP.code @?= Nothing
                d ^. LSP.message @?= T.pack "Example plugin diagnostic, triggered byDiagnosticOnSave"

    -- ---------------------------------

    , ignoreTestBecause "Broken" $ testCase "runs diagnostics on save, with liquid haskell" $
        runSession hlsCommand codeActionSupportCaps "test/testdata" $ do
        -- runSessionWithConfig logConfig hlsCommand codeActionSupportCaps "test/testdata" $ do
            doc <- openDoc "liquid/Evens.hs" "haskell"

            diags@(reduceDiag:_) <- waitForDiagnostics

            -- liftIO $ show diags @?= ""

            liftIO $ do
                length diags @?= 2
                reduceDiag ^. range @?= Range (Position 5 18) (Position 5 22)
                reduceDiag ^. severity @?= Just DsHint
                reduceDiag ^. code @?= Just (StringValue "Use negate")
                reduceDiag ^. source @?= Just "hlint"

            -- Enable liquid haskell plugin and disable hlint
            let config = def { liquidOn  = True, hlintOn = False }
            sendNotification WorkspaceDidChangeConfiguration (DidChangeConfigurationParams (toJSON config))

            -- docItem <- getDocItem file languageId
            sendNotification TextDocumentDidSave (DidSaveTextDocumentParams doc)
            -- TODO: what does that test?
            -- TODO: whether hlint is really disbabled?
            -- TODO: @fendor, document or remove
            -- diags2hlint <- waitForDiagnostics
            -- -- liftIO $ show diags2hlint @?= ""

            -- -- We turned hlint diagnostics off
            -- liftIO $ length diags2hlint @?= 0
            -- diags2liquid <- waitForDiagnostics
            -- liftIO $ length diags2liquid @?= 0
            -- liftIO $ show diags2liquid @?= ""
            diags3@(d:_) <- waitForDiagnosticsSource "liquid"
            -- liftIO $ show diags3 @?= ""
            liftIO $ do
                length diags3 @?= 1
                d ^. range @?= Range (Position 8 0) (Position 8 11)
                d ^. severity @?= Just DsError
                d ^. code @?= Nothing
                d ^. source @?= Just "liquid"
                (d ^. message) `T.isPrefixOf`
                    ("Error: Liquid Type Mismatch\n" <>
                     "  Inferred type\n" <>
                     "    VV : {v : GHC.Types.Int | v == 7}\n" <>
                     " \n" <>
                     "  not a subtype of Required type\n" <>
                     "    VV : {VV : GHC.Types.Int | VV mod 2 == 0}\n ")
                    @? "Contains error message"
    ]
