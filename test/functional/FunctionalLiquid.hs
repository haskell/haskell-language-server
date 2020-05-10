{-# LANGUAGE OverloadedStrings #-}

module FunctionalLiquid (tests) where

import           Control.Lens hiding (List)
import           Control.Monad.IO.Class
import           Data.Aeson
import           Data.Default
import qualified Data.Text as T
import           Language.Haskell.LSP.Test hiding (message)
import           Language.Haskell.LSP.Types as LSP
import           Language.Haskell.LSP.Types.Lens as LSP hiding (contents, error )
import           Ide.Plugin.Config
import           Test.HIE.Util
import           Test.Tasty
import           Test.Tasty.HUnit
import           Test.Tasty.Expectation

-- ---------------------------------------------------------------------

tests :: TestTree
tests = testGroup "liquid haskell diagnostics" [
    testCase "runs diagnostics on save, no liquid" $
        runSession hieCommandExamplePlugin codeActionSupportCaps "test/testdata" $ do
            doc <- openDoc "liquid/Evens.hs" "haskell"

            diags@(reduceDiag:_) <- waitForDiagnostics

            liftIO $ do
                length diags `shouldBe` 2
                reduceDiag ^. range `shouldBe` Range (Position 5 18) (Position 5 22)
                reduceDiag ^. severity `shouldBe` Just DsHint
                reduceDiag ^. code `shouldBe` Just (StringValue "Use negate")
                reduceDiag ^. source `shouldBe` Just "hlint"

            diags2hlint <- waitForDiagnostics

            liftIO $ length diags2hlint `shouldBe` 2

            sendNotification TextDocumentDidSave (DidSaveTextDocumentParams doc)

            diags3@(d:_) <- waitForDiagnosticsSource "eg2"

            liftIO $ do
                length diags3 `shouldBe` 1
                d ^. LSP.range `shouldBe` Range (Position 0 0) (Position 1 0)
                d ^. LSP.severity `shouldBe` Nothing
                d ^. LSP.code `shouldBe` Nothing
                d ^. LSP.message `shouldBe` T.pack "Example plugin diagnostic, triggered byDiagnosticOnSave"

    -- ---------------------------------

    , testCase "runs diagnostics on save, with liquid haskell" $
        runSession hieCommand codeActionSupportCaps "test/testdata" $ do
        -- runSessionWithConfig logConfig hieCommand codeActionSupportCaps "test/testdata" $ do
            doc <- openDoc "liquid/Evens.hs" "haskell"

            diags@(reduceDiag:_) <- waitForDiagnostics

            -- liftIO $ show diags `shouldBe` ""

            liftIO $ do
                length diags `shouldBe` 2
                reduceDiag ^. range `shouldBe` Range (Position 5 18) (Position 5 22)
                reduceDiag ^. severity `shouldBe` Just DsHint
                reduceDiag ^. code `shouldBe` Just (StringValue "Use negate")
                reduceDiag ^. source `shouldBe` Just "hlint"

            -- Enable liquid haskell plugin and disable hlint
            let config = def { liquidOn  = True, hlintOn = False }
            sendNotification WorkspaceDidChangeConfiguration (DidChangeConfigurationParams (toJSON config))

            -- docItem <- getDocItem file languageId
            sendNotification TextDocumentDidSave (DidSaveTextDocumentParams doc)
            -- TODO: what does that test?
            -- TODO: whether hlint is really disbabled?
            -- TODO: @fendor, document or remove
            -- diags2hlint <- waitForDiagnostics
            -- -- liftIO $ show diags2hlint `shouldBe` ""

            -- -- We turned hlint diagnostics off
            -- liftIO $ length diags2hlint `shouldBe` 0
            -- diags2liquid <- waitForDiagnostics
            -- liftIO $ length diags2liquid `shouldBe` 0
            -- liftIO $ show diags2liquid `shouldBe` ""
            diags3@(d:_) <- waitForDiagnosticsSource "liquid"
            -- liftIO $ show diags3 `shouldBe` ""
            liftIO $ do
                length diags3 `shouldBe` 1
                d ^. range `shouldBe` Range (Position 8 0) (Position 8 11)
                d ^. severity `shouldBe` Just DsError
                d ^. code `shouldBe` Nothing
                d ^. source `shouldBe` Just "liquid"
                d ^. message `shouldSatisfy` T.isPrefixOf ("Error: Liquid Type Mismatch\n" <>
                                                "  Inferred type\n" <>
                                                "    VV : {v : GHC.Types.Int | v == 7}\n" <>
                                                " \n" <>
                                                "  not a subtype of Required type\n" <>
                                                "    VV : {VV : GHC.Types.Int | VV mod 2 == 0}\n ")
    ]