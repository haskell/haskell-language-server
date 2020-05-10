{-# LANGUAGE OverloadedStrings #-}
module Progress (tests) where

import Control.Applicative.Combinators
import Control.Lens
import Control.Monad.IO.Class
import Data.Aeson
import Data.Default
import Ide.Plugin.Config
import Language.Haskell.LSP.Test
import Language.Haskell.LSP.Messages -- TODO: Move this into haskell-lsp-types
import Language.Haskell.LSP.Types
import qualified Language.Haskell.LSP.Types.Lens as L
import Language.Haskell.LSP.Types.Capabilities
import Test.HIE.Util
import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.Expectation

tests :: TestTree
tests = testGroup "window/workDoneProgress" [
    testCase "sends indefinite progress notifications" $
    -- Testing that ghc-mod sends progress notifications
        runSession hieCommand progressCaps "test/testdata" $ do
            doc <- openDoc "ApplyRefact2.hs" "haskell"

            skipMany loggingNotification

            createRequest <- message :: Session WorkDoneProgressCreateRequest
            liftIO $ do
                    createRequest ^. L.params `shouldBe` WorkDoneProgressCreateParams (ProgressNumericToken 0)

            startNotification <- message :: Session WorkDoneProgressBeginNotification
            liftIO $ do
                    -- Expect a stack cradle, since the given `hie.yaml` is expected
                    -- to contain a multi-stack cradle.
                    startNotification ^. L.params . L.value . L.title `shouldBe` "Initializing Stack project"
                    startNotification ^. L.params . L.token `shouldBe` (ProgressNumericToken 0)

            reportNotification <- message :: Session WorkDoneProgressReportNotification
            liftIO $ do
                    reportNotification ^. L.params . L.value . L.message `shouldBe` Just "Main"
                    reportNotification ^. L.params . L.token `shouldBe` (ProgressNumericToken 0)

            -- may produce diagnostics
            skipMany publishDiagnosticsNotification

            doneNotification <- message :: Session WorkDoneProgressEndNotification
            liftIO $ doneNotification ^. L.params . L.token `shouldBe` (ProgressNumericToken 0)

            -- Initial hlint notifications
            _ <- publishDiagnosticsNotification

            -- Test incrementing ids
            sendNotification TextDocumentDidSave (DidSaveTextDocumentParams doc)

            createRequest' <- skipManyTill loggingNotification (message :: Session WorkDoneProgressCreateRequest)
            liftIO $ do
                    createRequest' ^. L.params `shouldBe` WorkDoneProgressCreateParams (ProgressNumericToken 1)

            startNotification' <- message :: Session WorkDoneProgressBeginNotification
            liftIO $ do
                    startNotification' ^. L.params . L.value . L.title `shouldBe` "loading"
                    startNotification' ^. L.params . L.token `shouldBe` (ProgressNumericToken 1)

            reportNotification' <- message :: Session WorkDoneProgressReportNotification
            liftIO $ do
                    reportNotification' ^. L.params . L.value . L.message `shouldBe` Just "Main"
                    reportNotification' ^. L.params . L.token `shouldBe` (ProgressNumericToken 1)

            doneNotification' <- message :: Session WorkDoneProgressEndNotification
            liftIO $ doneNotification' ^. L.params . L.token `shouldBe` (ProgressNumericToken 1)

            -- Initial hlint notifications
            _ <- publishDiagnosticsNotification
            return ()

    , testCase "sends indefinite progress notifications with liquid" $
        -- Testing that Liquid Haskell sends progress notifications
        runSession hieCommand progressCaps "test/testdata" $ do
        doc <- openDoc "liquid/Evens.hs" "haskell"

        skipMany loggingNotification

        _ <- message :: Session WorkDoneProgressCreateRequest
        _ <- message :: Session WorkDoneProgressBeginNotification
        _ <- message :: Session WorkDoneProgressReportNotification
        _ <- message :: Session WorkDoneProgressEndNotification

        -- the hie-bios diagnostics
        _ <- skipManyTill loggingNotification publishDiagnosticsNotification

        -- Enable liquid haskell plugin
        let config = def { liquidOn  = True, hlintOn = False }
        sendNotification WorkspaceDidChangeConfiguration (DidChangeConfigurationParams (toJSON config))

        -- Test liquid
        sendNotification TextDocumentDidSave (DidSaveTextDocumentParams doc)

        -- hlint notifications
        -- TODO: potential race between typechecking, e.g. context intialisation
        -- TODO: and disabling hlint notifications
        -- _ <- skipManyTill loggingNotification publishDiagnosticsNotification

        let startPred (NotWorkDoneProgressBegin m) =
                m ^. L.params . L.value . L.title == "Running Liquid Haskell on Evens.hs"
            startPred _ = False

        let donePred (NotWorkDoneProgressEnd _) = True
            donePred _ = False

        _ <- skipManyTill anyMessage $ between (satisfy startPred) (satisfy donePred) $
                many (satisfy (\x -> not (startPred x || donePred x)))
        return ()
    ]

progressCaps :: ClientCapabilities
progressCaps = fullCaps { _window = Just (WindowClientCapabilities (Just True)) }
