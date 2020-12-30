{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Progress (tests) where

import Control.Applicative.Combinators
import Control.Lens hiding ((.=))
import Control.Monad.IO.Class
import Data.Aeson (Value, decode, encode, object, toJSON, (.=))
import Data.Default
import Data.List (delete)
import Data.Maybe (fromJust)
import Data.Text (Text, pack)
import Ide.Plugin.Config
import Language.Haskell.LSP.Test
import Language.Haskell.LSP.Types
import Language.Haskell.LSP.Types.Capabilities
import qualified Language.Haskell.LSP.Types.Lens as L
import System.FilePath ((</>))
import Test.Hls.Util
import Test.Tasty
import Test.Tasty.ExpectedFailure (ignoreTestBecause)
import Test.Tasty.HUnit

tests :: TestTree
tests =
    testGroup
        "window/workDoneProgress"
        [ testCase "sends indefinite progress notifications" $
            runSession hlsCommand progressCaps "test/testdata" $ do
                let path = "hlint" </> "ApplyRefact2.hs"
                _ <- openDoc path "haskell"
                expectProgressReports [pack ("Setting up hlint (for " ++ path ++ ")"), "Processing"]
        , testCase "eval plugin sends progress reports" $
            runSession hlsCommand progressCaps "plugins/hls-eval-plugin/test/testdata" $ do
                doc <- openDoc "T1.hs" "haskell"
                expectProgressReports ["Setting up testdata (for T1.hs)", "Processing"]
                [evalLens] <- getCodeLenses doc
                let cmd = evalLens ^?! L.command . _Just
                _ <- sendRequest WorkspaceExecuteCommand $ ExecuteCommandParams (cmd ^. L.command) (decode $ encode $ fromJust $ cmd ^. L.arguments) Nothing
                expectProgressReports ["Evaluating"]
        , testCase "ormolu plugin sends progress notifications" $ do
            runSession hlsCommand progressCaps "test/testdata/format" $ do
                sendNotification WorkspaceDidChangeConfiguration (DidChangeConfigurationParams (formatLspConfig "ormolu"))
                doc <- openDoc "Format.hs" "haskell"
                expectProgressReports ["Setting up testdata (for Format.hs)", "Processing"]
                _ <- sendRequest TextDocumentFormatting $ DocumentFormattingParams doc (FormattingOptions 2 True) Nothing
                expectProgressReports ["Formatting Format.hs"]
        , testCase "fourmolu plugin sends progress notifications" $ do
            runSession hlsCommand progressCaps "test/testdata/format" $ do
                sendNotification WorkspaceDidChangeConfiguration (DidChangeConfigurationParams (formatLspConfig "fourmolu"))
                doc <- openDoc "Format.hs" "haskell"
                expectProgressReports ["Setting up testdata (for Format.hs)", "Processing"]
                _ <- sendRequest TextDocumentFormatting $ DocumentFormattingParams doc (FormattingOptions 2 True) Nothing
                expectProgressReports ["Formatting Format.hs"]
        , ignoreTestBecause "no liquid Haskell support" $
            testCase "liquid haskell plugin sends progress notifications" $ do
                runSession hlsCommand progressCaps "test/testdata" $ do
                    doc <- openDoc "liquid/Evens.hs" "haskell"
                    let config = def{liquidOn = True, hlintOn = False}
                    sendNotification WorkspaceDidChangeConfiguration (DidChangeConfigurationParams (toJSON config))
                    sendNotification TextDocumentDidSave (DidSaveTextDocumentParams doc)
                    expectProgressReports ["Running Liquid Haskell on Evens.hs"]
        ]

formatLspConfig :: Value -> Value
formatLspConfig provider = object ["haskell" .= object ["formattingProvider" .= (provider :: Value)]]

progressCaps :: ClientCapabilities
progressCaps = fullCaps{_window = Just (WindowClientCapabilities (Just True))}

data CollectedProgressNotification
    = CreateM WorkDoneProgressCreateRequest
    | BeginM WorkDoneProgressBeginNotification
    | ProgressM WorkDoneProgressReportNotification
    | EndM WorkDoneProgressEndNotification

{- | Test that the server is correctly producing a sequence of progress related
 messages. Each create must be pair with a corresponding begin and end,
 optionally with some progress in between. Tokens must match. The begin
 messages have titles describing the work that is in-progress, we check that
 the titles we see are those we expect.
-}
expectProgressReports :: [Text] -> Session ()
expectProgressReports = expectProgressReports' []
  where
    expectProgressReports' [] [] = return ()
    expectProgressReports' tokens expectedTitles =
        do
            skipManyTill anyMessage (create <|> begin <|> progress <|> end)
            >>= \case
                CreateM msg ->
                    expectProgressReports' (token msg : tokens) expectedTitles
                BeginM msg -> do
                    liftIO $ title msg `expectElem` expectedTitles
                    liftIO $ token msg `expectElem` tokens
                    expectProgressReports' tokens (delete (title msg) expectedTitles)
                ProgressM msg -> do
                    liftIO $ token msg `expectElem` tokens
                    expectProgressReports' tokens expectedTitles
                EndM msg -> do
                    liftIO $ token msg `expectElem` tokens
                    expectProgressReports' (delete (token msg) tokens) expectedTitles
    title msg = msg ^. L.params ^. L.value ^. L.title
    token msg = msg ^. L.params ^. L.token
    create = CreateM <$> message
    begin = BeginM <$> message
    progress = ProgressM <$> message
    end = EndM <$> message
    expectElem a as = a `elem` as @? "Unexpected " ++ show a
