{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE OverloadedStrings     #-}

module Progress (tests) where

import           Control.Lens                    hiding ((.=))
import           Data.Aeson                      (Value, decode, encode, object,
                                                  toJSON, (.=))
import           Data.List                       (delete)
import           Data.Maybe                      (fromJust)
import           Data.Text                       (Text, pack)
import           Ide.Plugin.Config
import           Language.LSP.Types.Capabilities
import qualified Language.LSP.Types.Lens         as L
import           System.FilePath                 ((</>))
import           Test.Hls
import           Test.Hls.Command

tests :: TestTree
tests =
    testGroup
        "window/workDoneProgress"
        [ testCase "sends indefinite progress notifications" $
            runSession hlsCommand progressCaps "test/testdata" $ do
                let path = "hlint" </> "ApplyRefact2.hs"
                _ <- openDoc path "haskell"
                expectProgressReports [pack ("Setting up hlint (for " ++ path ++ ")"), "Processing", "Indexing"]
        , testCase "eval plugin sends progress reports" $
            runSession hlsCommand progressCaps "plugins/hls-eval-plugin/test/testdata" $ do
                doc <- openDoc "T1.hs" "haskell"
                expectProgressReports ["Setting up testdata (for T1.hs)", "Processing", "Indexing"]
                [evalLens] <- getCodeLenses doc
                let cmd = evalLens ^?! L.command . _Just
                _ <- sendRequest SWorkspaceExecuteCommand $ ExecuteCommandParams Nothing (cmd ^. L.command) (decode $ encode $ fromJust $ cmd ^. L.arguments)
                expectProgressReports ["Evaluating"]
        , testCase "ormolu plugin sends progress notifications" $ do
            runSession hlsCommand progressCaps "test/testdata/format" $ do
                sendNotification SWorkspaceDidChangeConfiguration (DidChangeConfigurationParams (formatLspConfig "ormolu"))
                doc <- openDoc "Format.hs" "haskell"
                expectProgressReports ["Setting up testdata (for Format.hs)", "Processing", "Indexing"]
                _ <- sendRequest STextDocumentFormatting $ DocumentFormattingParams Nothing doc (FormattingOptions 2 True Nothing Nothing Nothing)
                expectProgressReports ["Formatting Format.hs"]
        , testCase "fourmolu plugin sends progress notifications" $ do
            runSession hlsCommand progressCaps "test/testdata/format" $ do
                sendNotification SWorkspaceDidChangeConfiguration (DidChangeConfigurationParams (formatLspConfig "fourmolu"))
                doc <- openDoc "Format.hs" "haskell"
                expectProgressReports ["Setting up testdata (for Format.hs)", "Processing", "Indexing"]
                _ <- sendRequest STextDocumentFormatting $ DocumentFormattingParams Nothing doc (FormattingOptions 2 True Nothing Nothing Nothing)
                expectProgressReports ["Formatting Format.hs"]
        , ignoreTestBecause "no liquid Haskell support" $
            testCase "liquid haskell plugin sends progress notifications" $ do
                runSession hlsCommand progressCaps "test/testdata" $ do
                    doc <- openDoc "liquid/Evens.hs" "haskell"
                    let config = def{liquidOn = True, hlintOn = False}
                    sendNotification SWorkspaceDidChangeConfiguration (DidChangeConfigurationParams (toJSON config))
                    sendNotification STextDocumentDidSave (DidSaveTextDocumentParams doc Nothing)
                    expectProgressReports ["Running Liquid Haskell on Evens.hs"]
        ]

formatLspConfig :: Value -> Value
formatLspConfig provider = object ["haskell" .= object ["formattingProvider" .= (provider :: Value)]]

progressCaps :: ClientCapabilities
progressCaps = fullCaps{_window = Just (WindowClientCapabilities (Just True))}

data CollectedProgressNotification
    = CreateM WorkDoneProgressCreateParams
    | BeginM (ProgressParams WorkDoneProgressBeginParams)
    | ProgressM (ProgressParams WorkDoneProgressReportParams)
    | EndM (ProgressParams WorkDoneProgressEndParams)

{- | Test that the server is correctly producing a sequence of progress related
 messages. Each create must be pair with a corresponding begin and end,
 optionally with some progress in between. Tokens must match. The begin
 messages have titles describing the work that is in-progress, we check that
 the titles we see are those we expect.
-}
expectProgressReports :: [Text] -> Session ()
expectProgressReports xs = expectProgressReports' [] xs
  where
    expectProgressReports' [] [] = return ()
    expectProgressReports' tokens expectedTitles =
        do
            skipManyTill anyMessage (create <|> begin <|> progress <|> end)
            >>= \case
                CreateM msg ->
                    expectProgressReports' (token msg : tokens) expectedTitles
                BeginM msg -> do
                    liftIO $ token msg `expectElem` tokens
                    expectProgressReports' tokens (delete (title msg) expectedTitles)
                ProgressM msg -> do
                    liftIO $ token msg `expectElem` tokens
                    expectProgressReports' tokens expectedTitles
                EndM msg -> do
                    liftIO $ token msg `expectElem` tokens
                    expectProgressReports' (delete (token msg) tokens) expectedTitles
    title msg = msg ^. L.value . L.title
    token msg = msg ^. L.token
    create = CreateM . view L.params <$> message SWindowWorkDoneProgressCreate
    begin = BeginM <$> satisfyMaybe (\case
      FromServerMess SProgress (NotificationMessage _ _ (ProgressParams t (Begin x))) -> Just (ProgressParams t x)
      _ -> Nothing)
    progress = ProgressM <$> satisfyMaybe (\case
      FromServerMess SProgress (NotificationMessage _ _ (ProgressParams t (Report x))) -> Just (ProgressParams t x)
      _ -> Nothing)
    end = EndM <$> satisfyMaybe (\case
      FromServerMess SProgress (NotificationMessage _ _ (ProgressParams t (End x))) -> Just (ProgressParams t x)
      _ -> Nothing)
    expectElem a as = a `elem` as @? "Unexpected " ++ show a
