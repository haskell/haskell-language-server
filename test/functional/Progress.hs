{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
module Progress (tests) where

import Control.Applicative.Combinators
import Control.Lens hiding ((.=))
import Control.Monad.IO.Class
import Language.Haskell.LSP.Test
import Language.Haskell.LSP.Types
import qualified Language.Haskell.LSP.Types.Lens as L
import Language.Haskell.LSP.Types.Capabilities
import Test.Hls.Util
import Test.Tasty
import Test.Tasty.HUnit
import Data.Text (Text)
import Data.Aeson (encode, decode, object, Value, (.=))
import Data.Maybe (fromJust)
import Data.List (delete)

tests :: TestTree
tests = testGroup "window/workDoneProgress" [
    testCase "sends indefinite progress notifications" $
        runSession hlsCommand progressCaps "test/testdata" $ do
            _ <- openDoc "hlint/ApplyRefact2.hs" "haskell"
            expectProgressReports ["Setting up hlint (for hlint/ApplyRefact2.hs)", "Processing"]
    , testCase "eval plugin sends progress reports" $
          runSession hlsCommand progressCaps "test/testdata/eval" $ do
              doc <- openDoc "T1.hs" "haskell"
              expectProgressReports ["Setting up eval (for T1.hs)", "Processing"]
              [evalLens] <- getCodeLenses doc
              let cmd = evalLens ^?! L.command . _Just
              _ <- sendRequest WorkspaceExecuteCommand $ ExecuteCommandParams (cmd ^. L.command) (decode $ encode $ fromJust $ cmd ^. L.arguments) Nothing
              expectProgressReports ["Evaluating"]
    , testCase "ormolu plugin sends progress notifications" $ do
          runSession hlsCommand progressCaps "test/testdata" $ do
              sendNotification WorkspaceDidChangeConfiguration (DidChangeConfigurationParams (formatLspConfig "ormolu"))
              doc <- openDoc "Format.hs" "haskell"
              expectProgressReports ["Setting up testdata (for Format.hs)", "Processing"]
              _ <- sendRequest TextDocumentFormatting $ DocumentFormattingParams doc (FormattingOptions 2 True) Nothing
              expectProgressReports ["Formatting Format.hs"]
    , testCase "fourmolu plugin sends progress notifications" $ do
           runSession hlsCommand progressCaps "test/testdata" $ do
              sendNotification WorkspaceDidChangeConfiguration (DidChangeConfigurationParams (formatLspConfig "fourmolu"))
              doc <- openDoc "Format.hs" "haskell"
              expectProgressReports ["Setting up testdata (for Format.hs)", "Processing"]
              _ <- sendRequest TextDocumentFormatting $ DocumentFormattingParams doc (FormattingOptions 2 True) Nothing
              expectProgressReports ["Formatting Format.hs"]
    ]

formatLspConfig :: Value -> Value
formatLspConfig provider = object [ "haskell" .= object ["formattingProvider" .= (provider :: Value)] ]

progressCaps :: ClientCapabilities
progressCaps = fullCaps { _window = Just (WindowClientCapabilities (Just True)) }

data CollectedProgressNotification =
    CreateM WorkDoneProgressCreateRequest
    | BeginM WorkDoneProgressBeginNotification
    | ProgressM WorkDoneProgressReportNotification
    | EndM WorkDoneProgressEndNotification

-- | Test that the server is correctly producing a sequence of progress related
-- messages. Each create must be pair with a corresponding begin and end,
-- optionally with some progress in between. Tokens must match. The begin
-- messages have titles describing the work that is in-progress, we check that
-- the titles we see are those we expect.
expectProgressReports :: [Text] -> Session ()
expectProgressReports = expectProgressReports' []
    where expectProgressReports' [] [] = return ()
          expectProgressReports' tokens expectedTitles = do
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
