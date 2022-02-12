{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE PolyKinds             #-}

module Progress (tests) where

import           Control.Exception               (throw)
import           Control.Lens                    hiding ((.=))
import           Data.Aeson                      (Value, decode, encode, object,
                                                  (.=))
import           Data.List                       (delete)
import           Data.Maybe                      (fromJust)
import           Data.Text                       (Text, pack)
import qualified Language.LSP.Types              as LSP
import           Language.LSP.Types.Capabilities
import qualified Language.LSP.Types.Lens         as L
import           System.FilePath                 ((</>))
import           Test.Hls
import           Test.Hls.Command
import           Test.Hls.Flags


tests :: TestTree
tests =
    testGroup
        "window/workDoneProgress"
        [ testCase "sends indefinite progress notifications" $
            runSession hlsCommand progressCaps "test/testdata" $ do
                let path = "diagnostics" </> "Foo.hs"
                _ <- openDoc path "haskell"
                expectProgressMessages [pack ("Setting up testdata (for " ++ path ++ ")"), "Processing", "Indexing"] []
        ,  knownBrokenForGhcVersions [GHC92] "No evaluation status with GHC 9.2" $ requiresEvalPlugin $ testCase "eval plugin sends progress reports" $
            runSession hlsCommand progressCaps "plugins/hls-eval-plugin/test/testdata" $ do
              doc <- openDoc "T1.hs" "haskell"
              lspId <- sendRequest STextDocumentCodeLens (CodeLensParams Nothing Nothing doc)

              (codeLensResponse, activeProgressTokens) <- expectProgressMessagesTill
                (responseForId STextDocumentCodeLens lspId)
                ["Setting up testdata (for T1.hs)", "Processing", "Indexing"]
                []

              -- this is a test so exceptions result in fails
              let response = getResponseResult codeLensResponse
              case response of
                  LSP.List [evalLens] -> do
                      let command = evalLens ^?! L.command . _Just

                      _ <- sendRequest SWorkspaceExecuteCommand $
                          ExecuteCommandParams
                          Nothing
                          (command ^. L.command)
                          (decode $ encode $ fromJust $ command ^. L.arguments)

                      expectProgressMessages ["Evaluating"] activeProgressTokens
                  _ -> error $ "Unexpected response result: " ++ show response
        , requiresOrmoluPlugin $ testCase "ormolu plugin sends progress notifications" $ do
            runSession hlsCommand progressCaps "test/testdata/format" $ do
                sendConfigurationChanged (formatLspConfig "ormolu")
                doc <- openDoc "Format.hs" "haskell"
                expectProgressMessages ["Setting up testdata (for Format.hs)", "Processing", "Indexing"] []
                _ <- sendRequest STextDocumentFormatting $ DocumentFormattingParams Nothing doc (FormattingOptions 2 True Nothing Nothing Nothing)
                expectProgressMessages ["Formatting Format.hs"] []
        , requiresFourmoluPlugin $ testCase "fourmolu plugin sends progress notifications" $ do
            runSession hlsCommand progressCaps "test/testdata/format" $ do
                sendConfigurationChanged (formatLspConfig "fourmolu")
                doc <- openDoc "Format.hs" "haskell"
                expectProgressMessages ["Setting up testdata (for Format.hs)", "Processing", "Indexing"] []
                _ <- sendRequest STextDocumentFormatting $ DocumentFormattingParams Nothing doc (FormattingOptions 2 True Nothing Nothing Nothing)
                expectProgressMessages ["Formatting Format.hs"] []
        ]

formatLspConfig :: Value -> Value
formatLspConfig provider = object ["haskell" .= object ["formattingProvider" .= (provider :: Value)]]

progressCaps :: ClientCapabilities
progressCaps = fullCaps{_window = Just (WindowClientCapabilities (Just True) Nothing Nothing)}

data ProgressMessage
  = ProgressCreate WorkDoneProgressCreateParams
  | ProgressBegin (ProgressParams WorkDoneProgressBeginParams)
  | ProgressReport (ProgressParams WorkDoneProgressReportParams)
  | ProgressEnd (ProgressParams WorkDoneProgressEndParams)

data InterestingMessage a
  = InterestingMessage a
  | ProgressMessage ProgressMessage

progressMessage :: Session ProgressMessage
progressMessage =
  progressCreate <|> progressBegin <|> progressReport <|> progressEnd
  where
    progressCreate = ProgressCreate . view L.params <$> message SWindowWorkDoneProgressCreate
    progressBegin = ProgressBegin <$> satisfyMaybe (\case
      FromServerMess SProgress (NotificationMessage _ _ (ProgressParams t (Begin x))) -> Just (ProgressParams t x)
      _ -> Nothing)
    progressReport = ProgressReport <$> satisfyMaybe (\case
      FromServerMess SProgress (NotificationMessage _ _ (ProgressParams t (Report x))) -> Just (ProgressParams t x)
      _ -> Nothing)
    progressEnd = ProgressEnd <$> satisfyMaybe (\case
      FromServerMess SProgress (NotificationMessage _ _ (ProgressParams t (End x))) -> Just (ProgressParams t x)
      _ -> Nothing)

interestingMessage :: Session a -> Session (InterestingMessage a)
interestingMessage theMessage =
  fmap InterestingMessage theMessage <|> fmap ProgressMessage progressMessage

expectProgressMessagesTill :: Session a -> [Text] -> [ProgressToken] -> Session (a, [ProgressToken])
expectProgressMessagesTill stopMessage expectedTitles activeProgressTokens = do
  message <- skipManyTill anyMessage (interestingMessage stopMessage)
  case message of
    InterestingMessage a -> do
      liftIO $ null expectedTitles @? "Expected titles not empty " <> show expectedTitles
      pure (a, activeProgressTokens)
    ProgressMessage progressMessage ->
      updateExpectProgressStateAndRecurseWith
        (expectProgressMessagesTill stopMessage)
        progressMessage
        expectedTitles
        activeProgressTokens

{- | Test that the server is correctly producing a sequence of progress related
 messages. Each create must be pair with a corresponding begin and end,
 optionally with some progress in between. Tokens must match. The begin
 messages have titles describing the work that is in-progress, we check that
 the titles we see are those we expect.
-}
expectProgressMessages :: [Text] -> [ProgressToken] -> Session ()
expectProgressMessages [] [] = pure ()
expectProgressMessages expectedTitles activeProgressTokens = do
  message <- skipManyTill anyMessage progressMessage
  updateExpectProgressStateAndRecurseWith expectProgressMessages message expectedTitles activeProgressTokens

updateExpectProgressStateAndRecurseWith :: ([Text] -> [ProgressToken] -> Session a)
                                        -> ProgressMessage
                                        -> [Text]
                                        -> [ProgressToken]
                                        -> Session a
updateExpectProgressStateAndRecurseWith f progressMessage expectedTitles activeProgressTokens = do
  case progressMessage of
    ProgressCreate params -> do
      f expectedTitles (getToken params : activeProgressTokens)
    ProgressBegin params -> do
      liftIO $ getToken params `expectedIn` activeProgressTokens
      f (delete (getTitle params) expectedTitles) activeProgressTokens
    ProgressReport params -> do
      liftIO $ getToken params `expectedIn` activeProgressTokens
      f expectedTitles activeProgressTokens
    ProgressEnd params -> do
      liftIO $ getToken params `expectedIn` activeProgressTokens
      f expectedTitles (delete (getToken params) activeProgressTokens)

getTitle :: (L.HasValue s a1, L.HasTitle a1 a2) => s -> a2
getTitle msg = msg ^. L.value . L.title

getToken :: L.HasToken s a => s -> a
getToken msg = msg ^. L.token

expectedIn :: (Foldable t, Eq a, Show a) => a -> t a -> Assertion
expectedIn a as = a `elem` as @? "Unexpected " ++ show a

getResponseResult :: ResponseMessage m -> ResponseResult m
getResponseResult rsp =
  case rsp ^. L.result of
    Right x -> x
    Left err -> throw $ UnexpectedResponseError (SomeLspId $ fromJust $ rsp ^. L.id) err
