{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE PolyKinds             #-}
{-# LANGUAGE ViewPatterns          #-}
module Progress (tests) where

import           Control.Exception                  (throw)
import           Control.Lens                       hiding ((.=))
import           Data.Aeson                         (decode, encode)
import           Data.Functor                       (void)
import           Data.List                          (delete)
import           Data.Maybe                         (fromJust)
import           Data.Text                          (Text, pack)
import           Ide.Types
import           Language.LSP.Protocol.Capabilities
import qualified Language.LSP.Protocol.Lens         as L
import           Test.Hls
import           Test.Hls.Command
import           Test.Hls.Flags


tests :: TestTree
tests =
    testGroup
        "window/workDoneProgress"
        [ testCase "sends indefinite progress notifications" $
            runSession hlsCommand progressCaps "test/testdata/diagnostics" $ do
                let path = "Foo.hs"
                _ <- openDoc path "haskell"
                expectProgressMessages [pack ("Setting up diagnostics (for " ++ path ++ ")"), "Processing", "Indexing"] []
        , requiresEvalPlugin $ testCase "eval plugin sends progress reports" $
            runSession hlsCommand progressCaps "plugins/hls-eval-plugin/test/testdata" $ do
              doc <- openDoc "T1.hs" "haskell"
              lspId <- sendRequest SMethod_TextDocumentCodeLens (CodeLensParams Nothing Nothing doc)

              (codeLensResponse, activeProgressTokens) <- expectProgressMessagesTill
                (responseForId SMethod_TextDocumentCodeLens lspId)
                ["Setting up testdata (for T1.hs)", "Processing", "Indexing"]
                []

              -- this is a test so exceptions result in fails
              let response = getMessageResult codeLensResponse
              case response of
                  InL [evalLens] -> do
                      let command = evalLens ^?! L.command . _Just

                      _ <- sendRequest SMethod_WorkspaceExecuteCommand $
                          ExecuteCommandParams
                          Nothing
                          (command ^. L.command)
                          (decode $ encode $ fromJust $ command ^. L.arguments)

                      expectProgressMessages ["Evaluating"] activeProgressTokens
                  _ -> error $ "Unexpected response result: " ++ show response
        , requiresOrmoluPlugin $ testCase "ormolu plugin sends progress notifications" $ do
            runSessionWithConfig (def { ignoreConfigurationRequests = False }) hlsCommand progressCaps "test/testdata/format" $ do
                void configurationRequest
                setHlsConfig (formatLspConfig "ormolu")
                doc <- openDoc "Format.hs" "haskell"
                expectProgressMessages ["Setting up testdata (for Format.hs)", "Processing", "Indexing"] []
                _ <- sendRequest SMethod_TextDocumentFormatting $ DocumentFormattingParams Nothing doc (FormattingOptions 2 True Nothing Nothing Nothing)
                expectProgressMessages ["Formatting Format.hs"] []
        , requiresFourmoluPlugin $ testCase "fourmolu plugin sends progress notifications" $ do
            runSessionWithConfig (def { ignoreConfigurationRequests = False }) hlsCommand progressCaps "test/testdata/format" $ do
                void configurationRequest
                setHlsConfig (formatLspConfig "fourmolu")
                doc <- openDoc "Format.hs" "haskell"
                expectProgressMessages ["Setting up testdata (for Format.hs)", "Processing", "Indexing"] []
                _ <- sendRequest SMethod_TextDocumentFormatting $ DocumentFormattingParams Nothing doc (FormattingOptions 2 True Nothing Nothing Nothing)
                expectProgressMessages ["Formatting Format.hs"] []
        ]

formatLspConfig :: Text -> Config
formatLspConfig provider = def { formattingProvider = provider }

progressCaps :: ClientCapabilities
progressCaps = fullCaps{_window = Just (WindowClientCapabilities (Just True) Nothing Nothing)}

data ProgressMessage
  = ProgressCreate WorkDoneProgressCreateParams
  | ProgressBegin ProgressToken WorkDoneProgressBegin
  | ProgressReport ProgressToken WorkDoneProgressReport
  | ProgressEnd ProgressToken WorkDoneProgressEnd

data InterestingMessage a
  = InterestingMessage a
  | ProgressMessage ProgressMessage

progressMessage :: Session ProgressMessage
progressMessage =
  progressCreate <|> progressBegin <|> progressReport <|> progressEnd
  where
    progressCreate = ProgressCreate . view L.params <$> message SMethod_WindowWorkDoneProgressCreate
    progressBegin :: Session ProgressMessage
    progressBegin = satisfyMaybe (\case
      FromServerMess  SMethod_Progress  (TNotificationMessage _ _ (ProgressParams t (preview _workDoneProgressBegin -> Just params))) ->
        Just (ProgressBegin t params)
      _ -> Nothing)
    progressReport :: Session ProgressMessage
    progressReport = satisfyMaybe (\case
      FromServerMess  SMethod_Progress  (TNotificationMessage _ _ (ProgressParams t (preview _workDoneProgressReport -> Just params))) ->
             Just (ProgressReport t params)
      _ -> Nothing)
    progressEnd :: Session ProgressMessage
    progressEnd = satisfyMaybe (\case
      FromServerMess  SMethod_Progress  (TNotificationMessage _ _ (ProgressParams t (preview _workDoneProgressEnd -> Just params)))
         -> Just (ProgressEnd t params)
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
      f expectedTitles ((params ^. L.token): activeProgressTokens)
    ProgressBegin token params -> do
      liftIO $ token `expectedIn` activeProgressTokens
      f (delete (params ^. L.title) expectedTitles) activeProgressTokens
    ProgressReport token _ -> do
      liftIO $ token `expectedIn` activeProgressTokens
      f expectedTitles activeProgressTokens
    ProgressEnd token _ -> do
      liftIO $ token `expectedIn` activeProgressTokens
      f expectedTitles (delete token activeProgressTokens)


expectedIn :: (Foldable t, Eq a, Show a) => a -> t a -> Assertion
expectedIn a as = a `elem` as @? "Unexpected " ++ show a

getMessageResult :: TResponseMessage m -> MessageResult m
getMessageResult rsp =
  case rsp ^. L.result of
    Right x -> x
    Left err -> throw $ UnexpectedResponseError (SomeLspId $ fromJust $ rsp ^. L.id) err
