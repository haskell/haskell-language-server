{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE ViewPatterns          #-}
module Progress (tests) where

import           Control.Exception                  (throw)
import           Control.Lens                       hiding ((.=))
import           Data.Aeson                         (decode, encode)
import           Data.Functor                       (void)
import           Data.List                          (delete)
import           Data.Maybe                         (fromJust)
import           Data.Text                          (Text, pack)
import           GHC.Stack                          (withFrozenCallStack)
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
            runSession hlsLspCommand progressCaps "test/testdata/diagnostics" $ do
                let path = "Foo.hs"
                _ <- openDoc path "haskell"
                expectProgressMessages
                  ["Processing"]
                  [pack ("Setting up diagnostics (for " ++ path ++ ")"), "Indexing"]
                  []
                  []
        , requiresEvalPlugin $ testCase "eval plugin sends progress reports" $
            runSession hlsLspCommand progressCaps "plugins/hls-eval-plugin/test/testdata" $ do
              doc <- openDoc "TIO.hs" "haskell"
              lspId <- sendRequest SMethod_TextDocumentCodeLens (CodeLensParams Nothing Nothing doc)

              (codeLensResponse, createdProgressTokens, activeProgressTokens) <- expectProgressMessagesTill
                (responseForId SMethod_TextDocumentCodeLens lspId)
                ["Setting up testdata (for TIO.hs)"]
                ["Processing", "Indexing"]
                []
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

                      expectProgressMessages ["Evaluating"] ["Processing", "Indexing"] createdProgressTokens activeProgressTokens
                  _ -> error $ "Unexpected response result: " ++ show response
        , requiresOrmoluPlugin $ testCase "ormolu plugin sends progress notifications" $ do
            runSessionWithConfig (def { ignoreConfigurationRequests = False }) hlsLspCommand progressCaps "test/testdata/format" $ do
                void configurationRequest
                setHlsConfig (formatLspConfig "ormolu")
                doc <- openDoc "Format.hs" "haskell"
                expectProgressMessages ["Setting up format (for Format.hs)"] ["Processing", "Indexing"] [] []
                _ <- sendRequest SMethod_TextDocumentFormatting $ DocumentFormattingParams Nothing doc (FormattingOptions 2 True Nothing Nothing Nothing)
                expectProgressMessages ["Formatting Format.hs"] ["Processing", "Indexing"] [] []
        , requiresFourmoluPlugin $ testCase "fourmolu plugin sends progress notifications" $ do
            runSessionWithConfig (def { ignoreConfigurationRequests = False }) hlsLspCommand progressCaps "test/testdata/format" $ do
                void configurationRequest
                setHlsConfig (formatLspConfig "fourmolu")
                doc <- openDoc "Format.hs" "haskell"
                expectProgressMessages ["Setting up format (for Format.hs)"] ["Processing", "Indexing"] [] []
                _ <- sendRequest SMethod_TextDocumentFormatting $ DocumentFormattingParams Nothing doc (FormattingOptions 2 True Nothing Nothing Nothing)
                expectProgressMessages ["Formatting Format.hs"] ["Processing", "Indexing"] [] []
        ]

formatLspConfig :: Text -> Config
formatLspConfig provider = def { formattingProvider = provider }

progressCaps :: ClientCapabilities
progressCaps = fullLatestClientCaps{_window = Just (WindowClientCapabilities (Just True) Nothing Nothing)}

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

expectProgressMessagesTill :: HasCallStack => Session a -> [Text] -> [Text] -> [ProgressToken] -> [ProgressToken] -> Session (a, [ProgressToken], [ProgressToken])
expectProgressMessagesTill stopMessage requiredTitles optionalTitles createdProgressTokens activeProgressTokens = do
  message <- skipManyTill anyMessage (interestingMessage stopMessage)
  withFrozenCallStack $ do
    case message of
      InterestingMessage a -> do
        liftIO $ null requiredTitles @? "Required progress titles were not seen (consider moving to the optional list): " <> show requiredTitles
        pure (a, createdProgressTokens, activeProgressTokens)
      ProgressMessage progressMessage ->
        updateExpectProgressStateAndRecurseWith
          (expectProgressMessagesTill stopMessage)
          progressMessage
          requiredTitles
          optionalTitles
          createdProgressTokens
          activeProgressTokens

{- | Test that the server is correctly producing a sequence of progress related
 messages. `create` can be dangling, but `begin` cannot. Each `begin` should
 have a respective `end`, optionally with some progress in between. Tokens must
 match between these requests. The `begin` messages have titles describing the
 work that is in-progress.

 'requiredTitles' must all be consumed before the loop terminates.
 'optionalTitles' may appear (and if they do, their titles must match), but
 their absence is not an error. The LSP spec permits a server to create a
 progress token and then abandon it without ever sending a `begin` notification
 (e.g. when the underlying work completes before the progress-reporting thread
 starts). Any `begin` title that is neither required nor optional is treated as
 unexpected and fails the test.

 The loop terminates when all required titles have been seen and all active progress
 sessions have ended.
-}
expectProgressMessages :: HasCallStack => [Text] -> [Text] -> [ProgressToken] -> [ProgressToken] -> Session ()
expectProgressMessages [] _ _ [] = pure ()
expectProgressMessages requiredTitles optionalTitles createdProgressTokens activeProgressTokens = do
  message <- skipManyTill anyMessage progressMessage
  withFrozenCallStack $
    updateExpectProgressStateAndRecurseWith expectProgressMessages message requiredTitles optionalTitles createdProgressTokens activeProgressTokens

updateExpectProgressStateAndRecurseWith :: HasCallStack
                                        => ([Text] -> [Text] -> [ProgressToken] -> [ProgressToken] -> Session a)
                                        -> ProgressMessage
                                        -> [Text]
                                        -> [Text]
                                        -> [ProgressToken]
                                        -> [ProgressToken]
                                        -> Session a
updateExpectProgressStateAndRecurseWith f progressMessage requiredTitles optionalTitles createdProgressTokens activeProgressTokens = do
  case progressMessage of
    ProgressCreate params -> do
      f requiredTitles optionalTitles ((params ^. L.token): createdProgressTokens) activeProgressTokens
    ProgressBegin token params -> do
      liftIO $ token `expectedIn` createdProgressTokens
      let title = params ^. L.title
          (requiredTitles', optionalTitles')
            | title `elem` requiredTitles = (delete title requiredTitles, optionalTitles)
             -- Note that we do not delete from the optional titles list.
            | title `elem` optionalTitles = (requiredTitles, optionalTitles)
            | otherwise = error $ "Unexpected progress title: " ++ show title
                ++ "\n  required: " ++ show requiredTitles
                ++ "\n  optional: " ++ show optionalTitles
      f requiredTitles' optionalTitles' (delete token createdProgressTokens) (token:activeProgressTokens)
    ProgressReport token _ -> do
      liftIO $ token `expectedIn` activeProgressTokens
      f requiredTitles optionalTitles createdProgressTokens activeProgressTokens
    ProgressEnd token _ -> do
      liftIO $ token `expectedIn` activeProgressTokens
      f requiredTitles optionalTitles createdProgressTokens (delete token activeProgressTokens)


expectedIn :: (Foldable t, Eq a, Show a, HasCallStack) => a -> t a -> Assertion
expectedIn a as = a `elem` as @? "Unexpected " ++ show a

getMessageResult :: (Show (ErrorData m), HasCallStack) => TResponseMessage m -> MessageResult m
getMessageResult rsp =
  case rsp ^. L.result of
    Right x  -> x
    Left err -> throw $ UnexpectedResponseError (fromJust $ rsp ^. L.id) err
