-- Copyright (c) 2019 The DAML Authors. All rights reserved.
-- SPDX-License-Identifier: Apache-2.0

{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE PolyKinds             #-}

module Development.IDE.Test
  ( Cursor
  , cursorPosition
  , requireDiagnostic
  , diagnostic
  , expectDiagnostics
  , expectDiagnosticsWithTags
  , expectNoMoreDiagnostics
  , expectMessages
  , expectCurrentDiagnostics
  , checkDiagnosticsForDoc
  , canonicalizeUri
  , standardizeQuotes
  , flushMessages
  , waitForAction
  , getInterfaceFilesDir
  , garbageCollectDirtyKeys
  , getFilesOfInterest
  , waitForTypecheck
  , waitForBuildQueue
  , getStoredKeys
  , waitForCustomMessage
  , waitForGC
  , getBuildKeysBuilt
  , getBuildKeysVisited
  , getBuildKeysChanged
  , getBuildEdgesCount
  , configureCheckProject
  , isReferenceReady
  , referenceReady) where

import           Control.Applicative.Combinators
import           Control.Lens                    hiding (List)
import           Control.Monad
import           Control.Monad.IO.Class
import           Data.Aeson                      (toJSON)
import qualified Data.Aeson                      as A
import           Data.Bifunctor                  (second)
import           Data.Default
import qualified Data.Map.Strict                 as Map
import           Data.Maybe                      (fromJust)
import           Data.Text                       (Text)
import qualified Data.Text                       as T
import           Development.IDE.Plugin.Test     (TestRequest (..),
                                                  WaitForIdeRuleResult,
                                                  ideResultSuccess)
import           Development.IDE.Test.Diagnostic
import           Ide.Plugin.Config               (CheckParents, checkProject)
import           Language.LSP.Test               hiding (message)
import qualified Language.LSP.Test               as LspTest
import           Language.LSP.Types              hiding
                                                 (SemanticTokenAbsolute (length, line),
                                                  SemanticTokenRelative (length),
                                                  SemanticTokensEdit (_start))
import           Language.LSP.Types.Lens         as Lsp
import           System.Directory                (canonicalizePath)
import           System.Time.Extra
import           Test.Tasty.HUnit
import System.FilePath (equalFilePath)

requireDiagnosticM
    :: (Foldable f, Show (f Diagnostic), HasCallStack)
    => f Diagnostic
    -> (DiagnosticSeverity, Cursor, T.Text, Maybe DiagnosticTag)
    -> Assertion
requireDiagnosticM actuals expected = case requireDiagnostic actuals expected of
    Nothing  -> pure ()
    Just err -> assertFailure err

-- |wait for @timeout@ seconds and report an assertion failure
-- if any diagnostic messages arrive in that period
expectNoMoreDiagnostics :: HasCallStack => Seconds -> Session ()
expectNoMoreDiagnostics timeout =
  expectMessages STextDocumentPublishDiagnostics timeout $ \diagsNot -> do
    let fileUri = diagsNot ^. params . uri
        actual = diagsNot ^. params . diagnostics
    liftIO $
      assertFailure $
        "Got unexpected diagnostics for " <> show fileUri
          <> " got "
          <> show actual

expectMessages :: SMethod m -> Seconds -> (ServerMessage m -> Session ()) -> Session ()
expectMessages m timeout handle = do
    -- Give any further diagnostic messages time to arrive.
    liftIO $ sleep timeout
    -- Send a dummy message to provoke a response from the server.
    -- This guarantees that we have at least one message to
    -- process, so message won't block or timeout.
    let cm = SCustomMethod "test"
    i <- sendRequest cm $ A.toJSON GetShakeSessionQueueCount
    go cm i
  where
    go cm i = handleMessages
      where
        handleMessages = (LspTest.message m >>= handle) <|> (void $ responseForId cm i) <|> ignoreOthers
        ignoreOthers = void anyMessage >> handleMessages

flushMessages :: Session ()
flushMessages = do
    let cm = SCustomMethod "non-existent-method"
    i <- sendRequest cm A.Null
    void (responseForId cm i) <|> ignoreOthers cm i
    where
        ignoreOthers cm i = skipManyTill anyMessage (responseForId cm i) >> flushMessages

-- | It is not possible to use 'expectDiagnostics []' to assert the absence of diagnostics,
--   only that existing diagnostics have been cleared.
--
--   Rather than trying to assert the absence of diagnostics, introduce an
--   expected diagnostic (e.g. a redundant import) and assert the singleton diagnostic.
expectDiagnostics :: HasCallStack => [(FilePath, [(DiagnosticSeverity, Cursor, T.Text)])] -> Session ()
expectDiagnostics
  = expectDiagnosticsWithTags
  . map (second (map (\(ds, c, t) -> (ds, c, t, Nothing))))

unwrapDiagnostic :: NotificationMessage TextDocumentPublishDiagnostics  -> (Uri, List Diagnostic)
unwrapDiagnostic diagsNot = (diagsNot^.params.uri, diagsNot^.params.diagnostics)

expectDiagnosticsWithTags :: HasCallStack => [(String, [(DiagnosticSeverity, Cursor, T.Text, Maybe DiagnosticTag)])] -> Session ()
expectDiagnosticsWithTags expected = do
    let f = getDocUri >=> liftIO . canonicalizeUri >=> pure . toNormalizedUri
        next = unwrapDiagnostic <$> skipManyTill anyMessage diagnostic
    expected' <- Map.fromListWith (<>) <$> traverseOf (traverse . _1) f expected
    expectDiagnosticsWithTags' next expected'

expectDiagnosticsWithTags' ::
  (HasCallStack, MonadIO m) =>
  m (Uri, List Diagnostic) ->
  Map.Map NormalizedUri [(DiagnosticSeverity, Cursor, T.Text, Maybe DiagnosticTag)] ->
  m ()
expectDiagnosticsWithTags' next m | null m = do
    (_,actual) <- next
    case actual of
        List [] ->
            return ()
        _ ->
            liftIO $ assertFailure $ "Got unexpected diagnostics:" <> show actual

expectDiagnosticsWithTags' next expected = go expected
  where
    go m
      | Map.null m = pure ()
      | otherwise = do
        (fileUri, actual) <- next
        canonUri <- liftIO $ toNormalizedUri <$> canonicalizeUri fileUri
        case Map.lookup canonUri m of
          Nothing -> do
            liftIO $
              assertFailure $
                "Got diagnostics for " <> show fileUri
                  <> " but only expected diagnostics for "
                  <> show (Map.keys m)
                  <> " got "
                  <> show actual
          Just expected -> do
            liftIO $ mapM_ (requireDiagnosticM actual) expected
            liftIO $
              unless (length expected == length actual) $
                assertFailure $
                  "Incorrect number of diagnostics for " <> show fileUri
                    <> ", expected "
                    <> show expected
                    <> " but got "
                    <> show actual
            go $ Map.delete canonUri m

expectCurrentDiagnostics :: HasCallStack => TextDocumentIdentifier -> [(DiagnosticSeverity, Cursor, T.Text)] -> Session ()
expectCurrentDiagnostics doc expected = do
    diags <- getCurrentDiagnostics doc
    checkDiagnosticsForDoc doc expected diags

checkDiagnosticsForDoc :: HasCallStack => TextDocumentIdentifier -> [(DiagnosticSeverity, Cursor, T.Text)] -> [Diagnostic] -> Session ()
checkDiagnosticsForDoc TextDocumentIdentifier {_uri} expected obtained = do
    let expected' = Map.fromList [(nuri, map (\(ds, c, t) -> (ds, c, t, Nothing)) expected)]
        nuri = toNormalizedUri _uri
    expectDiagnosticsWithTags' (return (_uri, List obtained)) expected'

canonicalizeUri :: Uri -> IO Uri
canonicalizeUri uri = filePathToUri <$> canonicalizePath (fromJust (uriToFilePath uri))

diagnostic :: Session (NotificationMessage TextDocumentPublishDiagnostics)
diagnostic = LspTest.message STextDocumentPublishDiagnostics

tryCallTestPlugin :: (A.FromJSON b) => TestRequest -> Session (Either ResponseError b)
tryCallTestPlugin cmd = do
    let cm = SCustomMethod "test"
    waitId <- sendRequest cm (A.toJSON cmd)
    ResponseMessage{_result} <- skipManyTill anyMessage $ responseForId cm waitId
    return $ case _result of
         Left e -> Left e
         Right json -> case A.fromJSON json of
             A.Success a -> Right a
             A.Error e   -> error e

callTestPlugin :: (A.FromJSON b) => TestRequest -> Session b
callTestPlugin cmd = do
    res <- tryCallTestPlugin cmd
    case res of
        Left (ResponseError t err _) -> error $ show t <> ": " <> T.unpack err
        Right a                      -> pure a


waitForAction :: String -> TextDocumentIdentifier -> Session WaitForIdeRuleResult
waitForAction key TextDocumentIdentifier{_uri} =
    callTestPlugin (WaitForIdeRule key _uri)

getBuildKeysBuilt :: Session (Either ResponseError [T.Text])
getBuildKeysBuilt = tryCallTestPlugin GetBuildKeysBuilt

getBuildKeysVisited :: Session (Either ResponseError [T.Text])
getBuildKeysVisited = tryCallTestPlugin GetBuildKeysVisited

getBuildKeysChanged :: Session (Either ResponseError [T.Text])
getBuildKeysChanged = tryCallTestPlugin GetBuildKeysChanged

getBuildEdgesCount :: Session (Either ResponseError Int)
getBuildEdgesCount = tryCallTestPlugin GetBuildEdgesCount

getInterfaceFilesDir :: TextDocumentIdentifier -> Session FilePath
getInterfaceFilesDir TextDocumentIdentifier{_uri} = callTestPlugin (GetInterfaceFilesDir _uri)

garbageCollectDirtyKeys :: CheckParents -> Int -> Session [String]
garbageCollectDirtyKeys parents age = callTestPlugin (GarbageCollectDirtyKeys parents age)

getStoredKeys :: Session [Text]
getStoredKeys = callTestPlugin GetStoredKeys

waitForTypecheck :: TextDocumentIdentifier -> Session Bool
waitForTypecheck tid = ideResultSuccess <$> waitForAction "typecheck" tid

waitForBuildQueue :: Session ()
waitForBuildQueue = callTestPlugin WaitForShakeQueue

getFilesOfInterest :: Session [FilePath]
getFilesOfInterest = callTestPlugin GetFilesOfInterest

waitForCustomMessage :: T.Text -> (A.Value -> Maybe res) -> Session res
waitForCustomMessage msg pred =
    skipManyTill anyMessage $ satisfyMaybe $ \case
        FromServerMess (SCustomMethod lbl) (NotMess NotificationMessage{_params = value})
            | lbl == msg -> pred value
        _ -> Nothing

waitForGC :: Session [T.Text]
waitForGC = waitForCustomMessage "ghcide/GC" $ \v ->
    case A.fromJSON v of
        A.Success x -> Just x
        _           -> Nothing

configureCheckProject :: Bool -> Session ()
configureCheckProject overrideCheckProject =
    sendNotification SWorkspaceDidChangeConfiguration
        (DidChangeConfigurationParams $ toJSON
            def{checkProject = overrideCheckProject})

-- | Pattern match a message from ghcide indicating that a file has been indexed
isReferenceReady :: FilePath -> Session ()
isReferenceReady p = void $ referenceReady (equalFilePath p)

referenceReady :: (FilePath -> Bool) -> Session FilePath
referenceReady pred = satisfyMaybe $ \case
  FromServerMess (SCustomMethod "ghcide/reference/ready") (NotMess NotificationMessage{_params})
    | A.Success fp <- A.fromJSON _params
    , pred fp
    -> Just fp
  _ -> Nothing

