-- Copyright (c) 2019 The DAML Authors. All rights reserved.
-- SPDX-License-Identifier: Apache-2.0

{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE OverloadedStrings     #-}

module Development.IDE.Test
  ( Cursor
  , cursorPosition
  , requireDiagnostic
  , diagnostic
  , expectDiagnostics
  , expectDiagnosticsWithTags
  , ExpectedDiagnostic
  , ExpectedDiagnosticWithTag
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
import           Data.Proxy
import           Data.Text                       (Text)
import qualified Data.Text                       as T
import           Development.IDE.Plugin.Test     (TestRequest (..),
                                                  WaitForIdeRuleResult,
                                                  ideResultSuccess)
import           Development.IDE.Test.Diagnostic
import           GHC.TypeLits                    (symbolVal)
import           Ide.Plugin.Config               (CheckParents, checkProject)
import qualified Language.LSP.Protocol.Lens      as L
import           Language.LSP.Protocol.Message
import           Language.LSP.Protocol.Types
import           Language.LSP.Test               hiding (message)
import qualified Language.LSP.Test               as LspTest
import           System.Directory                (canonicalizePath)
import           System.FilePath                 (equalFilePath)
import           System.Time.Extra
import           Test.Tasty.HUnit

expectedDiagnosticWithNothing :: ExpectedDiagnostic -> ExpectedDiagnosticWithTag
expectedDiagnosticWithNothing (ds, c, t, code) = (ds, c, t, code, Nothing)

requireDiagnosticM
    :: (Foldable f, Show (f Diagnostic), HasCallStack)
    => f Diagnostic
    -> ExpectedDiagnosticWithTag
    -> Assertion
requireDiagnosticM actuals expected = case requireDiagnostic actuals expected of
    Nothing  -> pure ()
    Just err -> assertFailure err

-- |wait for @timeout@ seconds and report an assertion failure
-- if any diagnostic messages arrive in that period
expectNoMoreDiagnostics :: HasCallStack => Seconds -> Session ()
expectNoMoreDiagnostics timeout =
  expectMessages SMethod_TextDocumentPublishDiagnostics timeout $ \diagsNot -> do
    let fileUri = diagsNot ^. L.params . L.uri
        actual = diagsNot ^. L.params . L.diagnostics
    unless (null actual) $ liftIO $
      assertFailure $
        "Got unexpected diagnostics for " <> show fileUri
          <> " got "
          <> show actual

expectMessages :: SMethod m -> Seconds -> (TServerMessage m -> Session ()) -> Session ()
expectMessages m timeout handle = do
    -- Give any further diagnostic messages time to arrive.
    liftIO $ sleep timeout
    -- Send a dummy message to provoke a response from the server.
    -- This guarantees that we have at least one message to
    -- process, so message won't block or timeout.
    let cm = SMethod_CustomMethod (Proxy @"test")
    i <- sendRequest cm $ A.toJSON GetShakeSessionQueueCount
    go cm i
  where
    go cm i = handleMessages
      where
        handleMessages = (LspTest.message m >>= handle) <|> (void $ responseForId cm i) <|> ignoreOthers
        ignoreOthers = void anyMessage >> handleMessages

flushMessages :: Session ()
flushMessages = do
    let cm = SMethod_CustomMethod (Proxy @"non-existent-method")
    i <- sendRequest cm A.Null
    void (responseForId cm i) <|> ignoreOthers cm i
    where
        ignoreOthers cm i = skipManyTill anyMessage (responseForId cm i) >> flushMessages

-- | It is not possible to use 'expectDiagnostics []' to assert the absence of diagnostics,
--   only that existing diagnostics have been cleared.
--
--   Rather than trying to assert the absence of diagnostics, introduce an
--   expected diagnostic (e.g. a redundant import) and assert the singleton diagnostic.
expectDiagnostics :: HasCallStack => [(FilePath, [ExpectedDiagnostic])] -> Session ()
expectDiagnostics
  = expectDiagnosticsWithTags
  . map (second (map expectedDiagnosticWithNothing))

unwrapDiagnostic :: TServerMessage Method_TextDocumentPublishDiagnostics  -> (Uri, [Diagnostic])
unwrapDiagnostic diagsNot = (diagsNot^. L.params . L.uri, diagsNot^. L.params . L.diagnostics)

expectDiagnosticsWithTags :: HasCallStack => [(String, [ExpectedDiagnosticWithTag])] -> Session ()
expectDiagnosticsWithTags expected = do
    let toSessionPath = getDocUri >=> liftIO . canonicalizeUri >=> pure . toNormalizedUri
        next = unwrapDiagnostic <$> skipManyTill anyMessage diagnostic
    expected' <- Map.fromListWith (<>) <$> traverseOf (traverse . _1) toSessionPath expected
    expectDiagnosticsWithTags' next expected'

expectDiagnosticsWithTags' ::
  (HasCallStack, MonadIO m) =>
  m (Uri, [Diagnostic]) ->
  Map.Map NormalizedUri [ExpectedDiagnosticWithTag] ->
  m ()
expectDiagnosticsWithTags' next m | null m = do
    (_,actual) <- next
    case actual of
        [] ->
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

expectCurrentDiagnostics :: HasCallStack => TextDocumentIdentifier -> [ExpectedDiagnostic] -> Session ()
expectCurrentDiagnostics doc expected = do
    diags <- getCurrentDiagnostics doc
    checkDiagnosticsForDoc doc expected diags

checkDiagnosticsForDoc :: HasCallStack => TextDocumentIdentifier -> [ExpectedDiagnostic] -> [Diagnostic] -> Session ()
checkDiagnosticsForDoc TextDocumentIdentifier {_uri} expected obtained = do
    let expected' = Map.singleton nuri (map expectedDiagnosticWithNothing expected)
        nuri = toNormalizedUri _uri
    expectDiagnosticsWithTags' (return (_uri, obtained)) expected'

canonicalizeUri :: Uri -> IO Uri
canonicalizeUri uri = filePathToUri <$> canonicalizePath (fromJust (uriToFilePath uri))

diagnostic :: Session (TNotificationMessage Method_TextDocumentPublishDiagnostics)
diagnostic = LspTest.message SMethod_TextDocumentPublishDiagnostics

tryCallTestPlugin :: (A.FromJSON b) => TestRequest -> Session (Either (TResponseError @ClientToServer (Method_CustomMethod "test")) b)
tryCallTestPlugin cmd = do
    let cm = SMethod_CustomMethod (Proxy @"test")
    waitId <- sendRequest cm (A.toJSON cmd)
    TResponseMessage{_result} <- skipManyTill anyMessage $ responseForId cm waitId
    return $ case _result of
         Left e -> Left e
         Right json -> case A.fromJSON json of
             A.Success a -> Right a
             A.Error e   -> error e

callTestPlugin :: (A.FromJSON b) => TestRequest -> Session b
callTestPlugin cmd = do
    res <- tryCallTestPlugin cmd
    case res of
        Left (TResponseError t err _) -> error $ show t <> ": " <> T.unpack err
        Right a                       -> pure a


waitForAction :: String -> TextDocumentIdentifier -> Session WaitForIdeRuleResult
waitForAction key TextDocumentIdentifier{_uri} =
    callTestPlugin (WaitForIdeRule key _uri)

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
        FromServerMess (SMethod_CustomMethod p) (NotMess TNotificationMessage{_params = value})
            | symbolVal p == T.unpack msg -> pred value
        _ -> Nothing

waitForGC :: Session [T.Text]
waitForGC = waitForCustomMessage "ghcide/GC" $ \v ->
    case A.fromJSON v of
        A.Success x -> Just x
        _           -> Nothing

configureCheckProject :: Bool -> Session ()
configureCheckProject overrideCheckProject = setConfigSection "haskell" (toJSON $ def{checkProject = overrideCheckProject})

-- | Pattern match a message from ghcide indicating that a file has been indexed
isReferenceReady :: FilePath -> Session ()
isReferenceReady p = void $ referenceReady (equalFilePath p)

referenceReady :: (FilePath -> Bool) -> Session FilePath
referenceReady pred = satisfyMaybe $ \case
  FromServerMess (SMethod_CustomMethod p) (NotMess TNotificationMessage{_params})
    | A.Success fp <- A.fromJSON _params
    , pred fp
    , symbolVal p == "ghcide/reference/ready"
    -> Just fp
  _ -> Nothing

