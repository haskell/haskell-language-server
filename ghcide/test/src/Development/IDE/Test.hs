-- Copyright (c) 2019 The DAML Authors. All rights reserved.
-- SPDX-License-Identifier: Apache-2.0

{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE PolyKinds #-}

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
  ) where

import qualified Data.Aeson as A
import Control.Applicative.Combinators
import Control.Lens hiding (List)
import Control.Monad
import Control.Monad.IO.Class
import Data.Bifunctor (second)
import qualified Data.Map.Strict as Map
import qualified Data.Text as T
import Language.LSP.Test hiding (message)
import qualified Language.LSP.Test as LspTest
import Language.LSP.Types
import Language.LSP.Types.Lens as Lsp
import System.Time.Extra
import Test.Tasty.HUnit
import System.Directory (canonicalizePath)
import Data.Maybe (fromJust)
import Development.IDE.Plugin.Test (WaitForIdeRuleResult, TestRequest(..))
import Data.Aeson (FromJSON)
import Data.Typeable (Typeable)


-- | (0-based line number, 0-based column number)
type Cursor = (Int, Int)

cursorPosition :: Cursor -> Position
cursorPosition (line,  col) = Position line col

requireDiagnostic :: List Diagnostic -> (DiagnosticSeverity, Cursor, T.Text, Maybe DiagnosticTag) -> Assertion
requireDiagnostic actuals expected@(severity, cursor, expectedMsg, expectedTag) = do
    unless (any match actuals) $
        assertFailure $
            "Could not find " <> show expected <>
            " in " <> show actuals
  where
    match :: Diagnostic -> Bool
    match d =
        Just severity == _severity d
        && cursorPosition cursor == d ^. range . start
        && standardizeQuotes (T.toLower expectedMsg) `T.isInfixOf`
           standardizeQuotes (T.toLower $ d ^. message)
        && hasTag expectedTag (d ^. tags)

    hasTag :: Maybe DiagnosticTag -> Maybe (List DiagnosticTag) -> Bool
    hasTag Nothing  _       = True
    hasTag (Just _) Nothing = False
    hasTag (Just actualTag) (Just (List tags)) = actualTag `elem` tags

-- |wait for @timeout@ seconds and report an assertion failure
-- if any diagnostic messages arrive in that period
expectNoMoreDiagnostics :: Seconds -> Session ()
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
    (void $ responseForId cm i) <|> ignoreOthers
    where
        ignoreOthers = void anyMessage >> flushMessages

-- | It is not possible to use 'expectDiagnostics []' to assert the absence of diagnostics,
--   only that existing diagnostics have been cleared.
--
--   Rather than trying to assert the absence of diagnostics, introduce an
--   expected diagnostic (e.g. a redundant import) and assert the singleton diagnostic.
expectDiagnostics :: [(FilePath, [(DiagnosticSeverity, Cursor, T.Text)])] -> Session ()
expectDiagnostics
  = expectDiagnosticsWithTags
  . map (second (map (\(ds, c, t) -> (ds, c, t, Nothing))))

unwrapDiagnostic :: NotificationMessage TextDocumentPublishDiagnostics  -> (Uri, List Diagnostic)
unwrapDiagnostic diagsNot = (diagsNot^.params.uri, diagsNot^.params.diagnostics)

expectDiagnosticsWithTags :: [(String, [(DiagnosticSeverity, Cursor, T.Text, Maybe DiagnosticTag)])] -> Session ()
expectDiagnosticsWithTags expected = do
    let f = getDocUri >=> liftIO . canonicalizeUri >=> pure . toNormalizedUri
        next = unwrapDiagnostic <$> skipManyTill anyMessage diagnostic
    expected' <- Map.fromListWith (<>) <$> traverseOf (traverse . _1) f expected
    expectDiagnosticsWithTags' next expected'

expectDiagnosticsWithTags' ::
  MonadIO m =>
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
            liftIO $ mapM_ (requireDiagnostic actual) expected
            liftIO $
              unless (length expected == length actual) $
                assertFailure $
                  "Incorrect number of diagnostics for " <> show fileUri
                    <> ", expected "
                    <> show expected
                    <> " but got "
                    <> show actual
            go $ Map.delete canonUri m

expectCurrentDiagnostics :: TextDocumentIdentifier -> [(DiagnosticSeverity, Cursor, T.Text)] -> Session ()
expectCurrentDiagnostics doc expected = do
    diags <- getCurrentDiagnostics doc
    checkDiagnosticsForDoc doc expected diags

checkDiagnosticsForDoc :: TextDocumentIdentifier -> [(DiagnosticSeverity, Cursor, T.Text)] -> [Diagnostic] -> Session ()
checkDiagnosticsForDoc TextDocumentIdentifier {_uri} expected obtained = do
    let expected' = Map.fromList [(nuri, map (\(ds, c, t) -> (ds, c, t, Nothing)) expected)]
        nuri = toNormalizedUri _uri
    expectDiagnosticsWithTags' (return (_uri, List obtained)) expected'

canonicalizeUri :: Uri -> IO Uri
canonicalizeUri uri = filePathToUri <$> canonicalizePath (fromJust (uriToFilePath uri))

diagnostic :: Session (NotificationMessage TextDocumentPublishDiagnostics)
diagnostic = LspTest.message STextDocumentPublishDiagnostics

standardizeQuotes :: T.Text -> T.Text
standardizeQuotes msg = let
        repl '‘' = '\''
        repl '’' = '\''
        repl '`' = '\''
        repl  c   = c
    in  T.map repl msg

waitForAction :: String -> TextDocumentIdentifier -> Session (Either ResponseError WaitForIdeRuleResult)
waitForAction key TextDocumentIdentifier{_uri} = do
    let cm = SCustomMethod "test"
    waitId <- sendRequest cm (A.toJSON $ WaitForIdeRule key _uri)
    ResponseMessage{_result} <- skipManyTill anyMessage $ responseForId cm waitId
    return $ do
      e <- _result
      case A.fromJSON e of
        A.Error e -> Left $ ResponseError InternalError (T.pack e) Nothing
        A.Success a -> pure a
