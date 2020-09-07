-- Copyright (c) 2019 The DAML Authors. All rights reserved.
-- SPDX-License-Identifier: Apache-2.0

{-# LANGUAGE DuplicateRecordFields #-}

module Development.IDE.Test
  ( Cursor
  , cursorPosition
  , requireDiagnostic
  , diagnostic
  , expectDiagnostics
  , expectNoMoreDiagnostics
  , canonicalizeUri
  ) where

import Control.Applicative.Combinators
import Control.Lens hiding (List)
import Control.Monad
import Control.Monad.IO.Class
import qualified Data.Map.Strict as Map
import qualified Data.Text as T
import Language.Haskell.LSP.Test hiding (message)
import qualified Language.Haskell.LSP.Test as LspTest
import Language.Haskell.LSP.Types
import Language.Haskell.LSP.Types.Lens as Lsp
import System.Time.Extra
import Test.Tasty.HUnit
import System.Directory (canonicalizePath)
import Data.Maybe (fromJust)


-- | (0-based line number, 0-based column number)
type Cursor = (Int, Int)

cursorPosition :: Cursor -> Position
cursorPosition (line,  col) = Position line col

requireDiagnostic :: List Diagnostic -> (DiagnosticSeverity, Cursor, T.Text) -> Assertion
requireDiagnostic actuals expected@(severity, cursor, expectedMsg) = do
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

-- |wait for @timeout@ seconds and report an assertion failure
-- if any diagnostic messages arrive in that period
expectNoMoreDiagnostics :: Seconds -> Session ()
expectNoMoreDiagnostics timeout = do
    -- Give any further diagnostic messages time to arrive.
    liftIO $ sleep timeout
    -- Send a dummy message to provoke a response from the server.
    -- This guarantees that we have at least one message to
    -- process, so message won't block or timeout.
    void $ sendRequest (CustomClientMethod "non-existent-method") ()
    handleMessages
  where
    handleMessages = handleDiagnostic <|> handleCustomMethodResponse <|> ignoreOthers
    handleDiagnostic = do
        diagsNot <- LspTest.message :: Session PublishDiagnosticsNotification
        let fileUri = diagsNot ^. params . uri
            actual = diagsNot ^. params . diagnostics
        liftIO $ assertFailure $
            "Got unexpected diagnostics for " <> show fileUri <>
            " got " <> show actual
    handleCustomMethodResponse =
        -- the CustomClientMethod triggers a RspCustomServer
        -- handle that and then exit
        void (LspTest.message :: Session CustomResponse)
    ignoreOthers = void anyMessage >> handleMessages

expectDiagnostics :: [(FilePath, [(DiagnosticSeverity, Cursor, T.Text)])] -> Session ()
expectDiagnostics expected = do
    let f = getDocUri >=> liftIO . canonicalizeUri >=> pure . toNormalizedUri
    expected' <- Map.fromListWith (<>) <$> traverseOf (traverse . _1) f expected
    go expected'
    where
        go m
            | Map.null m = pure ()
            | otherwise = do
                  diagsNot <- skipManyTill anyMessage diagnostic
                  let fileUri = diagsNot ^. params . uri
                  canonUri <- liftIO $ toNormalizedUri <$> canonicalizeUri fileUri
                  case Map.lookup canonUri m of
                      Nothing -> do
                          let actual = diagsNot ^. params . diagnostics
                          liftIO $ assertFailure $
                              "Got diagnostics for " <> show fileUri <>
                              " but only expected diagnostics for " <> show (Map.keys m) <>
                              " got " <> show actual
                      Just expected -> do
                          let actual = diagsNot ^. params . diagnostics
                          liftIO $ mapM_ (requireDiagnostic actual) expected
                          liftIO $ unless (length expected == length actual) $
                              assertFailure $
                              "Incorrect number of diagnostics for " <> show fileUri <>
                              ", expected " <> show expected <>
                              " but got " <> show actual
                          go $ Map.delete canonUri m

canonicalizeUri :: Uri -> IO Uri
canonicalizeUri uri = filePathToUri <$> canonicalizePath (fromJust (uriToFilePath uri))

diagnostic :: Session PublishDiagnosticsNotification
diagnostic = LspTest.message

standardizeQuotes :: T.Text -> T.Text
standardizeQuotes msg = let
        repl '‘' = '\''
        repl '’' = '\''
        repl '`' = '\''
        repl  c   = c
    in  T.map repl msg
