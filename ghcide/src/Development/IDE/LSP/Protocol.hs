-- Copyright (c) 2019 Digital Asset (Switzerland) GmbH and/or its affiliates. All rights reserved.
-- SPDX-License-Identifier: Apache-2.0
{-# LANGUAGE PatternSynonyms #-}

module Development.IDE.LSP.Protocol
    ( module Language.Haskell.LSP.Types
    , ServerRequest(..)
    , ServerNotification(..)
    , pattern EventFileDiagnostics
    ) where

import qualified Data.Aeson       as Aeson
import qualified Data.Text        as T
import Development.IDE.Types.Diagnostics
import Development.IDE.Types.Location
import Language.Haskell.LSP.Messages


import Language.Haskell.LSP.Types hiding
    ( CodeLens
    , DocumentSymbol
    , Hover
    , Shutdown
    , SignatureHelp
    , WorkspaceSymbol
    )

-- | Request sent by the client to the server.
data ServerRequest
    = Shutdown
    | KeepAlive
    | Completion      !CompletionParams
    | SignatureHelp   !TextDocumentPositionParams
    | Hover           !TextDocumentPositionParams
    | Definition      !TextDocumentPositionParams
    | References      !ReferenceParams
    | CodeLens        !CodeLensParams
    | Rename          !RenameParams
    | DocumentSymbol  !DocumentSymbolParams
    | WorkspaceSymbol !WorkspaceSymbolParams
    | Formatting      !DocumentFormattingParams
    | UnknownRequest  !T.Text !Aeson.Value
    deriving Show

data ServerNotification
    = DidOpenTextDocument   DidOpenTextDocumentParams
    | DidChangeTextDocument DidChangeTextDocumentParams
    | DidCloseTextDocument  DidCloseTextDocumentParams
    | DidSaveTextDocument   DidSaveTextDocumentParams
    | UnknownNotification   T.Text Aeson.Value

----------------------------------------------------------------------------------------------------
-- Pretty printing
----------------------------------------------------------------------------------------------------

-- | Pattern synonym to make it a bit more convenient to match on diagnostics
-- in things like damlc test.
pattern EventFileDiagnostics :: FilePath -> [Diagnostic] -> FromServerMessage
pattern EventFileDiagnostics fp diags <-
    NotPublishDiagnostics
        (NotificationMessage _ _ (PublishDiagnosticsParams (uriToFilePath' -> Just fp) (List diags)))
