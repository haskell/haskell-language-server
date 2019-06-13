-- Copyright (c) 2019 Digital Asset (Switzerland) GmbH and/or its affiliates. All rights reserved.
-- SPDX-License-Identifier: Apache-2.0

module Development.IDE.LSP.Protocol
    ( module Language.Haskell.LSP.Types
    , ServerRequest(..)
    , ServerNotification(..)
    , prettyPosition
    ) where

import qualified Data.Aeson       as Aeson
import qualified Data.Text        as T
import Data.Text.Prettyprint.Doc

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

prettyPosition :: Position -> Doc a
prettyPosition Position{..} = pretty (_line + 1) <> colon <> pretty (_character + 1)
