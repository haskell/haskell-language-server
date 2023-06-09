{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PolyKinds             #-}

module Ide.TempLSPTypeFunctions (defClientCapabilities,
                                 defGeneralClientCapabilities,
                                 defNotebookDocumentClientCapabilities,
                                 defNotebookDocumentSyncClientCapabilities,
                                 defTextDocumentCapabilities,
                                 defWindowClientCapabilities,
                                 defWorkspaceCapabilities, maybeToNull) where

import           Data.Hashable
import           Data.Semigroup                ()
import           Data.Text                     (Text)
import           Language.LSP.Protocol.Message (LspId (IdInt, IdString))
import           Language.LSP.Protocol.Types   (ClientCapabilities (ClientCapabilities),
                                                GeneralClientCapabilities (GeneralClientCapabilities),
                                                Int32, Location,
                                                NotebookDocumentClientCapabilities (NotebookDocumentClientCapabilities),
                                                NotebookDocumentSyncClientCapabilities (NotebookDocumentSyncClientCapabilities),
                                                Null (Null), Position, Range,
                                                TextDocumentClientCapabilities (TextDocumentClientCapabilities),
                                                WindowClientCapabilities (WindowClientCapabilities),
                                                WorkspaceClientCapabilities (WorkspaceClientCapabilities),
                                                WorkspaceEdit (WorkspaceEdit),
                                                type (|?) (..))

-- The functions below may be added to the lsp-types package if they end up being
-- useful. temporarily including them here now.




maybeToNull :: Maybe a -> a |? Null
maybeToNull (Just x) = InL x
maybeToNull Nothing  = InR Null
instance Semigroup s => Semigroup (s |? Null) where
  InL x <> InL y = InL (x <> y)
  InL x <> InR _ = InL x
  InR _ <> InL x = InL x
  InR _ <> InR y = InR y

instance Semigroup WorkspaceEdit where
  (WorkspaceEdit a b c) <> (WorkspaceEdit a' b' c') = WorkspaceEdit (a <> a') (b <> b') (c <> c')
instance Monoid WorkspaceEdit where
  mempty = WorkspaceEdit Nothing Nothing Nothing

instance Hashable Location
instance Hashable Range
instance Hashable Position



-- TODO: Find some saner default ClientCapabilities so we don't need to
-- use Nothing 54 times.
defClientCapabilities :: ClientCapabilities
defClientCapabilities =
    ClientCapabilities (Just defWorkspaceCapabilities)
                       (Just defTextDocumentCapabilities)
                       (Just defNotebookDocumentClientCapabilities)
                       (Just defWindowClientCapabilities)
                       (Just defGeneralClientCapabilities)
                       Nothing

defWorkspaceCapabilities :: WorkspaceClientCapabilities
defWorkspaceCapabilities =
    WorkspaceClientCapabilities Nothing Nothing Nothing Nothing Nothing Nothing
                                Nothing Nothing Nothing Nothing Nothing Nothing
                                Nothing Nothing

defTextDocumentCapabilities :: TextDocumentClientCapabilities
defTextDocumentCapabilities =
    TextDocumentClientCapabilities Nothing Nothing Nothing Nothing Nothing
                                   Nothing Nothing Nothing Nothing Nothing
                                   Nothing Nothing Nothing Nothing Nothing
                                   Nothing Nothing Nothing Nothing Nothing
                                   Nothing Nothing Nothing Nothing Nothing
                                   Nothing Nothing Nothing Nothing Nothing

defNotebookDocumentClientCapabilities :: NotebookDocumentClientCapabilities
defNotebookDocumentClientCapabilities =
    NotebookDocumentClientCapabilities defNotebookDocumentSyncClientCapabilities

defNotebookDocumentSyncClientCapabilities :: NotebookDocumentSyncClientCapabilities
defNotebookDocumentSyncClientCapabilities =
    NotebookDocumentSyncClientCapabilities Nothing Nothing

defWindowClientCapabilities :: WindowClientCapabilities
defWindowClientCapabilities = WindowClientCapabilities Nothing Nothing Nothing

defGeneralClientCapabilities :: GeneralClientCapabilities
defGeneralClientCapabilities = GeneralClientCapabilities Nothing Nothing Nothing Nothing
