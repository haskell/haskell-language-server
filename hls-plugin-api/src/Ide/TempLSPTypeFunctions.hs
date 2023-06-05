{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PolyKinds             #-}
module Ide.TempLSPTypeFunctions (takeLefts, dumpNulls, nullToMaybe', NullToMaybe,
                                 toLspId, defClientCapabilities,
                                 defGeneralClientCapabilities,
                                 defNotebookDocumentClientCapabilities,
                                 defNotebookDocumentSyncClientCapabilities,
                                 defTextDocumentCapabilities,
                                 defWindowClientCapabilities,
                                 defWorkspaceCapabilities, nullToEmpty) where

import           Data.Semigroup                ()
import           Data.Text                     (Text)
import           Language.LSP.Protocol.Message (LspId (IdInt, IdString))
import           Language.LSP.Protocol.Types   (ClientCapabilities (ClientCapabilities),
                                                GeneralClientCapabilities (GeneralClientCapabilities),
                                                Int32,
                                                NotebookDocumentClientCapabilities (NotebookDocumentClientCapabilities),
                                                NotebookDocumentSyncClientCapabilities (NotebookDocumentSyncClientCapabilities),
                                                Null (Null),
                                                TextDocumentClientCapabilities (TextDocumentClientCapabilities),
                                                WindowClientCapabilities (WindowClientCapabilities),
                                                WorkspaceClientCapabilities (WorkspaceClientCapabilities),
                                                WorkspaceEdit (WorkspaceEdit),
                                                type (|?) (..))


-- The functions below may be added to the lsp-types package if they end up being
-- useful. temporarily including them here now.


takeLefts :: Foldable f => f (a |? b) -> [a]
takeLefts = foldr (\x acc -> case x of
                                InL x' -> x' : acc
                                InR _  -> acc) []

-- Especially when we want to use concat, we are not interested in nulls,
-- because of this we need to filter them out
dumpNulls :: (Foldable f, NullToMaybe a b) => f a -> [b]
dumpNulls = foldr (\x acc -> case nullToMaybe' x of
                                Just x' -> x' : acc
                                Nothing -> acc) []

nullToEmpty :: Monoid m => (m |? Null) -> m
nullToEmpty (InR Null) = mempty
nullToEmpty (InL ls)   = ls
instance Semigroup s => Semigroup (s |? Null) where
  InL x <> InL y = InL (x <> y)
  InL x <> InR _ = InL x
  InR _ <> InL x = InL x
  InR _ <> InR y = InR y

instance Semigroup WorkspaceEdit where
  (WorkspaceEdit a b c) <> (WorkspaceEdit a' b' c') = WorkspaceEdit (a <> a') (b <> b') (c <> c')

class NullToMaybe a b where
  nullToMaybe' :: a -> Maybe b

instance NullToMaybe (a |? Null) a where
  nullToMaybe' (InL x) = Just x
  nullToMaybe' (InR _) = Nothing

instance NullToMaybe (a |? (b |? Null)) (a |? b) where
  nullToMaybe' (InL x)       = Just $ InL x
  nullToMaybe' (InR (InL x)) = Just $ InR x
  nullToMaybe' (InR (InR _)) = Nothing

instance NullToMaybe (a |? (b |? (c |? Null))) (a |? (b |? c)) where
  nullToMaybe' (InL x)             = Just $ InL x
  nullToMaybe' (InR (InL x))       = Just $ InR $ InL x
  nullToMaybe' (InR (InR (InL x))) = Just $ InR $ InR x
  nullToMaybe' (InR (InR (InR _))) = Nothing

toLspId :: (Int32 |? Text) -> LspId a
toLspId (InL x) = IdInt x
toLspId (InR y) = IdString y

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
