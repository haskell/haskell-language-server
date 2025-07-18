-- Copyright (c) 2019 The DAML Authors. All rights reserved.
-- SPDX-License-Identifier: Apache-2.0
{-# LANGUAGE GADTs #-}

-- | Display information on hover.
module Development.IDE.LSP.HoverDefinition
    ( Log(..)
    -- * For haskell-language-server
    , hover
    , foundHover
    , gotoDefinition
    , gotoTypeDefinition
    , gotoImplementation
    , documentHighlight
    , references
    , wsSymbols
    ) where

import           Control.Monad.Except           (ExceptT)
import           Control.Monad.IO.Class
import           Data.Maybe                     (fromMaybe)
import           Development.IDE.Core.Actions
import qualified Development.IDE.Core.Rules     as Shake
import           Development.IDE.Core.Shake     (IdeAction, IdeState (..),
                                                 runIdeAction)
import           Development.IDE.Types.Location
import           Ide.Logger
import           Ide.Plugin.Error
import           Ide.Types
import           Language.LSP.Protocol.Message
import           Language.LSP.Protocol.Types

import qualified Data.Text                      as T


data Log
  = LogWorkspaceSymbolRequest !T.Text
  | LogRequest !T.Text !Position !NormalizedUri
  deriving (Show)

instance Pretty Log where
  pretty = \case
    LogWorkspaceSymbolRequest query -> "Workspace symbols request:" <+> pretty query
    LogRequest label pos nfp ->
      pretty label <+> "request at position" <+> pretty (showPosition pos) <+>
        "in file:" <+> pretty (fromNormalizedUri nfp)

gotoDefinition     :: Recorder (WithPriority Log) -> IdeState -> TextDocumentPositionParams -> ExceptT PluginError (HandlerM c) (MessageResult Method_TextDocumentDefinition)
hover              :: Recorder (WithPriority Log) -> IdeState -> TextDocumentPositionParams -> ExceptT PluginError (HandlerM c) (Hover |? Null)
gotoTypeDefinition :: Recorder (WithPriority Log) -> IdeState -> TextDocumentPositionParams -> ExceptT PluginError (HandlerM c) (MessageResult Method_TextDocumentTypeDefinition)
gotoImplementation :: Recorder (WithPriority Log) -> IdeState -> TextDocumentPositionParams -> ExceptT PluginError (HandlerM c) (MessageResult Method_TextDocumentImplementation)
documentHighlight  :: Recorder (WithPriority Log) -> IdeState -> TextDocumentPositionParams -> ExceptT PluginError (HandlerM c) ([DocumentHighlight] |? Null)
gotoDefinition = request "Definition" getDefinition (InR $ InR Null) (InL . Definition . InR . map fst)
gotoTypeDefinition = request "TypeDefinition" getTypeDefinition (InR $ InR Null) (InL . Definition . InR . map fst)
gotoImplementation = request "Implementation" getImplementationDefinition (InR $ InR Null) (InL . Definition . InR)
hover          = request "Hover"      getAtPoint     (InR Null)     foundHover
documentHighlight = request "DocumentHighlight" highlightAtPoint (InR Null) InL

references :: Recorder (WithPriority Log) -> PluginMethodHandler IdeState Method_TextDocumentReferences
references recorder ide _ (ReferenceParams (TextDocumentIdentifier uri) pos _ _ _) = do
  let nuri = toNormalizedUri uri
  liftIO $ logWith recorder Debug $ LogRequest "References" pos nuri
  InL <$> (liftIO $ Shake.runAction "references" ide $ refsAtPoint nuri pos)

wsSymbols :: Recorder (WithPriority Log) -> PluginMethodHandler IdeState Method_WorkspaceSymbol
wsSymbols recorder ide _ (WorkspaceSymbolParams _ _ query) = liftIO $ do
  logWith recorder Debug $ LogWorkspaceSymbolRequest query
  runIdeAction "WorkspaceSymbols" (shakeExtras ide) $ InL . fromMaybe [] <$> workspaceSymbols query

foundHover :: (Maybe Range, [T.Text]) -> Hover |? Null
foundHover (mbRange, contents) =
  InL $ Hover (InL $ MarkupContent MarkupKind_Markdown $ T.intercalate sectionSeparator contents) mbRange

-- | Respond to and log a hover or go-to-definition request
request
  :: T.Text
  -> (NormalizedUri -> Position -> IdeAction (Maybe a))
  -> b
  -> (a -> b)
  -> Recorder (WithPriority Log)
  -> IdeState
  -> TextDocumentPositionParams
  -> ExceptT PluginError (HandlerM c) b
request label getResults notFound found recorder ide (TextDocumentPositionParams (TextDocumentIdentifier uri) pos) = liftIO $ do
    res <- logAndRunRequest recorder label getResults ide pos uri
    pure $ maybe notFound found res

logAndRunRequest :: Recorder (WithPriority Log) -> T.Text -> (NormalizedUri -> Position -> IdeAction b) -> IdeState -> Position -> Uri -> IO b
logAndRunRequest recorder label getResults ide pos path = do
  let nuri = toNormalizedUri path
  logWith recorder Debug $ LogRequest label pos nuri
  runIdeAction (T.unpack label) (shakeExtras ide) (getResults nuri pos)
