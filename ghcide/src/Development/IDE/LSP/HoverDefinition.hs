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
import           Development.IDE.Core.RuleInput  (toSomeFileInput,
                                                  toSomeHaskellInput)
import qualified Development.IDE.Core.Rules     as Shake
import           Development.IDE.Core.Shake     (IdeAction, IdeState (..),
                                                 runIdeAction)
import           Development.IDE.Types.Location
import           GHC.Iface.Ext.Types           (Identifier)
import           Ide.Logger
import           Ide.Plugin.Error
import           Ide.Types
import           Language.LSP.Protocol.Message
import           Language.LSP.Protocol.Types

import qualified Data.Text                      as T


data Log
  = LogWorkspaceSymbolRequest !T.Text
  | LogRequest !T.Text !Position !NormalizedFilePath
  deriving (Show)

instance Pretty Log where
  pretty = \case
    LogWorkspaceSymbolRequest query -> "Workspace symbols request:" <+> pretty query
    LogRequest label pos nfp ->
      pretty label <+> "request at position" <+> pretty (showPosition pos) <+>
        "in file:" <+> pretty (fromNormalizedFilePath nfp)

gotoDefinition     :: Recorder (WithPriority Log) -> IdeState -> TextDocumentPositionParams -> ExceptT PluginError (HandlerM c) (MessageResult Method_TextDocumentDefinition)
hover              :: Recorder (WithPriority Log) -> IdeState -> TextDocumentPositionParams -> ExceptT PluginError (HandlerM c) (Hover |? Null)
gotoTypeDefinition :: Recorder (WithPriority Log) -> IdeState -> TextDocumentPositionParams -> ExceptT PluginError (HandlerM c) (MessageResult Method_TextDocumentTypeDefinition)
gotoImplementation :: Recorder (WithPriority Log) -> IdeState -> TextDocumentPositionParams -> ExceptT PluginError (HandlerM c) (MessageResult Method_TextDocumentImplementation)
documentHighlight  :: Recorder (WithPriority Log) -> IdeState -> TextDocumentPositionParams -> ExceptT PluginError (HandlerM c) ([DocumentHighlight] |? Null)
gotoDefinition = request "Definition" toSomeHaskellInput getDefinition (InR (InR Null)) (InL . Definition . InR . map fst)
gotoTypeDefinition = request "TypeDefinition" (Just . toSomeFileInput) getTypeDefinition (InR (InR Null)) (InL . Definition . InR . map fst)
gotoImplementation = request "Implementation" (Just . toSomeFileInput) getImplementationDefinition (InR (InR Null)) (InL . Definition . InR)
hover          = request "Hover" toSomeHaskellInput getAtPoint (InR Null) foundHover
documentHighlight = request "DocumentHighlight" toSomeHaskellInput highlightAtPoint (InR Null) InL

references :: Recorder (WithPriority Log) -> PluginMethodHandler IdeState Method_TextDocumentReferences
references recorder ide _ (ReferenceParams (TextDocumentIdentifier uri) pos _ _ _) = do
  nfp <- getNormalizedFilePathE uri
  liftIO $ logWith recorder Debug $ LogRequest "References" pos nfp
  case toSomeHaskellInput nfp of
    Nothing -> pure $ InL []
    Just input -> InL <$> (liftIO $ Shake.runAction "references" ide $ refsAtPoint input pos)

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
  -> (NormalizedFilePath -> Maybe input)
  -> (input -> Position -> IdeAction (Maybe a))
  -> b
  -> (a -> b)
  -> Recorder (WithPriority Log)
  -> IdeState
  -> TextDocumentPositionParams
  -> ExceptT PluginError (HandlerM c) b
request label classify getResults notFound found recorder ide (TextDocumentPositionParams (TextDocumentIdentifier uri) pos) = liftIO $ do
    mbResult <- case uriToFilePath' uri of
        Just path -> logAndRunRequest recorder label classify getResults ide pos path
        Nothing   -> pure Nothing
    pure $ maybe notFound found mbResult

logAndRunRequest :: Recorder (WithPriority Log) -> T.Text -> (NormalizedFilePath -> Maybe input) -> (input -> Position -> IdeAction (Maybe a)) -> IdeState -> Position -> String -> IO (Maybe a)
logAndRunRequest recorder label classify getResults ide pos path = do
  let filePath = toNormalizedFilePath' path
  logWith recorder Debug $ LogRequest label pos filePath
  maybe (pure Nothing) (runIdeAction (T.unpack label) (shakeExtras ide) . flip getResults pos) (classify filePath)
