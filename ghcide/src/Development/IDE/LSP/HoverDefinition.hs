-- Copyright (c) 2019 The DAML Authors. All rights reserved.
-- SPDX-License-Identifier: Apache-2.0
{-# LANGUAGE GADTs #-}

-- | Display information on hover.
module Development.IDE.LSP.HoverDefinition
    ( Log(..)
    -- * For haskell-language-server
    , hover
    , hoverRange
    , foundHover
    , gotoDefinition
    , gotoTypeDefinition
    , gotoImplementation
    , documentHighlight
    , references
    , wsSymbols
    ) where

import           Control.Monad.Except           (ExceptT, throwError)
import           Control.Monad.IO.Class
import qualified Data.Aeson                     as A
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
  | LogRequest !T.Text !Position !NormalizedFilePath
  | LogRequestRange !T.Text !Range !NormalizedFilePath
  deriving (Show)

instance Pretty Log where
  pretty = \case
    LogWorkspaceSymbolRequest query -> "Workspace symbols request:" <+> pretty query
    LogRequest label pos nfp ->
      pretty label <+> "request at position" <+> pretty (showPosition pos) <+>
        "in file:" <+> pretty (fromNormalizedFilePath nfp)
    LogRequestRange label (Range start end) nfp ->
      pretty label <+> "request for range" <+>
        pretty (showPosition start) <> "-" <> pretty (showPosition end) <+>
        "in file:" <+> pretty (fromNormalizedFilePath nfp)

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
  nfp <- getNormalizedFilePathE uri
  liftIO $ logWith recorder Debug $ LogRequest "References" pos nfp
  InL <$> (liftIO $ Shake.runAction "references" ide $ refsAtPoint nfp pos)

wsSymbols :: Recorder (WithPriority Log) -> PluginMethodHandler IdeState Method_WorkspaceSymbol
wsSymbols recorder ide _ (WorkspaceSymbolParams _ _ query) = liftIO $ do
  logWith recorder Debug $ LogWorkspaceSymbolRequest query
  runIdeAction "WorkspaceSymbols" (shakeExtras ide) $ InL . fromMaybe [] <$> workspaceSymbols query

foundHover :: (Maybe Range, [T.Text]) -> Hover |? Null
foundHover (mbRange, contents) =
  InL $ Hover (InL $ MarkupContent MarkupKind_Markdown $ T.intercalate sectionSeparator contents) mbRange

-- | Parameters of the @haskell/hoverRange@ request: a text document and the
-- range to hover, typically the current selection.
data HoverRangeParams = HoverRangeParams !TextDocumentIdentifier !Range

instance A.FromJSON HoverRangeParams where
  parseJSON = A.withObject "HoverRangeParams" $ \o ->
    HoverRangeParams <$> o A..: "textDocument" <*> o A..: "range"

-- | Handler for the @haskell/hoverRange@ custom request, an LSP extension
-- modelled after rust-analyzer's \"Hover Range\" extension. It behaves like
-- @textDocument/hover@, but hovers the smallest expression that covers the
-- whole given range (typically the current selection), so clients can show
-- the type of an arbitrary selected expression.
--
-- The response is a standard @Hover | null@ value.
hoverRange :: Recorder (WithPriority Log) -> IdeState -> A.Value -> ExceptT PluginError (HandlerM c) A.Value
hoverRange recorder ide params = case A.fromJSON params of
  A.Error err ->
    throwError $ PluginInvalidParams $ T.pack $ "Invalid hoverRange request parameters: " <> err
  A.Success (HoverRangeParams (TextDocumentIdentifier uri) range) -> do
    nfp <- getNormalizedFilePathE uri
    liftIO $ do
      logWith recorder Debug $ LogRequestRange "HoverRange" range nfp
      mbResult <- runIdeAction "HoverRange" (shakeExtras ide) (getAtPointRange nfp range)
      -- A node with neither identifiers nor types (e.g. a selection spanning
      -- multiple declarations lands on the module root) produces an empty
      -- hover; report null instead so clients don't render an empty popup.
      let mbNonEmpty = case mbResult of
            Just (_, texts) | any (not . T.null) texts -> mbResult
            _                                          -> Nothing
      pure $ A.toJSON $ maybe (InR Null) foundHover mbNonEmpty

-- | Respond to and log a hover or go-to-definition request
request
  :: T.Text
  -> (NormalizedFilePath -> Position -> IdeAction (Maybe a))
  -> b
  -> (a -> b)
  -> Recorder (WithPriority Log)
  -> IdeState
  -> TextDocumentPositionParams
  -> ExceptT PluginError (HandlerM c) b
request label getResults notFound found recorder ide (TextDocumentPositionParams (TextDocumentIdentifier uri) pos) = liftIO $ do
    mbResult <- case uriToFilePath' uri of
        Just path -> logAndRunRequest recorder label getResults ide pos path
        Nothing   -> pure Nothing
    pure $ maybe notFound found mbResult

logAndRunRequest :: Recorder (WithPriority Log) -> T.Text -> (NormalizedFilePath -> Position -> IdeAction b) -> IdeState -> Position -> String -> IO b
logAndRunRequest recorder label getResults ide pos path = do
  let filePath = toNormalizedFilePath' path
  logWith recorder Debug $ LogRequest label pos filePath
  runIdeAction (T.unpack label) (shakeExtras ide) (getResults filePath pos)
