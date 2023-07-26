-- Copyright (c) 2019 The DAML Authors. All rights reserved.
-- SPDX-License-Identifier: Apache-2.0
{-# LANGUAGE GADTs      #-}
{-# LANGUAGE RankNTypes #-}

-- | Display information on hover.
module Development.IDE.LSP.HoverDefinition
    (
    -- * For haskell-language-server
    hover
    , gotoDefinition
    , gotoTypeDefinition
    , documentHighlight
    , references
    , wsSymbols
    ) where

import           Control.Monad.IO.Class
import           Data.Maybe                     (fromMaybe)
import           Development.IDE.Core.Actions
import           Development.IDE.Core.Rules
import           Development.IDE.Core.Shake
import           Development.IDE.Types.Location
import           Ide.Logger
import           Ide.Plugin.Error
import           Language.LSP.Protocol.Message
import           Language.LSP.Protocol.Types
import qualified Language.LSP.Server            as LSP

import qualified Data.Text                      as T

gotoDefinition :: IdeState -> TextDocumentPositionParams -> LSP.LspM c (Either PluginError (MessageResult Method_TextDocumentDefinition))
hover          :: IdeState -> TextDocumentPositionParams -> LSP.LspM c (Either PluginError (Hover |? Null))
gotoTypeDefinition :: IdeState -> TextDocumentPositionParams -> LSP.LspM c (Either PluginError (MessageResult Method_TextDocumentTypeDefinition))
documentHighlight :: IdeState -> TextDocumentPositionParams -> LSP.LspM c (Either PluginError ([DocumentHighlight] |? Null))
gotoDefinition = request "Definition" getDefinition (InR $ InR Null) (InL . Definition. InR)
gotoTypeDefinition = request "TypeDefinition" getTypeDefinition (InR $ InR Null) (InL . Definition. InR)
hover          = request "Hover"      getAtPoint     (InR Null)     foundHover
documentHighlight = request "DocumentHighlight" highlightAtPoint (InR Null) InL

references :: IdeState -> ReferenceParams -> LSP.LspM c (Either PluginError ([Location] |? Null))
references ide (ReferenceParams (TextDocumentIdentifier uri) pos _ _ _) = runExceptT $ do
  nfp <- getNormalizedFilePathE uri
  liftIO $ logDebug (ideLogger ide) $
        "References request at position " <> T.pack (showPosition pos) <>
        " in file: " <> T.pack (show nfp)
  InL <$> (liftIO $ runAction "references" ide $ refsAtPoint nfp pos)

wsSymbols :: IdeState -> WorkspaceSymbolParams -> LSP.LspM c (Either PluginError [SymbolInformation])
wsSymbols ide (WorkspaceSymbolParams _ _ query) = liftIO $ do
  logDebug (ideLogger ide) $ "Workspace symbols request: " <> query
  runIdeAction "WorkspaceSymbols" (shakeExtras ide) $ Right . fromMaybe [] <$> workspaceSymbols query

foundHover :: (Maybe Range, [T.Text]) -> Hover |? Null
foundHover (mbRange, contents) =
  InL $ Hover (InL $ MarkupContent MarkupKind_Markdown $ T.intercalate sectionSeparator contents) mbRange

-- | Respond to and log a hover or go-to-definition request
request
  :: T.Text
  -> (NormalizedFilePath -> Position -> IdeAction (Maybe a))
  -> b
  -> (a -> b)
  -> IdeState
  -> TextDocumentPositionParams
  -> LSP.LspM c (Either PluginError b)
request label getResults notFound found ide (TextDocumentPositionParams (TextDocumentIdentifier uri) pos) = liftIO $ do
    mbResult <- case uriToFilePath' uri of
        Just path -> logAndRunRequest label getResults ide pos path
        Nothing   -> pure Nothing
    pure $ Right $ maybe notFound found mbResult

logAndRunRequest :: T.Text -> (NormalizedFilePath -> Position -> IdeAction b) -> IdeState -> Position -> String -> IO b
logAndRunRequest label getResults ide pos path = do
  let filePath = toNormalizedFilePath' path
  logDebug (ideLogger ide) $
    label <> " request at position " <> T.pack (showPosition pos) <>
    " in file: " <> T.pack path
  runIdeAction (T.unpack label) (shakeExtras ide) (getResults filePath pos)
