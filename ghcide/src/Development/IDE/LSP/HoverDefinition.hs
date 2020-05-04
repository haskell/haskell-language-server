-- Copyright (c) 2019 The DAML Authors. All rights reserved.
-- SPDX-License-Identifier: Apache-2.0


-- | Display information on hover.
module Development.IDE.LSP.HoverDefinition
    ( setHandlersDefinition
    , setHandlersTypeDefinition
    , setHandlersDocHighlight
    , setHandlersReferences
    , setHandlersWsSymbols
    -- * For haskell-language-server
    , hover
    , gotoDefinition
    , gotoTypeDefinition
    ) where

import           Development.IDE.Core.Rules
import           Development.IDE.Core.Shake
import           Development.IDE.LSP.Server
import           Development.IDE.Types.Location
import           Development.IDE.Types.Logger
import qualified Language.Haskell.LSP.Core       as LSP
import           Language.Haskell.LSP.Messages
import           Language.Haskell.LSP.Types

import qualified Data.Text as T

gotoDefinition :: IdeState -> TextDocumentPositionParams -> IO (Either ResponseError LocationResponseParams)
hover          :: IdeState -> TextDocumentPositionParams -> IO (Either ResponseError (Maybe Hover))
gotoTypeDefinition :: IdeState -> TextDocumentPositionParams -> IO (Either ResponseError LocationResponseParams)
documentHighlight :: IdeState -> TextDocumentPositionParams -> IO (Either ResponseError (List DocumentHighlight))
gotoDefinition = request "Definition" getDefinition (MultiLoc []) MultiLoc
gotoTypeDefinition = request "TypeDefinition" getTypeDefinition (MultiLoc []) MultiLoc
hover          = request "Hover"      getAtPoint     Nothing      foundHover
documentHighlight = request "DocumentHighlight" highlightAtPoint (List []) List

references :: IdeState -> ReferenceParams -> IO (Either ResponseError (List Location))
references ide (ReferenceParams (TextDocumentIdentifier uri) pos _ _) =
  case uriToFilePath' uri of
    Just path -> do
      let filePath = toNormalizedFilePath' path
      logDebug (ideLogger ide) $
        "References request at position " <> T.pack (showPosition pos) <>
        " in file: " <> T.pack path
      Right . List <$> (runAction "references" ide $ refsAtPoint filePath pos)
    Nothing -> pure $ Left $ ResponseError InvalidParams ("Invalid URI " <> T.pack (show uri)) Nothing

wsSymbols :: IdeState -> WorkspaceSymbolParams -> IO (Either ResponseError (List SymbolInformation))
wsSymbols ide (WorkspaceSymbolParams query _) = do
  logDebug (ideLogger ide) $ "Workspace symbols request: " <> query
  runIdeAction "WorkspaceSymbols" (shakeExtras ide) $ Right . maybe (List []) List <$> workspaceSymbols query

foundHover :: (Maybe Range, [T.Text]) -> Maybe Hover
foundHover (mbRange, contents) =
  Just $ Hover (HoverContents $ MarkupContent MkMarkdown $ T.intercalate sectionSeparator contents) mbRange

setHandlersDefinition, setHandlersTypeDefinition, setHandlersDocHighlight,
  setHandlersReferences, setHandlersWsSymbols :: PartialHandlers c
setHandlersDefinition = PartialHandlers $ \WithMessage{..} x ->
  return x{LSP.definitionHandler = withResponse RspDefinition $ const gotoDefinition}
setHandlersTypeDefinition = PartialHandlers $ \WithMessage{..} x ->
  return x {LSP.typeDefinitionHandler = withResponse RspDefinition $ const gotoTypeDefinition}
setHandlersDocHighlight = PartialHandlers $ \WithMessage{..} x ->
  return x{LSP.documentHighlightHandler = withResponse RspDocumentHighlights $ const documentHighlight}
setHandlersReferences = PartialHandlers $ \WithMessage{..} x ->
  return x{LSP.referencesHandler = withResponse RspFindReferences $ const references}
setHandlersWsSymbols = PartialHandlers $ \WithMessage{..} x ->
  return x{LSP.workspaceSymbolHandler = withResponse RspWorkspaceSymbols $ const wsSymbols}

-- | Respond to and log a hover or go-to-definition request
request
  :: T.Text
  -> (NormalizedFilePath -> Position -> IdeAction (Maybe a))
  -> b
  -> (a -> b)
  -> IdeState
  -> TextDocumentPositionParams
  -> IO (Either ResponseError b)
request label getResults notFound found ide (TextDocumentPositionParams (TextDocumentIdentifier uri) pos _) = do
    mbResult <- case uriToFilePath' uri of
        Just path -> logAndRunRequest label getResults ide pos path
        Nothing   -> pure Nothing
    pure $ Right $ maybe notFound found mbResult

logAndRunRequest :: T.Text -> (NormalizedFilePath -> Position -> IdeAction b) -> IdeState -> Position -> String -> IO b
logAndRunRequest label getResults ide pos path = do
  let filePath = toNormalizedFilePath' path
  logInfo (ideLogger ide) $
    label <> " request at position " <> T.pack (showPosition pos) <>
    " in file: " <> T.pack path
  runIdeAction (T.unpack label) (shakeExtras ide) (getResults filePath pos)
