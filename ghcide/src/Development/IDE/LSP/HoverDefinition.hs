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
import           Development.IDE.Core.Actions
import           Development.IDE.Core.Rules
import           Development.IDE.Core.Shake
import           Development.IDE.Types.Location
import           Development.IDE.Types.Logger
import qualified Language.LSP.Server            as LSP
import           Language.LSP.Types

import qualified Data.Text                      as T

gotoDefinition :: IdeState -> TextDocumentPositionParams -> LSP.LspM c (Either ResponseError (ResponseResult TextDocumentDefinition))
hover          :: IdeState -> TextDocumentPositionParams -> LSP.LspM c (Either ResponseError (Maybe Hover))
gotoTypeDefinition :: IdeState -> TextDocumentPositionParams -> LSP.LspM c (Either ResponseError (ResponseResult TextDocumentTypeDefinition))
documentHighlight :: IdeState -> TextDocumentPositionParams -> LSP.LspM c (Either ResponseError (List DocumentHighlight))
gotoDefinition = request "Definition" getDefinition (InR $ InL $ List []) (InR . InL . List)
gotoTypeDefinition = request "TypeDefinition" getTypeDefinition (InR $ InL $ List []) (InR . InL . List)
hover          = request "Hover"      getAtPoint     Nothing      foundHover
documentHighlight = request "DocumentHighlight" highlightAtPoint (List []) List

references :: IdeState -> ReferenceParams -> LSP.LspM c (Either ResponseError (List Location))
references ide (ReferenceParams (TextDocumentIdentifier uri) pos _ _ _) = liftIO $
  case uriToFilePath' uri of
    Just path -> do
      let filePath = toNormalizedFilePath' path
      logDebug (ideLogger ide) $
        "References request at position " <> T.pack (showPosition pos) <>
        " in file: " <> T.pack path
      Right . List <$> (runAction "references" ide $ refsAtPoint filePath pos)
    Nothing -> pure $ Left $ ResponseError InvalidParams ("Invalid URI " <> T.pack (show uri)) Nothing

wsSymbols :: IdeState -> WorkspaceSymbolParams -> LSP.LspM c (Either ResponseError (List SymbolInformation))
wsSymbols ide (WorkspaceSymbolParams _ _ query) = liftIO $ do
  logDebug (ideLogger ide) $ "Workspace symbols request: " <> query
  runIdeAction "WorkspaceSymbols" (shakeExtras ide) $ Right . maybe (List []) List <$> workspaceSymbols query

foundHover :: (Maybe Range, [T.Text]) -> Maybe Hover
foundHover (mbRange, contents) =
  Just $ Hover (HoverContents $ MarkupContent MkMarkdown $ T.intercalate sectionSeparator contents) mbRange

-- | Respond to and log a hover or go-to-definition request
request
  :: T.Text
  -> (NormalizedFilePath -> Position -> IdeAction (Maybe a))
  -> b
  -> (a -> b)
  -> IdeState
  -> TextDocumentPositionParams
  -> LSP.LspM c (Either ResponseError b)
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
