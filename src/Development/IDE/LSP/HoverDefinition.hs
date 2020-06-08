-- Copyright (c) 2019 The DAML Authors. All rights reserved.
-- SPDX-License-Identifier: Apache-2.0


-- | Display information on hover.
module Development.IDE.LSP.HoverDefinition
    ( setHandlersHover
    , setHandlersDefinition
    -- * For haskell-language-server
    , hover
    , gotoDefinition
    ) where

import           Development.IDE.Core.Rules
import           Development.IDE.Core.Service
import           Development.IDE.LSP.Server
import           Development.IDE.Types.Location
import           Development.IDE.Types.Logger
import           Development.Shake
import qualified Language.Haskell.LSP.Core       as LSP
import           Language.Haskell.LSP.Messages
import           Language.Haskell.LSP.Types

import qualified Data.Text as T
import System.Time.Extra (showDuration, duration)

gotoDefinition :: IdeState -> TextDocumentPositionParams -> IO (Either ResponseError LocationResponseParams)
hover          :: IdeState -> TextDocumentPositionParams -> IO (Either ResponseError (Maybe Hover))
gotoDefinition = request "Definition" getDefinition (MultiLoc []) SingleLoc
hover          = request "Hover"      getAtPoint     Nothing      foundHover

foundHover :: (Maybe Range, [T.Text]) -> Maybe Hover
foundHover (mbRange, contents) =
  Just $ Hover (HoverContents $ MarkupContent MkMarkdown $ T.intercalate sectionSeparator contents) mbRange

setHandlersDefinition, setHandlersHover :: PartialHandlers c
setHandlersDefinition = PartialHandlers $ \WithMessage{..} x ->
  return x{LSP.definitionHandler = withResponse RspDefinition $ const gotoDefinition}
setHandlersHover      = PartialHandlers $ \WithMessage{..} x ->
  return x{LSP.hoverHandler      = withResponse RspHover      $ const hover}

-- | Respond to and log a hover or go-to-definition request
request
  :: T.Text
  -> (NormalizedFilePath -> Position -> Action (Maybe a))
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

logAndRunRequest :: T.Text -> (NormalizedFilePath -> Position -> Action b) -> IdeState -> Position -> String -> IO b
logAndRunRequest label getResults ide pos path = do
  let filePath = toNormalizedFilePath' path
  (t, res) <- duration $ runAction ide $ getResults filePath pos
  logDebug (ideLogger ide) $
    label <> " request at position " <> T.pack (showPosition pos) <>
    " in file: " <> T.pack path <> " took " <> T.pack (showDuration t)
  return res
