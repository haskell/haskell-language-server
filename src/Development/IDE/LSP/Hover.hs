-- Copyright (c) 2019 The DAML Authors. All rights reserved.
-- SPDX-License-Identifier: Apache-2.0


-- | Display information on hover.
module Development.IDE.LSP.Hover
    ( setHandlersHover
    ) where

import Language.Haskell.LSP.Types
import Development.IDE.Types.Location
import Development.IDE.Core.Service
import Development.IDE.LSP.Server
import Development.IDE.Types.Logger
import qualified Language.Haskell.LSP.Core as LSP
import Language.Haskell.LSP.Messages

import qualified Data.Text as T

import Development.IDE.Core.Rules

-- | Display information on hover.
onHover
    :: IdeState
    -> TextDocumentPositionParams
    -> IO (Maybe Hover)
onHover ide (TextDocumentPositionParams (TextDocumentIdentifier uri) pos) = do
    mbResult <- case uriToFilePath' uri of
        Just (toNormalizedFilePath -> filePath) -> do
          logInfo (ideLogger ide) $
              "Hover request at position " <> T.pack (showPosition pos) <>
              " in file: " <> T.pack (fromNormalizedFilePath filePath)
          runAction ide $ getAtPoint filePath pos
        Nothing       -> pure Nothing

    case mbResult of
        Just (mbRange, contents) ->
            pure $ Just $ Hover
                        (HoverContents $ MarkupContent MkMarkdown $ T.intercalate sectionSeparator contents)
                        mbRange

        Nothing -> pure Nothing

setHandlersHover :: PartialHandlers
setHandlersHover = PartialHandlers $ \WithMessage{..} x -> return x{
    LSP.hoverHandler = withResponse RspHover $ const onHover
    }
