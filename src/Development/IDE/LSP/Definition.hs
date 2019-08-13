-- Copyright (c) 2019 The DAML Authors. All rights reserved.
-- SPDX-License-Identifier: Apache-2.0


-- | Go to the definition of a variable.
module Development.IDE.LSP.Definition
    ( setHandlersDefinition
    ) where

import           Language.Haskell.LSP.Types
import Development.IDE.Types.Location

import Development.IDE.Types.Logger
import Development.IDE.Core.Rules
import Development.IDE.Core.Service
import Development.IDE.LSP.Server
import qualified Language.Haskell.LSP.Core as LSP
import Language.Haskell.LSP.Messages

import qualified Data.Text as T

-- | Go to the definition of a variable.
gotoDefinition
    :: IdeState
    -> TextDocumentPositionParams
    -> IO LocationResponseParams
gotoDefinition ide (TextDocumentPositionParams (TextDocumentIdentifier uri) pos) = do
    mbResult <- case uriToFilePath' uri of
        Just path -> do
            logInfo (ideLogger ide) $
                "Definition request at position " <> T.pack (showPosition pos) <>
                " in file: " <> T.pack path
            runAction ide $ getDefinition (toNormalizedFilePath path) pos
        Nothing -> pure Nothing
    pure $ case mbResult of
        Nothing -> MultiLoc []
        Just loc -> SingleLoc loc


setHandlersDefinition :: PartialHandlers
setHandlersDefinition = PartialHandlers $ \WithMessage{..} x -> return x{
    LSP.definitionHandler = withResponse RspDefinition $ const gotoDefinition
    }
