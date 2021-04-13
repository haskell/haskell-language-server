{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings     #-}

-- TODO: explicit export list
module Ide.Plugin.RenameSymbol where

import           Data.Function
import           Data.HashMap.Internal               hiding (map)
import           Data.List
import qualified Data.Text                           as T
import           Development.IDE                     hiding (pluginHandlers)
import           Development.IDE.LSP.HoverDefinition
import           Ide.Types
import           Language.LSP.Server
import           Language.LSP.Types

descriptor :: PluginId -> PluginDescriptor IdeState
descriptor pluginId = (defaultPluginDescriptor pluginId) {
    pluginHandlers = mconcat
        [ mkPluginHandler STextDocumentRename renameProvider
        -- , mkPluginHandler STextDocumentCodeLens codeLensProvider
        ]
}

renameProvider :: PluginMethodHandler IdeState TextDocumentRename
renameProvider
    state
    _pluginId
    (RenameParams tdi pos _progToken name)
        = do
            locs <- getTextEdits state tdi pos name
            return $ Right (WorkspaceEdit {
                _changes=Just (fromList locs),
                _documentChanges=Nothing,
                _changeAnnotations=Nothing
            })

getTextEdits :: IdeState
    -> TextDocumentIdentifier
    -> Position
    -> T.Text
    -> LspT c IO [(Uri, List TextEdit)]
getTextEdits state tdi pos name
    = do
        mbLocs <- references state $ ReferenceParams tdi pos Nothing Nothing (ReferenceContext False)
        case mbLocs of
            Right (List locs)
                -> return
                    $ map ((\(uri':_,tes) -> (uri', List tes)) . unzip)
                    $ groupBy ((==) `on` fst) [(uri, TextEdit range name) | Location uri range <- locs]
            _   -> return []

-- helloWorldLens :: CodeLens
-- helloWorldLens = CodeLens
--     { _range = Range (Position 0 0) (Position 0 1)
--     , _command = Just $ Command "Hello, World!" "hello-world-command-id" Nothing
--     , _xdata = Nothing
--     }

-- codeLensProvider :: PluginMethodHandler IdeState TextDocumentCodeLens
-- codeLensProvider _ _ _
--     = do return $ Right $ List [helloWorldLens]
