{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Ide.Plugin
    (
      asGhcIdePlugin
    , formatterPlugins
    , hoverPlugins
    ) where

import           Control.Lens ( (^.) )
import           Data.Either
import           Data.Maybe
import qualified Data.Map  as Map
import qualified Data.Text                     as T
import           Development.IDE.Core.FileStore
import           Development.IDE.Core.Rules
import           Development.IDE.LSP.Server
import           Development.IDE.Plugin
import           Development.IDE.Types.Diagnostics as D
import           Development.IDE.Types.Location
import           Development.Shake hiding ( Diagnostic )
import           Ide.Plugin.Config
import           Ide.Plugin.Formatter
import           Ide.Types
import qualified Language.Haskell.LSP.Core as LSP
import           Language.Haskell.LSP.Messages
import           Text.Regex.TDFA.Text()

import qualified Language.Haskell.LSP.Core as LSP
import           Language.Haskell.LSP.Messages
import           Language.Haskell.LSP.Types
import           Language.Haskell.LSP.Types.Lens as L hiding (formatting, rangeFormatting)

-- ---------------------------------------------------------------------

-- | Map a set of plugins to the underlying ghcide engine.  Main point is
-- IdePlugins are arranged by kind of operation, 'Plugin' is arranged by message
-- category ('Notifaction', 'Request' etc).
asGhcIdePlugin :: IdePlugins -> Plugin Config
asGhcIdePlugin _ = Plugin mempty mempty

-- First strp will be to bring the machinery from Ide.Plugin.Formatter over.

-- ---------------------------------------------------------------------

hoverPlugins :: [HoverProvider] -> Plugin Config
hoverPlugins hs = Plugin hoverRules (hoverHandlers hs)

hoverRules :: Rules ()
hoverRules = mempty

hoverHandlers :: [HoverProvider] -> PartialHandlers Config
hoverHandlers hps = PartialHandlers $ \WithMessage{..} x ->
  return x{LSP.hoverHandler = withResponse RspHover (makeHover hps)}

makeHover :: [HoverProvider]
      -> LSP.LspFuncs Config -> IdeState
      -> TextDocumentPositionParams
      -> IO (Either ResponseError (Maybe Hover))
makeHover hps lf ideState params
  = do
      mhs <- mapM (\p -> p ideState params) hps
      -- TODO: We should support ServerCapabilities and declare that
      -- we don't support hover requests during initialization if we
      -- don't have any hover providers
      -- TODO: maybe only have provider give MarkedString and
      -- work out range here?
      let hs = catMaybes (rights mhs)
          r = listToMaybe $ mapMaybe (^. range) hs
          h = case mconcat ((map (^. contents) hs) :: [HoverContents]) of
            HoverContentsMS (List []) -> Nothing
            hh                        -> Just $ Hover hh r
      return $ Right h

-- ---------------------------------------------------------------------
-- ---------------------------------------------------------------------

formatterPlugins :: [(T.Text, FormattingProvider IO)] -> Plugin Config
formatterPlugins providers
    = Plugin formatterRules
             (formatterHandlers (Map.fromList (("none",noneProvider):providers)))

formatterRules :: Rules ()
formatterRules = mempty

formatterHandlers :: Map.Map T.Text (FormattingProvider IO) -> PartialHandlers Config
formatterHandlers providers = PartialHandlers $ \WithMessage{..} x -> return x
    { LSP.documentFormattingHandler
        = withResponse RspDocumentFormatting (formatting providers)
    , LSP.documentRangeFormattingHandler
        = withResponse RspDocumentRangeFormatting (rangeFormatting providers)
    }

-- ---------------------------------------------------------------------
