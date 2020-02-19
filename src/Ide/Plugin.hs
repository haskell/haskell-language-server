{-# LANGUAGE DeriveAnyClass    #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Ide.Plugin
    (
      asGhcIdePlugin
    , formatterPlugins
    , hoverPlugins
    , codeActionPlugins
    ) where

import           Control.Lens ( (^.) )
import qualified Data.Aeson as J
import           Data.Either
import qualified Data.Map  as Map
import           Data.Maybe
import qualified Data.Text                     as T
-- import           Development.IDE.Core.FileStore
import           Development.IDE.Core.Rules
import           Development.IDE.LSP.Server
import           Development.IDE.Plugin
import           Development.IDE.Types.Diagnostics as D
-- import           Development.IDE.Types.Location
import           Development.Shake hiding ( Diagnostic, command )
import           GHC.Generics
import           Ide.Compat
import           Ide.Plugin.Config
import           Ide.Plugin.Formatter
import           Ide.Types
import qualified Language.Haskell.LSP.Core as LSP
import           Language.Haskell.LSP.Messages
import           Language.Haskell.LSP.Types
import qualified Language.Haskell.LSP.Types.Capabilities as C
import           Language.Haskell.LSP.Types.Lens as L hiding (formatting, rangeFormatting)
import           Text.Regex.TDFA.Text()

-- ---------------------------------------------------------------------

-- | Map a set of plugins to the underlying ghcide engine.  Main point is
-- IdePlugins are arranged by kind of operation, 'Plugin' is arranged by message
-- category ('Notifaction', 'Request' etc).
asGhcIdePlugin :: IdePlugins -> Plugin Config
asGhcIdePlugin _ = Plugin mempty mempty

-- First strp will be to bring the machinery from Ide.Plugin.Formatter over.

-- ---------------------------------------------------------------------

codeActionPlugins :: [(T.Text, CodeActionProvider)] -> Plugin Config
codeActionPlugins cas = Plugin codeActionRules (codeActionHandlers cas)

codeActionRules :: Rules ()
codeActionRules = mempty

codeActionHandlers :: [(T.Text, CodeActionProvider)] -> PartialHandlers Config
codeActionHandlers cas = PartialHandlers $ \WithMessage{..} x -> return x
    { LSP.codeActionHandler
        = withResponse RspCodeAction (makeCodeAction cas)
    }

makeCodeAction :: [(T.Text, CodeActionProvider)]
      -> LSP.LspFuncs Config -> IdeState
      -> CodeActionParams
      -> IO (Either ResponseError (List CAResult))
makeCodeAction cas lf ideState (CodeActionParams docId range context _) = do
    let caps = LSP.clientCapabilities lf
        unL (List ls) = ls
    r <- mapM (\(pid,provider) -> provider ideState (PluginId pid) docId range context) cas
    let actions = filter wasRequested . concat $ map unL $ rights r
    res <- send caps actions
    return $ Right res
  where
    wasRequested :: CAResult -> Bool
    wasRequested (CACommand _) = True
    wasRequested (CACodeAction ca)
      | Nothing <- only context = True
      | Just (List allowed) <- only context
      , Just caKind <- ca ^. kind = caKind `elem` allowed
      | otherwise = False

    wrapCodeAction :: C.ClientCapabilities -> CAResult -> IO (Maybe CAResult)
    wrapCodeAction _ (CACommand cmd) = return $ Just (CACommand cmd)
    wrapCodeAction caps (CACodeAction action) = do

      let (C.ClientCapabilities _ textDocCaps _ _) = caps
      let literalSupport = textDocCaps >>= C._codeAction >>= C._codeActionLiteralSupport

      case literalSupport of
        Nothing -> do
            let cmdParams = [J.toJSON (FallbackCodeActionParams (action ^. edit) (action ^. command))]
            cmd <- mkLspCommand "hie" "fallbackCodeAction" (action ^. title) (Just cmdParams)
            return $ Just (CACommand cmd)
        Just _ -> return $ Just (CACodeAction action)

    send :: C.ClientCapabilities -> [CAResult] -> IO (List CAResult)
    send caps codeActions = List . catMaybes <$> mapM (wrapCodeAction caps) codeActions

data FallbackCodeActionParams =
  FallbackCodeActionParams
    { fallbackWorkspaceEdit :: Maybe WorkspaceEdit
    , fallbackCommand       :: Maybe Command
    }
  deriving (Generic, J.ToJSON, J.FromJSON)

mkLspCommand :: PluginId -> CommandId -> T.Text -> Maybe [J.Value] -> IO Command
mkLspCommand plid cn title args' = do
  cmdId <- mkLspCmdId plid cn
  let args = List <$> args'
  return $ Command title cmdId args

mkLspCmdId :: PluginId -> CommandId -> IO T.Text
mkLspCmdId (PluginId plid) (CommandId cid) = do
  pid <- T.pack . show <$> getProcessID
  return $ pid <> ":" <> plid <> ":" <> cid
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
makeHover hps _lf ideState params
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
