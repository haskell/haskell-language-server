{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE DeriveAnyClass    #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Ide.Plugin
    (
      asGhcIdePlugin
    , pluginDescToIdePlugins
    , formatterPlugins
    , hoverPlugins
    , codeActionPlugins
    , executeCommandPlugins
    , mkLspCommand
    , allLspCmdIds
    , getPid
    ) where

import           Control.Lens ( (^.) )
import           Control.Monad
import qualified Data.Aeson as J
import           Data.Either
import qualified Data.List                     as List
import qualified Data.Map  as Map
import           Data.Maybe
import qualified Data.Text                     as T
import           Development.IDE.Core.Rules
import           Development.IDE.LSP.Server
import           Development.IDE.Plugin hiding (pluginCommands)
import           Development.IDE.Types.Diagnostics as D
import           Development.Shake hiding ( Diagnostic, command )
import           GHC.Generics
import           Ide.Compat
import           Ide.Plugin.Config
import           Ide.Plugin.Formatter
import           Ide.Types
import qualified Language.Haskell.LSP.Core as LSP
import           Language.Haskell.LSP.Messages
import           Language.Haskell.LSP.Types
import qualified Language.Haskell.LSP.Types              as J
import qualified Language.Haskell.LSP.Types.Capabilities as C
import           Language.Haskell.LSP.Types.Lens as L hiding (formatting, rangeFormatting)
import           Text.Regex.TDFA.Text()

-- ---------------------------------------------------------------------

-- | Map a set of plugins to the underlying ghcide engine.  Main point is
-- IdePlugins are arranged by kind of operation, 'Plugin' is arranged by message
-- category ('Notifaction', 'Request' etc).
asGhcIdePlugin :: IdePlugins -> Plugin Config
asGhcIdePlugin mp =
    mkPlugin executeCommandPlugins (Just . pluginCommands) <>
    mkPlugin codeActionPlugins     pluginCodeActionProvider <>
    -- diagnostics from pluginDiagnosticProvider
    mkPlugin hoverPlugins          pluginHoverProvider <>
    -- symbols via pluginSymbolProvider
    mkPlugin formatterPlugins      pluginFormattingProvider
    -- completions
    where
        justs (p, Just x)  = [(p, x)]
        justs (_, Nothing) = []

        ls = Map.toList (ipMap mp)

        mkPlugin :: ([(PluginId, b)] -> t) -> (PluginDescriptor -> Maybe b) -> t
        mkPlugin maker selector
            = maker $ concatMap (\(pid, p) -> justs (pid, selector p)) ls


pluginDescToIdePlugins :: [PluginDescriptor] -> IdePlugins
pluginDescToIdePlugins plugins = IdePlugins $ Map.fromList $ map (\p -> (pluginId p, p)) plugins

-- ---------------------------------------------------------------------

codeActionPlugins :: [(PluginId, CodeActionProvider)] -> Plugin Config
codeActionPlugins cas = Plugin mempty codeActionRules (codeActionHandlers cas)

codeActionRules :: Rules ()
codeActionRules = mempty

codeActionHandlers :: [(PluginId, CodeActionProvider)] -> PartialHandlers Config
codeActionHandlers cas = PartialHandlers $ \WithMessage{..} x -> return x
    { LSP.codeActionHandler
        = withResponse RspCodeAction (makeCodeAction cas)
    }

makeCodeAction :: [(PluginId, CodeActionProvider)]
      -> LSP.LspFuncs Config -> IdeState
      -> CodeActionParams
      -> IO (Either ResponseError (List CAResult))
makeCodeAction cas lf ideState (CodeActionParams docId range context _) = do
    let caps = LSP.clientCapabilities lf
        unL (List ls) = ls
    r <- mapM (\(pid,provider) -> provider ideState pid docId range context) cas
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

-- -----------------------------------------------------------

executeCommandPlugins :: [(PluginId, [PluginCommand])] -> Plugin Config
executeCommandPlugins ecs = Plugin mempty mempty (executeCommandHandlers ecs)

executeCommandHandlers :: [(PluginId, [PluginCommand])] -> PartialHandlers Config
executeCommandHandlers ecs = PartialHandlers $ \WithMessage{..} x -> return x{
    LSP.executeCommandHandler = withResponseAndRequest RspExecuteCommand ReqApplyWorkspaceEdit (makeExecuteCommands ecs)
    }

-- type ExecuteCommandProvider = IdeState
--                             -> ExecuteCommandParams
--                             -> IO (Either ResponseError Value, Maybe (ServerMethod, ApplyWorkspaceEditParams))
makeExecuteCommands :: [(PluginId, [PluginCommand])] -> LSP.LspFuncs Config -> ExecuteCommandProvider
makeExecuteCommands ecs _lf _params = do
  let
      pluginMap = Map.fromList ecs
      parseCmdId :: T.Text -> Maybe (PluginId, CommandId)
      parseCmdId x = case T.splitOn ":" x of
        [plugin, command] -> Just (PluginId plugin, CommandId command)
        [_, plugin, command] -> Just (PluginId plugin, CommandId command)
        _ -> Nothing

      execCmd :: ExecuteCommandParams -> IO (Either ResponseError J.Value, Maybe (ServerMethod, ApplyWorkspaceEditParams))
      execCmd (ExecuteCommandParams cmdId args _) = do
        -- The parameters to the HIE command are always the first element
        let cmdParams :: J.Value
            cmdParams = case args of
             Just (J.List (x:_)) -> x
             _ -> J.Null

        case parseCmdId cmdId of
          -- Shortcut for immediately applying a applyWorkspaceEdit as a fallback for v3.8 code actions
          Just ("hie", "fallbackCodeAction") ->
            case J.fromJSON cmdParams of
              J.Success (FallbackCodeActionParams mEdit mCmd) -> do

                -- Send off the workspace request if it has one
                forM_ mEdit $ \edit -> do
                  let eParams = J.ApplyWorkspaceEditParams edit
                  -- TODO: Use lspfuncs to send an applyedit message. Or change
                  -- the API to allow a list of messages to be returned.
                  return (Right J.Null, Just(J.WorkspaceApplyEdit, eParams))

                case mCmd of
                  -- If we have a command, continue to execute it
                  Just (J.Command _ innerCmdId innerArgs)
                      -> execCmd (ExecuteCommandParams innerCmdId innerArgs Nothing)
                  Nothing -> return (Right J.Null, Nothing)

              J.Error _str -> return (Right J.Null, Nothing)
              -- Couldn't parse the fallback command params
              -- _ -> liftIO $
              --   LSP.sendErrorResponseS (LSP.sendFunc lf)
              --                           (J.responseId (req ^. J.id))
              --                           J.InvalidParams
              --                           "Invalid fallbackCodeAction params"

          -- Just an ordinary HIE command
          Just (plugin, cmd) -> runPluginCommand pluginMap plugin cmd cmdParams

          -- Couldn't parse the command identifier
          _ -> return (Left $ ResponseError InvalidParams "Invalid command identifier" Nothing, Nothing)

  execCmd

{-
       ReqExecuteCommand req -> do
          liftIO $ U.logs $ "reactor:got ExecuteCommandRequest:" ++ show req
          lf <- asks lspFuncs

          let params = req ^. J.params

              parseCmdId :: T.Text -> Maybe (PluginId, CommandId)
              parseCmdId x = case T.splitOn ":" x of
                [plugin, command] -> Just (PluginId plugin, CommandId command)
                [_, plugin, command] -> Just (PluginId plugin, CommandId command)
                _ -> Nothing

              callback obj = do
                liftIO $ U.logs $ "ExecuteCommand response got:r=" ++ show obj
                case fromDynJSON obj :: Maybe J.WorkspaceEdit of
                  Just v -> do
                    lid <- nextLspReqId
                    reactorSend $ RspExecuteCommand $ Core.makeResponseMessage req (A.Object mempty)
                    let msg = fmServerApplyWorkspaceEditRequest lid $ J.ApplyWorkspaceEditParams v
                    liftIO $ U.logs $ "ExecuteCommand sending edit: " ++ show msg
                    reactorSend $ ReqApplyWorkspaceEdit msg
                  Nothing -> reactorSend $ RspExecuteCommand $ Core.makeResponseMessage req $ dynToJSON obj

              execCmd cmdId args = do
                -- The parameters to the HIE command are always the first element
                let cmdParams = case args of
                     Just (J.List (x:_)) -> x
                     _ -> A.Null

                case parseCmdId cmdId of
                  -- Shortcut for immediately applying a applyWorkspaceEdit as a fallback for v3.8 code actions
                  Just ("hie", "fallbackCodeAction") -> do
                    case A.fromJSON cmdParams of
                      A.Success (FallbackCodeActionParams mEdit mCmd) -> do

                        -- Send off the workspace request if it has one
                        forM_ mEdit $ \edit -> do
                          lid <- nextLspReqId
                          let eParams = J.ApplyWorkspaceEditParams edit
                              eReq = fmServerApplyWorkspaceEditRequest lid eParams
                          reactorSend $ ReqApplyWorkspaceEdit eReq

                        case mCmd of
                          -- If we have a command, continue to execute it
                          Just (J.Command _ innerCmdId innerArgs) -> execCmd innerCmdId innerArgs

                          -- Otherwise we need to send back a response oureslves
                          Nothing -> reactorSend $ RspExecuteCommand $ Core.makeResponseMessage req (A.Object mempty)

                      -- Couldn't parse the fallback command params
                      _ -> liftIO $
                        Core.sendErrorResponseS (Core.sendFunc lf)
                                                (J.responseId (req ^. J.id))
                                                J.InvalidParams
                                                "Invalid fallbackCodeAction params"
                  -- Just an ordinary HIE command
                  Just (plugin, cmd) ->
                    let preq = GReq tn "plugin" Nothing Nothing (Just $ req ^. J.id) callback (toDynJSON (Nothing :: Maybe J.WorkspaceEdit))
                               $ runPluginCommand plugin cmd cmdParams
                    in makeRequest preq

                  -- Couldn't parse the command identifier
                  _ -> liftIO $
                    Core.sendErrorResponseS (Core.sendFunc lf)
                                            (J.responseId (req ^. J.id))
                                            J.InvalidParams
                                            "Invalid command identifier"

          execCmd (params ^. J.command) (params ^. J.arguments)
-}
-- | Runs a plugin command given a PluginId, CommandId and
-- arguments in the form of a JSON object.
runPluginCommand :: Map.Map PluginId [PluginCommand] -> PluginId -> CommandId -> J.Value
                  -> IO (Either ResponseError J.Value,
                        Maybe (ServerMethod, ApplyWorkspaceEditParams))
runPluginCommand m p@(PluginId p') com@(CommandId com') arg =
  case Map.lookup p m of
    Nothing -> return
      (Left $ ResponseError InvalidRequest ("Plugin " <> p' <> " doesn't exist") Nothing, Nothing)
    Just xs -> case List.find ((com ==) . commandId) xs of
      Nothing -> return (Left $
        ResponseError InvalidRequest ("Command " <> com' <> " isn't defined for plugin " <> p' <> ". Legal commands are: " <> T.pack(show $ map commandId xs)) Nothing, Nothing)
      Just (PluginCommand _ _ f) -> case J.fromJSON arg of
        J.Error err -> return (Left $
          ResponseError InvalidParams ("error while parsing args for " <> com' <> " in plugin " <> p' <> ": " <> T.pack err) Nothing, Nothing)
        J.Success a -> do
            res <- f a
            case res of
                Left e ->  return (Left e,             Nothing)
                Right r -> return (Right $ J.toJSON r, Nothing)

-- -----------------------------------------------------------

mkLspCommand :: PluginId -> CommandId -> T.Text -> Maybe [J.Value] -> IO Command
mkLspCommand plid cn title args' = do
  pid <- getPid
  let cmdId = mkLspCmdId pid plid cn
  let args = List <$> args'
  return $ Command title cmdId args

mkLspCmdId :: T.Text -> PluginId -> CommandId -> T.Text
mkLspCmdId pid (PluginId plid) (CommandId cid)
  = pid <> ":" <> plid <> ":" <> cid

getPid :: IO T.Text
getPid = T.pack . show <$> getProcessID

allLspCmdIds :: T.Text -> [(PluginId, [PluginCommand])] -> [T.Text]
allLspCmdIds pid commands = concat $ map go commands
  where
    go (plid, cmds) = map (mkLspCmdId pid plid . commandId) cmds

-- ---------------------------------------------------------------------

hoverPlugins :: [(PluginId, HoverProvider)] -> Plugin Config
hoverPlugins hs = Plugin mempty hoverRules (hoverHandlers hs)

hoverRules :: Rules ()
hoverRules = mempty

hoverHandlers :: [(PluginId, HoverProvider)] -> PartialHandlers Config
hoverHandlers hps = PartialHandlers $ \WithMessage{..} x ->
  return x{LSP.hoverHandler = withResponse RspHover (makeHover hps)}

makeHover :: [(PluginId, HoverProvider)]
      -> LSP.LspFuncs Config -> IdeState
      -> TextDocumentPositionParams
      -> IO (Either ResponseError (Maybe Hover))
makeHover hps _lf ideState params
  = do
      mhs <- mapM (\(_,p) -> p ideState params) hps
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

formatterPlugins :: [(PluginId, FormattingProvider IO)] -> Plugin Config
formatterPlugins providers
    = Plugin mempty
             formatterRules
             (formatterHandlers (Map.fromList (("none",noneProvider):providers)))

formatterRules :: Rules ()
formatterRules = mempty

formatterHandlers :: Map.Map PluginId (FormattingProvider IO) -> PartialHandlers Config
formatterHandlers providers = PartialHandlers $ \WithMessage{..} x -> return x
    { LSP.documentFormattingHandler
        = withResponse RspDocumentFormatting (formatting providers)
    , LSP.documentRangeFormattingHandler
        = withResponse RspDocumentRangeFormatting (rangeFormatting providers)
    }

-- ---------------------------------------------------------------------
