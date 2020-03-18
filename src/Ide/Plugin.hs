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
    , mkLspCommand
    , allLspCmdIds
    , allLspCmdIds'
    , getPid
    , responseError
    ) where

import           Control.Lens ( (^.) )
import           Control.Monad
import qualified Data.Aeson as J
import qualified Data.Default
import           Data.Either
import qualified Data.List                     as List
import qualified Data.Map  as Map
import           Data.Maybe
import qualified Data.Text                     as T
import           Development.IDE.Core.Rules
import           Development.IDE.Core.Shake
import           Development.IDE.LSP.Server
import           Development.IDE.Plugin hiding (pluginRules)
import           Development.IDE.Types.Diagnostics as D
import           Development.IDE.Types.Logger
import           Development.Shake hiding ( Diagnostic, command )
import           GHC.Generics
import           Ide.Compat
import           Ide.Plugin.Config
import           Ide.Plugin.Formatter
import           Ide.Types
import qualified Language.Haskell.LSP.Core as LSP
import qualified Language.Haskell.LSP.Messages as LSP
import           Language.Haskell.LSP.Messages
import           Language.Haskell.LSP.Types
import qualified Language.Haskell.LSP.Types              as J
import qualified Language.Haskell.LSP.Types.Capabilities as C
import           Language.Haskell.LSP.Types.Lens as L hiding (formatting, rangeFormatting)
import qualified Language.Haskell.LSP.VFS                as VFS
import           Text.Regex.TDFA.Text()

-- ---------------------------------------------------------------------

-- | Map a set of plugins to the underlying ghcide engine.  Main point is
-- IdePlugins are arranged by kind of operation, 'Plugin' is arranged by message
-- category ('Notifaction', 'Request' etc).
asGhcIdePlugin :: IdePlugins -> Plugin Config
asGhcIdePlugin mp =
    mkPlugin rulesPlugins (Just . pluginRules) <>
    mkPlugin executeCommandPlugins (Just . pluginCommands) <>
    mkPlugin codeActionPlugins     pluginCodeActionProvider <>
    mkPlugin codeLensPlugins       pluginCodeLensProvider <>
    -- Note: diagnostics are provided via Rules from pluginDiagnosticProvider
    mkPlugin hoverPlugins          pluginHoverProvider <>
    mkPlugin symbolsPlugins        pluginSymbolsProvider <>
    mkPlugin formatterPlugins      pluginFormattingProvider <>
    mkPlugin completionsPlugins    pluginCompletionProvider
    where
        justs (p, Just x)  = [(p, x)]
        justs (_, Nothing) = []

        ls = Map.toList (ipMap mp)

        mkPlugin :: ([(PluginId, b)] -> Plugin Config) -> (PluginDescriptor -> Maybe b) -> Plugin Config
        mkPlugin maker selector
            = maker $ concatMap (\(pid, p) -> justs (pid, selector p)) ls


pluginDescToIdePlugins :: [PluginDescriptor] -> IdePlugins
pluginDescToIdePlugins plugins = IdePlugins $ Map.fromList $ map (\p -> (pluginId p, p)) plugins

allLspCmdIds' :: T.Text -> IdePlugins -> [T.Text]
allLspCmdIds' pid mp = mkPlugin (allLspCmdIds pid) (Just . pluginCommands)
    where
        justs (p, Just x)  = [(p, x)]
        justs (_, Nothing) = []

        ls = Map.toList (ipMap mp)

        mkPlugin maker selector
            = maker $ concatMap (\(pid, p) -> justs (pid, selector p)) ls

-- ---------------------------------------------------------------------

rulesPlugins :: [(PluginId, Rules ())] -> Plugin Config
rulesPlugins rs = Plugin rules mempty
    where
        rules = mconcat $ map snd rs

codeActionPlugins :: [(PluginId, CodeActionProvider)] -> Plugin Config
codeActionPlugins cas = Plugin codeActionRules (codeActionHandlers cas)

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

codeLensPlugins :: [(PluginId, CodeLensProvider)] -> Plugin Config
codeLensPlugins cas = Plugin codeLensRules (codeLensHandlers cas)

codeLensRules :: Rules ()
codeLensRules = mempty

codeLensHandlers :: [(PluginId, CodeLensProvider)] -> PartialHandlers Config
codeLensHandlers cas = PartialHandlers $ \WithMessage{..} x -> return x
    { LSP.codeLensHandler
        = withResponse RspCodeLens (makeCodeLens cas)
    }

makeCodeLens :: [(PluginId, CodeLensProvider)]
      -> LSP.LspFuncs Config
      -> IdeState
      -> CodeLensParams
      -> IO (Either ResponseError (List CodeLens))
makeCodeLens cas _lf ideState params = do
    logInfo (ideLogger ideState) "Plugin.makeCodeLens (ideLogger)" -- AZ
    let
      makeLens (pid, provider) = do
          r <- provider ideState pid params
          return (pid, r)
      breakdown :: [(PluginId, Either ResponseError a)] -> ([(PluginId, ResponseError)], [(PluginId, a)])
      breakdown ls = (concatMap doOneLeft ls, concatMap doOneRight ls)
        where
          doOneLeft (pid, Left err) = [(pid,err)]
          doOneLeft (_, Right _) = []

          doOneRight (pid, Right a) = [(pid,a)]
          doOneRight (_, Left _) = []

    r <- mapM makeLens cas
    case breakdown r of
        ([],[]) -> return $ Right $ List []
        (es,[]) -> return $ Left $ ResponseError InternalError (T.pack $ "codeLens failed:" ++ show es) Nothing
        (_,rs) -> return $ Right $ List (concatMap (\(_,List cs) -> cs) rs)

-- -----------------------------------------------------------

executeCommandPlugins :: [(PluginId, [PluginCommand])] -> Plugin Config
executeCommandPlugins ecs = Plugin mempty (executeCommandHandlers ecs)

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

-- -----------------------------------------------------------

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
        J.Success a -> f a

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
hoverPlugins hs = Plugin hoverRules (hoverHandlers hs)

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

symbolsPlugins :: [(PluginId, SymbolsProvider)] -> Plugin Config
symbolsPlugins hs = Plugin symbolsRules (symbolsHandlers hs)

symbolsRules :: Rules ()
symbolsRules = mempty

symbolsHandlers :: [(PluginId, SymbolsProvider)] -> PartialHandlers Config
symbolsHandlers hps = PartialHandlers $ \WithMessage{..} x ->
  return x {LSP.documentSymbolHandler = withResponse RspDocumentSymbols (makeSymbols hps)}

makeSymbols :: [(PluginId, SymbolsProvider)]
      -> LSP.LspFuncs Config
      -> IdeState
      -> DocumentSymbolParams
      -> IO (Either ResponseError DSResult)
makeSymbols sps lf ideState params
  = do
      let uri' = params ^. textDocument . uri
          (C.ClientCapabilities _ tdc _ _) = LSP.clientCapabilities lf
          supportsHierarchy = fromMaybe False $ tdc >>= C._documentSymbol
                              >>= C._hierarchicalDocumentSymbolSupport
          convertSymbols :: [DocumentSymbol] -> DSResult
          convertSymbols symbs
            | supportsHierarchy = DSDocumentSymbols $ List symbs
            | otherwise = DSSymbolInformation (List $ concatMap (go Nothing) symbs)
            where
                go :: Maybe T.Text -> DocumentSymbol -> [SymbolInformation]
                go parent ds =
                  let children' :: [SymbolInformation]
                      children' = concatMap (go (Just name')) (fromMaybe mempty (ds ^. children))
                      loc = Location uri' (ds ^. range)
                      name' = ds ^. name
                      si = SymbolInformation name' (ds ^. kind) (ds ^. deprecated) loc parent
                  in [si] <> children'

      mhs <- mapM (\(_,p) -> p ideState params) sps
      case rights mhs of
          [] -> return $ Left $ responseError $ T.pack $ show $ lefts mhs
          hs -> return $ Right $ convertSymbols $ concat hs

-- ---------------------------------------------------------------------
-- ---------------------------------------------------------------------

formatterPlugins :: [(PluginId, FormattingProvider IO)] -> Plugin Config
formatterPlugins providers
    = Plugin formatterRules
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
-- ---------------------------------------------------------------------

completionsPlugins :: [(PluginId, CompletionProvider)] -> Plugin Config
completionsPlugins cs = Plugin completionsRules (completionsHandlers cs)

completionsRules :: Rules ()
completionsRules = mempty

completionsHandlers :: [(PluginId, CompletionProvider)] -> PartialHandlers Config
completionsHandlers cps = PartialHandlers $ \WithMessage{..} x ->
  return x {LSP.completionHandler = withResponse RspCompletion (makeCompletions cps)}

makeCompletions :: [(PluginId, CompletionProvider)]
      -> LSP.LspFuncs Config
      -> IdeState
      -> CompletionParams
      -> IO (Either ResponseError CompletionResponseResult)
makeCompletions sps lf ideState params@(CompletionParams (TextDocumentIdentifier doc) pos _context _mt)
  = do
      mprefix <- getPrefixAtPos lf doc pos
      _snippets <- WithSnippets <$> completionSnippetsOn <$> (getClientConfig lf)

      let
          combine :: [CompletionResponseResult] -> CompletionResponseResult
          combine cs = go (Completions $ List []) cs
              where
                  go acc [] = acc
                  go (Completions (List ls)) (Completions (List ls2):rest)
                      = go (Completions (List (ls <> ls2))) rest
                  go (Completions (List ls)) (CompletionList (CompletionListType complete (List ls2)):rest)
                      = go (CompletionList $ CompletionListType complete (List (ls <> ls2))) rest
                  go (CompletionList (CompletionListType complete (List ls))) (CompletionList (CompletionListType complete2 (List ls2)):rest)
                      = go (CompletionList $ CompletionListType (complete || complete2) (List (ls <> ls2))) rest
                  go (CompletionList (CompletionListType complete (List ls))) (Completions (List ls2):rest)
                      = go (CompletionList $ CompletionListType complete (List (ls <> ls2))) rest

      case mprefix of
          Nothing -> return $ Right $ Completions $ List []
          Just _prefix -> do
            mhs <- mapM (\(_,p) -> p ideState params) sps
            case rights mhs of
                [] -> return $ Left $ responseError $ T.pack $ show $ lefts mhs
                hs -> return $ Right $ combine hs

{-
        ReqCompletion req -> do
          liftIO $ U.logs $ "reactor:got CompletionRequest:" ++ show req
          let (_, doc, pos) = reqParams req

          mprefix <- getPrefixAtPos doc pos

          let callback compls = do
                let rspMsg = Core.makeResponseMessage req
                              $ J.Completions $ J.List compls
                reactorSend $ RspCompletion rspMsg
          case mprefix of
            Nothing -> callback []
            Just prefix -> do
              snippets <- Completions.WithSnippets <$> configVal completionSnippetsOn
              let hreq = IReq tn "completion" (req ^. J.id) callback
                           $ lift $ Completions.getCompletions doc prefix snippets
              makeRequest hreq
-}

getPrefixAtPos :: LSP.LspFuncs Config -> Uri -> Position -> IO (Maybe VFS.PosPrefixInfo)
getPrefixAtPos lf uri pos = do
  mvf <-  (LSP.getVirtualFileFunc lf) (J.toNormalizedUri uri)
  case mvf of
    Just vf -> VFS.getCompletionPrefix pos vf
    Nothing -> return Nothing

-- ---------------------------------------------------------------------
-- | Returns the current client configuration. It is not wise to permanently
-- cache the returned value of this function, as clients can at runitime change
-- their configuration.
--
-- If no custom configuration has been set by the client, this function returns
-- our own defaults.
getClientConfig :: LSP.LspFuncs Config -> IO Config
getClientConfig lf = fromMaybe Data.Default.def <$> LSP.config lf

-- ---------------------------------------------------------------------
