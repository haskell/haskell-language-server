{-# LANGUAGE DeriveAnyClass #-}

module Development.IDE.Plugin.HLS
    (
      asGhcIdePlugin
    ) where

import           Control.Exception(SomeException, catch)
import           Control.Lens ( (^.) )
import           Control.Monad
import qualified Data.Aeson as J
import           Data.Either
import qualified Data.List                     as List
import qualified Data.Map  as Map
import           Data.Maybe
import qualified Data.Text                     as T
import           Development.IDE.Core.Shake
import           Development.IDE.LSP.Server
import           Development.IDE.Plugin
import           Development.IDE.Plugin.HLS.Formatter
import           GHC.Generics
import           Ide.Plugin.Config
import           Ide.Types as HLS
import qualified Language.Haskell.LSP.Core as LSP
import           Language.Haskell.LSP.Messages
import           Language.Haskell.LSP.Types
import qualified Language.Haskell.LSP.Types              as J
import qualified Language.Haskell.LSP.Types.Capabilities as C
import           Language.Haskell.LSP.Types.Lens as L hiding (formatting, rangeFormatting)
import qualified Language.Haskell.LSP.VFS                as VFS
import           Text.Regex.TDFA.Text()
import Development.Shake (Rules)
import Ide.PluginUtils (getClientConfig, pluginEnabled, getPluginConfig, responseError, getProcessID)
import Development.IDE.Types.Logger (logInfo)

-- ---------------------------------------------------------------------

-- | Map a set of plugins to the underlying ghcide engine.  Main point is
-- IdePlugins are arranged by kind of operation, 'Plugin' is arranged by message
-- category ('Notifaction', 'Request' etc).
asGhcIdePlugin :: IdePlugins IdeState -> Plugin Config
asGhcIdePlugin mp =
    mkPlugin rulesPlugins (Just . HLS.pluginRules) <>
    mkPlugin executeCommandPlugins (Just . pluginCommands) <>
    mkPlugin codeActionPlugins     pluginCodeActionProvider <>
    mkPlugin codeLensPlugins       pluginCodeLensProvider <>
    -- Note: diagnostics are provided via Rules from pluginDiagnosticProvider
    mkPlugin hoverPlugins          pluginHoverProvider <>
    mkPlugin symbolsPlugins        pluginSymbolsProvider <>
    mkPlugin formatterPlugins      pluginFormattingProvider <>
    mkPlugin completionsPlugins    pluginCompletionProvider <>
    mkPlugin renamePlugins         pluginRenameProvider
    where
        justs (p, Just x)  = [(p, x)]
        justs (_, Nothing) = []

        ls = Map.toList (ipMap mp)

        mkPlugin :: ([(PluginId, b)] -> Plugin Config) -> (PluginDescriptor IdeState -> Maybe b) -> Plugin Config
        mkPlugin maker selector =
          case concatMap (\(pid, p) -> justs (pid, selector p)) ls of
            -- If there are no plugins that provide a descriptor, use mempty to
            -- create the plugin – otherwise we we end up declaring handlers for
            -- capabilities that there are no plugins for
            [] -> mempty
            xs -> maker xs

-- ---------------------------------------------------------------------

rulesPlugins :: [(PluginId, Rules ())] -> Plugin Config
rulesPlugins rs = Plugin rules mempty
    where
        rules = foldMap snd rs

codeActionPlugins :: [(PluginId, CodeActionProvider IdeState)] -> Plugin Config
codeActionPlugins cas = Plugin codeActionRules (codeActionHandlers cas)

codeActionRules :: Rules ()
codeActionRules = mempty

codeActionHandlers :: [(PluginId, CodeActionProvider IdeState)] -> PartialHandlers Config
codeActionHandlers cas = PartialHandlers $ \WithMessage{..} x -> return x
    { LSP.codeActionHandler
        = withResponse RspCodeAction (makeCodeAction cas)
    }

makeCodeAction :: [(PluginId, CodeActionProvider IdeState)]
      -> LSP.LspFuncs Config -> IdeState
      -> CodeActionParams
      -> IO (Either ResponseError (List CAResult))
makeCodeAction cas lf ideState (CodeActionParams docId range context _) = do
    let caps = LSP.clientCapabilities lf
        unL (List ls) = ls
        makeAction (pid,provider) = do
          pluginConfig <- getPluginConfig lf pid
          if pluginEnabled pluginConfig plcCodeActionsOn
            then provider lf ideState pid docId range context
            else return $ Right (List [])
    r <- mapM makeAction cas
    let actions = filter wasRequested . foldMap unL $ rights r
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
            cmd <- mkLspCommand "hls" "fallbackCodeAction" (action ^. title) (Just cmdParams)
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

codeLensPlugins :: [(PluginId, CodeLensProvider IdeState)] -> Plugin Config
codeLensPlugins cas = Plugin codeLensRules (codeLensHandlers cas)

codeLensRules :: Rules ()
codeLensRules = mempty

codeLensHandlers :: [(PluginId, CodeLensProvider IdeState)] -> PartialHandlers Config
codeLensHandlers cas = PartialHandlers $ \WithMessage{..} x -> return x
    { LSP.codeLensHandler
        = withResponse RspCodeLens (makeCodeLens cas)
    }

makeCodeLens :: [(PluginId, CodeLensProvider IdeState)]
      -> LSP.LspFuncs Config
      -> IdeState
      -> CodeLensParams
      -> IO (Either ResponseError (List CodeLens))
makeCodeLens cas lf ideState params = do
    logInfo (ideLogger ideState) "Plugin.makeCodeLens (ideLogger)" -- AZ
    let
      makeLens (pid, provider) = do
          pluginConfig <- getPluginConfig lf pid
          r <- if pluginEnabled pluginConfig plcCodeLensOn
                 then provider lf ideState pid params
                 else return $ Right (List [])
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

executeCommandPlugins :: [(PluginId, [PluginCommand IdeState])] -> Plugin Config
executeCommandPlugins ecs = Plugin mempty (executeCommandHandlers ecs)

executeCommandHandlers :: [(PluginId, [PluginCommand IdeState])] -> PartialHandlers Config
executeCommandHandlers ecs = PartialHandlers $ \WithMessage{..} x -> return x{
    LSP.executeCommandHandler = withResponseAndRequest RspExecuteCommand ReqApplyWorkspaceEdit (makeExecuteCommands ecs)
    }

makeExecuteCommands :: [(PluginId, [PluginCommand IdeState])] -> LSP.LspFuncs Config -> ExecuteCommandProvider IdeState
makeExecuteCommands ecs lf ide = wrapUnhandledExceptions $ do
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
          Just ("hls", "fallbackCodeAction") ->
            case J.fromJSON cmdParams of
              J.Success (FallbackCodeActionParams mEdit mCmd) -> do

                -- Send off the workspace request if it has one
                forM_ mEdit $ \edit -> do
                  let eParams = J.ApplyWorkspaceEditParams edit
                  reqId <- LSP.getNextReqId lf
                  LSP.sendFunc lf $ ReqApplyWorkspaceEdit $ RequestMessage "2.0" reqId WorkspaceApplyEdit eParams

                case mCmd of
                  -- If we have a command, continue to execute it
                  Just (J.Command _ innerCmdId innerArgs)
                      -> execCmd (ExecuteCommandParams innerCmdId innerArgs Nothing)
                  Nothing -> return (Right J.Null, Nothing)

              J.Error _str -> return (Right J.Null, Nothing)

          -- Just an ordinary HIE command
          Just (plugin, cmd) -> runPluginCommand pluginMap lf ide plugin cmd cmdParams

          -- Couldn't parse the command identifier
          _ -> return (Left $ ResponseError InvalidParams "Invalid command identifier" Nothing, Nothing)

  execCmd


-- -----------------------------------------------------------
wrapUnhandledExceptions ::
    (a -> IO (Either ResponseError J.Value, Maybe b)) ->
       a -> IO (Either ResponseError J.Value, Maybe b)
wrapUnhandledExceptions action input =
    catch (action input) $ \(e::SomeException) -> do
        let resp = ResponseError InternalError (T.pack $ show e) Nothing
        return (Left resp, Nothing)


-- | Runs a plugin command given a PluginId, CommandId and
-- arguments in the form of a JSON object.
runPluginCommand :: Map.Map PluginId [PluginCommand IdeState]
                 -> LSP.LspFuncs Config
                 -> IdeState
                 -> PluginId
                 -> CommandId
                 -> J.Value
                 -> IO (Either ResponseError J.Value,
                        Maybe (ServerMethod, ApplyWorkspaceEditParams))
runPluginCommand m lf ide  p@(PluginId p') com@(CommandId com') arg =
  case Map.lookup p m of
    Nothing -> return
      (Left $ ResponseError InvalidRequest ("Plugin " <> p' <> " doesn't exist") Nothing, Nothing)
    Just xs -> case List.find ((com ==) . commandId) xs of
      Nothing -> return (Left $
        ResponseError InvalidRequest ("Command " <> com' <> " isn't defined for plugin " <> p'
                                      <> ". Legal commands are: " <> T.pack(show $ map commandId xs)) Nothing, Nothing)
      Just (PluginCommand _ _ f) -> case J.fromJSON arg of
        J.Error err -> return (Left $
          ResponseError InvalidParams ("error while parsing args for " <> com' <> " in plugin " <> p'
                                       <> ": " <> T.pack err
                                       <> "\narg = " <> T.pack (show arg)) Nothing, Nothing)
        J.Success a -> f lf ide a

-- -----------------------------------------------------------

mkLspCommand :: PluginId -> CommandId -> T.Text -> Maybe [J.Value] -> IO Command
mkLspCommand plid cn title args' = do
  pid <- T.pack . show <$> getProcessID
  let cmdId = mkLspCmdId pid plid cn
  let args = List <$> args'
  return $ Command title cmdId args

mkLspCmdId :: T.Text -> PluginId -> CommandId -> T.Text
mkLspCmdId pid (PluginId plid) (CommandId cid)
  = pid <> ":" <> plid <> ":" <> cid

-- ---------------------------------------------------------------------

hoverPlugins :: [(PluginId, HoverProvider IdeState)] -> Plugin Config
hoverPlugins hs = Plugin hoverRules (hoverHandlers hs)

hoverRules :: Rules ()
hoverRules = mempty

hoverHandlers :: [(PluginId, HoverProvider IdeState)] -> PartialHandlers Config
hoverHandlers hps = PartialHandlers $ \WithMessage{..} x ->
  return x{LSP.hoverHandler = withResponse RspHover (makeHover hps)}

makeHover :: [(PluginId, HoverProvider IdeState)]
      -> LSP.LspFuncs Config -> IdeState
      -> TextDocumentPositionParams
      -> IO (Either ResponseError (Maybe Hover))
makeHover hps lf ideState params
  = do
      let
        makeHover(pid,p) = do
          pluginConfig <- getPluginConfig lf pid
          if pluginEnabled pluginConfig plcHoverOn
             then p ideState params
             else return $ Right Nothing
      mhs <- mapM makeHover hps
      -- TODO: We should support ServerCapabilities and declare that
      -- we don't support hover requests during initialization if we
      -- don't have any hover providers
      -- TODO: maybe only have provider give MarkedString and
      -- work out range here?
      let hs = catMaybes (rights mhs)
          r = listToMaybe $ mapMaybe (^. range) hs
          h = case foldMap (^. contents) hs of
            HoverContentsMS (List []) -> Nothing
            hh                        -> Just $ Hover hh r
      return $ Right h

-- ---------------------------------------------------------------------
-- ---------------------------------------------------------------------

symbolsPlugins :: [(PluginId, SymbolsProvider IdeState)] -> Plugin Config
symbolsPlugins hs = Plugin symbolsRules (symbolsHandlers hs)

symbolsRules :: Rules ()
symbolsRules = mempty

symbolsHandlers :: [(PluginId, SymbolsProvider IdeState)] -> PartialHandlers Config
symbolsHandlers hps = PartialHandlers $ \WithMessage{..} x ->
  return x {LSP.documentSymbolHandler = withResponse RspDocumentSymbols (makeSymbols hps)}

makeSymbols :: [(PluginId, SymbolsProvider IdeState)]
      -> LSP.LspFuncs Config
      -> IdeState
      -> DocumentSymbolParams
      -> IO (Either ResponseError DSResult)
makeSymbols sps lf ideState params
  = do
      let uri' = params ^. textDocument . uri
          (C.ClientCapabilities _ tdc _ _) = LSP.clientCapabilities lf
          supportsHierarchy = Just True == (tdc >>= C._documentSymbol >>= C._hierarchicalDocumentSymbolSupport)
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

          makeSymbols (pid,p) = do
            pluginConfig <- getPluginConfig lf pid
            if pluginEnabled pluginConfig plcSymbolsOn
              then p lf ideState params
              else return $ Right []
      mhs <- mapM makeSymbols sps
      case rights mhs of
          [] -> return $ Left $ responseError $ T.pack $ show $ lefts mhs
          hs -> return $ Right $ convertSymbols $ concat hs


-- ---------------------------------------------------------------------
-- ---------------------------------------------------------------------

renamePlugins :: [(PluginId, RenameProvider IdeState)] -> Plugin Config
renamePlugins providers = Plugin rules handlers
  where
    rules = mempty
    handlers = PartialHandlers $ \WithMessage{..} x -> return x
      { LSP.renameHandler = withResponse RspRename (renameWith providers)}

renameWith ::
  [(PluginId, RenameProvider IdeState)] ->
  LSP.LspFuncs Config ->
  IdeState ->
  RenameParams ->
  IO (Either ResponseError WorkspaceEdit)
renameWith providers lspFuncs state params = do
    let
        makeAction (pid,p) = do
          pluginConfig <- getPluginConfig lspFuncs pid
          if pluginEnabled pluginConfig plcRenameOn
             then p lspFuncs state params
             else return $ Right $ WorkspaceEdit Nothing Nothing
    -- TODO:AZ: we need to consider the right way to combine possible renamers
    results <- mapM makeAction providers
    case partitionEithers results of
        (errors, []) -> return $ Left $ responseError $ T.pack $ show errors
        (_, edits) -> return $ Right $ mconcat edits

-- ---------------------------------------------------------------------
-- ---------------------------------------------------------------------

formatterPlugins :: [(PluginId, FormattingProvider IdeState IO)] -> Plugin Config
formatterPlugins providers
    = Plugin formatterRules
             (formatterHandlers (Map.fromList (("none",noneProvider):providers)))

formatterRules :: Rules ()
formatterRules = mempty

formatterHandlers :: Map.Map PluginId (FormattingProvider IdeState IO) -> PartialHandlers Config
formatterHandlers providers = PartialHandlers $ \WithMessage{..} x -> return x
    { LSP.documentFormattingHandler
        = withResponse RspDocumentFormatting (formatting providers)
    , LSP.documentRangeFormattingHandler
        = withResponse RspDocumentRangeFormatting (rangeFormatting providers)
    }

-- ---------------------------------------------------------------------
-- ---------------------------------------------------------------------

completionsPlugins :: [(PluginId, CompletionProvider IdeState)] -> Plugin Config
completionsPlugins cs = Plugin completionsRules (completionsHandlers cs)

completionsRules :: Rules ()
completionsRules = mempty

completionsHandlers :: [(PluginId, CompletionProvider IdeState)] -> PartialHandlers Config
completionsHandlers cps = PartialHandlers $ \WithMessage{..} x ->
  return x {LSP.completionHandler = withResponse RspCompletion (makeCompletions cps)}

makeCompletions :: [(PluginId, CompletionProvider IdeState)]
      -> LSP.LspFuncs Config
      -> IdeState
      -> CompletionParams
      -> IO (Either ResponseError CompletionResponseResult)
makeCompletions sps lf ideState params@(CompletionParams (TextDocumentIdentifier doc) pos _context _mt)
  = do
      mprefix <- getPrefixAtPos lf doc pos
      _snippets <- WithSnippets . completionSnippetsOn <$> getClientConfig lf

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
          makeAction (pid,p) = do
            pluginConfig <- getPluginConfig lf pid
            if pluginEnabled pluginConfig plcCompletionOn
               then p lf ideState params
               else return $ Right $ Completions $ List []

      case mprefix of
          Nothing -> return $ Right $ Completions $ List []
          Just _prefix -> do
            mhs <- mapM makeAction sps
            case rights mhs of
                [] -> return $ Left $ responseError $ T.pack $ show $ lefts mhs
                hs -> return $ Right $ combine hs

getPrefixAtPos :: LSP.LspFuncs Config -> Uri -> Position -> IO (Maybe VFS.PosPrefixInfo)
getPrefixAtPos lf uri pos = do
  mvf <-  LSP.getVirtualFileFunc lf (J.toNormalizedUri uri)
  case mvf of
    Just vf -> VFS.getCompletionPrefix pos vf
    Nothing -> return Nothing
