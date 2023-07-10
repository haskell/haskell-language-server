{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE GADTs             #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PolyKinds         #-}

module Development.IDE.Plugin.HLS
    (
      asGhcIdePlugin
    , Log(..)
    ) where

import           Control.Exception             (SomeException)
import           Control.Lens                  ((^.))
import           Control.Monad
import qualified Data.Aeson                    as A
import           Data.Bifunctor                (first)
import           Data.Dependent.Map            (DMap)
import qualified Data.Dependent.Map            as DMap
import           Data.Dependent.Sum
import           Data.Either
import qualified Data.List                     as List
import           Data.List.NonEmpty            (NonEmpty, nonEmpty, toList)
import qualified Data.List.NonEmpty            as NE
import qualified Data.Map                      as Map
import           Data.Some
import           Data.String
import           Data.Text                     (Text)
import qualified Data.Text                     as T
import           Development.IDE.Core.Shake    hiding (Log)
import           Development.IDE.Core.Tracing
import           Development.IDE.Graph         (Rules)
import           Development.IDE.LSP.Server
import           Development.IDE.Plugin
import qualified Development.IDE.Plugin        as P
import           Development.IDE.Types.Logger  hiding (Error)
import           Ide.Plugin.Config
import           Ide.PluginUtils               (getClientConfig)
import           Ide.Types                     as HLS
import qualified Language.LSP.Protocol.Lens    as L
import           Language.LSP.Protocol.Message
import           Language.LSP.Protocol.Types
import qualified Language.LSP.Server           as LSP
import           Language.LSP.VFS
import           Prettyprinter.Render.String   (renderString)
import           Text.Regex.TDFA.Text          ()
import           UnliftIO                      (MonadUnliftIO)
import           UnliftIO.Async                (forConcurrently)
import           UnliftIO.Exception            (catchAny)

-- ---------------------------------------------------------------------
--

data Log
    =  LogPluginError PluginId ResponseError
    | LogNoPluginForMethod (Some SMethod)
    | LogInvalidCommandIdentifier
    | ExceptionInPlugin PluginId (Some SMethod) SomeException
instance Pretty Log where
  pretty = \case
    LogPluginError (PluginId pId) err -> pretty pId <> ":" <+> prettyResponseError err
    LogNoPluginForMethod (Some method) ->
        "No plugin enabled for " <> pretty (show method)
    LogInvalidCommandIdentifier-> "Invalid command identifier"
    ExceptionInPlugin plId (Some method) exception ->
        "Exception in plugin " <> viaShow plId <> " while processing "<> viaShow method <> ": " <> viaShow exception

instance Show Log where show = renderString . layoutCompact . pretty

-- various error message specific builders
prettyResponseError :: ResponseError -> Doc a
prettyResponseError err = errorCode <> ":" <+> errorBody
    where
        errorCode = pretty $ show $ err ^. L.code
        errorBody = pretty $ err ^. L.message

pluginNotEnabled :: SMethod m -> [(PluginId, b, a)] -> Text
pluginNotEnabled method availPlugins =
    "No plugin enabled for " <> T.pack (show method) <> ", available: "
        <> (T.intercalate ", " $ map (\(PluginId plid, _, _) -> plid) availPlugins)

pluginDoesntExist :: PluginId -> Text
pluginDoesntExist (PluginId pid) = "Plugin " <> pid <> " doesn't exist"

commandDoesntExist :: CommandId -> PluginId -> [PluginCommand ideState] -> Text
commandDoesntExist (CommandId com) (PluginId pid) legalCmds =
    "Command " <> com <> " isn't defined for plugin " <> pid <> ". Legal commands are: "
        <> (T.intercalate ", " $ map (\(PluginCommand{commandId = CommandId cid}) -> cid) legalCmds)

failedToParseArgs :: CommandId  -- ^ command that failed to parse
                    -> PluginId -- ^ Plugin that created the command
                    -> String   -- ^ The JSON Error message
                    -> A.Value  -- ^ The Argument Values
                    -> Text
failedToParseArgs (CommandId com) (PluginId pid) err arg =
    "Error while parsing args for " <> com <> " in plugin " <> pid <> ": "
        <> T.pack err <> ", arg = " <> T.pack (show arg)

exceptionInPlugin :: PluginId -> SMethod m -> SomeException -> Text
exceptionInPlugin plId method exception =
    "Exception in plugin " <> T.pack (show plId) <> " while processing "<> T.pack (show method) <> ": " <> T.pack (show exception)

-- | Build a ResponseError and log it before returning to the caller
logAndReturnError :: Recorder (WithPriority Log) -> PluginId -> (LSPErrorCodes |? ErrorCodes) -> Text -> LSP.LspT Config IO (Either ResponseError a)
logAndReturnError recorder p errCode msg = do
    let err = ResponseError errCode msg Nothing
    logWith recorder Warning $ LogPluginError p err
    pure $ Left err

-- | Logs the provider error before returning it to the caller
logAndReturnError' :: Recorder (WithPriority Log) -> (LSPErrorCodes |? ErrorCodes) -> Log -> LSP.LspT Config IO (Either ResponseError a)
logAndReturnError' recorder errCode msg = do
    let err = ResponseError errCode (fromString $ show msg) Nothing
    logWith recorder Warning $ msg
    pure $ Left err

-- | Map a set of plugins to the underlying ghcide engine.
asGhcIdePlugin :: Recorder (WithPriority Log) -> IdePlugins IdeState -> Plugin Config
asGhcIdePlugin recorder (IdePlugins ls) =
    mkPlugin rulesPlugins HLS.pluginRules <>
    mkPlugin (executeCommandPlugins recorder) HLS.pluginCommands <>
    mkPlugin (extensiblePlugins recorder) id <>
    mkPlugin (extensibleNotificationPlugins recorder) id <>
    mkPluginFromDescriptor dynFlagsPlugins HLS.pluginModifyDynflags
    where
        mkPlugin f = mkPluginFromDescriptor (f . map (first pluginId))

        mkPluginFromDescriptor
            :: ([(PluginDescriptor IdeState, b)]
            -> Plugin Config)
            -> (PluginDescriptor IdeState -> b)
            -> Plugin Config
        mkPluginFromDescriptor maker selector =
          case map (\p -> (p, selector p)) ls of
            -- If there are no plugins that provide a descriptor, use mempty to
            -- create the plugin – otherwise we we end up declaring handlers for
            -- capabilities that there are no plugins for
            [] -> mempty
            xs -> maker xs

-- ---------------------------------------------------------------------

rulesPlugins :: [(PluginId, Rules ())] -> Plugin Config
rulesPlugins rs = mempty { P.pluginRules = rules }
    where
        rules = foldMap snd rs

dynFlagsPlugins :: [(PluginDescriptor c, DynFlagsModifications)] -> Plugin Config
dynFlagsPlugins rs = mempty
  { P.pluginModifyDynflags =
      flip foldMap rs $ \(plId, dflag_mods) cfg ->
        let plg_cfg = configForPlugin cfg plId
         in if plcGlobalOn plg_cfg
              then dflag_mods
              else mempty
  }

-- ---------------------------------------------------------------------

executeCommandPlugins :: Recorder (WithPriority Log) -> [(PluginId, [PluginCommand IdeState])] -> Plugin Config
executeCommandPlugins recorder ecs = mempty { P.pluginHandlers = executeCommandHandlers recorder ecs }

executeCommandHandlers :: Recorder (WithPriority Log) -> [(PluginId, [PluginCommand IdeState])] -> LSP.Handlers (ServerM Config)
executeCommandHandlers recorder ecs = requestHandler SMethod_WorkspaceExecuteCommand execCmd
  where
    pluginMap = Map.fromListWith (++) ecs

    parseCmdId :: T.Text -> Maybe (PluginId, CommandId)
    parseCmdId x = case T.splitOn ":" x of
      [plugin, command]    -> Just (PluginId plugin, CommandId command)
      [_, plugin, command] -> Just (PluginId plugin, CommandId command)
      _                    -> Nothing

    -- The parameters to the HLS command are always the first element
    execCmd :: IdeState -> ExecuteCommandParams -> LSP.LspT Config IO (Either ResponseError (A.Value |? Null))
    execCmd ide (ExecuteCommandParams _ cmdId args) = do
      let cmdParams :: A.Value
          cmdParams = case args of
            Just ((x:_)) -> x
            _            -> A.Null
      case parseCmdId cmdId of
        -- Shortcut for immediately applying a applyWorkspaceEdit as a fallback for v3.8 code actions
        Just ("hls", "fallbackCodeAction") ->
          case A.fromJSON cmdParams of
            A.Success (FallbackCodeActionParams mEdit mCmd) -> do

              -- Send off the workspace request if it has one
              forM_ mEdit $ \edit ->
                LSP.sendRequest SMethod_WorkspaceApplyEdit (ApplyWorkspaceEditParams Nothing edit) (\_ -> pure ())

              case mCmd of
                -- If we have a command, continue to execute it
                Just (Command _ innerCmdId innerArgs)
                    -> execCmd ide (ExecuteCommandParams Nothing innerCmdId innerArgs)
                Nothing -> return $ Right $ InR Null

            A.Error _str -> return $ Right $ InR Null

        -- Just an ordinary HIE command
        Just (plugin, cmd) -> runPluginCommand ide plugin cmd cmdParams

        -- Couldn't parse the command identifier
        _ -> do
            logWith recorder Warning LogInvalidCommandIdentifier
            return $ Left $ ResponseError (InR ErrorCodes_InvalidParams) "Invalid command identifier" Nothing

    runPluginCommand :: IdeState -> PluginId -> CommandId -> A.Value -> LSP.LspT Config IO (Either ResponseError (A.Value |? Null))
    runPluginCommand ide p com arg =
      case Map.lookup p pluginMap  of
        Nothing -> logAndReturnError recorder p (InR ErrorCodes_InvalidRequest) (pluginDoesntExist p)
        Just xs -> case List.find ((com ==) . commandId) xs of
          Nothing -> logAndReturnError recorder p (InR ErrorCodes_InvalidRequest) (commandDoesntExist com p xs)
          Just (PluginCommand _ _ f) -> case A.fromJSON arg of
            A.Error err -> logAndReturnError recorder p (InR ErrorCodes_InvalidParams) (failedToParseArgs com p err arg)
            A.Success a ->
                f ide a `catchAny`
                (\e -> logAndReturnError' recorder (InR ErrorCodes_InternalError) (ExceptionInPlugin p (Some SMethod_WorkspaceApplyEdit) e))

-- ---------------------------------------------------------------------

extensiblePlugins ::  Recorder (WithPriority Log) -> [(PluginId, PluginDescriptor IdeState)] -> Plugin Config
extensiblePlugins recorder xs = mempty { P.pluginHandlers = handlers }
  where
    IdeHandlers handlers' = foldMap bakePluginId xs
    bakePluginId :: (PluginId, PluginDescriptor IdeState) -> IdeHandlers
    bakePluginId (pid,pluginDesc) = IdeHandlers $ DMap.map
      (\(PluginHandler f) -> IdeHandler [(pid,pluginDesc,f pid)])
      hs
      where
        PluginHandlers hs = HLS.pluginHandlers pluginDesc
    handlers = mconcat $ do
      (IdeMethod m :=> IdeHandler fs') <- DMap.assocs handlers'
      pure $ requestHandler m $ \ide params -> do
        config <- Ide.PluginUtils.getClientConfig
        -- Only run plugins that are allowed to run on this request
        let fs = filter (\(_, desc, _) -> pluginEnabled m params desc config) fs'
        -- Clients generally don't display ResponseErrors so instead we log any that we come across
        case nonEmpty fs of
          Nothing -> do
            logWith recorder Warning (LogNoPluginForMethod $ Some m)
            let err = ResponseError (InR ErrorCodes_InvalidRequest) msg Nothing
                msg = pluginNotEnabled m fs'
            return $ Left err
          Just fs -> do
            let  handlers = fmap (\(plid,_,handler) -> (plid,handler)) fs
            es <- runConcurrently exceptionInPlugin m handlers ide params

            let (errs,succs) = partitionEithers $ toList $ join $ NE.zipWith (\(pId,_) -> fmap (first (pId,))) handlers es
            unless (null errs) $ forM_ errs $ \(pId, err) ->
                logWith recorder Warning $ LogPluginError pId err
            case nonEmpty succs of
              Nothing -> pure $ Left $ combineErrors $ map snd errs
              Just xs -> do
                caps <- LSP.getClientCapabilities
                pure $ Right $ combineResponses m config caps params xs

-- ---------------------------------------------------------------------

extensibleNotificationPlugins :: Recorder (WithPriority Log) -> [(PluginId, PluginDescriptor IdeState)] -> Plugin Config
extensibleNotificationPlugins recorder xs = mempty { P.pluginHandlers = handlers }
  where
    IdeNotificationHandlers handlers' = foldMap bakePluginId xs
    bakePluginId :: (PluginId, PluginDescriptor IdeState) -> IdeNotificationHandlers
    bakePluginId (pid,pluginDesc) = IdeNotificationHandlers $ DMap.map
      (\(PluginNotificationHandler f) -> IdeNotificationHandler [(pid,pluginDesc,f pid)])
      hs
      where PluginNotificationHandlers hs = HLS.pluginNotificationHandlers pluginDesc
    handlers = mconcat $ do
      (IdeNotification m :=> IdeNotificationHandler fs') <- DMap.assocs handlers'
      pure $ notificationHandler m $ \ide vfs params -> do
        config <- Ide.PluginUtils.getClientConfig
        -- Only run plugins that are allowed to run on this request
        let fs = filter (\(_, desc, _) -> pluginEnabled m params desc config) fs'
        case nonEmpty fs of
          Nothing -> do
            logWith recorder Warning (LogNoPluginForMethod $ Some m)
          Just fs -> do
            -- We run the notifications in order, so the core ghcide provider
            -- (which restarts the shake process) hopefully comes last
            mapM_ (\(pid,_,f) -> otTracedProvider pid (fromString $ show m) $ f ide vfs params
                                    `catchAny`
                                    (\e -> logWith recorder Warning (ExceptionInPlugin pid (Some m) e))) fs


-- ---------------------------------------------------------------------

runConcurrently
  :: MonadUnliftIO m
  => (PluginId -> SMethod method -> SomeException -> T.Text)
  -> SMethod method -- ^ Method (used for errors and tracing)
  -> NonEmpty (PluginId, a -> b -> m (NonEmpty (Either ResponseError d)))
  -- ^ Enabled plugin actions that we are allowed to run
  -> a
  -> b
  -> m (NonEmpty(NonEmpty (Either ResponseError d)))
runConcurrently msg method fs a b = forConcurrently fs $ \(pid,f) -> otTracedProvider pid (fromString (show method)) $ do
  f a b
     `catchAny` (\e -> pure $ pure $ Left $ ResponseError (InR ErrorCodes_InternalError) (msg pid method e) Nothing)

combineErrors :: [ResponseError] -> ResponseError
combineErrors [x] = x
combineErrors xs  = ResponseError (InR ErrorCodes_InternalError) (T.pack (show xs)) Nothing

-- | Combine the 'PluginHandler' for all plugins
newtype IdeHandler (m :: Method ClientToServer Request)
  = IdeHandler [(PluginId, PluginDescriptor IdeState, IdeState -> MessageParams m -> LSP.LspM Config (NonEmpty (Either ResponseError (MessageResult m))))]

-- | Combine the 'PluginHandler' for all plugins
newtype IdeNotificationHandler (m :: Method ClientToServer Notification)
  = IdeNotificationHandler [(PluginId, PluginDescriptor IdeState, IdeState -> VFS -> MessageParams m -> LSP.LspM Config ())]
-- type NotificationHandler (m :: Method ClientToServer Notification) = MessageParams m -> IO ()`

-- | Combine the 'PluginHandlers' for all plugins
newtype IdeHandlers             = IdeHandlers             (DMap IdeMethod       IdeHandler)
newtype IdeNotificationHandlers = IdeNotificationHandlers (DMap IdeNotification IdeNotificationHandler)

instance Semigroup IdeHandlers where
  (IdeHandlers a) <> (IdeHandlers b) = IdeHandlers $ DMap.unionWithKey go a b
    where
      go _ (IdeHandler a) (IdeHandler b) = IdeHandler (a <> b)
instance Monoid IdeHandlers where
  mempty = IdeHandlers mempty

instance Semigroup IdeNotificationHandlers where
  (IdeNotificationHandlers a) <> (IdeNotificationHandlers b) = IdeNotificationHandlers $ DMap.unionWithKey go a b
    where
      go _ (IdeNotificationHandler a) (IdeNotificationHandler b) = IdeNotificationHandler (a <> b)
instance Monoid IdeNotificationHandlers where
  mempty = IdeNotificationHandlers mempty
