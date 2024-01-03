{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE GADTs             #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PolyKinds         #-}

module Development.IDE.Plugin.HLS
    (
      asGhcIdePlugin
    , toResponseError
    , Log(..)
    ) where

import           Control.Exception             (SomeException)
import           Control.Monad
import           Control.Monad.Trans.Except    (runExceptT)
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
import           Data.Maybe                    (mapMaybe)
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
import           Ide.Logger
import           Ide.Plugin.Config
import           Ide.Plugin.Error
import           Ide.Plugin.HandleRequestTypes
import           Ide.PluginUtils               (getClientConfig)
import           Ide.Types                     as HLS
import           Language.LSP.Protocol.Message
import           Language.LSP.Protocol.Types
import qualified Language.LSP.Server           as LSP
import           Language.LSP.VFS
import           Prettyprinter.Render.String   (renderString)
import           Text.Regex.TDFA.Text          ()
import           UnliftIO                      (MonadUnliftIO, liftIO)
import           UnliftIO.Async                (forConcurrently)
import           UnliftIO.Exception            (catchAny)

-- ---------------------------------------------------------------------
--

data Log
    =  LogPluginError PluginId PluginError
    | LogResponseError PluginId ResponseError
    | LogNoPluginForMethod (Some SMethod)
    | LogInvalidCommandIdentifier
    | ExceptionInPlugin PluginId (Some SMethod) SomeException

instance Pretty Log where
  pretty = \case
    LogPluginError (PluginId pId) err ->
      pretty pId <> ":" <+> pretty err
    LogResponseError (PluginId pId) err ->
      pretty pId <> ":" <+> pretty err
    LogNoPluginForMethod (Some method) ->
        "No plugin handles this " <> pretty method <> " request."
    LogInvalidCommandIdentifier-> "Invalid command identifier"
    ExceptionInPlugin plId (Some method) exception ->
        "Exception in plugin " <> viaShow plId <> " while processing "
          <> pretty method <> ": " <> viaShow exception
instance Show Log where show = renderString . layoutCompact . pretty

noPluginHandles :: Recorder (WithPriority Log) -> SMethod m -> [(PluginId, HandleRequestResult)] -> IO (Either ResponseError c)
noPluginHandles recorder m fs' = do
  logWith recorder Warning (LogNoPluginForMethod $ Some m)
  let err = ResponseError (InR ErrorCodes_MethodNotFound) msg Nothing
      msg = noPluginHandlesMsg m fs'
  return $ Left err
  where noPluginHandlesMsg :: SMethod m -> [(PluginId, HandleRequestResult)] -> Text
        noPluginHandlesMsg method [] = "No plugins are available to handle this " <> T.pack (show method) <> " request."
        noPluginHandlesMsg method availPlugins =
            "No plugins are available to handle this " <> T.pack (show method) <> " request.\n Plugins installed for this method, but not available to handle this request are:\n"
                <> (T.intercalate "\n" $
                      map (\(PluginId plid, pluginStatus) ->
                              plid
                              <> " "
                              <> (renderStrict . layoutCompact . pretty) pluginStatus)
                          availPlugins)

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
    logWith recorder Warning $ LogResponseError p err
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
            A.Success a -> do
              res <- runExceptT (f ide a) `catchAny` -- See Note [Exception handling in plugins]
                (\e -> pure $ Left $ PluginInternalError (exceptionInPlugin p SMethod_WorkspaceExecuteCommand e))
              case res of
                (Left (PluginRequestRefused r)) ->
                  liftIO $ noPluginHandles recorder SMethod_WorkspaceExecuteCommand [(p,DoesNotHandleRequest r)]
                (Left pluginErr) -> do
                  liftIO $ logErrors recorder [(p, pluginErr)]
                  pure $ Left $ toResponseError (p, pluginErr)
                (Right result) -> pure $ Right result

-- ---------------------------------------------------------------------

extensiblePlugins ::  Recorder (WithPriority Log) -> [(PluginId, PluginDescriptor IdeState)] -> Plugin Config
extensiblePlugins recorder plugins = mempty { P.pluginHandlers = handlers }
  where
    IdeHandlers handlers' = foldMap bakePluginId plugins
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
        -- Only run plugins that are allowed to run on this request, save the
        -- list of disabled plugins incase that's all we have
        let (fs, dfs) = List.partition (\(_, desc, _) -> handlesRequest m params desc config == HandlesRequest) fs'
        let disabledPluginsReason = (\(x, desc, _) -> (x, handlesRequest m params desc config)) <$> dfs
        -- Clients generally don't display ResponseErrors so instead we log any that we come across
        case nonEmpty fs of
          Nothing -> liftIO $ noPluginHandles recorder m disabledPluginsReason
          Just neFs -> do
            let  plidsAndHandlers = fmap (\(plid,_,handler) -> (plid,handler)) neFs
            es <- runConcurrently exceptionInPlugin m plidsAndHandlers ide params
            caps <- LSP.getClientCapabilities
            let (errs,succs) = partitionEithers $ toList $ join $ NE.zipWith (\(pId,_) -> fmap (first (pId,))) plidsAndHandlers es
            liftIO $ unless (null errs) $ logErrors recorder errs
            case nonEmpty succs of
              Nothing -> do
                let noRefused (_, PluginRequestRefused _) = False
                    noRefused (_, _)                      = True
                    (asErrors, asRefused) = List.partition noRefused errs
                    convertPRR (pId, PluginRequestRefused r) = Just (pId, DoesNotHandleRequest r)
                    convertPRR _ = Nothing
                    asRefusedReason = mapMaybe convertPRR asRefused
                case nonEmpty asErrors of
                  Nothing -> liftIO $ noPluginHandles recorder m  (disabledPluginsReason <> asRefusedReason)
                  Just xs -> pure $ Left $ combineErrors xs
              Just xs -> do
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
        -- Only run plugins that are enabled for this request
        let fs = filter (\(_, desc, _) -> handlesRequest m params desc config == HandlesRequest) fs'
        case nonEmpty fs of
          Nothing -> do
            logWith recorder Warning (LogNoPluginForMethod $ Some m)
          Just neFs -> do
            -- We run the notifications in order, so the core ghcide provider
            -- (which restarts the shake process) hopefully comes last
            mapM_ (\(pid,_,f) -> otTracedProvider pid (fromString $ show m) $ f ide vfs params
                                    `catchAny` -- See Note [Exception handling in plugins]
                                    (\e -> logWith recorder Warning (ExceptionInPlugin pid (Some m) e))) neFs


-- ---------------------------------------------------------------------

runConcurrently
  :: MonadUnliftIO m
  => (PluginId -> SMethod method -> SomeException -> T.Text)
  -> SMethod method -- ^ Method (used for errors and tracing)
  -> NonEmpty (PluginId, a -> b -> m (NonEmpty (Either PluginError d)))
  -- ^ Enabled plugin actions that we are allowed to run
  -> a
  -> b
  -> m (NonEmpty(NonEmpty (Either PluginError d)))
runConcurrently msg method fs a b = forConcurrently fs $ \(pid,f) -> otTracedProvider pid (fromString (show method)) $ do
  f a b  -- See Note [Exception handling in plugins]
     `catchAny` (\e -> pure $ pure $ Left $ PluginInternalError (msg pid method e))

combineErrors :: NonEmpty (PluginId, PluginError) -> ResponseError
combineErrors (x NE.:| []) = toResponseError x
combineErrors xs = toResponseError $ NE.last $ NE.sortWith (toPriority . snd) xs

toResponseError :: (PluginId, PluginError) -> ResponseError
toResponseError (PluginId plId, err) =
        ResponseError (toErrorCode err) (plId <> ": " <> tPretty err) Nothing
    where tPretty = T.pack . show . pretty

logErrors :: Recorder (WithPriority Log) -> [(PluginId, PluginError)] -> IO ()
logErrors recorder errs = do
  forM_ errs $ \(pId, err) ->
                logIndividualErrors pId err
  where logIndividualErrors plId err =
          logWith recorder (toPriority err) $ LogPluginError plId err


-- | Combine the 'PluginHandler' for all plugins
newtype IdeHandler (m :: Method ClientToServer Request)
  = IdeHandler [(PluginId, PluginDescriptor IdeState, IdeState -> MessageParams m -> LSP.LspM Config (NonEmpty (Either PluginError (MessageResult m))))]

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
      go _ (IdeHandler c) (IdeHandler d) = IdeHandler (c <> d)
instance Monoid IdeHandlers where
  mempty = IdeHandlers mempty

instance Semigroup IdeNotificationHandlers where
  (IdeNotificationHandlers a) <> (IdeNotificationHandlers b) = IdeNotificationHandlers $ DMap.unionWithKey go a b
    where
      go _ (IdeNotificationHandler c) (IdeNotificationHandler d) = IdeNotificationHandler (c <> d)
instance Monoid IdeNotificationHandlers where
  mempty = IdeNotificationHandlers mempty

{- Note [Exception handling in plugins]
Plugins run in LspM, and so have access to IO. This means they are likely to
throw exceptions, even if only by accident or through calling libraries that
throw exceptions. Ultimately, we're running a bunch of less-trusted IO code,
so we should be robust to it throwing.

We don't want these to bring down HLS. So we catch and log exceptions wherever
we run a handler defined in a plugin.

The flip side of this is that it's okay for plugins to throw exceptions as a
way of signalling failure!
-}
