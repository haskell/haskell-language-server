{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE GADTs             #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PolyKinds         #-}

module Development.IDE.Plugin.HLS
    (
      asGhcIdePlugin
    , Log(..)
    ) where

import           Control.Exception            (SomeException)
import           Control.Monad
import qualified Data.Aeson                   as J
import           Data.Bifunctor
import           Data.Dependent.Map           (DMap)
import qualified Data.Dependent.Map           as DMap
import           Data.Dependent.Sum
import           Data.Either
import qualified Data.List                    as List
import           Data.List.NonEmpty           (NonEmpty, nonEmpty, toList)
import qualified Data.Map                     as Map
import           Data.String
import qualified Data.Text                    as T
import           Development.IDE.Core.Shake   hiding (Log)
import           Development.IDE.Core.Tracing
import           Development.IDE.Graph        (Rules)
import           Development.IDE.LSP.Server
import           Development.IDE.Plugin
import qualified Development.IDE.Plugin       as P
import           Development.IDE.Types.Logger
import           Ide.Plugin.Config
import           Ide.PluginUtils              (getClientConfig)
import           Ide.Types                    as HLS
import qualified Language.LSP.Server          as LSP
import           Language.LSP.VFS
import           Language.LSP.Types
import qualified Language.LSP.Types           as J
import           Text.Regex.TDFA.Text         ()
import           UnliftIO                     (MonadUnliftIO)
import           UnliftIO.Async               (forConcurrently)
import           UnliftIO.Exception           (catchAny)

-- ---------------------------------------------------------------------
--

data Log
  = LogNoEnabledPlugins
  deriving Show

instance Pretty Log where
  pretty = \case
    LogNoEnabledPlugins ->
      "extensibleNotificationPlugins no enabled plugins"

-- | Map a set of plugins to the underlying ghcide engine.
asGhcIdePlugin :: Recorder (WithPriority Log) -> IdePlugins IdeState -> Plugin Config
asGhcIdePlugin recorder (IdePlugins ls) =
    mkPlugin rulesPlugins HLS.pluginRules <>
    mkPlugin executeCommandPlugins HLS.pluginCommands <>
    mkPlugin (extensiblePlugins recorder) id <>
    mkPlugin (extensibleNotificationPlugins recorder) id <>
    mkPlugin dynFlagsPlugins HLS.pluginModifyDynflags
    where

        mkPlugin :: ([(PluginId, b)] -> Plugin Config) -> (PluginDescriptor IdeState -> b) -> Plugin Config
        mkPlugin maker selector =
          case map (second selector) ls of
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

dynFlagsPlugins :: [(PluginId, DynFlagsModifications)] -> Plugin Config
dynFlagsPlugins rs = mempty
  { P.pluginModifyDynflags =
      flip foldMap rs $ \(plId, dflag_mods) cfg ->
        let plg_cfg = configForPlugin cfg plId
         in if plcGlobalOn plg_cfg
              then dflag_mods
              else mempty
  }

-- ---------------------------------------------------------------------

executeCommandPlugins :: [(PluginId, [PluginCommand IdeState])] -> Plugin Config
executeCommandPlugins ecs = mempty { P.pluginHandlers = executeCommandHandlers ecs }

executeCommandHandlers :: [(PluginId, [PluginCommand IdeState])] -> LSP.Handlers (ServerM Config)
executeCommandHandlers ecs = requestHandler SWorkspaceExecuteCommand execCmd
  where
    pluginMap = Map.fromList ecs

    parseCmdId :: T.Text -> Maybe (PluginId, CommandId)
    parseCmdId x = case T.splitOn ":" x of
      [plugin, command]    -> Just (PluginId plugin, CommandId command)
      [_, plugin, command] -> Just (PluginId plugin, CommandId command)
      _                    -> Nothing

    -- The parameters to the HLS command are always the first element

    execCmd ide (ExecuteCommandParams _ cmdId args) = do
      let cmdParams :: J.Value
          cmdParams = case args of
            Just (J.List (x:_)) -> x
            _                   -> J.Null
      case parseCmdId cmdId of
        -- Shortcut for immediately applying a applyWorkspaceEdit as a fallback for v3.8 code actions
        Just ("hls", "fallbackCodeAction") ->
          case J.fromJSON cmdParams of
            J.Success (FallbackCodeActionParams mEdit mCmd) -> do

              -- Send off the workspace request if it has one
              forM_ mEdit $ \edit ->
                LSP.sendRequest SWorkspaceApplyEdit (ApplyWorkspaceEditParams Nothing edit) (\_ -> pure ())

              case mCmd of
                -- If we have a command, continue to execute it
                Just (J.Command _ innerCmdId innerArgs)
                    -> execCmd ide (ExecuteCommandParams Nothing innerCmdId innerArgs)
                Nothing -> return $ Right J.Null

            J.Error _str -> return $ Right J.Null

        -- Just an ordinary HIE command
        Just (plugin, cmd) -> runPluginCommand ide plugin cmd cmdParams

        -- Couldn't parse the command identifier
        _ -> return $ Left $ ResponseError InvalidParams "Invalid command identifier" Nothing

    runPluginCommand ide p@(PluginId p') com@(CommandId com') arg =
      case Map.lookup p pluginMap  of
        Nothing -> return
          (Left $ ResponseError InvalidRequest ("Plugin " <> p' <> " doesn't exist") Nothing)
        Just xs -> case List.find ((com ==) . commandId) xs of
          Nothing -> return $ Left $
            ResponseError InvalidRequest ("Command " <> com' <> " isn't defined for plugin " <> p'
                                          <> ". Legal commands are: " <> T.pack(show $ map commandId xs)) Nothing
          Just (PluginCommand _ _ f) -> case J.fromJSON arg of
            J.Error err -> return $ Left $
              ResponseError InvalidParams ("error while parsing args for " <> com' <> " in plugin " <> p'
                                           <> ": " <> T.pack err
                                           <> "\narg = " <> T.pack (show arg)) Nothing
            J.Success a -> f ide a

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
        case nonEmpty fs of
          Nothing -> do
            logWith recorder Info LogNoEnabledPlugins
            pure $ Left $ ResponseError InvalidRequest
              (  "No plugin enabled for " <> T.pack (show m) 
              <> ", available: " <> T.pack (show $ map (\(plid,_,_) -> plid) fs)
              )
              Nothing
          Just fs -> do
            let msg e pid = "Exception in plugin " <> T.pack (show pid) <> "while processing " <> T.pack (show m) <> ": " <> T.pack (show e)
                handlers = fmap (\(plid,_,handler) -> (plid,handler)) fs
            es <- runConcurrently msg (show m) handlers ide params
            let (errs,succs) = partitionEithers $ toList es
            case nonEmpty succs of
              Nothing -> pure $ Left $ combineErrors errs
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
            logWith recorder Info LogNoEnabledPlugins
            pure ()
          Just fs -> do
            -- We run the notifications in order, so the core ghcide provider
            -- (which restarts the shake process) hopefully comes last
            mapM_ (\(pid,_,f) -> otTracedProvider pid (fromString $ show m) $ f ide vfs params) fs

-- ---------------------------------------------------------------------

runConcurrently
  :: MonadUnliftIO m
  => (SomeException -> PluginId -> T.Text)
  -> String -- ^ label
  -> NonEmpty (PluginId, a -> b -> m (NonEmpty (Either ResponseError d)))
  -- ^ Enabled plugin actions that we are allowed to run
  -> a
  -> b
  -> m (NonEmpty (Either ResponseError d))
runConcurrently msg method fs a b = fmap join $ forConcurrently fs $ \(pid,f) -> otTracedProvider pid (fromString method) $ do
  f a b
    `catchAny` (\e -> pure $ pure $ Left $ ResponseError InternalError (msg e pid) Nothing)

combineErrors :: [ResponseError] -> ResponseError
combineErrors [x] = x
combineErrors xs  = ResponseError InternalError (T.pack (show xs)) Nothing

-- | Combine the 'PluginHandler' for all plugins
newtype IdeHandler (m :: J.Method FromClient Request)
  = IdeHandler [(PluginId, PluginDescriptor IdeState, IdeState -> MessageParams m -> LSP.LspM Config (NonEmpty (Either ResponseError (ResponseResult m))))]

-- | Combine the 'PluginHandler' for all plugins
newtype IdeNotificationHandler (m :: J.Method FromClient Notification)
  = IdeNotificationHandler [(PluginId, PluginDescriptor IdeState, IdeState -> VFS -> MessageParams m -> LSP.LspM Config ())]
-- type NotificationHandler (m :: Method FromClient Notification) = MessageParams m -> IO ()`

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
