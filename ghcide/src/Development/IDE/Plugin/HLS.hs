{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}

module Development.IDE.Plugin.HLS
    (
      asGhcIdePlugin
    ) where

import           Control.Exception(SomeException)
import           Control.Lens ((^.))
import           Control.Monad
import           Control.Monad.IO.Class
import qualified Data.Aeson as J
import qualified Data.DList as DList
import           Data.Either
import qualified Data.List                     as List
import qualified Data.Map  as Map
import           Data.Maybe
import qualified Data.Text                     as T
import           Development.IDE.Core.Shake
import           Development.IDE.LSP.Server
import           Development.IDE.Plugin
import           Ide.Plugin.Config
import           Ide.PluginUtils
import           Ide.Types as HLS
import qualified Language.LSP.Server             as LSP
import qualified Language.LSP.Types              as J
import qualified Language.LSP.Types.Capabilities as C
import Language.LSP.Types
import           Language.LSP.Types.Lens as L hiding (formatting, rangeFormatting)
import qualified Language.LSP.VFS                as VFS
import           Text.Regex.TDFA.Text()
import Development.Shake (Rules)
import Ide.PluginUtils (getClientConfig, pluginEnabled, getPluginConfig, responseError, getProcessID)
import Development.IDE.Core.Tracing
import Development.IDE.Types.Logger (logDebug)
import UnliftIO.Async (forConcurrently)
import UnliftIO.Exception (catchAny)
import           Data.Dependent.Map (DMap)
import qualified Data.Dependent.Map as DMap
import           Data.Dependent.Sum
import Data.List.NonEmpty (nonEmpty,NonEmpty,toList)
import UnliftIO (MonadUnliftIO)

-- ---------------------------------------------------------------------
--

-- | Map a set of plugins to the underlying ghcide engine.
asGhcIdePlugin :: IdePlugins IdeState -> Plugin Config
asGhcIdePlugin mp =
    mkPlugin rulesPlugins          HLS.pluginRules <>
    mkPlugin executeCommandPlugins HLS.pluginCommands <>
    mkPlugin extensiblePlugins     HLS.pluginHandlers
    where
        ls = Map.toList (ipMap mp)

        mkPlugin :: ([(PluginId, b)] -> Plugin Config) -> (PluginDescriptor IdeState -> b) -> Plugin Config
        mkPlugin maker selector =
          case map (\(pid, p) -> (pid, selector p)) ls of
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

-- ---------------------------------------------------------------------

executeCommandPlugins :: [(PluginId, [PluginCommand IdeState])] -> Plugin Config
executeCommandPlugins ecs = Plugin mempty (executeCommandHandlers ecs)

executeCommandHandlers :: [(PluginId, [PluginCommand IdeState])] -> LSP.Handlers (ServerM Config)
executeCommandHandlers ecs = requestHandler SWorkspaceExecuteCommand execCmd
  where
    pluginMap = Map.fromList ecs

    parseCmdId :: T.Text -> Maybe (PluginId, CommandId)
    parseCmdId x = case T.splitOn ":" x of
      [plugin, command] -> Just (PluginId plugin, CommandId command)
      [_, plugin, command] -> Just (PluginId plugin, CommandId command)
      _ -> Nothing

    -- The parameters to the HLS command are always the first element

    execCmd ide (ExecuteCommandParams _ cmdId args) = do
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

extensiblePlugins :: [(PluginId, PluginHandlers IdeState)] -> Plugin Config
extensiblePlugins xs = Plugin mempty handlers
  where
    IdeHandlers handlers' = foldMap bakePluginId xs
    bakePluginId :: (PluginId, PluginHandlers IdeState) -> IdeHandlers
    bakePluginId (pid,PluginHandlers hs) = IdeHandlers $ DMap.map
      (\(PluginHandler f) -> IdeHandler [(pid,f pid)])
      hs
    handlers = mconcat $ do
      (IdeMethod m :=> IdeHandler fs') <- DMap.assocs handlers'
      pure $ requestHandler m $ \ide params -> do
        pid <- liftIO getPid
        config <- getClientConfig
        let fs = filter (\(pid,_) -> pluginEnabled m pid config) fs'
        case nonEmpty fs of
          Nothing -> pure $ Left $ ResponseError InvalidRequest
            ("No plugin enabled for " <> T.pack (show m) <> ", available: " <> T.pack (show $ map fst fs))
            Nothing
          Just fs -> do
            let msg e pid = "Exception in plugin " <> T.pack (show pid) <> "while processing " <> T.pack (show m) <> ": " <> T.pack (show e)
            es <- runConcurrently msg fs ide params
            let (errs,succs) = partitionEithers $ toList es
            case nonEmpty succs of
              Nothing -> pure $ Left $ combineErrors errs
              Just xs -> do
                caps <- LSP.getClientCapabilities
                pure $ Right $ combineResponses m pid config caps params xs

runConcurrently
  :: MonadUnliftIO m
  => (SomeException -> PluginId -> T.Text)
  -> NonEmpty (PluginId, a -> b -> m (NonEmpty (Either ResponseError d)))
  -> a
  -> b
  -> m (NonEmpty (Either ResponseError d))
runConcurrently msg fs a b = fmap join $ forConcurrently fs $ \(pid,f) ->
  f a b
    `catchAny` (\e -> pure $ pure $ Left $ ResponseError InternalError (msg e pid) Nothing)

combineErrors :: [ResponseError] -> ResponseError
combineErrors [x] = x
combineErrors xs = ResponseError InternalError (T.pack (show xs)) Nothing

-- | Combine the 'PluginHandler' for all plugins
newtype IdeHandler (m :: J.Method FromClient Request)
  = IdeHandler [(PluginId,(IdeState -> MessageParams m -> LSP.LspM Config (NonEmpty (Either ResponseError (ResponseResult m)))))]

-- | Combine the 'PluginHandlers' for all plugins
newtype IdeHandlers = IdeHandlers (DMap IdeMethod IdeHandler)

instance Semigroup IdeHandlers where
  (IdeHandlers a) <> (IdeHandlers b) = IdeHandlers $ DMap.unionWithKey go a b
    where
      go _ (IdeHandler a) (IdeHandler b) = IdeHandler (a ++ b)
instance Monoid IdeHandlers where
  mempty = IdeHandlers mempty
