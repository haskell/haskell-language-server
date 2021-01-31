{-# LANGUAGE DeriveAnyClass #-}
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
import           GHC.Generics
import           Ide.Plugin.Config
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

-- | Map a set of plugins to the underlying ghcide engine.  Main point is
-- IdePlugins are arranged by kind of operation, 'Plugin' is arranged by message
-- category ('Notifaction', 'Request' etc).
asGhcIdePlugin :: IdePlugins IdeState -> Plugin Config
asGhcIdePlugin mp =
    mkPlugin rulesPlugins (Just . HLS.pluginRules) <>
    -- mkPlugin executeCommandPlugins (Just . pluginCommands) <>
    mkPlugin extensiblePlugins     (Just . HLS.pluginHandlers)
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

extensiblePlugins :: [(PluginId, PluginHandlers IdeState)] -> Plugin Config
extensiblePlugins xs = Plugin mempty handlers
  where
    IdeHandlers handlers' = foldMap bakePluginId xs
    bakePluginId :: (PluginId, PluginHandlers IdeState) -> IdeHandlers IdeState
    bakePluginId (pid,PluginHandlers hs) = IdeHandlers $ DMap.map
      (\(PluginHandler f) -> IdeHandler [(pid,f pid)])
      hs
    handlers = mconcat $ do
      (IdeMethod m :=> IdeHandler fs') <- DMap.assocs handlers'
      pure $ requestHandler m $ \ide params -> do
        config <- getClientConfig
        let fs = filter (\(pid,_) -> pluginEnabled m pid config) fs'
        case nonEmpty fs of
          Nothing -> pure $ Left $ ResponseError InvalidRequest
            ("No plugin enabled for " <> T.pack (show m) <> ", available: " <> T.pack (show $ map fst fs))
            Nothing
          Just fs -> do
            ex <- getExtraParams m params
            case ex of
              Left err -> pure $ Left err
              Right ex -> do
                let msg e pid = "Exception in plugin " <> T.pack (show pid) <> "while processing " <> T.pack (show m) <> ": " <> T.pack (show e)
                es <- runConcurrently msg fs ide ex params
                let (errs,succs) = partitionEithers $ toList es
                case nonEmpty succs of
                  Nothing -> pure $ Left $ combineErrors errs
                  Just xs -> do
                    caps <- LSP.getClientCapabilities
                    pure $ Right $ combineResponses m config caps params xs

runConcurrently
  :: MonadUnliftIO m
  => (SomeException -> PluginId -> T.Text)
  -> NonEmpty (PluginId, a -> b -> c -> m (NonEmpty (Either ResponseError d)))
  -> a
  -> b
  -> c
  -> m (NonEmpty (Either ResponseError d))
runConcurrently msg fs a b c = fmap join $ forConcurrently fs $ \(pid,f) ->
  f a b c
    `catchAny` (\e -> pure $ pure $ Left $ ResponseError InternalError (msg e pid) Nothing)

combineErrors :: [ResponseError] -> ResponseError
combineErrors [x] = x
combineErrors xs = ResponseError InternalError (T.pack (show xs)) Nothing

-- | Combine the 'PluginHandler' for all plugins
newtype IdeHandler a (m :: J.Method FromClient Request)
  = IdeHandler [(PluginId,(a -> ExtraParams m -> MessageParams m -> LSP.LspM Config (NonEmpty (Either ResponseError (ResponseResult m)))))]

-- | Combine the 'PluginHandlers' for all plugins
newtype IdeHandlers a = IdeHandlers (DMap IdeMethod (IdeHandler a))

instance Semigroup (IdeHandlers a) where
  (IdeHandlers a) <> (IdeHandlers b) = IdeHandlers $ DMap.unionWithKey go a b
    where
      go _ (IdeHandler a) (IdeHandler b) = IdeHandler (a ++ b)
instance Monoid (IdeHandlers a) where
  mempty = IdeHandlers mempty
