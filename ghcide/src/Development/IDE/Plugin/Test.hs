{-# LANGUAGE DeriveAnyClass     #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GADTs              #-}
{-# LANGUAGE PolyKinds          #-}
-- | A plugin that adds custom messages for use in tests
module Development.IDE.Plugin.Test
  ( TestRequest(..)
  , WaitForIdeRuleResult(..)
  , plugin
  , blockCommandDescriptor
  , blockCommandId
  ) where

import           Control.Concurrent             (threadDelay)
import           Control.Monad
import           Control.Monad.IO.Class
import           Control.Monad.STM
import           Data.Aeson
import           Data.Aeson.Types
import           Data.Bifunctor
import           Data.CaseInsensitive           (CI, original)
import           Data.Maybe                     (isJust)
import           Data.String
import           Data.Text                      (Text, pack)
import           Development.IDE.Core.RuleTypes
import           Development.IDE.Core.Service
import           Development.IDE.Core.Shake
import           Development.IDE.GHC.Compat
import           Development.IDE.Graph          (Action)
import           Development.IDE.Types.Action
import           Development.IDE.Types.HscEnvEq (HscEnvEq (hscEnv))
import           Development.IDE.Types.Location (fromUri)
import           GHC.Generics                   (Generic)
import           Ide.Types
import qualified Language.LSP.Server            as LSP
import           Language.LSP.Types
import           System.Time.Extra

data TestRequest
    = BlockSeconds Seconds           -- ^ :: Null
    | GetInterfaceFilesDir FilePath  -- ^ :: String
    | GetShakeSessionQueueCount      -- ^ :: Number
    | WaitForShakeQueue -- ^ Block until the Shake queue is empty. Returns Null
    | WaitForIdeRule String Uri      -- ^ :: WaitForIdeRuleResult
    deriving Generic
    deriving anyclass (FromJSON, ToJSON)

newtype WaitForIdeRuleResult = WaitForIdeRuleResult { ideResultSuccess::Bool}
    deriving newtype (FromJSON, ToJSON)

plugin :: PluginDescriptor IdeState
plugin = (defaultPluginDescriptor "test") {
    pluginHandlers = mkPluginHandler (SCustomMethod "test") $ \st _ ->
        testRequestHandler' st
    }
  where
      testRequestHandler' ide req
        | Just customReq <- parseMaybe parseJSON req
        = testRequestHandler ide customReq
        | otherwise
        = return $ Left
        $ ResponseError InvalidRequest "Cannot parse request" Nothing


testRequestHandler ::  IdeState
                -> TestRequest
                -> LSP.LspM c (Either ResponseError Value)
testRequestHandler _ (BlockSeconds secs) = do
    LSP.sendNotification (SCustomMethod "ghcide/blocking/request") $
      toJSON secs
    liftIO $ sleep secs
    return (Right Null)
testRequestHandler s (GetInterfaceFilesDir fp) = liftIO $ do
    let nfp = toNormalizedFilePath fp
    sess <- runAction "Test - GhcSession" s $ use_ GhcSession nfp
    let hiPath = hiDir $ hsc_dflags $ hscEnv sess
    return $ Right (toJSON hiPath)
testRequestHandler s GetShakeSessionQueueCount = liftIO $ do
    n <- atomically $ countQueue $ actionQueue $ shakeExtras s
    return $ Right (toJSON n)
testRequestHandler s WaitForShakeQueue = liftIO $ do
    atomically $ do
        n <- countQueue $ actionQueue $ shakeExtras s
        when (n>0) retry
    return $ Right Null
testRequestHandler s (WaitForIdeRule k file) = liftIO $ do
    let nfp = fromUri $ toNormalizedUri file
    success <- runAction ("WaitForIdeRule " <> k <> " " <> show file) s $ parseAction (fromString k) nfp
    let res = WaitForIdeRuleResult <$> success
    return $ bimap mkResponseError toJSON res

mkResponseError :: Text -> ResponseError
mkResponseError msg = ResponseError InvalidRequest msg Nothing

parseAction :: CI String -> NormalizedFilePath -> Action (Either Text Bool)
parseAction "typecheck" fp = Right . isJust <$> use TypeCheck fp
parseAction "getLocatedImports" fp = Right . isJust <$> use GetLocatedImports fp
parseAction "getmodsummary" fp = Right . isJust <$> use GetModSummary fp
parseAction "getmodsummarywithouttimestamps" fp = Right . isJust <$> use GetModSummaryWithoutTimestamps fp
parseAction "getparsedmodule" fp = Right . isJust <$> use GetParsedModule fp
parseAction "ghcsession" fp = Right . isJust <$> use GhcSession fp
parseAction "ghcsessiondeps" fp = Right . isJust <$> use GhcSessionDeps fp
parseAction "gethieast" fp = Right . isJust <$> use GetHieAst fp
parseAction "getDependencies" fp = Right . isJust <$> use GetDependencies fp
parseAction "getFileContents" fp = Right . isJust <$> use GetFileContents fp
parseAction other _ = return $ Left $ "Cannot parse ide rule: " <> pack (original other)

-- | a command that blocks forever. Used for testing
blockCommandId :: Text
blockCommandId = "ghcide.command.block"

blockCommandDescriptor :: PluginId -> PluginDescriptor state
blockCommandDescriptor plId = (defaultPluginDescriptor plId) {
    pluginCommands = [PluginCommand (CommandId blockCommandId) "blocks forever" blockCommandHandler]
}

blockCommandHandler :: CommandFunction state ExecuteCommandParams
blockCommandHandler _ideState _params = do
  LSP.sendNotification (SCustomMethod "ghcide/blocking/command") Null
  liftIO $ threadDelay maxBound
  return (Right Null)
