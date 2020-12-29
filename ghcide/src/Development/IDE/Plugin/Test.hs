{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DerivingStrategies #-}
-- | A plugin that adds custom messages for use in tests
module Development.IDE.Plugin.Test
  ( TestRequest(..)
  , WaitForIdeRuleResult(..)
  , plugin
  ) where

import Control.Monad.STM
import Data.Aeson
import Data.Aeson.Types
import Data.CaseInsensitive (CI, original)
import Development.IDE.Core.Service
import Development.IDE.Core.Shake
import Development.IDE.GHC.Compat
import Development.IDE.GHC.Util (HscEnvEq(hscEnv))
import Development.IDE.LSP.Server
import Development.IDE.Plugin
import Development.IDE.Types.Action
import GHC.Generics (Generic)
import GhcPlugins (HscEnv(hsc_dflags))
import Language.Haskell.LSP.Core
import Language.Haskell.LSP.Messages
import Language.Haskell.LSP.Types
import System.Time.Extra
import Development.IDE.Core.RuleTypes
import Control.Monad
import Development.Shake (Action)
import Data.Maybe (isJust)
import Data.Bifunctor
import Data.Text (pack, Text)
import Data.String
import Development.IDE.Types.Location (fromUri)

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

plugin :: Plugin c
plugin = Plugin {
    pluginRules = return (),
    pluginHandler = PartialHandlers $ \WithMessage{..} x -> return x {
        customRequestHandler = withResponse RspCustomServer requestHandler'
    }
}
  where
      requestHandler' lsp ide req
        | Just customReq <- parseMaybe parseJSON req
        = requestHandler lsp ide customReq
        | otherwise
        = return $ Left
        $ ResponseError InvalidRequest "Cannot parse request" Nothing

requestHandler :: LspFuncs c
                -> IdeState
                -> TestRequest
                -> IO (Either ResponseError Value)
requestHandler lsp _ (BlockSeconds secs) = do
    sendFunc lsp $ NotCustomServer $
        NotificationMessage "2.0" (CustomServerMethod "ghcide/blocking/request") $
        toJSON secs
    sleep secs
    return (Right Null)
requestHandler _ s (GetInterfaceFilesDir fp) = do
    let nfp = toNormalizedFilePath fp
    sess <- runAction "Test - GhcSession" s $ use_ GhcSession nfp
    let hiPath = hiDir $ hsc_dflags $ hscEnv sess
    return $ Right (toJSON hiPath)
requestHandler _ s GetShakeSessionQueueCount = do
    n <- atomically $ countQueue $ actionQueue $ shakeExtras s
    return $ Right (toJSON n)
requestHandler _ s WaitForShakeQueue = do
    atomically $ do
        n <- countQueue $ actionQueue $ shakeExtras s
        when (n>0) retry
    return $ Right Null
requestHandler _ s (WaitForIdeRule k file) = do
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
