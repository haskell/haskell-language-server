{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DerivingStrategies #-}
-- | A plugin that adds custom messages for use in tests
module Development.IDE.Plugin.Test (TestRequest(..), plugin) where

import Control.Monad.STM
import Data.Aeson
import Data.Aeson.Types
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

data TestRequest
    = BlockSeconds Seconds           -- ^ :: Null
    | GetInterfaceFilesDir FilePath  -- ^ :: String
    | GetShakeSessionQueueCount      -- ^ :: Number
    deriving Generic
    deriving anyclass (FromJSON, ToJSON)

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

