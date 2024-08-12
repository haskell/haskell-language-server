{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
module Ide.Plugin.Error (
      -- * Plugin Error Handling API
    PluginError(..),
    toErrorCode,
    toPriority,
    handleMaybe,
    handleMaybeM,
    getNormalizedFilePathE,
) where

import           Control.Monad.Extra           (maybeM)
import           Control.Monad.Trans.Class     (lift)
import           Control.Monad.Trans.Except    (ExceptT (..), throwE)
import qualified Data.Text                     as T
import           Ide.Logger
import           Ide.Plugin.HandleRequestTypes (RejectionReason)
import           Language.LSP.Protocol.Types


-- ----------------------------------------------------------------------------
-- Plugin Error wrapping
-- ----------------------------------------------------------------------------

-- |Each PluginError corresponds to either a specific ResponseError we want to
-- return or a specific way we want to log the error. If the currently present
-- ones are insufficient for the needs of your plugin, please feel free to add
-- a new one.
--
-- Currently the PluginErrors we provide can be broken up into several groups.
-- First is PluginInternalError, which is the most serious of the errors, and
-- also the "default" error that is used for things such as uncaught exceptions.
-- Then we have PluginInvalidParams, which along with PluginInternalError map
-- to a corresponding ResponseError.
--
-- Next we have PluginRuleFailed and PluginInvalidUserState, with the only
-- difference being PluginRuleFailed is specific to Shake rules and
-- PluginInvalidUserState can be used for everything else. Both of these are
-- "non-errors", and happen whenever the user's code is in a state where the
-- plugin is unable to provide a answer to the users request. PluginStaleResolve
-- is similar to the above two Error types, but is specific to resolve plugins,
-- and is used only when the data provided by the resolve request is stale,
-- preventing the proper resolution of it.
--
-- Finally we have the outlier, PluginRequestRefused, where we allow a handler
-- to preform "pluginEnabled" checks inside the handler, and reject the request
-- after viewing it. The behavior of only one handler passing `pluginEnabled`
-- and then returning PluginRequestRefused should be the same as if no plugins
-- passed the `pluginEnabled` stage.
data PluginError
  = -- |PluginInternalError should be used if an error has occurred. This
    -- should only rarely be returned. As it's logged with Error, it will be
    -- shown by the client to the user via `showWindow`. All uncaught exceptions
    -- will be caught and converted to this error.
    --
    -- This error will be be converted into an InternalError response code. It
    -- will be logged with Error and takes the highest precedence (1) in being
    -- returned as a response to the client.
    PluginInternalError T.Text
    -- |PluginInvalidParams should be used if the parameters of the request are
    -- invalid. This error means that there is a bug in the client's code
    -- (otherwise they wouldn't be sending you requests with invalid
    -- parameters).
    --
    -- This error will be will be converted into a InvalidParams response code.
    -- It will be logged with Warning and takes medium precedence (2) in being
    -- returned as a response to the client.
  | PluginInvalidParams T.Text
    -- |PluginInvalidUserState should be thrown when a function that your plugin
    -- depends on fails. This should only be used when the function fails
    -- because the user's code is in an invalid state.
    --
    -- This error takes the name of the function that failed. Prefer to catch
    -- this error as close to the source as possible.
    --
    -- This error will be logged with Debug, and will be converted into a
    -- RequestFailed response. It takes a low precedence (3) in being returned
    -- as a response to the client.
  | PluginInvalidUserState T.Text
    -- |PluginRequestRefused allows your handler to inspect a request before
    -- rejecting it. In effect it allows your plugin to act make a secondary
    -- `handlesRequest` decision after receiving the request. This should only be
    -- used if the decision to accept the request can not be made in
    -- `handlesRequest`.
    --
    -- This error will be with Debug. If it's the only response to a request,
    -- HLS will respond as if no plugins passed the `handlesRequest` stage.
  | PluginRequestRefused RejectionReason
    -- |PluginRuleFailed should be thrown when a Rule your response depends on
    -- fails.
    --
    -- This error takes the name of the Rule that failed.
    --
    -- This error will be logged with Debug, and will be converted into a
    -- RequestFailed response code. It takes a low precedence (3) in being
    -- returned as a response to the client.
  | PluginRuleFailed T.Text
    -- |PluginStaleResolve should be thrown when your resolve request is
    -- provided with data it can no longer resolve.
    --
    -- This error will be logged with Debug, and will be converted into a
    -- ContentModified response. It takes a low precedence (3) in being returned
    -- as a response to the client.
  | PluginStaleResolve

instance Pretty PluginError where
    pretty = \case
      PluginInternalError msg     -> "Internal Error:"     <+> pretty msg
      PluginStaleResolve          -> "Stale Resolve"
      PluginRuleFailed rule       -> "Rule Failed:"        <+> pretty rule
      PluginInvalidParams text    -> "Invalid Params:"     <+> pretty text
      PluginInvalidUserState text -> "Invalid User State:" <+> pretty text
      PluginRequestRefused msg    -> "Request Refused: "   <+> pretty msg

-- |Converts to ErrorCode used in LSP ResponseErrors
toErrorCode :: PluginError -> (LSPErrorCodes |? ErrorCodes)
toErrorCode (PluginInternalError _)    = InR ErrorCodes_InternalError
toErrorCode (PluginInvalidParams _)    = InR ErrorCodes_InvalidParams
toErrorCode (PluginInvalidUserState _) = InL LSPErrorCodes_RequestFailed
-- PluginRequestRefused should never be a argument to `toResponseError`, as
-- it should be dealt with in `extensiblePlugins`, but this is here to make
-- this function complete
toErrorCode (PluginRequestRefused _)   = InR ErrorCodes_MethodNotFound
toErrorCode (PluginRuleFailed _)       = InL LSPErrorCodes_RequestFailed
toErrorCode PluginStaleResolve         = InL LSPErrorCodes_ContentModified

-- |Converts to a logging priority. In addition to being used by the logger,
-- `combineResponses` currently uses this to  choose which response to return,
-- so care should be taken in changing it.
toPriority :: PluginError -> Priority
toPriority (PluginInternalError _)    = Error
toPriority (PluginInvalidParams _)    = Warning
toPriority (PluginInvalidUserState _) = Debug
toPriority (PluginRequestRefused _)   = Debug
toPriority (PluginRuleFailed _)       = Debug
toPriority PluginStaleResolve         = Debug

handleMaybe :: Monad m => e -> Maybe b -> ExceptT e m b
handleMaybe msg = maybe (throwE msg) return

handleMaybeM :: Monad m => e -> m (Maybe b) -> ExceptT e m b
handleMaybeM msg act = maybeM (throwE msg) return $ lift act

getNormalizedFilePathE :: Monad m => Uri -> ExceptT PluginError m (NormalizedFilePath)
getNormalizedFilePathE uri = handleMaybe (PluginInvalidParams (T.pack $ "uriToNormalizedFile failed. Uri:" <>  show uri))
        $ uriToNormalizedFilePath
        $ toNormalizedUri uri
