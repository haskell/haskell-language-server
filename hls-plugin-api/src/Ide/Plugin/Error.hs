{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE LambdaCase                #-}
{-# LANGUAGE OverloadedStrings         #-}
module Ide.Plugin.Error (
      -- * Plugin Error Handling API
    PluginError(..),
    runExceptT,
    withExceptT,
    pluginResponseM,
    hoistExceptT,
    hoistMaybeT,
    handleMaybe,
    handleMaybeM,
    getNormalizedFilePathE,
) where

import           Control.Monad.Extra         (maybeM)
import           Control.Monad.IO.Class      (MonadIO, liftIO)
import           Control.Monad.Trans.Class   (lift)
import           Control.Monad.Trans.Except  (ExceptT (..), mapExceptT,
                                              runExceptT, throwE, withExceptT)
import           Control.Monad.Trans.Maybe   (MaybeT, mapMaybeT)
import qualified Data.Text                   as T
import           Language.LSP.Protocol.Types
import           Prettyprinter

-- ----------------------------------------------------------------------------
-- Plugin Error wrapping
-- ----------------------------------------------------------------------------

pluginResponseM :: Monad m => (t -> m (Either a b)) -> ExceptT t m b -> m (Either a b)
pluginResponseM handler act =
    runExceptT act >>= \case
        Right r  -> pure $ Right r
        Left err -> handler err

-- See Note [PluginError]
data PluginError
  = -- |PluginInternalError should be used if something has gone horribly wrong.
    -- All uncaught exceptions will be caught and converted to this error.
    --
    -- This error will be logged individually with Error, and will be converted
    -- into an InternalError response code. It takes the highest precedence (1)
    -- in being returned as a response to the client.
    PluginInternalError T.Text
    -- |PluginInvalidParams should be used if the parameters of the request are
    -- invalid. This error means that there is a bug in the client's code
    -- (otherwise they wouldn't be sending you requests with invalid
    -- parameters).
    --
    -- This error will be logged individually with Warning, and will be
    -- converted into a InvalidParams response code. It takes medium precedence
    --  (2)in being returned as a response to the client.
  | PluginInvalidParams T.Text
    -- |PluginInvalidRequest should be used if the request is invalid. This
    -- error means that there is a bug in the client's code (otherwise they
    -- wouldn't be sending you an invalid request).
    --
    -- This error will be logged individually with Warning, and will be
    -- converted into a InvalidRequest response code. It takes medium precedence
    --  (2) in being returned as a response to the client.
  | PluginInvalidRequest T.Text
    -- |PluginParseError should be used sparingly for parse errors. Prefer to
    -- use PluginInternalError for bugs in the plugin's code, or
    -- PluginInvalidRequest/PluginInvalidParams for invalid requests from the
    -- client.
    --
    -- This error will be logged individually with Warning, and will be
    -- converted into a ParseError response code. It takes a medium precedence
    --  (2) in being returned as a response to the client.
  | PluginParseError T.Text
    -- |PluginDependencyFailed should be thrown when a function that your plugin
    -- depends on fails. This should only be used when the function fails
    -- because the files the user is working on is in an invalid state.
    --
    -- This error takes the name of the function that failed. Prefer to catch
    -- this error as close to the source as possible.
    --
    -- This error will be logged together with other errors of the same type
    -- with Info, and will be converted into a ContentModified response. It
    -- takes a low precedence (3) in being returned as a response to the client.
  | PluginDependencyFailed T.Text
    -- |PluginRequestRefused allows your handler to inspect a request before
    -- rejecting it. In effect it allows your plugin to act make a secondary
    -- `pluginEnabled` decision after receiving the request. This should only be
    -- used if the decision to accept the request can not be made in
    -- `pluginEnabled`.
    --
    -- This error will be logged together with other errors of the same type
    -- with Info. If it's the only response to a request, HLS will respond as if
    -- no plugins passed the `pluginEnabled` stage.
  | PluginRequestRefused
    -- |PluginRuleFailed should be thrown when a Rule your response depends on
    -- fails.
    --
    -- This error takes the name of the Rule that failed.
    --
    -- This error will be logged together with other errors of the same type
    -- with Info, and will be converted into a ContentModified response code. It
    -- takes a low precedence (3) in being returned as a response to the client.
  | PluginRuleFailed T.Text
    -- |PluginStaleResolve should be thrown when your resolve request is
    -- provided with data it can no longer resolve.
    --
    -- This error will be logged individually with Info, and will be converted
    -- into a ContentModified response. It takes a low precedence (3) in being
    -- returned as a response to the client.
  | PluginStaleResolve

instance Pretty PluginError where
    pretty = \case
      PluginInternalError msg -> "Internal Error:" <+> pretty msg
      PluginStaleResolve -> "Stale Resolve"
      PluginRuleFailed rule       -> "Rule Failed:" <+> pretty rule
      PluginInvalidParams text -> "Invalid Params:" <+> pretty text
      PluginParseError text -> "Parse Error:" <+> pretty text
      PluginInvalidRequest text -> "Invalid Request:" <+> pretty text
      PluginDependencyFailed text -> "Dependency Failed:" <+> pretty text
      PluginRequestRefused -> "Request Refused"

handleMaybe :: Monad m => e -> Maybe b -> ExceptT e m b
handleMaybe msg = maybe (throwE msg) return

handleMaybeM :: Monad m => e -> m (Maybe b) -> ExceptT e m b
handleMaybeM msg act = maybeM (throwE msg) return $ lift act

hoistExceptT :: MonadIO m => ExceptT e IO a -> ExceptT e m a
hoistExceptT = mapExceptT liftIO

hoistMaybeT :: MonadIO m => MaybeT IO a -> MaybeT m a
hoistMaybeT = mapMaybeT liftIO

getNormalizedFilePathE :: Monad m => Uri -> ExceptT PluginError m NormalizedFilePath
getNormalizedFilePathE uri = handleMaybe (PluginInvalidParams (T.pack $ "uriToNormalizedFile failed. Uri:" <>  show uri))
        $ uriToNormalizedFilePath
        $ toNormalizedUri uri

-- ---------------------------------------------------------------------
{- Note [PluginError]
-- Each PluginError corresponds to either a specific ResponseError we want to
-- return or a specific way we want to log the error. If the currently present
-- ones are insufficient for the needs of your plugin, please feel free to add
-- a new one.
-- Currently the PluginErrors we provide can be broken up into several groups.
-- First is PluginInternalError, which is the most serious of the errors, and
-- also the "default" error that is used for things such as uncaught exceptions.
-- Then we have PluginInvalidRequest, PluginInvalidParams, and PluginParseError.
-- All three of these along with PluginInternalError are treated individually
-- and map to a corresponding ResponseError.
-- Next we have PluginRuleFailed and PluginDependencyFailed, with the only
-- difference being PluginRuleFailed is specific to Shake rules and
-- PluginDependencyFailed can be used for everything else. Both of these are
-- "non-errors", and happen whenever the user's code is in a state where the
-- plugin is unable to provide a answer to the users request. PluginStaleResolve
-- is similar to the above two Error types, but is specific to resolve plugins,
-- and is used only when the data provided by the resolve request is stale,
-- preventing the proper resolution of it.
-- Finally we have the outlier, PluginRequestRefused, where we allow a handler
-- to preform "pluginEnabled" checks inside the handler, and reject the request
-- after viewing it. The behavior of only one handler passing `pluginEnabled`
-- and then returning PluginRequestRefused should be the same as if no plugins
-- passed the `pluginEnabled` stage.
-}
