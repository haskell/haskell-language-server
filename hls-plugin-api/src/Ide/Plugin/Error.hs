{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE LambdaCase                #-}
{-# LANGUAGE OverloadedStrings         #-}
module Ide.Plugin.Error (
      -- * Plugin Error Handling API
    PluginError(..),
    pluginResponse,
    pluginResponse',
    pluginResponseM,
    handlePluginError,
    mkPluginErrorMessage,
    hoistExceptT,
    handleMaybe,
    handleMaybeM,
    mkSimpleResponseError,
    withError,
) where


import           Control.Monad.Extra           (maybeM)
import           Control.Monad.IO.Class        (MonadIO, liftIO)
import           Control.Monad.Trans.Class     (lift)
import           Control.Monad.Trans.Except    (ExceptT (..), mapExceptT,
                                                runExceptT, throwE, withExceptT)
import           Data.Bifunctor                (Bifunctor (first))

import qualified Data.Text                     as T
import           Language.LSP.Protocol.Message
import           Language.LSP.Protocol.Types
import           Prettyprinter
import           Prettyprinter.Render.Text     (renderStrict)

type PluginHandler e m a = ExceptT e m a

-- ----------------------------------------------------------------------------
-- Plugin Error wrapping
-- ----------------------------------------------------------------------------


pluginResponse :: Monad m => ExceptT String m a -> m (Either ResponseError a)
pluginResponse =
  fmap (first (\msg -> ResponseError (InR ErrorCodes_InternalError) (T.pack msg) Nothing))
    . runExceptT

pluginResponse' :: Monad m => ExceptT PluginError m a -> m (Either ResponseError a)
pluginResponse' =
  fmap (first handlePluginError)
    . runExceptT

pluginResponseM :: Monad m => (t -> m (Either a b)) -> ExceptT t m b -> m (Either a b)
pluginResponseM handler act =
    runExceptT act >>= \case
        Right r  -> pure $ Right r
        Left err -> handler err

handlePluginError :: PluginError -> ResponseError
handlePluginError msg = ResponseError (InR ErrorCodes_InternalError) (renderStrict simpleDoc) Nothing
  where simpleDoc = layoutPretty defaultLayoutOptions $ pretty msg

data PluginError
  = PluginInternalError
  | PluginUriToFilePath Uri
  | PluginUriToNormalizedFilePath Uri
  | PluginErrorMessage T.Text
  | forall a . Show a => FastRuleNotReady a
  | forall a . Show a => RuleFailed a

instance Pretty PluginError where
    pretty = \case
      PluginInternalError -> "Internal Plugin Error"
      PluginUriToFilePath uri -> "Failed to translate URI " <+> viaShow uri
      PluginUriToNormalizedFilePath uri -> "Failed converting " <+> viaShow uri <+> " to NormalizedFilePath"
      PluginErrorMessage msg -> "Plugin failed: " <+> viaShow msg
      FastRuleNotReady rule -> "FastRuleNotReady:" <+> viaShow rule
      RuleFailed rule       -> "RuleFailed:" <+> viaShow rule

mkPluginErrorMessage :: T.Text -> PluginError
mkPluginErrorMessage = PluginErrorMessage

mkSimpleResponseError :: T.Text -> ResponseError
mkSimpleResponseError err = ResponseError (InR ErrorCodes_InternalError) err Nothing

handleMaybe :: Monad m => e -> Maybe b -> ExceptT e m b
handleMaybe msg = maybe (throwE msg) return

handleMaybeM :: Monad m => e -> m (Maybe b) -> ExceptT e m b
handleMaybeM msg act = maybeM (throwE msg) return $ lift act

withError :: Functor m => (e' -> e) -> ExceptT e' m a -> ExceptT e m a
withError = withExceptT

hoistExceptT :: MonadIO m => ExceptT e IO a -> ExceptT e m a
hoistExceptT = mapExceptT liftIO
