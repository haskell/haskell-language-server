{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE LambdaCase                #-}
{-# LANGUAGE OverloadedStrings         #-}
module Ide.Plugin.Error (
      -- * Plugin Error Handling API
    PluginError(..),
    runExceptT,
    runExceptT,
    pluginResponseM,
    handlePluginError,
    hoistExceptT,
    hoistMaybeT,
    handleMaybe,
    handleMaybeM,
    withError,
    getNormalizedFilePathE,
) where

import           Control.Monad.Extra           (maybeM)
import           Control.Monad.IO.Class        (MonadIO, liftIO)
import           Control.Monad.Trans.Class     (lift)
import           Control.Monad.Trans.Except    (ExceptT (..), mapExceptT,
                                                runExceptT, throwE, withExceptT)
import           Data.Bifunctor                (Bifunctor (first))
import           Data.String

import           Control.Monad.Trans.Maybe     (MaybeT, mapMaybeT)
import qualified Data.Text                     as T
import           Language.LSP.Protocol.Message
import           Language.LSP.Protocol.Types
import           Prettyprinter
import           Prettyprinter.Render.Text     (renderStrict)

-- ----------------------------------------------------------------------------
-- Plugin Error wrapping
-- ----------------------------------------------------------------------------

pluginResponseM :: Monad m => (t -> m (Either a b)) -> ExceptT t m b -> m (Either a b)
pluginResponseM handler act =
    runExceptT act >>= \case
        Right r  -> pure $ Right r
        Left err -> handler err

handlePluginError :: PluginError -> ResponseError
handlePluginError msg = ResponseError (InR ErrorCodes_InternalError) (renderStrict simpleDoc) Nothing
  where simpleDoc = layoutPretty defaultLayoutOptions $ pretty msg

data PluginError
  = PluginInternalError T.Text
  | PluginInvalidParams T.Text
  | PluginParseError T.Text
  | PluginInvalidRequest T.Text
  | PluginStaleResolve
  | PluginBadDependency T.Text
  | forall a . Show a => FastRuleNotReady a
  | forall a . Show a => RuleFailed a

instance Pretty PluginError where
    pretty = \case
      PluginInternalError msg -> "Internal Plugin Error: " <+> viaShow msg
      PluginStaleResolve -> "Stale Resolve"
      FastRuleNotReady rule -> "FastRuleNotReady:" <+> viaShow rule
      RuleFailed rule       -> "RuleFailed:" <+> viaShow rule
      PluginInvalidParams text -> "Invalid Params:" <+> viaShow text
      PluginParseError text -> "Parse Error:" <+> viaShow text
      PluginInvalidRequest text -> "Invalid Request:" <+> viaShow text
      PluginBadDependency text -> "Bad dependency" <+> viaShow text


handleMaybe :: Monad m => e -> Maybe b -> ExceptT e m b
handleMaybe msg = maybe (throwE msg) return

handleMaybeM :: Monad m => e -> m (Maybe b) -> ExceptT e m b
handleMaybeM msg act = maybeM (throwE msg) return $ lift act

withError :: Functor m => (e' -> e) -> ExceptT e' m a -> ExceptT e m a
withError = withExceptT

hoistExceptT :: MonadIO m => ExceptT e IO a -> ExceptT e m a
hoistExceptT = mapExceptT liftIO

hoistMaybeT :: MonadIO m => MaybeT IO a -> MaybeT m a
hoistMaybeT = mapMaybeT liftIO

getNormalizedFilePathE :: Monad m => Uri -> ExceptT PluginError m NormalizedFilePath
getNormalizedFilePathE uri = handleMaybe (PluginInvalidParams (T.pack $ "uriToNormalizedFile failed. Uri:" <>  show uri))
        $ uriToNormalizedFilePath
        $ toNormalizedUri uri

-- ---------------------------------------------------------------------
