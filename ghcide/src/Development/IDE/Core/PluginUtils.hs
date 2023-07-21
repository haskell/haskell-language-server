{-# LANGUAGE GADTs #-}
module Development.IDE.Core.PluginUtils where

import           Control.Monad.Extra
import           Control.Monad.IO.Class
import           Control.Monad.Reader                 (runReaderT)
import           Control.Monad.Trans.Except
import           Control.Monad.Trans.Maybe
import           Data.Either.Extra                    (maybeToEither)
import           Data.Functor.Identity
import qualified Data.Text                            as T
import           Development.IDE.Core.PositionMapping
import           Development.IDE.Core.Shake           (IdeAction, IdeRule,
                                                       IdeState (shakeExtras),
                                                       mkDelayedAction,
                                                       shakeEnqueue)
import qualified Development.IDE.Core.Shake           as Shake
import           Development.IDE.GHC.Orphans          ()
import           Development.IDE.Graph                hiding (ShakeValue)
import           Development.IDE.Types.Location       (NormalizedFilePath)
import qualified Development.IDE.Types.Location       as Location
import qualified Development.IDE.Types.Logger         as Logger
import           Ide.Plugin.Error
import qualified Language.LSP.Protocol.Message        as LSP
import qualified Language.LSP.Protocol.Types          as LSP

-- ----------------------------------------------------------------------------
-- Action wrappers
-- ----------------------------------------------------------------------------

runActionE :: MonadIO m => String -> IdeState -> ExceptT e Action a -> ExceptT e m a
runActionE herald ide act =
  hoistExceptT . ExceptT $
    join $ shakeEnqueue (shakeExtras ide) (mkDelayedAction herald Logger.Debug $ runExceptT act)

runActionMT :: MonadIO m => String -> IdeState -> MaybeT Action a -> MaybeT m a
runActionMT herald ide act =
  hoistMaybeT . MaybeT $
    join $ shakeEnqueue (shakeExtras ide) (mkDelayedAction herald Logger.Debug $ runMaybeT act)

-- | useE is useful to implement functions that aren’t rules but need shortcircuiting
-- e.g. getDefinition.
useE :: IdeRule k v => k -> NormalizedFilePath -> ExceptT PluginError Action v
useE k = maybeToExceptT (PluginRuleFailed (T.pack $ show k)) . useMaybeT k

useMaybeT :: IdeRule k v => k -> NormalizedFilePath -> MaybeT Action v
useMaybeT k = MaybeT . Shake.use k

useWithStaleE :: IdeRule k v
    => k -> NormalizedFilePath -> ExceptT PluginError Action (v, PositionMapping)
useWithStaleE key = maybeToExceptT (PluginRuleFailed (T.pack $ show key)) . useWithStaleMT key

useWithStaleMT :: IdeRule k v
    => k -> NormalizedFilePath -> MaybeT Action (v, PositionMapping)
useWithStaleMT key file = MaybeT $ runIdentity <$> Shake.usesWithStale key (Identity file)

hoistAction :: Action a -> ExceptT e Action a
hoistAction = ExceptT . fmap Right

-- ----------------------------------------------------------------------------
-- IdeAction wrappers
-- ----------------------------------------------------------------------------

runIdeActionE :: MonadIO m => String -> Shake.ShakeExtras -> ExceptT e IdeAction a -> ExceptT e m a
runIdeActionE _herald s i = ExceptT $ liftIO $ runReaderT (Shake.runIdeActionT $ runExceptT i) s

-- | useE is useful to implement functions that aren’t rules but need shortcircuiting
-- e.g. getDefinition.
useWithStaleFastE :: IdeRule k v => k -> NormalizedFilePath -> ExceptT PluginError IdeAction (v, PositionMapping)
useWithStaleFastE k = maybeToExceptT (PluginRuleFailed (T.pack $ show k)) . useWithStaleFastMT k

useWithStaleFastMT :: IdeRule k v => k -> NormalizedFilePath -> MaybeT IdeAction (v, PositionMapping)
useWithStaleFastMT k = MaybeT . Shake.useWithStaleFast k

-- ----------------------------------------------------------------------------
-- Location wrappers
-- ----------------------------------------------------------------------------

uriToFilePathE :: Monad m => LSP.Uri -> ExceptT PluginError m FilePath
uriToFilePathE uri = maybeToExceptT (PluginInvalidParams (T.pack $ "uriToFilePath' failed. Uri:" <>  show uri)) $ uriToFilePathMT uri

uriToFilePathMT :: Monad m => LSP.Uri -> MaybeT m FilePath
uriToFilePathMT = MaybeT . pure . Location.uriToFilePath'

-- ----------------------------------------------------------------------------
-- PositionMapping wrappers
-- ----------------------------------------------------------------------------

toCurrentPositionE :: Monad m => PositionMapping -> LSP.Position -> ExceptT PluginError m LSP.Position
toCurrentPositionE mapping = maybeToExceptT PluginPositionMappingFailed . toCurrentPositionMT mapping

toCurrentPositionMT :: Monad m => PositionMapping -> LSP.Position -> MaybeT m LSP.Position
toCurrentPositionMT mapping = MaybeT . pure . toCurrentPosition mapping

fromCurrentPositionE :: Monad m => PositionMapping -> LSP.Position -> ExceptT PluginError m LSP.Position
fromCurrentPositionE mapping = maybeToExceptT PluginPositionMappingFailed . fromCurrentPositionMT mapping

fromCurrentPositionMT :: Monad m => PositionMapping -> LSP.Position -> MaybeT m LSP.Position
fromCurrentPositionMT mapping = MaybeT . pure . fromCurrentPosition mapping

toCurrentRangeE :: Monad m => PositionMapping -> LSP.Range -> ExceptT PluginError m LSP.Range
toCurrentRangeE mapping = maybeToExceptT PluginPositionMappingFailed . toCurrentRangeMT mapping

toCurrentRangeMT :: Monad m => PositionMapping -> LSP.Range -> MaybeT m LSP.Range
toCurrentRangeMT mapping = MaybeT . pure . toCurrentRange mapping

fromCurrentRangeE :: Monad m => PositionMapping -> LSP.Range -> ExceptT PluginError m LSP.Range
fromCurrentRangeE mapping = maybeToExceptT PluginPositionMappingFailed . fromCurrentRangeMT mapping

fromCurrentRangeMT :: Monad m => PositionMapping -> LSP.Range -> MaybeT m LSP.Range
fromCurrentRangeMT mapping = MaybeT . pure . fromCurrentRange mapping
