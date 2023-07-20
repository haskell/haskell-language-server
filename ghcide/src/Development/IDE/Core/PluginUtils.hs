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

runActionMaybeT :: MonadIO m => String -> IdeState -> MaybeT Action a -> MaybeT m a
runActionMaybeT herald ide act =
  hoistMaybeT . MaybeT $
    join $ shakeEnqueue (shakeExtras ide) (mkDelayedAction herald Logger.Debug $ runMaybeT act)

-- | useE is useful to implement functions that aren’t rules but need shortcircuiting
-- e.g. getDefinition.
useE :: IdeRule k v => k -> NormalizedFilePath -> ExceptT PluginError Action v
useE k = maybeToExceptT (RuleFailed k) . useMaybeT k

useMaybeT :: IdeRule k v => k -> NormalizedFilePath -> MaybeT Action v
useMaybeT k = MaybeT . Shake.use k

useWithStaleE :: IdeRule k v
    => k -> NormalizedFilePath -> ExceptT PluginError Action (v, PositionMapping)
useWithStaleE key = maybeToExceptT (FastRuleNotReady key) . useWithStaleMaybeT key

useWithStaleMaybeT :: IdeRule k v
    => k -> NormalizedFilePath -> MaybeT Action (v, PositionMapping)
useWithStaleMaybeT key file = MaybeT $ runIdentity <$> Shake.usesWithStale key (Identity file)

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
useWithStaleFastE k = maybeToExceptT (RuleFailed k) . useWithStaleFastMaybeT k

useWithStaleFastMaybeT :: IdeRule k v => k -> NormalizedFilePath -> MaybeT IdeAction (v, PositionMapping)
useWithStaleFastMaybeT k = MaybeT . Shake.useWithStaleFast k

uriToFilePathE :: Monad m => LSP.Uri -> ExceptT PluginError m FilePath
uriToFilePathE uri = maybeToExceptT (PluginInvalidParams (T.pack $ "uriToFilePath' failed. Uri:" <>  show uri)) $ uriToFilePathMaybeT uri

uriToFilePathMaybeT :: Monad m => LSP.Uri -> MaybeT m FilePath
uriToFilePathMaybeT = MaybeT . pure . Location.uriToFilePath'
