{-# LANGUAGE GADTs #-}
module Development.IDE.Core.PluginUtils where

import           Control.Monad.Extra
import           Control.Monad.IO.Class
import           Control.Monad.Reader                 (runReaderT)
import           Control.Monad.Trans.Except
import           Control.Monad.Trans.Maybe
import           Data.Bifunctor                       (first)
import           Data.Either.Extra                    (maybeToEither)
import           Data.Functor.Identity
import           Data.String                          (IsString (fromString))
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

runAction :: MonadIO m => String -> IdeState -> ExceptT e Action a -> ExceptT e m a
runAction herald ide act =
  hoistExceptT . ExceptT $
    join $ shakeEnqueue (shakeExtras ide) (mkDelayedAction herald Logger.Debug $ runExceptT act)

-- | Request a Rule result, it not available return the last computed result which may be stale.
--   Errors out if none available.
useWithStale_ ::(IdeRule k v)
    => k -> NormalizedFilePath -> ExceptT e Action (v, PositionMapping)
useWithStale_ key file = ExceptT $ fmap Right $ Shake.useWithStale_ key file

useWithStale :: IdeRule k v
    => k -> NormalizedFilePath -> ExceptT PluginError Action (v, PositionMapping)
useWithStale key file = maybeToExceptT (FastRuleNotReady key) $ useWithStaleMaybeT key file

-- | useE is useful to implement functions that aren’t rules but need shortcircuiting
-- e.g. getDefinition.
use :: IdeRule k v => k -> NormalizedFilePath -> ExceptT PluginError Action v
use k = maybeToExceptT (RuleFailed k) . MaybeT . Shake.use k

useWithStaleMaybeT :: IdeRule k v
    => k -> NormalizedFilePath -> MaybeT Action (v, PositionMapping)
useWithStaleMaybeT key file = MaybeT $ runIdentity <$> Shake.usesWithStale key (Identity file)

-- ----------------------------------------------------------------------------
-- IdeAction wrappers
-- ----------------------------------------------------------------------------

runIdeAction :: MonadIO m => String -> Shake.ShakeExtras -> ExceptT e IdeAction a -> ExceptT e m a
runIdeAction _herald s i = ExceptT $ liftIO $ runReaderT (Shake.runIdeActionT $ runExceptT i) s

-- | useE is useful to implement functions that aren’t rules but need shortcircuiting
-- e.g. getDefinition.
useWithStaleFast :: IdeRule k v => k -> NormalizedFilePath -> ExceptT PluginError IdeAction (v, PositionMapping)
useWithStaleFast k = maybeToExceptT (RuleFailed k) . MaybeT . Shake.useWithStaleFast k

uriToFilePath' :: Monad m => LSP.Uri -> ExceptT PluginError m FilePath
uriToFilePath' uri = ExceptT . pure . maybeToEither (PluginUriToFilePath uri) $ Location.uriToFilePath' uri

-- ----------------------------------------------------------------------------
-- Internal Helper function, not exported
-- ----------------------------------------------------------------------------

hoistAction :: Action a -> ExceptT e Action a
hoistAction = ExceptT . fmap Right
