{-# LANGUAGE GADTs #-}
module Development.IDE.Core.PluginUtils
(-- Wrapped Action functions
  runActionE
, runActionMT
, useE
, useMT
, usesE
, usesMT
, useWithStaleE
, useWithStaleMT
-- Wrapped IdeAction functions
, runIdeActionE
, runIdeActionMT
, useWithStaleFastE
, useWithStaleFastMT
, uriToFilePathE
-- Wrapped PositionMapping functions
, toCurrentPositionE
, toCurrentPositionMT
, fromCurrentPositionE
, fromCurrentPositionMT
, toCurrentRangeE
, toCurrentRangeMT
, fromCurrentRangeE
, fromCurrentRangeMT) where

import           Control.Monad.Extra
import           Control.Monad.IO.Class
import           Control.Monad.Reader                 (runReaderT)
import           Control.Monad.Trans.Except
import           Control.Monad.Trans.Maybe
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
import qualified Development.IDE.Types.Location       as Location
import qualified Ide.Logger                           as Logger
import           Ide.Plugin.Error
import qualified Language.LSP.Protocol.Types          as LSP
import Development.IDE.Core.InputPath (InputPath)

-- ----------------------------------------------------------------------------
-- Action wrappers
-- ----------------------------------------------------------------------------

-- |ExceptT version of `runAction`, takes a ExceptT Action
runActionE :: MonadIO m => String -> IdeState -> ExceptT e Action a -> ExceptT e m a
runActionE herald ide act =
  mapExceptT liftIO . ExceptT $
    join $ shakeEnqueue (shakeExtras ide) (mkDelayedAction herald Logger.Debug $ runExceptT act)

-- |MaybeT version of `runAction`, takes a MaybeT Action
runActionMT :: MonadIO m => String -> IdeState -> MaybeT Action a -> MaybeT m a
runActionMT herald ide act =
  mapMaybeT liftIO . MaybeT $
    join $ shakeEnqueue (shakeExtras ide) (mkDelayedAction herald Logger.Debug $ runMaybeT act)

-- |ExceptT version of `use` that throws a PluginRuleFailed upon failure
useE :: IdeRule k i v => k -> InputPath i -> ExceptT PluginError Action v
useE k = maybeToExceptT (PluginRuleFailed (T.pack $ show k)) . useMT k

-- |MaybeT version of `use`
useMT :: IdeRule k i v => k -> InputPath i -> MaybeT Action v
useMT k = MaybeT . Shake.use k

-- |ExceptT version of `uses` that throws a PluginRuleFailed upon failure
usesE :: (Traversable f, IdeRule k i v) => k -> f (InputPath i) -> ExceptT PluginError Action (f v)
usesE k = maybeToExceptT (PluginRuleFailed (T.pack $ show k)) . usesMT k

-- |MaybeT version of `uses`
usesMT :: (Traversable f, IdeRule k i v) => k -> f (InputPath i) -> MaybeT Action (f v)
usesMT k xs = MaybeT $ sequence <$> Shake.uses k xs

-- |ExceptT version of `useWithStale` that throws a PluginRuleFailed upon
-- failure
useWithStaleE :: IdeRule k i v
    => k -> InputPath i -> ExceptT PluginError Action (v, PositionMapping)
useWithStaleE key = maybeToExceptT (PluginRuleFailed (T.pack $ show key)) . useWithStaleMT key

-- |MaybeT version of `useWithStale`
useWithStaleMT :: IdeRule k i v
    => k -> InputPath i -> MaybeT Action (v, PositionMapping)
useWithStaleMT key file = MaybeT $ runIdentity <$> Shake.usesWithStale key (Identity file)

-- ----------------------------------------------------------------------------
-- IdeAction wrappers
-- ----------------------------------------------------------------------------

-- |ExceptT version of `runIdeAction`, takes a ExceptT IdeAction
runIdeActionE :: MonadIO m => String -> Shake.ShakeExtras -> ExceptT e IdeAction a -> ExceptT e m a
runIdeActionE _herald s i = ExceptT $ liftIO $ runReaderT (Shake.runIdeActionT $ runExceptT i) s

-- |MaybeT version of `runIdeAction`, takes a MaybeT IdeAction
runIdeActionMT :: MonadIO m => String -> Shake.ShakeExtras -> MaybeT IdeAction a -> MaybeT m a
runIdeActionMT _herald s i = MaybeT $ liftIO $ runReaderT (Shake.runIdeActionT $ runMaybeT i) s

-- |ExceptT version of `useWithStaleFast` that throws a PluginRuleFailed upon
-- failure
useWithStaleFastE :: IdeRule k i v => k -> InputPath i -> ExceptT PluginError IdeAction (v, PositionMapping)
useWithStaleFastE k = maybeToExceptT (PluginRuleFailed (T.pack $ show k)) . useWithStaleFastMT k

-- |MaybeT version of `useWithStaleFast`
useWithStaleFastMT :: IdeRule k i v => k -> InputPath i -> MaybeT IdeAction (v, PositionMapping)
useWithStaleFastMT k = MaybeT . Shake.useWithStaleFast k

-- ----------------------------------------------------------------------------
-- Location wrappers
-- ----------------------------------------------------------------------------

-- |ExceptT version of `uriToFilePath` that throws a PluginInvalidParams upon
-- failure
uriToFilePathE :: Monad m => LSP.Uri -> ExceptT PluginError m FilePath
uriToFilePathE uri = maybeToExceptT (PluginInvalidParams (T.pack $ "uriToFilePath' failed. Uri:" <>  show uri)) $ uriToFilePathMT uri

-- |MaybeT version of `uriToFilePath`
uriToFilePathMT :: Monad m => LSP.Uri -> MaybeT m FilePath
uriToFilePathMT = MaybeT . pure . Location.uriToFilePath'

-- ----------------------------------------------------------------------------
-- PositionMapping wrappers
-- ----------------------------------------------------------------------------

-- |ExceptT version of `toCurrentPosition` that throws a PluginInvalidUserState
-- upon failure
toCurrentPositionE :: Monad m => PositionMapping -> LSP.Position -> ExceptT PluginError m LSP.Position
toCurrentPositionE mapping = maybeToExceptT (PluginInvalidUserState "toCurrentPosition"). toCurrentPositionMT mapping

-- |MaybeT version of `toCurrentPosition`
toCurrentPositionMT :: Monad m => PositionMapping -> LSP.Position -> MaybeT m LSP.Position
toCurrentPositionMT mapping = MaybeT . pure . toCurrentPosition mapping

-- |ExceptT version of `fromCurrentPosition` that throws a
-- PluginInvalidUserState upon failure
fromCurrentPositionE :: Monad m => PositionMapping -> LSP.Position -> ExceptT PluginError m LSP.Position
fromCurrentPositionE mapping = maybeToExceptT (PluginInvalidUserState "fromCurrentPosition") . fromCurrentPositionMT mapping

-- |MaybeT version of `fromCurrentPosition`
fromCurrentPositionMT :: Monad m => PositionMapping -> LSP.Position -> MaybeT m LSP.Position
fromCurrentPositionMT mapping = MaybeT . pure . fromCurrentPosition mapping

-- |ExceptT version of `toCurrentRange` that throws a PluginInvalidUserState
-- upon failure
toCurrentRangeE :: Monad m => PositionMapping -> LSP.Range -> ExceptT PluginError m LSP.Range
toCurrentRangeE mapping = maybeToExceptT (PluginInvalidUserState "toCurrentRange") . toCurrentRangeMT mapping

-- |MaybeT version of `toCurrentRange`
toCurrentRangeMT :: Monad m => PositionMapping -> LSP.Range -> MaybeT m LSP.Range
toCurrentRangeMT mapping = MaybeT . pure . toCurrentRange mapping

-- |ExceptT version of `fromCurrentRange` that throws a PluginInvalidUserState
-- upon failure
fromCurrentRangeE :: Monad m => PositionMapping -> LSP.Range -> ExceptT PluginError m LSP.Range
fromCurrentRangeE mapping = maybeToExceptT (PluginInvalidUserState "fromCurrentRange") . fromCurrentRangeMT mapping

-- |MaybeT version of `fromCurrentRange`
fromCurrentRangeMT :: Monad m => PositionMapping -> LSP.Range -> MaybeT m LSP.Range
fromCurrentRangeMT mapping = MaybeT . pure . fromCurrentRange mapping
