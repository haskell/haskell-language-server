{-# LANGUAGE GADTs #-}
module Development.IDE.Core.PluginUtils
(-- * Wrapped Action functions
  runActionE
, runActionMT
, useE
, useMT
, usesE
, usesMT
, useWithStaleE
, useWithStaleMT
-- * Wrapped IdeAction functions
, runIdeActionE
, runIdeActionMT
, useWithStaleFastE
, useWithStaleFastMT
, uriToFilePathE
-- * Wrapped PositionMapping functions
, toCurrentPositionE
, toCurrentPositionMT
, fromCurrentPositionE
, fromCurrentPositionMT
, toCurrentRangeE
, toCurrentRangeMT
, fromCurrentRangeE
, fromCurrentRangeMT
-- * Diagnostics
, activeDiagnosticsInRange
, activeDiagnosticsInRangeMT
-- * Formatting handlers
, mkFormattingHandlers) where

import           Control.Concurrent.STM
import           Control.Lens                         ((^.))
import           Control.Monad.Error.Class            (MonadError (throwError))
import           Control.Monad.Extra
import           Control.Monad.IO.Class
import           Control.Monad.Reader                 (runReaderT)
import           Control.Monad.Trans.Except
import           Control.Monad.Trans.Maybe
import           Data.Functor.Identity
import qualified Data.Text                            as T
import qualified Data.Text.Utf16.Rope.Mixed           as Rope
import           Development.IDE.Core.FileStore
import           Development.IDE.Core.PositionMapping
import           Development.IDE.Core.Service         (runAction)
import           Development.IDE.Core.Shake           (IdeAction, IdeRule,
                                                       IdeState (shakeExtras),
                                                       mkDelayedAction,
                                                       shakeEnqueue)
import qualified Development.IDE.Core.Shake           as Shake
import           Development.IDE.GHC.Orphans          ()
import           Development.IDE.Graph                hiding (ShakeValue)
import           Development.IDE.Types.Diagnostics
import           Development.IDE.Types.Location       (NormalizedFilePath)
import qualified Development.IDE.Types.Location       as Location
import qualified Ide.Logger                           as Logger
import           Ide.Plugin.Error
import           Ide.PluginUtils                      (rangesOverlap)
import           Ide.Types
import qualified Language.LSP.Protocol.Lens           as LSP
import           Language.LSP.Protocol.Message        (SMethod (..))
import qualified Language.LSP.Protocol.Types          as LSP
import qualified StmContainers.Map                    as STM

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
useE :: IdeRule k v => k -> NormalizedFilePath -> ExceptT PluginError Action v
useE k = maybeToExceptT (PluginRuleFailed (T.pack $ show k)) . useMT k

-- |MaybeT version of `use`
useMT :: IdeRule k v => k -> NormalizedFilePath -> MaybeT Action v
useMT k = MaybeT . Shake.use k

-- |ExceptT version of `uses` that throws a PluginRuleFailed upon failure
usesE :: (Traversable f, IdeRule k v) => k -> f NormalizedFilePath -> ExceptT PluginError Action (f v)
usesE k = maybeToExceptT (PluginRuleFailed (T.pack $ show k)) . usesMT k

-- |MaybeT version of `uses`
usesMT :: (Traversable f, IdeRule k v) => k -> f NormalizedFilePath -> MaybeT Action (f v)
usesMT k xs = MaybeT $ sequence <$> Shake.uses k xs

-- |ExceptT version of `useWithStale` that throws a PluginRuleFailed upon
-- failure
useWithStaleE :: IdeRule k v
    => k -> NormalizedFilePath -> ExceptT PluginError Action (v, PositionMapping)
useWithStaleE key = maybeToExceptT (PluginRuleFailed (T.pack $ show key)) . useWithStaleMT key

-- |MaybeT version of `useWithStale`
useWithStaleMT :: IdeRule k v
    => k -> NormalizedFilePath -> MaybeT Action (v, PositionMapping)
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
useWithStaleFastE :: IdeRule k v => k -> NormalizedFilePath -> ExceptT PluginError IdeAction (v, PositionMapping)
useWithStaleFastE k = maybeToExceptT (PluginRuleFailed (T.pack $ show k)) . useWithStaleFastMT k

-- |MaybeT version of `useWithStaleFast`
useWithStaleFastMT :: IdeRule k v => k -> NormalizedFilePath -> MaybeT IdeAction (v, PositionMapping)
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

-- ----------------------------------------------------------------------------
-- Diagnostics
-- ----------------------------------------------------------------------------

-- | @'activeDiagnosticsInRangeMT' shakeExtras nfp range@ computes the
-- 'FileDiagnostic' 's that HLS produced and overlap with the given @range@.
--
-- This function is to be used whenever we need an authoritative source of truth
-- for which diagnostics are shown to the user.
-- These diagnostics can be used to provide various IDE features, for example
-- CodeActions, CodeLenses, or refactorings.
--
-- However, why do we need this when computing 'CodeAction's? A 'CodeActionParam'
-- has the 'CodeActionContext' which already contains the diagnostics!
-- But according to the LSP docs, the server shouldn't rely that these Diagnostic
-- are actually up-to-date and accurately reflect the state of the document.
--
-- From the LSP docs:
-- > An array of diagnostics known on the client side overlapping the range
-- > provided to the `textDocument/codeAction` request. They are provided so
-- > that the server knows which errors are currently presented to the user
-- > for the given range. There is no guarantee that these accurately reflect
-- > the error state of the resource. The primary parameter
-- > to compute code actions is the provided range.
--
-- Thus, even when the client sends us the context, we should compute the
-- diagnostics on the server side.
activeDiagnosticsInRangeMT :: MonadIO m => Shake.ShakeExtras -> NormalizedFilePath -> LSP.Range -> MaybeT m [FileDiagnostic]
activeDiagnosticsInRangeMT ide nfp range = do
    MaybeT $ liftIO $ atomically $ do
        mDiags <- STM.lookup (LSP.normalizedFilePathToUri nfp) (Shake.publishedDiagnostics ide)
        case mDiags of
            Nothing -> pure Nothing
            Just fileDiags -> do
                pure $ Just $ filter diagRangeOverlaps fileDiags
    where
        diagRangeOverlaps = \fileDiag ->
            rangesOverlap range (fileDiag ^. fdLspDiagnosticL . LSP.range)

-- | Just like 'activeDiagnosticsInRangeMT'. See the docs of 'activeDiagnosticsInRangeMT' for details.
activeDiagnosticsInRange :: MonadIO m => Shake.ShakeExtras -> NormalizedFilePath -> LSP.Range -> m (Maybe [FileDiagnostic])
activeDiagnosticsInRange ide nfp range = runMaybeT (activeDiagnosticsInRangeMT ide nfp range)

-- ----------------------------------------------------------------------------
-- Formatting handlers
-- ----------------------------------------------------------------------------

-- `mkFormattingHandlers` was moved here from hls-plugin-api package so that
-- `mkFormattingHandlers` can refer to `IdeState`. `IdeState` is defined in the
-- ghcide package, but hls-plugin-api does not depend on ghcide, so `IdeState`
-- is not in scope there.

mkFormattingHandlers :: FormattingHandler IdeState -> PluginHandlers IdeState
mkFormattingHandlers f = mkPluginHandler SMethod_TextDocumentFormatting ( provider SMethod_TextDocumentFormatting)
                      <> mkPluginHandler SMethod_TextDocumentRangeFormatting (provider SMethod_TextDocumentRangeFormatting)
  where
    provider :: forall m. FormattingMethod m => SMethod m -> PluginMethodHandler IdeState m
    provider m ide _pid params
      | Just nfp <- LSP.uriToNormalizedFilePath $ LSP.toNormalizedUri uri = do
        contentsMaybe <- liftIO $ runAction "mkFormattingHandlers" ide $ getFileContents nfp
        case contentsMaybe of
          Just contents -> do
            let (typ, mtoken) = case m of
                  SMethod_TextDocumentFormatting -> (FormatText, params ^. LSP.workDoneToken)
                  SMethod_TextDocumentRangeFormatting -> (FormatRange (params ^. LSP.range), params ^. LSP.workDoneToken)
                  _ -> Prelude.error "mkFormattingHandlers: impossible"
            f ide mtoken typ (Rope.toText contents) nfp opts
          Nothing -> throwError $ PluginInvalidParams $ T.pack $ "Formatter plugin: could not get file contents for " ++ show uri

      | otherwise = throwError $ PluginInvalidParams $ T.pack $ "Formatter plugin: uriToFilePath failed for: " ++ show uri
      where
        uri = params ^. LSP.textDocument . LSP.uri
        opts = params ^. LSP.options
