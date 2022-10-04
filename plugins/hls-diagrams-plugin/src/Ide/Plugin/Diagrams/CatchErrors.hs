-- Annoyingly, this is needed for the MFunctor instance
{-# OPTIONS_GHC -Wno-redundant-constraints #-}

module Ide.Plugin.Diagrams.CatchErrors (
    PluginError(..)
  , CatchErrors -- opaque
    -- * Error handlers
  , handleMaybe
  , handleMaybeM
    -- * HLS utilities
  , runAction
  , pluginResponse
  , getVirtualFileText
  , uriToNormalizedFilePath
  , uriToFilePath
    -- * GHC utilities
  , evalGhcEnv
  , getModSummary
  , getHieAst
  ) where

import Control.Monad.Catch
import Control.Monad.Except
import Control.Monad.IO.Unlift
import Control.Monad.Morph
import GHC.Stack
import Data.Text (Text)

import qualified Control.Exception as Exception

import Language.LSP.Server (MonadLsp)
import Development.IDE (IdeState)
import Development.IDE.GHC.Compat (HscEnv)
import Development.IDE.GHC.Compat.Core (Ghc)

import qualified Development.IDE                      as IDE
import qualified Development.IDE.Core.PositionMapping as IDE
import qualified Development.IDE.GHC.Util             as GHC

import qualified Ide.PluginUtils as HLS

import qualified Language.LSP.Server as LSP
import qualified Language.LSP.Types  as LSP
import qualified Language.LSP.VFS    as LSP

import qualified DynFlags  as GHC.NoCompat
import qualified Exception as GHC.NoCompat
import qualified GHC       as GHC.NoCompat

{-------------------------------------------------------------------------------
  Definition
-------------------------------------------------------------------------------}

data PluginError = PluginError String
  deriving stock (Show)
  deriving anyclass (Exception)

newtype CatchErrors c m a = CatchErrors {
      unwrapCatchErrors :: m a
    }
  deriving newtype (
      Functor
    , Applicative
    , Monad
    , MonadIO
    , MonadMask
    , MonadUnliftIO
    , MonadLsp c
    , GHC.NoCompat.ExceptionMonad
    , GHC.NoCompat.GhcMonad
    , GHC.NoCompat.HasDynFlags
    )

runCatchErrors :: forall c m a.
     MonadUnliftIO m
  => CatchErrors c m a -> ExceptT SomeException m a
runCatchErrors act = ExceptT $ unwrapCatchErrors act'
  where
    act' :: CatchErrors c m (Either SomeException a)
    act' = try act

{-------------------------------------------------------------------------------
  Standard instances
-------------------------------------------------------------------------------}

instance MonadIO m => MonadThrow (CatchErrors c m) where
  throwM :: Exception e => e -> CatchErrors c m a
  throwM = liftIO . Exception.throw

instance MonadUnliftIO m => MonadCatch (CatchErrors c m) where
  catch ::
       Exception e
    => CatchErrors c m a -> (e -> CatchErrors c m a) -> CatchErrors c m a
  catch x f = withRunInIO $ \runInIO -> do
      ma <- try (runInIO x)
      case ma of
        Left  e -> runInIO (f e)
        Right a -> return a

instance MonadUnliftIO m => MonadError String (CatchErrors c m) where
  throwError :: String -> CatchErrors c m a
  throwError = throwM . PluginError

  catchError ::
       CatchErrors c m a
    -> (String -> CatchErrors c m a)
    -> CatchErrors c m a
  catchError x f = catch x $ \(PluginError e) -> f e

instance MFunctor (CatchErrors c) where
  hoist ::
       Monad m
    => (forall a. m a -> n a)
    -> CatchErrors c m b -> CatchErrors c n b
  hoist nat = CatchErrors . nat . unwrapCatchErrors

{-------------------------------------------------------------------------------
  Error handlers
-------------------------------------------------------------------------------}

data UnexpectedNothing = UnexpectedNothing CallStack
  deriving stock (Show)
  deriving anyclass (Exception)

handleMaybe ::
     (MonadUnliftIO m, HasCallStack)
  => Maybe a -> CatchErrors c m a
handleMaybe = handleMaybeM . return

handleMaybeM ::
     (MonadUnliftIO m, HasCallStack)
  => CatchErrors c m (Maybe a) -> CatchErrors c m a
handleMaybeM ma = ma >>= maybe (throwM $ UnexpectedNothing callStack) return

{-------------------------------------------------------------------------------
  HLS utilities
-------------------------------------------------------------------------------}

runAction ::
     (MonadIO m, HasCallStack)
  => IDE.IdeState -> IDE.Action a -> CatchErrors c m a
runAction ide action = liftIO $
    IDE.runAction (prettyCallStack callStack) ide action

pluginResponse ::
     MonadUnliftIO m
  => CatchErrors c m a -> m (Either LSP.ResponseError a)
pluginResponse = HLS.pluginResponse . withExceptT show . runCatchErrors

getVirtualFileText ::
      MonadLsp c m
   => LSP.NormalizedUri -> CatchErrors c m Text
getVirtualFileText uri = handleMaybeM $
    fmap LSP.virtualFileText <$> LSP.getVirtualFile uri

uriToNormalizedFilePath ::
     MonadLsp c m
  => LSP.NormalizedUri -> CatchErrors c m LSP.NormalizedFilePath
uriToNormalizedFilePath uri = handleMaybe $ LSP.uriToNormalizedFilePath uri

uriToFilePath ::
     MonadLsp c m
  => LSP.Uri -> CatchErrors c m FilePath
uriToFilePath uri = handleMaybe $ IDE.uriToFilePath' uri

{-------------------------------------------------------------------------------
  GHC utilities
-------------------------------------------------------------------------------}

evalGhcEnv :: MonadIO m => HscEnv -> CatchErrors c Ghc a -> CatchErrors c m a
evalGhcEnv env = hoist (liftIO . GHC.evalGhcEnv env)

getModSummary ::
     MonadIO m
  => IdeState -> LSP.NormalizedFilePath -> CatchErrors c m IDE.ModSummaryResult
getModSummary ide nfp = runAction ide $
    IDE.use_ IDE.GetModSummary nfp

getHieAst ::
     MonadIO m
  => IdeState
  -> LSP.NormalizedFilePath
  -> CatchErrors c m (IDE.HieAstResult, IDE.PositionMapping)
getHieAst ide source = runAction ide $
    IDE.useWithStale_ IDE.GetHieAst source
