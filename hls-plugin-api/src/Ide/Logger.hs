{- | Provides an implementation of the ghcide @Logger@ which uses
   @System.Log.Logger@ under the hood.
-}
module Ide.Logger
  (
    logm
  , debugm
  , warningm
  , errorm
  ) where

import           Control.Monad.IO.Class
import           System.Log.Logger

-- ---------------------------------------------------------------------

logm :: MonadIO m => String -> m ()
logm s = liftIO $ infoM "hls" s

debugm :: MonadIO m => String -> m ()
debugm s = liftIO $ debugM "hls" s

warningm :: MonadIO m => String -> m ()
warningm s = liftIO $ warningM "hls" s

errorm :: MonadIO m => String -> m ()
errorm s = liftIO $ errorM "hls" s

-- ---------------------------------------------------------------------
