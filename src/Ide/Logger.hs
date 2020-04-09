{- | Provides an implementation of the ghcide @Logger@ which uses
   @System.Log.Logger@ under the hood.
-}
module Ide.Logger
  (
    hlsLogger
  , logm
  , debugm
  , warningm
  , errorm
  ) where

import           Control.Monad.IO.Class
import qualified Data.Text as T
import qualified Development.IDE.Types.Logger as L
import           System.Log.Logger

-- ---------------------------------------------------------------------

hlsLogger :: L.Logger
hlsLogger = L.Logger $ \pri txt ->
    case pri of
      L.Telemetry -> logm     (T.unpack txt)
      L.Debug     -> debugm   (T.unpack txt)
      L.Info      -> logm     (T.unpack txt)
      L.Warning   -> warningm (T.unpack txt)
      L.Error     -> errorm   (T.unpack txt)

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
