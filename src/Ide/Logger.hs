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
-- data Logger = Logger {logPriority :: Priority -> T.Text -> IO ()}
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
logm s = liftIO $ infoM "hie" s

debugm :: MonadIO m => String -> m ()
debugm s = liftIO $ debugM "hie" s

warningm :: MonadIO m => String -> m ()
warningm s = liftIO $ warningM "hie" s

errorm :: MonadIO m => String -> m ()
errorm s = liftIO $ errorM "hie" s

-- ---------------------------------------------------------------------
