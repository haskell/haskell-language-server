{-# LANGUAGE CPP          #-}

module Development.IDE.Core.FileUtils(
    getModTime,
    ) where


import           Data.Time.Clock.POSIX
#ifdef mingw32_HOST_OS
import qualified System.Directory                             as Dir
#else
import           System.Posix.Files                           (getFileStatus,
                                                               modificationTimeHiRes)
#endif

-- Dir.getModificationTime is surprisingly slow since it performs
-- a ton of conversions. Since we do not actually care about
-- the format of the time, we can get away with something cheaper.
-- For now, we only try to do this on Unix systems where it seems to get the
-- time spent checking file modifications (which happens on every change)
-- from > 0.5s to ~0.15s.
-- We might also want to try speeding this up on Windows at some point.
-- TODO leverage DidChangeWatchedFile lsp notifications on clients that
-- support them, as done for GetFileExists
getModTime :: FilePath -> IO POSIXTime
getModTime f =
#ifdef mingw32_HOST_OS
    utcTimeToPOSIXSeconds <$> Dir.getModificationTime f
#else
    modificationTimeHiRes <$> getFileStatus f
#endif
