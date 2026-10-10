{-# LANGUAGE CPP #-}

module Development.IDE.Core.FileUtils(
    getModTime,
    ) where


import           Data.Time.Clock.POSIX
import           System.OsPath                  (OsPath)

#ifdef mingw32_HOST_OS
import qualified System.Directory.OsPath        as Dir
#else
#if MIN_VERSION_filepath(1,5,0)
import           Data.ByteString                (ByteString)
import           Data.ByteString.Short          (fromShort)
import           System.OsString.Internal.Types (OsString (..),
                                                 PosixString (..))
import           System.Posix.Files.ByteString  (getFileStatus,
                                                 modificationTimeHiRes)
#else
import qualified System.OsPath                  as OsPath
import qualified System.Posix.Files             as PF
import           System.Posix.Files.ByteString  (modificationTimeHiRes)
#endif
#endif

#if !defined(mingw32_HOST_OS) && MIN_VERSION_filepath(1,5,0)
-- filepath >= 1.5 does not export 'toRawFilePath'; OsPath is a PosixString
-- wrapping the raw ShortByteString, so unwrap it directly.
toRawFilePath :: OsPath -> ByteString
toRawFilePath = fromShort . getPosixString . getOsString
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
-- Taking an OsPath lets us invoke the OS API directly, without decoding to FilePath.
getModTime :: OsPath -> IO POSIXTime
getModTime f =
#ifdef mingw32_HOST_OS
    utcTimeToPOSIXSeconds <$> Dir.getModificationTime f
#else
#if MIN_VERSION_filepath(1,5,0)
    modificationTimeHiRes <$> getFileStatus (toRawFilePath f)
#else
    -- filepath < 1.5 has no raw-byte access to OsPath; decode via the
    -- filesystem encoding and use the String-based POSIX API instead.
    OsPath.decodeFS f >>= (modificationTimeHiRes <$>) . PF.getFileStatus
#endif
#endif
