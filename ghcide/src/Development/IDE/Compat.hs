{-# LANGUAGE CPP #-}
module Development.IDE.Compat
    (
        getProcessID
    ) where

#ifdef mingw32_HOST_OS

import qualified System.Win32.Process as P (getCurrentProcessId)
getProcessID :: IO Int
getProcessID = fromIntegral <$> P.getCurrentProcessId

#else

import qualified System.Posix.Process as P (getProcessID)
getProcessID :: IO Int
getProcessID = fromIntegral <$> P.getProcessID

#endif
