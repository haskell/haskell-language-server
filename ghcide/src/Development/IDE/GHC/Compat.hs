-- Copyright (c) 2019 The DAML Authors. All rights reserved.
-- SPDX-License-Identifier: Apache-2.0

{-# LANGUAGE CPP #-}

-- | Attempt at hiding the GHC version differences we can.
module Development.IDE.GHC.Compat(
    HieFileResult(..),
    HieFile(..),
    mkHieFile,
    writeHieFile,
    readHieFile,
    hPutStringBuffer,
    includePathsGlobal,
    includePathsQuote,
    addIncludePathsQuote,
    ghcEnumerateExtensions
    ) where

import StringBuffer
import DynFlags
import GHC.LanguageExtensions.Type

#ifndef GHC_STABLE
import HieAst
import HieBin
import HieTypes
#else
import GHC
import GhcPlugins
import NameCache
import Avail
import TcRnTypes
import System.IO
import Foreign.ForeignPtr


hPutStringBuffer :: Handle -> StringBuffer -> IO ()
hPutStringBuffer hdl (StringBuffer buf len cur)
    = withForeignPtr (plusForeignPtr buf cur) $ \ptr ->
             hPutBuf hdl ptr len

mkHieFile :: ModSummary -> TcGblEnv -> RenamedSource -> Hsc HieFile
mkHieFile _ _ _ = return (HieFile () [])

writeHieFile :: FilePath -> HieFile -> IO ()
writeHieFile _ _ = return ()

readHieFile :: NameCache -> FilePath -> IO (HieFileResult, ())
readHieFile _ _ = return (HieFileResult (HieFile () []), ())

data HieFile = HieFile {hie_module :: (), hie_exports :: [AvailInfo]}
data HieFileResult = HieFileResult { hie_file_result :: HieFile }
#endif

#if __GLASGOW_HASKELL__ < 806
includePathsGlobal, includePathsQuote :: [String] -> [String]
includePathsGlobal = id
includePathsQuote = const []
#endif


addIncludePathsQuote :: FilePath -> DynFlags -> DynFlags
#if __GLASGOW_HASKELL__ >= 806
addIncludePathsQuote path x = x{includePaths = f $ includePaths x}
    where f i = i{includePathsQuote = path : includePathsQuote i}
#else
addIncludePathsQuote path x = x{includePaths = path : includePaths x}
#endif

ghcEnumerateExtensions :: [Extension]
#if __GLASGOW_HASKELL__ >= 808
ghcEnumerateExtensions = enumerate
#elif __GLASGOW_HASKELL__ >= 806
ghcEnumerateExtensions = [Cpp .. StarIsType]
#else
ghcEnumerateExtensions = [Cpp .. EmptyDataDeriving]
#endif
