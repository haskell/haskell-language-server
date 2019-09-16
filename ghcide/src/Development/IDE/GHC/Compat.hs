-- Copyright (c) 2019 The DAML Authors. All rights reserved.
-- SPDX-License-Identifier: Apache-2.0

{-# LANGUAGE CPP #-}
#include "ghc-api-version.h"

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

#if MIN_GHC_API_VERSION(8,8,0)
import Data.List.Extra (enumerate)
#endif

#if MIN_GHC_API_VERSION(8,8,0)
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


#if !MIN_GHC_API_VERSION(8,8,0)
hPutStringBuffer :: Handle -> StringBuffer -> IO ()
hPutStringBuffer hdl (StringBuffer buf len cur)
    = withForeignPtr (plusForeignPtr buf cur) $ \ptr ->
             hPutBuf hdl ptr len
#endif

mkHieFile :: ModSummary -> TcGblEnv -> RenamedSource -> Hsc HieFile
mkHieFile _ _ _ = return (HieFile () [])

writeHieFile :: FilePath -> HieFile -> IO ()
writeHieFile _ _ = return ()

readHieFile :: NameCache -> FilePath -> IO (HieFileResult, ())
readHieFile _ _ = return (HieFileResult (HieFile () []), ())

data HieFile = HieFile {hie_module :: (), hie_exports :: [AvailInfo]}
data HieFileResult = HieFileResult { hie_file_result :: HieFile }
#endif

#if !MIN_GHC_API_VERSION(8,6,0)
includePathsGlobal, includePathsQuote :: [String] -> [String]
includePathsGlobal = id
includePathsQuote = const []
#endif


addIncludePathsQuote :: FilePath -> DynFlags -> DynFlags
#if MIN_GHC_API_VERSION(8,6,0)
addIncludePathsQuote path x = x{includePaths = f $ includePaths x}
    where f i = i{includePathsQuote = path : includePathsQuote i}
#else
addIncludePathsQuote path x = x{includePaths = path : includePaths x}
#endif

ghcEnumerateExtensions :: [Extension]
#if MIN_GHC_API_VERSION(8,8,0)
ghcEnumerateExtensions = enumerate
#elif MIN_GHC_API_VERSION(8,6,0)
ghcEnumerateExtensions = [Cpp .. StarIsType]
#else
ghcEnumerateExtensions = [Cpp .. EmptyDataDeriving]
#endif
