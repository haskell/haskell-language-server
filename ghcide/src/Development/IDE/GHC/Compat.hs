-- Copyright (c) 2019 The DAML Authors. All rights reserved.
-- SPDX-License-Identifier: Apache-2.0

{-# LANGUAGE CPP #-}
{-# LANGUAGE PatternSynonyms #-}
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
    pattern DerivD,
    pattern ForD,
    pattern InstD,
    pattern TyClD,
    pattern ValD,
    pattern ClassOpSig,
    pattern IEThingWith,
    GHC.ModLocation,
    pattern ModLocation,

    module GHC
    ) where

import StringBuffer
import DynFlags
import FieldLabel

import qualified GHC
import GHC hiding (ClassOpSig, DerivD, ForD, IEThingWith, InstD, TyClD, ValD, ModLocation)

#if MIN_GHC_API_VERSION(8,8,0)
import HieAst
import HieBin
import HieTypes
#else
import GhcPlugins hiding (ModLocation)
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

pattern DerivD :: DerivDecl p -> HsDecl p
pattern DerivD x <-
#if MIN_GHC_API_VERSION(8,6,0)
    GHC.DerivD _ x
#else
    GHC.DerivD x
#endif

pattern ForD :: ForeignDecl p -> HsDecl p
pattern ForD x <-
#if MIN_GHC_API_VERSION(8,6,0)
    GHC.ForD _ x
#else
    GHC.ForD x
#endif

pattern ValD :: HsBind p -> HsDecl p
pattern ValD x <-
#if MIN_GHC_API_VERSION(8,6,0)
    GHC.ValD _ x
#else
    GHC.ValD x
#endif

pattern InstD :: InstDecl p -> HsDecl p
pattern InstD x <-
#if MIN_GHC_API_VERSION(8,6,0)
    GHC.InstD _ x
#else
    GHC.InstD x
#endif

pattern TyClD :: TyClDecl p -> HsDecl p
pattern TyClD x <-
#if MIN_GHC_API_VERSION(8,6,0)
    GHC.TyClD _ x
#else
    GHC.TyClD x
#endif

pattern ClassOpSig :: Bool -> [Located (IdP pass)] -> LHsSigType pass -> Sig pass
pattern ClassOpSig a b c <-
#if MIN_GHC_API_VERSION(8,6,0)
    GHC.ClassOpSig _ a b c
#else
    GHC.ClassOpSig a b c
#endif

pattern IEThingWith :: LIEWrappedName (IdP pass) -> IEWildcard -> [LIEWrappedName (IdP pass)] -> [Located (FieldLbl (IdP pass))] -> IE pass
pattern IEThingWith a b c d <-
#if MIN_GHC_API_VERSION(8,6,0)
    GHC.IEThingWith _ a b c d
#else
    GHC.IEThingWith a b c d
#endif

pattern ModLocation :: Maybe FilePath -> FilePath -> FilePath -> GHC.ModLocation
pattern ModLocation a b c <-
#if MIN_GHC_API_VERSION(8,8,0)
    GHC.ModLocation a b c _ where ModLocation a b c = GHC.ModLocation a b c ""
#else
    GHC.ModLocation a b c where ModLocation a b c = GHC.ModLocation a b c
#endif
