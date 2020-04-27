-- Copyright (c) 2019 The DAML Authors. All rights reserved.
-- SPDX-License-Identifier: Apache-2.0

{-# LANGUAGE CPP #-}
{-# LANGUAGE PatternSynonyms #-}
#include "ghc-api-version.h"

-- | Attempt at hiding the GHC version differences we can.
module Development.IDE.GHC.Compat(
    getHeaderImports,
    HieFileResult(..),
    HieFile,
    hieExportNames,
    hie_module,
    mkHieFile,
    writeHieFile,
    readHieFile,
    supportsHieFiles,
    setDefaultHieDir,
    dontWriteHieFiles,
#if !MIN_GHC_API_VERSION(8,8,0)
    ml_hie_file,
#endif
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
    pattern IEThingAll,
    pattern IEThingWith,
    GHC.ModLocation,
    Module.addBootSuffix,
    pattern ModLocation,

    module GHC
    ) where

import StringBuffer
import DynFlags
import FieldLabel
import qualified Module

import qualified GHC
import GHC hiding (ClassOpSig, DerivD, ForD, IEThingAll, IEThingWith, InstD, TyClD, ValD, ModLocation)
import qualified HeaderInfo as Hdr
import Avail
import ErrUtils (ErrorMessages)
import FastString (FastString)

#if MIN_GHC_API_VERSION(8,8,0)
import Control.Applicative ((<|>))
import Development.IDE.GHC.HieAst
import HieBin
import HieTypes

supportsHieFiles :: Bool
supportsHieFiles = True

hieExportNames :: HieFile -> [(SrcSpan, Name)]
hieExportNames = nameListFromAvails . hie_exports

#else

#if MIN_GHC_API_VERSION(8,6,0)
import BinIface
import Data.IORef
import IfaceEnv
#endif

import Binary
import Control.Exception (catch)
import Data.ByteString (ByteString)
import GhcPlugins hiding (ModLocation)
import NameCache
import TcRnTypes
import System.IO
import Foreign.ForeignPtr
import MkIface


hPutStringBuffer :: Handle -> StringBuffer -> IO ()
hPutStringBuffer hdl (StringBuffer buf len cur)
    = withForeignPtr (plusForeignPtr buf cur) $ \ptr ->
             hPutBuf hdl ptr len

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

pattern IEThingAll :: LIEWrappedName (IdP pass) -> IE pass
pattern IEThingAll a <-
#if MIN_GHC_API_VERSION(8,6,0)
    GHC.IEThingAll _ a
#else
    GHC.IEThingAll a
#endif

setDefaultHieDir :: FilePath -> DynFlags -> DynFlags
setDefaultHieDir _f d =
#if MIN_GHC_API_VERSION(8,8,0)
    d { hieDir     = hieDir d <|> Just _f}
#else
    d
#endif

dontWriteHieFiles :: DynFlags -> DynFlags
dontWriteHieFiles d =
#if MIN_GHC_API_VERSION(8,8,0)
    gopt_unset d Opt_WriteHie
#else
    d
#endif

nameListFromAvails :: [AvailInfo] -> [(SrcSpan, Name)]
nameListFromAvails as =
  map (\n -> (nameSrcSpan n, n)) (concatMap availNames as)

#if !MIN_GHC_API_VERSION(8,8,0)
-- Reimplementations of functions for HIE files for GHC 8.6

mkHieFile :: ModSummary -> TcGblEnv -> RenamedSource -> ByteString -> Hsc HieFile
mkHieFile ms ts _ _ = return (HieFile (ms_mod ms) es)
  where
    es = nameListFromAvails (mkIfaceExports (tcg_exports ts))

ml_hie_file :: GHC.ModLocation -> FilePath
ml_hie_file ml = ml_hi_file ml ++ ".hie"

data HieFile = HieFile {hie_module :: Module, hie_exports :: [(SrcSpan, Name)]}

hieExportNames :: HieFile -> [(SrcSpan, Name)]
hieExportNames = hie_exports

instance Binary HieFile where
  put_ bh (HieFile m es) = do
    put_ bh m
    put_ bh es

  get bh = do
    mod <- get bh
    es <- get bh
    return (HieFile mod es)

data HieFileResult = HieFileResult { hie_file_result :: HieFile }

writeHieFile :: FilePath -> HieFile -> IO ()
readHieFile :: NameCache -> FilePath -> IO (HieFileResult, ())
supportsHieFiles :: Bool

#if MIN_GHC_API_VERSION(8,6,0)

writeHieFile fp hie = do
  bh <- openBinMem (1024 * 1024)
  putWithUserData (const $ return ()) bh hie
  writeBinMem bh fp

readHieFile nc fp = do
  bh <- readBinMem fp
  nc' <- newIORef nc
  hie_file <- getWithUserData (NCU (atomicModifyIORef' nc')) bh
  return (HieFileResult hie_file, ())

supportsHieFiles = True

#else

supportsHieFiles = False

writeHieFile _ _ = return ()

readHieFile _ _ = return undefined

#endif

#endif

getHeaderImports
  :: DynFlags
  -> StringBuffer
  -> FilePath
  -> FilePath
  -> IO
       ( Either
           ErrorMessages
           ( [(Maybe FastString, Located ModuleName)]
           , [(Maybe FastString, Located ModuleName)]
           , Located ModuleName
           )
       )
#if MIN_GHC_API_VERSION(8,8,0)
getHeaderImports = Hdr.getImports
#else
getHeaderImports a b c d =
    catch (Right <$> Hdr.getImports a b c d)
          (return . Left . srcErrorMessages)
#endif
