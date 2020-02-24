-- Copyright (c) 2019 The DAML Authors. All rights reserved.
-- SPDX-License-Identifier: Apache-2.0

-- Copied from https://github.com/ghc/ghc/blob/master/compiler/main/DriverPipeline.hs on 14 May 2019
-- Requested to be exposed at https://gitlab.haskell.org/ghc/ghc/merge_requests/944.
-- Update the above MR got merged to master on 31 May 2019. When it becomes avialable to ghc-lib, this file can be removed.

{- HLINT ignore -} -- since copied from upstream

{-# LANGUAGE CPP, NamedFieldPuns, NondecreasingIndentation, BangPatterns, MultiWayIf #-}
{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}
#include "ghc-api-version.h"

-----------------------------------------------------------------------------
--
-- GHC Driver
--
-- (c) The University of Glasgow 2005
--
-----------------------------------------------------------------------------

module Development.IDE.GHC.CPP(doCpp, addOptP)
where

import Development.IDE.GHC.Compat
import Packages
import SysTools
import Module
import DynFlags
import Panic
import FileCleanup
#if MIN_GHC_API_VERSION(8,8,2)
import LlvmCodeGen (llvmVersionList)
#elif MIN_GHC_API_VERSION(8,8,0)
import LlvmCodeGen (LlvmVersion (..))
#endif
#if MIN_GHC_API_VERSION (8,10,0)
import Fingerprint
import ToolSettings
#endif

import System.Directory
import System.FilePath
import Control.Monad
import System.Info
import Data.List        ( intercalate )
import Data.Maybe
import Data.Version



doCpp :: DynFlags -> Bool -> FilePath -> FilePath -> IO ()
doCpp dflags raw input_fn output_fn = do
    let hscpp_opts = picPOpts dflags
    let cmdline_include_paths = includePaths dflags

    pkg_include_dirs <- getPackageIncludePath dflags []
    let include_paths_global = foldr (\ x xs -> ("-I" ++ x) : xs) []
          (includePathsGlobal cmdline_include_paths ++ pkg_include_dirs)
    let include_paths_quote = foldr (\ x xs -> ("-iquote" ++ x) : xs) []
          (includePathsQuote cmdline_include_paths)
    let include_paths = include_paths_quote ++ include_paths_global

    let verbFlags = getVerbFlags dflags

    let cpp_prog args | raw       = SysTools.runCpp dflags args
#if MIN_GHC_API_VERSION(8,10,0)
                      | otherwise = SysTools.runCc Nothing
#else
                      | otherwise = SysTools.runCc
#endif
                                          dflags (SysTools.Option "-E" : args)

    let target_defs =
          -- NEIL: Patched to use System.Info instead of constants from CPP
          [ "-D" ++ os     ++ "_BUILD_OS",
            "-D" ++ arch   ++ "_BUILD_ARCH",
            "-D" ++ os     ++ "_HOST_OS",
            "-D" ++ arch   ++ "_HOST_ARCH" ]
        -- remember, in code we *compile*, the HOST is the same our TARGET,
        -- and BUILD is the same as our HOST.

    let sse_defs =
          [ "-D__SSE__"      | isSseEnabled      dflags ] ++
          [ "-D__SSE2__"     | isSse2Enabled     dflags ] ++
          [ "-D__SSE4_2__"   | isSse4_2Enabled   dflags ]

    let avx_defs =
          [ "-D__AVX__"      | isAvxEnabled      dflags ] ++
          [ "-D__AVX2__"     | isAvx2Enabled     dflags ] ++
          [ "-D__AVX512CD__" | isAvx512cdEnabled dflags ] ++
          [ "-D__AVX512ER__" | isAvx512erEnabled dflags ] ++
          [ "-D__AVX512F__"  | isAvx512fEnabled  dflags ] ++
          [ "-D__AVX512PF__" | isAvx512pfEnabled dflags ]

    backend_defs <- getBackendDefs dflags

    let th_defs = [ "-D__GLASGOW_HASKELL_TH__" ]
    -- Default CPP defines in Haskell source
    ghcVersionH <- getGhcVersionPathName dflags
    let hsSourceCppOpts = [ "-include", ghcVersionH ]

    -- MIN_VERSION macros
    let uids = explicitPackages (pkgState dflags)
        pkgs = catMaybes (map (lookupPackage dflags) uids)
    mb_macro_include <-
        if not (null pkgs) && gopt Opt_VersionMacros dflags
            then do macro_stub <- newTempName dflags TFL_CurrentModule "h"
                    writeFile macro_stub (generatePackageVersionMacros pkgs)
                    -- Include version macros for every *exposed* package.
                    -- Without -hide-all-packages and with a package database
                    -- size of 1000 packages, it takes cpp an estimated 2
                    -- milliseconds to process this file. See #10970
                    -- comment 8.
                    return [SysTools.FileOption "-include" macro_stub]
            else return []

    cpp_prog       (   map SysTools.Option verbFlags
                    ++ map SysTools.Option include_paths
                    ++ map SysTools.Option hsSourceCppOpts
                    ++ map SysTools.Option target_defs
                    ++ map SysTools.Option backend_defs
                    ++ map SysTools.Option th_defs
                    ++ map SysTools.Option hscpp_opts
                    ++ map SysTools.Option sse_defs
                    ++ map SysTools.Option avx_defs
                    ++ mb_macro_include
        -- Set the language mode to assembler-with-cpp when preprocessing. This
        -- alleviates some of the C99 macro rules relating to whitespace and the hash
        -- operator, which we tend to abuse. Clang in particular is not very happy
        -- about this.
                    ++ [ SysTools.Option     "-x"
                       , SysTools.Option     "assembler-with-cpp"
                       , SysTools.Option     input_fn
        -- We hackily use Option instead of FileOption here, so that the file
        -- name is not back-slashed on Windows.  cpp is capable of
        -- dealing with / in filenames, so it works fine.  Furthermore
        -- if we put in backslashes, cpp outputs #line directives
        -- with *double* backslashes.   And that in turn means that
        -- our error messages get double backslashes in them.
        -- In due course we should arrange that the lexer deals
        -- with these \\ escapes properly.
                       , SysTools.Option     "-o"
                       , SysTools.FileOption "" output_fn
                       ])

getBackendDefs :: DynFlags -> IO [String]
getBackendDefs dflags | hscTarget dflags == HscLlvm = do
    llvmVer <- figureLlvmVersion dflags
    return $ case llvmVer of
#if MIN_GHC_API_VERSION(8,8,2)
               Just v
                 | [m] <- llvmVersionList v -> [ "-D__GLASGOW_HASKELL_LLVM__=" ++ format (m, 0) ]
                 | m:n:_   <- llvmVersionList v -> [ "-D__GLASGOW_HASKELL_LLVM__=" ++ format (m, n) ]
#elif MIN_GHC_API_VERSION(8,8,0)
               Just (LlvmVersion n) -> [ "-D__GLASGOW_HASKELL_LLVM__=" ++ format (n,0) ]
               Just (LlvmVersionOld m n) -> [ "-D__GLASGOW_HASKELL_LLVM__=" ++ format (m,n) ]
#else
               Just n -> [ "-D__GLASGOW_HASKELL_LLVM__=" ++ format n ]
#endif
               _      -> []
  where
    format (major, minor)
      | minor >= 100 = error "getBackendDefs: Unsupported minor version"
      | otherwise = show $ (100 * major + minor :: Int) -- Contract is Int

getBackendDefs _ =
    return []

addOptP :: String -> DynFlags -> DynFlags
#if MIN_GHC_API_VERSION (8,10,0)
addOptP f = alterToolSettings $ \s -> s
          { toolSettings_opt_P             = f : toolSettings_opt_P s
          , toolSettings_opt_P_fingerprint = fingerprintStrings (f : toolSettings_opt_P s)
          }
  where
    fingerprintStrings ss = fingerprintFingerprints $ map fingerprintString ss
    alterToolSettings f dynFlags = dynFlags { toolSettings = f (toolSettings dynFlags) }
#else
addOptP opt = onSettings (onOptP (opt:))
  where
    onSettings f x = x{settings = f $ settings x}
    onOptP f x = x{sOpt_P = f $ sOpt_P x}
#endif

-- ---------------------------------------------------------------------------
-- Macros (cribbed from Cabal)

generatePackageVersionMacros :: [PackageConfig] -> String
generatePackageVersionMacros pkgs = concat
  -- Do not add any C-style comments. See #3389.
  [ generateMacros "" pkgname version
  | pkg <- pkgs
  , let version = packageVersion pkg
        pkgname = map fixchar (packageNameString pkg)
  ]

fixchar :: Char -> Char
fixchar '-' = '_'
fixchar c   = c

generateMacros :: String -> String -> Version -> String
generateMacros prefix name version =
  concat
  ["#define ", prefix, "VERSION_",name," ",show (showVersion version),"\n"
  ,"#define MIN_", prefix, "VERSION_",name,"(major1,major2,minor) (\\\n"
  ,"  (major1) <  ",major1," || \\\n"
  ,"  (major1) == ",major1," && (major2) <  ",major2," || \\\n"
  ,"  (major1) == ",major1," && (major2) == ",major2," && (minor) <= ",minor,")"
  ,"\n\n"
  ]
  where
    (major1:major2:minor:_) = map show (versionBranch version ++ repeat 0)


-- | Find out path to @ghcversion.h@ file
getGhcVersionPathName :: DynFlags -> IO FilePath
getGhcVersionPathName dflags = do
  candidates <- case ghcVersionFile dflags of
    Just path -> return [path]
    Nothing -> (map (</> "ghcversion.h")) <$>
               (getPackageIncludePath dflags [toInstalledUnitId rtsUnitId])

  found <- filterM doesFileExist candidates
  case found of
      []    -> throwGhcExceptionIO (InstallationError
                                    ("ghcversion.h missing; tried: "
                                      ++ intercalate ", " candidates))
      (x:_) -> return x
