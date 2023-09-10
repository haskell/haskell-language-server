{-# LANGUAGE CPP #-}

-- | Compat module Interface file relevant code.
module Development.IDE.GHC.Compat.Iface (
    writeIfaceFile,
    cannotFindModule,
    ) where

import           Development.IDE.GHC.Compat.Env
import           Development.IDE.GHC.Compat.Outputable
import           GHC

-- See Note [Guidelines For Using CPP In GHCIDE Import Statements]

#if !MIN_VERSION_ghc(9,2,0)
import qualified GHC.Driver.Finder                     as Finder
import           GHC.Driver.Types                      (FindResult)
import qualified GHC.Iface.Load                        as Iface
#endif

#if MIN_VERSION_ghc(9,2,0)
import qualified GHC.Iface.Load                        as Iface
import           GHC.Unit.Finder.Types                 (FindResult)
#endif

#if MIN_VERSION_ghc(9,3,0)
import           GHC.Driver.Session                    (targetProfile)
#endif

writeIfaceFile :: HscEnv -> FilePath -> ModIface -> IO ()
#if MIN_VERSION_ghc(9,3,0)
writeIfaceFile env fp iface = Iface.writeIface (hsc_logger env) (targetProfile $ hsc_dflags env) fp iface
#elif MIN_VERSION_ghc(9,2,0)
writeIfaceFile env fp iface = Iface.writeIface (hsc_logger env) (hsc_dflags env) fp iface
#else
writeIfaceFile env = Iface.writeIface (hsc_dflags env)
#endif

cannotFindModule :: HscEnv -> ModuleName -> FindResult -> SDoc
cannotFindModule env modname fr =
#if MIN_VERSION_ghc(9,2,0)
    Iface.cannotFindModule env modname fr
#else
    Finder.cannotFindModule (hsc_dflags env) modname fr
#endif
