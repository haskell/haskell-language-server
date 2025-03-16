{-# LANGUAGE CPP #-}

-- | Compat module Interface file relevant code.
module Development.IDE.GHC.Compat.Iface (
    writeIfaceFile,
    cannotFindModule,
    ) where

import           Development.IDE.GHC.Compat.Env
import           Development.IDE.GHC.Compat.Outputable
import           GHC
import           GHC.Driver.Session                    (targetProfile)
import qualified GHC.Iface.Load                        as Iface
import           GHC.Unit.Finder.Types                 (FindResult)

-- See Note [Guidelines For Using CPP In GHCIDE Import Statements]

#if MIN_VERSION_ghc(9,7,0)
import           GHC.Iface.Errors.Ppr                  (missingInterfaceErrorDiagnostic)
import           GHC.Iface.Errors.Types                (IfaceMessage)
#endif

writeIfaceFile :: HscEnv -> FilePath -> ModIface -> IO ()
#if MIN_VERSION_ghc(9,11,0)
writeIfaceFile env fp iface = Iface.writeIface (hsc_logger env) (targetProfile $ hsc_dflags env) (Iface.flagsToIfCompression $ hsc_dflags env) fp iface
#else
writeIfaceFile env fp iface = Iface.writeIface (hsc_logger env) (targetProfile $ hsc_dflags env) fp iface
#endif

cannotFindModule :: HscEnv -> ModuleName -> FindResult -> SDoc
cannotFindModule env modname fr =
#if MIN_VERSION_ghc(9,7,0)
    missingInterfaceErrorDiagnostic (defaultDiagnosticOpts @IfaceMessage) $ Iface.cannotFindModule env modname fr
#else
    Iface.cannotFindModule env modname fr
#endif
