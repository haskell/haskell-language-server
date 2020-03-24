module Util (setupDynFlags, getLibdir) where

-- Set the GHC libdir to the nix libdir if it's present.
import qualified GHC.Paths                     as GHCPaths
import           DynFlags                       ( gopt_unset
                                                , GhcMode(CompManager)
                                                , HscTarget(HscNothing)
                                                , GhcLink(LinkInMemory)
                                                , GeneralFlag
                                                  ( Opt_IgnoreInterfacePragmas
                                                  , Opt_IgnoreOptimChanges
                                                  , Opt_WriteInterface
                                                  )
                                                , gopt_set
                                                , updOptLevel
                                                , DynFlags(..)
                                                )
import           Data.Maybe                     ( fromMaybe )
import           Development.IDE.GHC.Util       ( setDefaultHieDir
                                                , dontWriteHieFiles
                                                )
import           System.Environment             ( lookupEnv )
import           GHC                            (GhcMonad, setSessionDynFlags )
import           Data.Functor                   ( void )

setupDynFlags :: GhcMonad f => FilePath -> DynFlags -> f ()
setupDynFlags cacheDir =
  void
    . setSessionDynFlags
    -- disabled, generated directly by ghcide instead
    . flip gopt_unset Opt_WriteInterface
    -- disabled, generated directly by ghcide instead
    -- also, it can confuse the interface stale check
    . dontWriteHieFiles
    . setHiDir cacheDir
    . setDefaultHieDir cacheDir
    . setIgnoreInterfacePragmas
    . setLinkerOptions
    . disableOptimisation

getLibdir :: IO FilePath
getLibdir = fromMaybe GHCPaths.libdir <$> lookupEnv "NIX_GHC_LIBDIR"

-- we don't want to generate object code so we compile to bytecode
-- (HscInterpreted) which implies LinkInMemory

-- HscInterpreted
setLinkerOptions :: DynFlags -> DynFlags
setLinkerOptions df =
  df { ghcLink = LinkInMemory, hscTarget = HscNothing, ghcMode = CompManager }

setIgnoreInterfacePragmas :: DynFlags -> DynFlags
setIgnoreInterfacePragmas df =
  gopt_set (gopt_set df Opt_IgnoreInterfacePragmas) Opt_IgnoreOptimChanges

disableOptimisation :: DynFlags -> DynFlags
disableOptimisation df = updOptLevel 0 df

setHiDir :: FilePath -> DynFlags -> DynFlags
setHiDir f d =
    -- override user settings to avoid conflicts leading to recompilation
  d { hiDir = Just f }
