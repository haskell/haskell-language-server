{-# OPTIONS_GHC -Wno-orphans #-}
{-# LANGUAGE DeriveAnyClass     #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Ide.Plugin.CabalProject.Orphans where

import           Control.DeepSeq
import           Distribution.Fields.Field
import           Distribution.Parsec.Position
-- import Control.DeepSeq (NFData)
import qualified Distribution.Solver.Types.ProjectConfigPath as PCPath
import           GHC.Generics                                (Generic)

import qualified Distribution.Client.ProjectConfig.Types     as PC
import           Ide.Plugin.Cabal.Orphans                    ()


orphans = undefined
-- Project Config Orphans


-- more nfdata instances i need:
-- Distribution.Client.Types.SourceRepo.SourceRepositoryPackage []
-- NFData (NubList PathTemplate)
-- NFData (InstallDirs (Flag PathTemplate))
-- NFData (NubList FilePath)

-- deriving instance NFData PCPath.ProjectConfigPath

-- instance NFData PC.ProjectConfig where
--   rnf !_ = ()

-- {-# OPTIONS_GHC -Wno-orphans #-}
-- {-# LANGUAGE FlexibleInstances #-}
-- {-# LANGUAGE RecordWildCards    #-}

-- module Ide.Plugin.CabalProject.Orphans () where

-- import Control.DeepSeq            ( NFData, rnf )
-- import Distribution.Compat.Prelude ( genericRnf )
-- import Distribution.Verbosity (Verbosity)
-- import Distribution.Verbosity.Internal (VerbosityLevel(..), VerbosityFlag(..))
-- import Ide.Plugin.Cabal.Orphans ()

-- import Distribution.Client.ProjectConfig.Types
--   ( BuildTimeSettings(..) )
-- import Distribution.Simple.InstallDirs.Internal
--   ( PathComponent(..), PathTemplateVariable(..)
--   )
-- import Distribution.Simple.InstallDirs
--   ( PathTemplate(..) )
-- import Control.DeepSeq ( NFData(rnf) )
-- import Distribution.Client.BuildReports.Types (ReportLevel)

-- import Distribution.Client.Types.Repo (RemoteRepo, LocalRepo)

-- -- PathTemplate
-- instance NFData PathTemplate where
--   rnf = genericRnf

-- instance NFData PathComponent where
--   rnf = genericRnf

-- instance NFData PathTemplateVariable where
--   rnf = genericRnf

-- -- Verbosity
-- instance NFData Verbosity where
--   rnf = genericRnf

-- -- instance NFData VerbosityLevel where
-- --   rnf = genericRnf

-- -- instance NFData VerbosityFlag where
-- --   rnf = genericRnf

-- -- ReportLevel
-- instance NFData ReportLevel where
--   rnf = genericRnf

-- -- RemoteRepo
-- instance NFData RemoteRepo where
--   rnf = genericRnf

-- instance NFData LocalRepo where
--   rnf = genericRnf

-- instance NFData BuildTimeSettings where
--   rnf bts =
--       rnf (buildSettingDryRun                   bts)
--     `seq` rnf (buildSettingOnlyDeps               bts)
--     `seq` rnf (buildSettingOnlyDownload           bts)
--     `seq` rnf (buildSettingSummaryFile            bts)
--     `seq` ()
--     `seq` rnf (buildSettingLogVerbosity           bts)
--     `seq` rnf (buildSettingBuildReports           bts)
--     `seq` rnf (buildSettingReportPlanningFailure  bts)
--     `seq` rnf (buildSettingSymlinkBinDir          bts)
--     `seq` rnf (buildSettingNumJobs                bts)
--     `seq` rnf (buildSettingKeepGoing              bts)
--     `seq` rnf (buildSettingOfflineMode            bts)
--     `seq` rnf (buildSettingKeepTempFiles          bts)
--     `seq` rnf (buildSettingRemoteRepos            bts)
--     `seq` rnf (buildSettingLocalNoIndexRepos      bts)
--     `seq` rnf (buildSettingCacheDir               bts)
--     `seq` rnf (buildSettingHttpTransport          bts)
--     `seq` rnf (buildSettingIgnoreExpiry           bts)
--     `seq` rnf (buildSettingProgPathExtra          bts)
--     `seq` rnf (buildSettingHaddockOpen            bts)
--     `seq` ()
-- {-# OPTIONS_GHC -Wno-orphans #-}
-- module Ide.Plugin.CabalProject.Orphans () where

-- import Control.DeepSeq ( NFData, rnf)
-- import Distribution.Compat.Prelude (genericRnf)
-- import           Ide.Plugin.Cabal.Orphans                    ()
-- import           Distribution.Client.ProjectConfig.Types  (BuildTimeSettings(..))
-- import GHC.Generics               ( Generic )
-- import Control.DeepSeq            ( NFData(rnf) )
-- import Distribution.Simple.InstallDirs ( PathTemplate )
-- import Distribution.Verbosity     ( Verbosity )
-- import Distribution.Client.BuildReports.Types ( ReportLevel )
-- import Distribution.Types.ParStrat      ( ParStratInstall )
-- import Distribution.Client.Types.Repo   ( RemoteRepo, LocalRepo )

-- -- 1) Orphan NFData instances for all the “missing” imported types.
-- instance NFData PathTemplate     where rnf = genericRnf
-- instance NFData Verbosity        where rnf = genericRnf
-- instance NFData ReportLevel      where rnf = genericRnf
-- instance NFData ParStratInstall  where rnf = genericRnf
-- instance NFData RemoteRepo       where rnf = genericRnf
-- instance NFData LocalRepo        where rnf = genericRnf

-- instance NFData BuildTimeSettings where
--   rnf bts =
--       rnf (buildSettingDryRun                   bts)
--     `seq` rnf (buildSettingOnlyDeps               bts)
--     `seq` rnf (buildSettingOnlyDownload           bts)
--     `seq` rnf (buildSettingSummaryFile            bts)
--     `seq` ()
--     `seq` rnf (buildSettingLogVerbosity           bts)
--     `seq` rnf (buildSettingBuildReports           bts)
--     `seq` rnf (buildSettingReportPlanningFailure  bts)
--     `seq` rnf (buildSettingSymlinkBinDir          bts)
--     `seq` rnf (buildSettingNumJobs                bts)
--     `seq` rnf (buildSettingKeepGoing              bts)
--     `seq` rnf (buildSettingOfflineMode            bts)
--     `seq` rnf (buildSettingKeepTempFiles          bts)
--     `seq` rnf (buildSettingRemoteRepos            bts)
--     `seq` rnf (buildSettingLocalNoIndexRepos      bts)
--     `seq` rnf (buildSettingCacheDir               bts)
--     `seq` rnf (buildSettingHttpTransport          bts)
--     `seq` rnf (buildSettingIgnoreExpiry           bts)
--     `seq` rnf (buildSettingProgPathExtra          bts)
--     `seq` rnf (buildSettingHaddockOpen            bts)
--     `seq` ()


-- import Control.DeepSeq (NFData(rnf))
-- import qualified Data.Map as Map
-- import qualified Data.Set as Set
-- import           Ide.Plugin.Cabal.Orphans                    ()


-- import Distribution.Client.ProjectConfig.Types
--   ( ProjectConfig(..)
--   , ProjectConfigBuildOnly
--   , ProjectConfigShared
--   , ProjectConfigProvenance
--   , PackageConfig
--   , MapMappend(getMapMappend)
--   )
-- import Distribution.Client.Types.SourceRepo
--   ( SourceRepoList )
-- import Distribution.Types.PackageVersionConstraint
--   ( PackageVersionConstraint )
-- import Distribution.Types.PackageName
--   ( PackageName )

-- -- | The only “deep” NFData: we pattern‐match on all ten fields and
-- --   rnf them.  For the Set we convert to a list so we don’t need
-- --   a Set‐instance; for the MapMappend we unwrap to a list of pairs.
-- instance NFData ProjectConfig where
--   rnf (ProjectConfig
--          pkgs
--          pkgsOpt
--          pkgsRepo
--          pkgsNamed
--          buildOnly
--          shared
--          prov
--          allPkgs
--          localPkgs
--          specificM) =
--        rnf pkgs
--     `seq` rnf pkgsOpt
--     `seq` rnf pkgsRepo
--     `seq` rnf pkgsNamed
--     `seq` rnf buildOnly
--     `seq` rnf shared
--     `seq` rnf (Set.toList prov)
--     `seq` rnf allPkgs
--     `seq` rnf localPkgs
--     `seq` rnf (Map.toList (getMapMappend specificM))

-- -- Trivial NFData instances for all of the immediate field types
-- -- so that the above rnf calls will compile.

-- instance NFData SourceRepoList where
--   rnf _ = ()

-- instance NFData ProjectConfigBuildOnly where
--   rnf _ = ()

-- instance NFData ProjectConfigShared where
--   rnf _ = ()

-- instance NFData ProjectConfigProvenance where
--   rnf _ = ()

-- instance NFData PackageConfig where
--   rnf _ = ()


------------------------------------------------- OLD


