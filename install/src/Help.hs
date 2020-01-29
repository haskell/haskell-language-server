-- |Module for Help messages and traget descriptions
module Help where

import           Development.Shake
import           Data.List                                ( intersperse
                                                          , intercalate
                                                          )

import           Env
import           Print
import           Version
import           BuildSystem
import           Cabal

stackCommand :: TargetDescription -> String
stackCommand target = "stack install.hs " ++ fst target

cabalCommand :: TargetDescription -> String
cabalCommand target = "cabal v2-run install.hs --project-file install/shake.project " ++ fst target

buildCommand :: TargetDescription -> String
buildCommand | isRunFromCabal = cabalCommand
             | otherwise = stackCommand

printUsage :: Action ()
printUsage = do
  printLine ""
  printLine "Usage:"
  printLineIndented (stackCommand templateTarget)
  printLineIndented "or"
  printLineIndented (cabalCommand templateTarget)

-- | short help message is printed by default
shortHelpMessage :: Action ()
shortHelpMessage = do
  hieVersions <- getHieVersions
  printUsage
  printLine ""
  printLine "Targets:"
  mapM_ (printLineIndented . showTarget (spaces hieVersions)) (targets hieVersions)
  printLine ""
 where
  spaces hieVersions = space (targets hieVersions)
  targets hieVersions =
    [ ("help", "Show help message including all targets")
    , emptyTarget
    , buildTarget
    , buildLatestTarget
    , hieTarget $ last hieVersions
    , buildDataTarget
    , cabalGhcsTarget
    ]

-- | A record that specifies for each build system which versions of @haskell-ide@ can be built.
data BuildableVersions = BuildableVersions
  { stackVersions :: [VersionNumber]
  , cabalVersions :: [VersionNumber]
  }

getDefaultBuildSystemVersions :: BuildableVersions -> [VersionNumber]
getDefaultBuildSystemVersions BuildableVersions {..}
  | isRunFromStack = stackVersions
  | isRunFromCabal = cabalVersions
  | otherwise      = error $ "unknown build system: " ++ buildSystem

helpMessage :: BuildableVersions -> Action ()
helpMessage versions@BuildableVersions {..} = do
  printUsage
  printLine ""
  printLine "Targets:"
  mapM_ (printLineIndented . showTarget spaces) targets
  printLine ""
 where
  spaces = space targets
  -- All targets the shake file supports
  targets :: [(String, String)]
  targets = intercalate
    [emptyTarget]
    [ generalTargets
    , defaultTargets
    , if isRunFromCabal then [cabalGhcsTarget] else []
    , [macosIcuTarget]
    ]

  -- All targets with their respective help message.
  generalTargets = [helpTarget]

  defaultTargets = [buildTarget, buildLatestTarget, buildDataTarget]
    ++ map hieTarget (getDefaultBuildSystemVersions versions)

-- | Empty target. Purpose is to introduce a newline between the targets
emptyTarget :: (String, String)
emptyTarget = ("", "")

templateTarget :: (String, String)
templateTarget = ("<target>", "")

hieTarget :: String -> TargetDescription
hieTarget version =
  ("haskell-ide-" ++ version, "Builds haskell-ide for GHC version " ++ version)

buildTarget :: TargetDescription
buildTarget = ("haskell-ide", "Build haskell-ide with the latest available GHC and the data files")

buildLatestTarget :: TargetDescription
buildLatestTarget = ("latest", "Build haskell-ide with the latest available GHC")

buildDataTarget :: TargetDescription
buildDataTarget =
  ("data", "Get the required data-files for `haskell-ide` (Hoogle DB)")

-- special targets

macosIcuTarget :: TargetDescription
macosIcuTarget = ("icu-macos-fix", "Fixes icu related problems in MacOS")

helpTarget :: TargetDescription
helpTarget = ("help", "Show help message including all targets")

cabalGhcsTarget :: TargetDescription
cabalGhcsTarget =
  ( "ghcs"
  , "Show all GHC versions that can be installed via `cabal-build`."
  )
