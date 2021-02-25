{-# LANGUAGE DeriveAnyClass     #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE OverloadedStrings  #-}
module Experiments.Types (module Experiments.Types ) where

import           Data.Aeson
import           Data.Version
import           Development.Shake.Classes
import           GHC.Generics
import           Numeric.Natural
import           System.FilePath           (isPathSeparator)

data CabalStack = Cabal | Stack
  deriving (Eq, Show)

data Verbosity = Quiet | Normal | All
  deriving (Eq, Show)
data Config = Config
  { verbosity         :: !Verbosity,
    -- For some reason, the Shake profile files are truncated and won't load
    shakeProfiling    :: !(Maybe FilePath),
    otMemoryProfiling :: !(Maybe FilePath),
    outputCSV         :: !FilePath,
    buildTool         :: !CabalStack,
    ghcideOptions     :: ![String],
    matches           :: ![String],
    repetitions       :: Maybe Natural,
    ghcide            :: FilePath,
    timeoutLsp        :: Int,
    example           :: Example
  }
  deriving (Eq, Show)

data Example
    = GetPackage {exampleName :: !String, exampleModules :: [FilePath], exampleVersion :: Version}
    | UsePackage {examplePath :: FilePath, exampleModules :: [FilePath]}
  deriving (Eq, Generic, Show)
  deriving anyclass (Binary, Hashable, NFData)

getExampleName :: Example -> String
getExampleName UsePackage{examplePath} = map replaceSeparator examplePath
  where
      replaceSeparator x
        | isPathSeparator x = '_'
        | otherwise = x
getExampleName GetPackage{exampleName, exampleVersion} =
    exampleName <> "-" <> showVersion exampleVersion

instance FromJSON Example where
    parseJSON = withObject "example" $ \x -> do
        exampleModules <- x .: "modules"

        path <- x .:? "path"
        case path of
            Just examplePath -> return UsePackage{..}
            Nothing -> do
                exampleName <- x .: "name"
                exampleVersion <- x .: "version"
                return GetPackage {..}

exampleToOptions :: Example -> [String]
exampleToOptions GetPackage{..} =
    ["--example-package-name", exampleName
    ,"--example-package-version", showVersion exampleVersion
    ] ++
    ["--example-module=" <> m | m <- exampleModules]
exampleToOptions UsePackage{..} =
    ["--example-path", examplePath
    ] ++
    ["--example-module=" <> m | m <- exampleModules]
