{-# LANGUAGE DeriveAnyClass     #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE OverloadedStrings  #-}
module Experiments.Types (module Experiments.Types ) where

import           Control.DeepSeq
import           Data.Aeson
import           Data.Binary     (Binary)
import           Data.Hashable   (Hashable)
import           Data.Maybe      (fromMaybe)
import           Data.Version
import           GHC.Generics
import           Numeric.Natural

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
    example           :: Example,
    lspConfig         :: Bool
  }
  deriving (Eq, Show)

data ExamplePackage = ExamplePackage {packageName :: !String, packageVersion :: !Version}
  deriving (Eq, Generic, Show)
  deriving anyclass (Binary, Hashable, NFData)

data Example = Example
    { exampleName      :: !String
    , exampleDetails   :: ExampleDetails
    , exampleModules   :: [FilePath]
    , exampleExtraArgs :: [String]}
  deriving (Eq, Generic, Show)
  deriving anyclass (Binary, Hashable, NFData)

data ExampleDetails
  = ExamplePath FilePath -- ^ directory where the package is located
  | ExampleHackage ExamplePackage -- ^ package from hackage
  | ExampleScript FilePath -- ^ location of the script we are running
                  [String] -- ^ extra arguments for the script
  deriving (Eq, Generic, Show)
  deriving anyclass (Binary, Hashable, NFData)

instance FromJSON Example where
    parseJSON = withObject "example" $ \x -> do
        exampleName <- x .: "name"
        exampleModules <- x .: "modules"
        exampleExtraArgs <- fromMaybe [] <$> x .:? "extra-args"

        path <- x .:? "path"
        case path of
            Just examplePath -> do
                script <- fromMaybe False <$> x.:? "script"
                args <- fromMaybe [] <$> x .:? "script-args"
                let exampleDetails
                      | script = ExampleScript examplePath args
                      | otherwise = ExamplePath examplePath
                return Example{..}
            Nothing -> do
                packageName <- x .: "package"
                packageVersion <- x .: "version"
                let exampleDetails = ExampleHackage ExamplePackage{..}
                return Example{..}

exampleToOptions :: Example -> [String] -> [String]
exampleToOptions Example{exampleDetails = ExampleHackage ExamplePackage{..}, ..} extraArgs =
    ["--example-package-name", packageName
    ,"--example-package-version", showVersion packageVersion
    ,"--example-name", exampleName
    ] ++
    ["--example-module=" <> m | m <- exampleModules
    ] ++
    ["--ghcide-options=" <> o | o <- exampleExtraArgs ++ extraArgs]
exampleToOptions Example{exampleDetails = ExamplePath examplePath, ..} extraArgs =
    ["--example-path", examplePath
    ,"--example-name", exampleName
    ] ++
    ["--example-module=" <> m | m <- exampleModules
    ] ++
    ["--ghcide-options=" <> o | o <- exampleExtraArgs ++ extraArgs]
exampleToOptions Example{exampleDetails = ExampleScript examplePath exampleArgs, ..} extraArgs =
    ["--example-script", examplePath
    ,"--example-name", exampleName
    ] ++
    ["--example-script-args=" <> o | o <- exampleArgs
    ] ++
    ["--example-module=" <> m | m <- exampleModules
    ] ++
    ["--ghcide-options=" <> o | o <- exampleExtraArgs ++ extraArgs]
