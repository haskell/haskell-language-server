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
    , exampleDetails   :: Either FilePath ExamplePackage
    , exampleModules   :: [FilePath]
    , exampleExtraArgs :: [String]}
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
                let exampleDetails = Left examplePath
                return Example{..}
            Nothing -> do
                packageName <- x .: "package"
                packageVersion <- x .: "version"
                let exampleDetails = Right ExamplePackage{..}
                return Example{..}

exampleToOptions :: Example -> [String] -> [String]
exampleToOptions Example{exampleDetails = Right ExamplePackage{..}, ..} extraArgs =
    ["--example-package-name", packageName
    ,"--example-package-version", showVersion packageVersion
    ] ++
    ["--example-module=" <> m | m <- exampleModules
    ] ++
    ["--ghcide-options=" <> o | o <- exampleExtraArgs ++ extraArgs]
exampleToOptions Example{exampleDetails = Left examplePath, ..} extraArgs =
    ["--example-path", examplePath
    ] ++
    ["--example-module=" <> m | m <- exampleModules
    ] ++
    ["--ghcide-options=" <> o | o <- exampleExtraArgs ++ extraArgs]
