{-  Bench history

    A Shake script to analyze the performance of ghcide over the git history of the project

    Driven by a config file `bench/config.yaml` containing the list of Git references to analyze.

    Builds each one of them and executes a set of experiments using the ghcide-bench suite.

    The results of the benchmarks and the analysis are recorded in the file
    system with the following structure:

    bench-results
    ├── <git-reference>
    │   ├── ghc.path                          - path to ghc used to build the binary
    │   ├── ghcide                            - binary for this version
    ├─ <example>
    │   ├── results.csv                           - aggregated results for all the versions
    │   └── <git-reference>
    │       ├── <experiment>.benchmark-gcStats    - RTS -s output
    │       ├── <experiment>.csv                  - stats for the experiment
    │       ├── <experiment>.svg                  - Graph of bytes over elapsed time
    │       ├── <experiment>.diff.svg             - idem, including the previous version
    │       ├── <experiment>.log                  - ghcide-bench output
    │       └── results.csv                       - results of all the experiments for the example
    ├── results.csv        - aggregated results of all the experiments and versions
    └── <experiment>.svg   - graph of bytes over elapsed time, for all the included versions

   For diff graphs, the "previous version" is the preceding entry in the list of versions
   in the config file. A possible improvement is to obtain this info via `git rev-list`.

   To execute the script:

   > cabal/stack bench

   To build a specific analysis, enumerate the desired file artifacts

   > stack bench --ba "bench-results/HEAD/results.csv bench-results/HEAD/edit.diff.svg"
   > cabal bench --benchmark-options "bench-results/HEAD/results.csv bench-results/HEAD/edit.diff.svg"

 -}
{-# LANGUAGE DeriveAnyClass    #-}
{-# LANGUAGE DerivingStrategies#-}
{-# LANGUAGE TypeFamilies      #-}
{-# OPTIONS -Wno-orphans #-}

import Data.Foldable (find)
import Data.Yaml (FromJSON (..), decodeFileThrow)
import Development.Benchmark.Rules
import Development.Shake
import Experiments.Types (Example, exampleToOptions)
import qualified Experiments.Types as E
import GHC.Generics (Generic)
import Numeric.Natural (Natural)


config :: FilePath
config = "bench/config.yaml"

-- | Read the config without dependency
readConfigIO :: FilePath -> IO (Config BuildSystem)
readConfigIO = decodeFileThrow

instance IsExample Example where getExampleName = E.getExampleName
type instance RuleResult GetExample = Maybe Example
type instance RuleResult GetExamples = [Example]

main :: IO ()
main = shakeArgs shakeOptions {shakeChange = ChangeModtimeAndDigest} $ do
  createBuildSystem $ \resource -> do
      configStatic <- liftIO $ readConfigIO config
      let build = outputFolder configStatic
      buildRules build ghcideBuildRules
      benchRules build resource (MkBenchRules (benchGhcide $ samples configStatic) "ghcide")
      csvRules build
      svgRules build
      action $ allTargets build

ghcideBuildRules :: MkBuildRules BuildSystem
ghcideBuildRules = MkBuildRules findGhcForBuildSystem "ghcide" buildGhcide

--------------------------------------------------------------------------------

data Config buildSystem = Config
  { experiments :: [Unescaped String],
    examples :: [Example],
    samples :: Natural,
    versions :: [GitCommit],
    -- | Output folder ('foo' works, 'foo/bar' does not)
    outputFolder :: String,
    buildTool :: buildSystem
  }
  deriving (Generic, Show)
  deriving anyclass (FromJSON)

createBuildSystem :: (Resource -> Rules a) -> Rules a
createBuildSystem userRules = do
  readConfig <- newCache $ \fp -> need [fp] >> liftIO (readConfigIO fp)

  _ <- addOracle $ \GetExperiments {} -> experiments <$> readConfig config
  _ <- addOracle $ \GetVersions {} -> versions <$> readConfig config
  _ <- addOracle $ \GetExamples{} -> examples <$> readConfig config
  _ <- addOracle $ \(GetExample name) -> find (\e -> getExampleName e == name) . examples <$> readConfig config
  _ <- addOracle $ \GetBuildSystem {} -> buildTool <$> readConfig config

  benchResource <- newResource "ghcide-bench" 1

  userRules benchResource

--------------------------------------------------------------------------------

buildGhcide :: BuildSystem -> [CmdOption] -> FilePath -> Action ()
buildGhcide Cabal args out = do
    command_ args "cabal"
        ["install"
        ,"exe:ghcide"
        ,"--installdir=" ++ out
        ,"--install-method=copy"
        ,"--overwrite-policy=always"
        ,"--ghc-options=-rtsopts"
        ]

buildGhcide Stack args out =
    command_ args "stack"
        ["--local-bin-path=" <> out
        ,"build"
        ,"ghcide:ghcide"
        ,"--copy-bins"
        ,"--ghc-options=-rtsopts"
        ]

benchGhcide
  :: Natural -> BuildSystem -> [CmdOption] -> BenchProject Example -> Action ()
benchGhcide samples buildSystem args BenchProject{..} =
  command_ args "ghcide-bench" $
    [ "--timeout=3000",
        "-v",
        "--samples=" <> show samples,
        "--csv="     <> outcsv,
        "--ghcide="  <> exePath,
        "--select",
        unescaped (unescapeExperiment experiment)
    ] ++
    exampleToOptions example ++
    [ "--stack" | Stack == buildSystem
    ] ++
    exeExtraArgs

