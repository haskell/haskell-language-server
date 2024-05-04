
{-  Bench history

    A Shake script to analyze the performance of HLS over the git history of the project

    Driven by a config file `bench/config.yaml` containing the list of Git references to analyze.

    Builds each one of them and executes a set of experiments using the ghcide-bench suite.

    The results of the benchmarks and the analysis are recorded in the file
    system with the following structure:

    bench-results
    ├── <git-reference>
    │   ├── ghc.path                          - path to ghc used to build the binary
    │   └── haskell-language-server           - binary for this version
    ├─ <example>
    │   ├── results.csv                           - aggregated results for all the versions
    │   └── <git-reference>
    |       └── <HLS plugin>
    │           ├── <experiment>.gcStats.log          - RTS -s output
    │           ├── <experiment>.csv                  - stats for the experiment
    │           ├── <experiment>.svg                  - Graph of bytes over elapsed time
    │           ├── <experiment>.diff.svg             - idem, including the previous version
    │           ├── <experiment>.log                  - ghcide-bench output
    │           └── results.csv                       - results of all the experiments for the example
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
{-# LANGUAGE DeriveAnyClass     #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE TypeFamilies       #-}
{-# OPTIONS -Wno-orphans #-}

import           Control.Lens                (preview, (^.))
import           Control.Monad.Extra
import           Data.Aeson                  (Value (..), encode)
import           Data.Aeson.Lens
import           Data.Default
import           Data.Foldable               (find)
import qualified Data.Map.Strict             as Map
import           Data.Maybe
import           Data.Text                   (unpack)
import           Data.Yaml                   (FromJSON (..), ToJSON (toJSON),
                                              decodeFileThrow)
import           Development.Benchmark.Rules hiding (parallelism)
import           Development.Shake           (Action,
                                              Change (ChangeModtimeAndDigestInput),
                                              CmdOption (Cwd, StdinBS),
                                              RuleResult, Rules,
                                              ShakeOptions (shakeChange, shakeThreads),
                                              actionBracket, addOracle,
                                              askOracle, command, command_,
                                              getDirectoryFiles, liftIO, need,
                                              newCache, shakeArgsWith,
                                              shakeOptions, versioned, want)
import           Development.Shake.Classes
import           Experiments.Types           (Example (exampleName),
                                              exampleToOptions)
import           GHC.Exts                    (toList)
import           GHC.Generics                (Generic)
import           HlsPlugins                  (idePlugins)
import qualified Ide.Plugin.Config           as Plugin
import           Ide.Types                   hiding (Config)
import           Numeric.Natural             (Natural)
import           System.Console.GetOpt
import           System.Directory
import           System.FilePath
import           System.IO.Error             (tryIOError)

configPath :: FilePath
configPath = "bench/config.yaml"

configOpt :: OptDescr (Either String FilePath)
configOpt = Option [] ["config"] (ReqArg Right configPath) "config file"

binaryName :: String
binaryName = "haskell-language-server"

-- | Read the config without dependency
readConfigIO :: FilePath -> IO (Config BuildSystem)
readConfigIO = decodeFileThrow

instance IsExample Example where getExampleName = exampleName
type instance RuleResult GetExample = Maybe Example
type instance RuleResult GetExamples = [Example]

shakeOpts :: ShakeOptions
shakeOpts =
    shakeOptions{shakeChange = ChangeModtimeAndDigestInput, shakeThreads = 0}

main :: IO ()
main = shakeArgsWith shakeOpts [configOpt] $ \configs wants -> pure $ Just $ do
  let config = fromMaybe configPath $ listToMaybe configs
  _configStatic <- createBuildSystem config
  case wants of
      [] -> want ["all"]
      _  -> want wants

hlsBuildRules :: MkBuildRules BuildSystem
hlsBuildRules = MkBuildRules findGhcForBuildSystem binaryName projectDepends buildHls
  where
      recordDepends path =
        need . map (path </>) =<< getDirectoryFiles path ["//*.hs"]
      projectDepends = do
        recordDepends "src"
        recordDepends "exe"
        recordDepends "plugins"
        recordDepends "ghcide/session-loader"
        recordDepends "ghcide/src"
        recordDepends "hls-graph/src"
        recordDepends "hls-plugin-api/src"
        need =<< getDirectoryFiles "." ["*.cabal"]

--------------------------------------------------------------------------------
data Config buildSystem = Config
  { experiments     :: [Unescaped String],
    configurations  :: [ConfigurationDescriptor],
    examples        :: [Example],
    samples         :: Natural,
    versions        :: [GitCommit],
    -- | Output folder ('foo' works, 'foo/bar' does not)
    outputFolder    :: String,
    buildTool       :: buildSystem,
    profileInterval :: Maybe Double,
    parallelism     :: Natural
  }
  deriving (Generic, Show)
  deriving anyclass (FromJSON)

createBuildSystem :: FilePath -> Rules (Config BuildSystem)
createBuildSystem config = do
  readConfig <- newCache $ \fp -> need [fp] >> liftIO (readConfigIO fp)

  _ <- addOracle $ \GetExperiments {} -> experiments <$> readConfig config
  _ <- addOracle $ \GetVersions {} -> versions <$> readConfig config
  _ <- versioned 1 $ addOracle $ \GetExamples{} -> examples <$> readConfig config
  _ <- versioned 1 $ addOracle $ \(GetExample name) -> find (\e -> getExampleName e == name) . examples <$> readConfig config
  _ <- addOracle $ \GetBuildSystem {} -> buildTool <$> readConfig config
  _ <- addOracle $ \GetSamples{} -> samples <$> readConfig config
  _ <- addOracle $ \GetConfigurations{} -> do
      Config{configurations} <- readConfig config
      return [ Configuration confName (encode $ disableAllPluginsBut (`elem` confPlugins))
        | ConfigurationDescriptor{..} <- configurations
        ]

  configStatic <- liftIO $ readConfigIO config
  let build = outputFolder configStatic

  buildRules build hlsBuildRules
  benchRules build (MkBenchRules (askOracle $ GetSamples ()) benchHls warmupHls "haskell-language-server" (parallelism configStatic))
  addGetParentOracle
  csvRules build
  svgRules build
  heapProfileRules build
  phonyRules "" binaryName NoProfiling build (examples configStatic)

  whenJust (profileInterval configStatic) $ \i -> do
    phonyRules "profiled-" binaryName (CheapHeapProfiling i) build (examples configStatic)

  return configStatic

disableAllPluginsBut :: (PluginId -> Bool) -> Plugin.Config
disableAllPluginsBut pred = def {Plugin.plugins = pluginsMap} where
    pluginsMap = Map.fromList
        [ (plugin, def { Plugin.plcGlobalOn = globalOn})
        | PluginDescriptor{pluginId = plugin} <- plugins
        , let globalOn =
                    -- ghcide-core is required, nothing works without it
                   plugin == "ghcide-core"
                    -- document symbols is required by the benchmark suite
                || plugin == "ghcide-hover-and-symbols"
                || pred plugin
        ]
    IdePlugins plugins = idePlugins mempty

newtype GetSamples = GetSamples () deriving newtype (Binary, Eq, Hashable, NFData, Show)
type instance RuleResult GetSamples = Natural

--------------------------------------------------------------------------------

buildHls :: BuildSystem -> ProjectRoot -> OutputFolder -> Action ()
buildHls Cabal root out = actionBracket
    (do
        projectLocalExists <- liftIO $ doesFileExist projectLocal
        when projectLocalExists $ liftIO $ do
            void $ tryIOError $ removeFile (projectLocal <.> "restore-after-benchmark")
            renameFile projectLocal (projectLocal <.> "restore-after-benchmark")
        liftIO $ writeFile projectLocal $ unlines
            ["package haskell-language-server"
            ,"  ghc-options: -eventlog -rtsopts"
            ]
        return projectLocalExists)
    (\projectLocalExists -> do
        removeFile projectLocal
        when projectLocalExists $
          renameFile (projectLocal <.> "restore-after-benchmark") projectLocal
    ) $ \_ -> command_ [Cwd root] "cabal"
        ["install"
        ,"haskell-language-server:exe:haskell-language-server"
        ,"--installdir=" ++ out
        ,"--install-method=copy"
        ,"--overwrite-policy=always"
        ]
    where
        projectLocal = root </> "cabal.project.local"

buildHls Stack root out =
    command_ [Cwd root] "stack"
        ["--local-bin-path=" <> out
        ,"build"
        ,"haskell-language-server:haskell-language-server"
        ,"--copy-bins"
        ,"--ghc-options=-rtsopts"
        ,"--ghc-options=-eventlog"
        ]

benchHls
  :: Natural -> BuildSystem -> [CmdOption] -> BenchProject Example -> Action ()
benchHls samples buildSystem args BenchProject{..} = do
  command_ ([StdinBS configuration] ++ args) "ghcide-bench" $
    [ "--timeout=300",
      "--no-clean",
        "-v",
        "--samples=" <> show samples,
        "--csv="     <> outcsv,
        "--ghcide="  <> exePath,
        "--select",
        unescaped (unescapeExperiment experiment),
        "--lsp-config"
    ] ++
    exampleToOptions example exeExtraArgs ++
    [ "--stack" | Stack == buildSystem
    ]

warmupHls :: BuildSystem -> FilePath -> [CmdOption] -> Example -> Action ()
warmupHls buildSystem exePath args example = do
  command args "ghcide-bench" $
    [ "--no-clean",
      "-v",
      "--samples=1",
      "--ghcide=" <> exePath,
      "--select=hover"
    ] ++
    exampleToOptions example [] ++
    [ "--stack" | Stack == buildSystem
    ]

--------------------------------------------------------------------------------
data ConfigurationDescriptor = ConfigurationDescriptor
    { confName    :: String
    , confPlugins :: [PluginId]
    }
    deriving Show

instance FromJSON ConfigurationDescriptor where
    parseJSON (String s) = pure $ ConfigurationDescriptor (unpack s) [PluginId s]
    parseJSON o@Object{} = do
        let keymap = o ^. _Object
            matchKey = preview _String . toJSON
        case toList keymap of
            -- excuse the aeson 2.0 compatibility hack
            [(matchKey -> Just name, Array values)] -> do
                pluginIds <- traverse parseJSON values
                pure $ ConfigurationDescriptor (unpack name) (map PluginId $ toList pluginIds)
            other -> fail $ "Expected object with name and array of plugin ids: " <> show other
    parseJSON _ = fail "Expected plugin id or object with name and array of plugin ids"
