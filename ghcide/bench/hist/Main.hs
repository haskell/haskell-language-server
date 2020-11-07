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
{-# LANGUAGE ApplicativeDo     #-}
{-# LANGUAGE DeriveAnyClass    #-}
{-# LANGUAGE DerivingStrategies#-}
{-# LANGUAGE TypeFamilies      #-}

import Control.Applicative (Alternative (empty))
import Control.Monad (when, forM, forM_, replicateM)
import Data.Char (toLower)
import Data.Foldable (find)
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Yaml ((.!=), (.:?), FromJSON (..), ToJSON (..), Value (..), decodeFileThrow)
import Development.Shake
import Development.Shake.Classes (Binary, Hashable, NFData)
import Experiments.Types (getExampleName, exampleToOptions, Example(..))
import GHC.Exts (IsList (..))
import GHC.Generics (Generic)
import qualified Graphics.Rendering.Chart.Backend.Diagrams as E
import Graphics.Rendering.Chart.Easy ((.=))
import qualified Graphics.Rendering.Chart.Easy as E
import Numeric.Natural (Natural)
import System.Directory
import System.FilePath
import qualified Text.ParserCombinators.ReadP as P
import Text.Read (Read (..), get, readMaybe, readP_to_Prec)
import GHC.Stack (HasCallStack)
import Data.List (transpose)

config :: FilePath
config = "bench/config.yaml"

-- | Read the config without dependency
readConfigIO :: FilePath -> IO Config
readConfigIO = decodeFileThrow

newtype GetExample = GetExample String deriving newtype (Binary, Eq, Hashable, NFData, Show)
newtype GetExamples = GetExamples () deriving newtype (Binary, Eq, Hashable, NFData, Show)
newtype GetSamples = GetSamples () deriving newtype (Binary, Eq, Hashable, NFData, Show)
newtype GetExperiments = GetExperiments () deriving newtype (Binary, Eq, Hashable, NFData, Show)
newtype GetVersions = GetVersions () deriving newtype (Binary, Eq, Hashable, NFData, Show)
newtype GetParent = GetParent Text deriving newtype (Binary, Eq, Hashable, NFData, Show)
newtype GetCommitId = GetCommitId String deriving newtype (Binary, Eq, Hashable, NFData, Show)

type instance RuleResult GetExample = Maybe Example
type instance RuleResult GetExamples = [Example]
type instance RuleResult GetSamples = Natural
type instance RuleResult GetExperiments = [Unescaped String]
type instance RuleResult GetVersions = [GitCommit]
type instance RuleResult GetParent = Text
type instance RuleResult GetCommitId = String

main :: IO ()
main = shakeArgs shakeOptions {shakeChange = ChangeModtimeAndDigest} $ do
  want ["all"]

  readConfig <- newCache $ \fp -> need [fp] >> liftIO (readConfigIO fp)

  _ <- addOracle $ \GetSamples {} -> samples <$> readConfig config
  _ <- addOracle $ \GetExperiments {} -> experiments <$> readConfig config
  _ <- addOracle $ \GetVersions {} -> versions <$> readConfig config
  _ <- addOracle $ \GetExamples{} -> examples <$> readConfig config
  _ <- addOracle $ \(GetParent name) -> findPrev name . versions <$> readConfig config
  _ <- addOracle $ \(GetExample name) -> find (\e -> getExampleName e == name) . examples <$> readConfig config

  let readVersions = askOracle $ GetVersions ()
      readExperiments = askOracle $ GetExperiments ()
      readExamples = askOracle $ GetExamples ()
      readSamples = askOracle $ GetSamples ()
      getParent = askOracle . GetParent
      getExample = askOracle . GetExample

  configStatic <- liftIO $ readConfigIO config
  ghcideBenchPath <- ghcideBench <$> liftIO (readConfigIO config)
  let build = outputFolder configStatic
      buildSystem = buildTool configStatic

  phony "all" $ do
    Config {..} <- readConfig config

    need $
      [build </> getExampleName e </> "results.csv" | e <- examples ] ++
      [build </> "results.csv"]
        ++ [ build </> getExampleName ex </> escaped (escapeExperiment e) <.> "svg"
             | e <- experiments
             , ex <- examples
           ]
        ++ [ build </> getExampleName ex </> T.unpack (humanName ver) </> escaped (escapeExperiment e) <.> mode <.> "svg"
             | e <- experiments,
               ex <- examples,
               ver <- versions,
               mode <- ["", "diff"]
           ]

  build -/- "*/commitid" %> \out -> do
      alwaysRerun

      let [_,ver,_] = splitDirectories out
      mbEntry <- find ((== T.pack ver) . humanName) <$> readVersions
      let gitThing :: String
          gitThing = maybe ver (T.unpack . gitName) mbEntry
      Stdout commitid <- command [] "git" ["rev-list", "-n", "1", gitThing]
      writeFileChanged out $ init commitid

  priority 10 $ [ build -/- "HEAD/ghcide"
                , build -/- "HEAD/ghc.path"
                ]
    &%> \[out, ghcpath] -> do
      liftIO $ createDirectoryIfMissing True $ dropFileName out
      need =<< getDirectoryFiles "." ["src//*.hs", "exe//*.hs", "ghcide.cabal"]
      cmd_ $ buildGhcide buildSystem (takeDirectory out)
      ghcLoc <- findGhc "." buildSystem
      writeFile' ghcpath ghcLoc

  [ build -/- "*/ghcide",
    build -/- "*/ghc.path"
    ]
    &%> \[out, ghcpath] -> do
      let [b, ver, _] = splitDirectories out
      liftIO $ createDirectoryIfMissing True $ dropFileName out
      commitid <- readFile' $ b </> ver </> "commitid"
      cmd_ $ "git worktree add bench-temp " ++ commitid
      flip actionFinally (cmd_ (s "git worktree remove bench-temp --force")) $ do
        ghcLoc <- findGhc "bench-temp" buildSystem
        cmd_ [Cwd "bench-temp"] $ buildGhcide buildSystem (".." </> takeDirectory out)
        writeFile' ghcpath ghcLoc

  build -/- "*/*/results.csv" %> \out -> do
      experiments <- readExperiments

      let allResultFiles = [takeDirectory out </> escaped (escapeExperiment e) <.> "csv" | e <- experiments]
      allResults <- traverse readFileLines allResultFiles

      let header = head $ head allResults
          results = map tail allResults
      writeFileChanged out $ unlines $ header : concat results

  ghcideBenchResource <- newResource "ghcide-bench" 1

  priority 0 $
    [ build -/- "*/*/*.csv",
      build -/- "*/*/*.benchmark-gcStats",
      build -/- "*/*/*.log"
    ]
      &%> \[outcsv, _outGc, outLog] -> do
        let [_, exampleName, ver, exp] = splitDirectories outcsv
        example <- fromMaybe (error $ "Unknown example " <> exampleName) <$> getExample exampleName
        samples <- readSamples
        liftIO $ createDirectoryIfMissing True $ dropFileName outcsv
        let ghcide = build </> ver </> "ghcide"
            ghcpath = build </> ver </> "ghc.path"
        need [ghcide, ghcpath]
        ghcPath <- readFile' ghcpath
        withResource ghcideBenchResource 1 $ do
          command_
              [ EchoStdout False,
                FileStdout outLog,
                RemEnv "NIX_GHC_LIBDIR",
                RemEnv "GHC_PACKAGE_PATH",
                AddPath [takeDirectory ghcPath, "."] []
              ]
              ghcideBenchPath $
              [ "--timeout=3000",
                "-v",
                "--samples=" <> show samples,
                "--csv=" <> outcsv,
                "--ghcide-options= +RTS -I0.5 -RTS",
                "--ghcide=" <> ghcide,
                "--select",
                unescaped (unescapeExperiment (Escaped $ dropExtension exp))
              ] ++
              exampleToOptions example ++
              [ "--stack" | Stack == buildSystem]
          cmd_ Shell $ "mv *.benchmark-gcStats " <> dropFileName outcsv

  build -/- "results.csv" %> \out -> do
    examples <- map getExampleName <$> readExamples
    let allResultFiles = [build </> e </> "results.csv" | e <- examples]

    allResults <- traverse readFileLines allResultFiles

    let header = head $ head allResults
        results = map tail allResults
        header' = "example, " <> header
        results' = zipWith (\e -> map (\l -> e <> ", " <> l)) examples results

    writeFileChanged out $ unlines $ header' : concat results'

  build -/- "*/results.csv" %> \out -> do
    versions <- map (T.unpack . humanName) <$> readVersions
    let example = takeFileName $ takeDirectory out
        allResultFiles =
          [build </> example </> v </> "results.csv" | v <- versions]

    allResults <- traverse readFileLines allResultFiles

    let header = head $ head allResults
        results = map tail allResults
        header' = "version, " <> header
        results' = zipWith (\v -> map (\l -> v <> ", " <> l)) versions results

    writeFileChanged out $ unlines $ header' : interleave results'

  priority 2 $
    build -/- "*/*/*.diff.svg" %> \out -> do
      let [b, example, ver, exp_] = splitDirectories out
          exp = Escaped $ dropExtension $ dropExtension exp_
      prev <- getParent $ T.pack ver

      runLog <- loadRunLog b example exp ver
      runLogPrev <- loadRunLog b example exp $ T.unpack prev

      let diagram = Diagram Live [runLog, runLogPrev] title
          title = show (unescapeExperiment exp) <> " - live bytes over time compared"
      plotDiagram True diagram out

  priority 1 $
    build -/- "*/*/*.svg" %> \out -> do
      let [b, example, ver, exp] = splitDirectories out
      runLog <- loadRunLog b example (Escaped $ dropExtension exp) ver
      let diagram = Diagram Live [runLog] title
          title = ver <> " live bytes over time"
      plotDiagram True diagram out

  build -/- "*/*.svg" %> \out -> do
    let exp = Escaped $ dropExtension $ takeFileName out
        example = takeFileName $ takeDirectory out
    versions <- readVersions

    runLogs <- forM (filter include versions) $ \v -> do
      loadRunLog build example exp $ T.unpack $ humanName v

    let diagram = Diagram Live runLogs title
        title = show (unescapeExperiment exp) <> " - live bytes over time"
    plotDiagram False diagram out

--------------------------------------------------------------------------------

buildGhcide :: BuildSystem -> String -> String
buildGhcide Cabal out = unwords
    ["cabal install"
    ,"exe:ghcide"
    ,"--installdir=" ++ out
    ,"--install-method=copy"
    ,"--overwrite-policy=always"
    ,"--ghc-options -rtsopts"
    ]
buildGhcide Stack out =
    "stack --local-bin-path=" <> out
        <> " build ghcide:ghcide --copy-bins --ghc-options -rtsopts"


findGhc :: FilePath -> BuildSystem -> Action FilePath
findGhc _cwd Cabal =
    liftIO $ fromMaybe (error "ghc is not in the PATH") <$> findExecutable "ghc"
findGhc cwd Stack = do
    Stdout ghcLoc <- cmd [Cwd cwd] (s "stack exec which ghc")
    return ghcLoc

--------------------------------------------------------------------------------

data Config = Config
  { experiments :: [Unescaped String],
    examples :: [Example],
    samples :: Natural,
    versions :: [GitCommit],
    -- | Path to the ghcide-bench binary for the experiments
    ghcideBench :: FilePath,
    -- | Output folder ('foo' works, 'foo/bar' does not)
    outputFolder :: String,
    buildTool :: BuildSystem
  }
  deriving (Generic, Show)
  deriving anyclass (FromJSON)

data GitCommit = GitCommit
  { -- | A git hash, tag or branch name (e.g. v0.1.0)
    gitName :: Text,
    -- | A human understandable name (e.g. fix-collisions-leak)
    name :: Maybe Text,
    -- | The human understandable name of the parent, if specified explicitly
    parent :: Maybe Text,
    -- | Whether to include this version in the top chart
    include :: Bool
  }
  deriving (Binary, Eq, Hashable, Generic, NFData, Show)

instance FromJSON GitCommit where
  parseJSON (String s) = pure $ GitCommit s Nothing Nothing True
  parseJSON (Object (toList -> [(name, String gitName)])) =
    pure $ GitCommit gitName (Just name) Nothing True
  parseJSON (Object (toList -> [(name, Object props)])) =
    GitCommit
      <$> props .:? "git"  .!= name
      <*> pure (Just name)
      <*> props .:? "parent"
      <*> props .:? "include" .!= True
  parseJSON _ = empty

instance ToJSON GitCommit where
  toJSON GitCommit {..} =
    case name of
      Nothing -> String gitName
      Just n -> Object $ fromList [(n, String gitName)]

humanName :: GitCommit -> Text
humanName GitCommit {..} = fromMaybe gitName name

findPrev :: Text -> [GitCommit] -> Text
findPrev name (x : y : xx)
  | humanName y == name = humanName x
  | otherwise = findPrev name (y : xx)
findPrev name _ = name

data BuildSystem = Cabal | Stack
  deriving (Eq, Read, Show)

instance FromJSON BuildSystem where
    parseJSON x = fromString . map toLower <$> parseJSON x
      where
        fromString "stack" = Stack
        fromString "cabal" = Cabal
        fromString other = error $ "Unknown build system: " <> other

instance ToJSON BuildSystem where
    toJSON = toJSON . show
----------------------------------------------------------------------------------------------------

-- | A line in the output of -S
data Frame = Frame
  { allocated, copied, live :: !Int,
    user, elapsed, totUser, totElapsed :: !Double,
    generation :: !Int
  }
  deriving (Show)

instance Read Frame where
  readPrec = do
    spaces
    allocated <- readPrec @Int <* spaces
    copied <- readPrec @Int <* spaces
    live <- readPrec @Int <* spaces
    user <- readPrec @Double <* spaces
    elapsed <- readPrec @Double <* spaces
    totUser <- readPrec @Double <* spaces
    totElapsed <- readPrec @Double <* spaces
    _ <- readPrec @Int <* spaces
    _ <- readPrec @Int <* spaces
    "(Gen:  " <- replicateM 7 get
    generation <- readPrec @Int
    ')' <- get
    return Frame {..}
    where
      spaces = readP_to_Prec $ const P.skipSpaces

data TraceMetric = Allocated | Copied | Live | User | Elapsed
  deriving (Generic, Enum, Bounded, Read)

instance Show TraceMetric where
  show Allocated = "Allocated bytes"
  show Copied = "Copied bytes"
  show Live = "Live bytes"
  show User = "User time"
  show Elapsed = "Elapsed time"

frameMetric :: TraceMetric -> Frame -> Double
frameMetric Allocated = fromIntegral . allocated
frameMetric Copied = fromIntegral . copied
frameMetric Live = fromIntegral . live
frameMetric Elapsed = elapsed
frameMetric User = user

data Diagram = Diagram
  { traceMetric :: TraceMetric,
    runLogs :: [RunLog],
    title :: String
  }
  deriving (Generic)

-- | A file path containing the output of -S for a given run
data RunLog = RunLog
  { runVersion :: !String,
    _runExample :: !String,
    _runExperiment :: !String,
    runFrames :: ![Frame],
    runSuccess :: !Bool
  }

loadRunLog :: HasCallStack => FilePath -> String -> Escaped FilePath -> FilePath -> Action RunLog
loadRunLog buildF example exp ver = do
  let log_fp = buildF </> example </> ver </> escaped exp <.> "benchmark-gcStats"
      csv_fp = replaceExtension log_fp "csv"
  log <- readFileLines log_fp
  csv <- readFileLines csv_fp
  let frames =
        [ f
          | l <- log,
            Just f <- [readMaybe l],
            -- filter out gen 0 events as there are too many
            generation f == 1
        ]
      success = case map (T.split (== ',') . T.pack) csv of
          [_header, _name:s:_] | Just s <- readMaybe (T.unpack s) -> s
          _ -> error $ "Cannot parse: " <> csv_fp
  return $ RunLog ver example (dropExtension $ escaped exp) frames success

plotDiagram :: Bool -> Diagram -> FilePath -> Action ()
plotDiagram includeFailed t@Diagram {traceMetric, runLogs} out = do
  let extract = frameMetric traceMetric
  liftIO $ E.toFile E.def out $ do
    E.layout_title .= title t
    E.setColors myColors
    forM_ runLogs $ \rl ->
      when (includeFailed || runSuccess rl) $ E.plot $ do
        lplot <- E.line
            (runVersion rl ++ if runSuccess rl then "" else " (FAILED)")
            [ [ (totElapsed f, extract f)
                | f <- runFrames rl
                ]
            ]
        return (lplot E.& E.plot_lines_style . E.line_width E.*~ 2)

s :: String -> String
s = id

(-/-) :: FilePattern -> FilePattern -> FilePattern
a -/- b = a <> "/" <> b

newtype Escaped a = Escaped {escaped :: a}

newtype Unescaped a = Unescaped {unescaped :: a}
  deriving newtype (Show, FromJSON, ToJSON, Eq, NFData, Binary, Hashable)

escapeExperiment :: Unescaped String -> Escaped String
escapeExperiment = Escaped . map f . unescaped
  where
    f ' ' = '_'
    f other = other

unescapeExperiment :: Escaped String -> Unescaped String
unescapeExperiment = Unescaped . map f . escaped
  where
    f '_' = ' '
    f other = other

interleave :: [[a]] -> [a]
interleave = concat . transpose

myColors :: [E.AlphaColour Double]
myColors = map E.opaque
  [ E.blue
  , E.green
  , E.red
  , E.orange
  , E.yellow
  , E.violet
  , E.black
  , E.gold
  , E.brown
  , E.hotpink
  , E.aliceblue
  , E.aqua
  , E.beige
  , E.bisque
  , E.blueviolet
  , E.burlywood
  , E.cadetblue
  , E.chartreuse
  , E.coral
  , E.crimson
  , E.darkblue
  , E.darkgray
  , E.darkgreen
  , E.darkkhaki
  , E.darkmagenta
  , E.deeppink
  , E.dodgerblue
  , E.firebrick
  , E.forestgreen
  , E.fuchsia
  , E.greenyellow
  , E.lightsalmon
  , E.seagreen
  , E.olive
  , E.sandybrown
  , E.sienna
  , E.peru
  ]
