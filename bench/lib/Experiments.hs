{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE ImplicitParams #-}

module Experiments
( Bench(..)
, BenchRun(..)
, Config(..)
, Verbosity(..)
, CabalStack(..)
, experiments
, configP
, defConfig
, output
, setup
, runBench
, runBenchmarks
) where
import Control.Applicative.Combinators (skipManyTill)
import Control.Concurrent
import Control.Exception.Safe
import Control.Monad.Extra
import Control.Monad.IO.Class
import Data.Char (isDigit)
import Data.List
import Data.Maybe
import Data.Version
import Language.Haskell.LSP.Test
import Language.Haskell.LSP.Types
import Language.Haskell.LSP.Types.Capabilities
import Numeric.Natural
import Options.Applicative
import System.Directory
import System.FilePath ((</>))
import System.Process
import System.Time.Extra
import Text.ParserCombinators.ReadP (readP_to_S)
import System.Environment.Blank (getEnv)

-- Points to a string in the target file,
-- convenient for hygienic edits
hygienicP :: Position
hygienicP = Position 854 23

hygienicEdit :: TextDocumentContentChangeEvent
hygienicEdit =
    TextDocumentContentChangeEvent
    { _range = Just (Range hygienicP hygienicP),
        _rangeLength = Nothing,
        _text = " "
    }

breakingEdit :: TextDocumentContentChangeEvent
breakingEdit =
    TextDocumentContentChangeEvent
    { _range = Just (Range identifierP identifierP),
        _rangeLength = Nothing,
        _text = "a"
    }

-- Points to the middle of an identifier,
-- convenient for requesting goto-def, hover and completions
identifierP :: Position
identifierP = Position 853 12

experiments :: [Bench]
experiments =
    [ ---------------------------------------------------------------------------------------
      bench "hover" 10 $ \doc ->
        isJust <$> getHover doc identifierP,
      ---------------------------------------------------------------------------------------
      bench "edit" 10 $ \doc -> do
        changeDoc doc [hygienicEdit]
        void (skipManyTill anyMessage message :: Session WorkDoneProgressEndNotification)
        return True,
      ---------------------------------------------------------------------------------------
      bench "hover after edit" 10 $ \doc -> do
        changeDoc doc [hygienicEdit]
        isJust <$> getHover doc identifierP,
      ---------------------------------------------------------------------------------------
      bench "getDefinition" 10 $ \doc ->
        not . null <$> getDefinitions doc identifierP,
      ---------------------------------------------------------------------------------------
      bench "documentSymbols" 100 $
        fmap (either (not . null) (not . null)) . getDocumentSymbols,
      ---------------------------------------------------------------------------------------
      bench "documentSymbols after edit" 100 $ \doc -> do
        changeDoc doc [hygienicEdit]
        either (not . null) (not . null) <$> getDocumentSymbols doc,
      ---------------------------------------------------------------------------------------
      bench "completions after edit" 10 $ \doc -> do
        changeDoc doc [hygienicEdit]
        not . null <$> getCompletions doc identifierP,
      ---------------------------------------------------------------------------------------
      benchWithSetup
        "code actions"
        10
        ( \doc -> do
            changeDoc doc [breakingEdit]
            void (skipManyTill anyMessage message :: Session WorkDoneProgressEndNotification)
            return identifierP
        )
        ( \p doc -> do
            not . null <$> getCodeActions doc (Range p p)
        ),
      ---------------------------------------------------------------------------------------
      benchWithSetup
        "code actions after edit"
        10
        ( \doc -> do
            changeDoc doc [breakingEdit]
            return identifierP
        )
        ( \p doc -> do
            changeDoc doc [hygienicEdit]
            whileM (null <$> waitForDiagnostics)
            not . null <$> getCodeActions doc (Range p p)
        )
    ]

---------------------------------------------------------------------------------------------

examplePackageName :: HasConfig => String
examplePackageName = name
  where
      (name, _, _) = examplePackageUsed ?config

examplePackage :: HasConfig => String
examplePackage = name <> "-" <> showVersion version
  where
      (name, version, _) = examplePackageUsed ?config

exampleModulePath :: HasConfig => FilePath
exampleModulePath = path
  where
      (_,_, path) = examplePackageUsed ?config

examplesPath :: FilePath
examplesPath = "bench/example"

data Verbosity = Quiet | Normal | All
  deriving (Eq, Show)
data Config = Config
  { verbosity :: !Verbosity,
    -- For some reason, the Shake profile files are truncated and won't load
    shakeProfiling :: !(Maybe FilePath),
    outputCSV :: !FilePath,
    buildTool :: !CabalStack,
    ghcideOptions :: ![String],
    matches :: ![String],
    repetitions :: Maybe Natural,
    ghcide :: FilePath,
    timeoutLsp :: Int,
    examplePackageUsed :: (String, Version, String)
  }
  deriving (Eq, Show)

defConfig :: Config
Success defConfig = execParserPure defaultPrefs (info configP fullDesc) []

quiet, verbose :: Config -> Bool
verbose = (== All) . verbosity
quiet   = (== Quiet) . verbosity

data CabalStack = Cabal | Stack
  deriving (Eq, Show)

type HasConfig = (?config :: Config)

configP :: Parser Config
configP =
  Config
    <$> (flag' All (short 'v' <> long "verbose")
         <|> flag' Quiet (short 'q' <> long "quiet")
         <|> pure Normal
        )
    <*> optional (strOption (long "shake-profiling" <> metavar "PATH"))
    <*> strOption (long "csv" <> metavar "PATH" <> value "results.csv" <> showDefault)
    <*> flag Cabal Stack (long "stack" <> help "Use stack (by default cabal is used)")
    <*> many (strOption (long "ghcide-options" <> help "additional options for ghcide"))
    <*> many (strOption (short 's' <> long "select" <> help "select which benchmarks to run"))
    <*> optional (option auto (long "samples" <> metavar "NAT" <> help "override sampling count"))
    <*> strOption (long "ghcide" <> metavar "PATH" <> help "path to ghcide" <> value "ghcide")
    <*> option auto (long "timeout" <> value 60 <> help "timeout for waiting for a ghcide response")
    <*> ( (,,) <$> strOption (long "example-package-name" <> value "Cabal")
               <*> option versionP (long "example-package-version" <> value (makeVersion [3,2,0,0]))
               <*> strOption (long "example-package-module" <> metavar "PATH" <> value "Distribution/Simple.hs"))

versionP :: ReadM Version
versionP = maybeReader $ extract . readP_to_S parseVersion
  where
      extract parses = listToMaybe [ res | (res,"") <- parses]

output :: (MonadIO m, HasConfig) => String -> m ()
output = if quiet?config then (\_ -> pure ()) else liftIO . putStrLn

---------------------------------------------------------------------------------------

type Experiment = TextDocumentIdentifier -> Session Bool

data Bench = forall setup.
  Bench
  { name :: !String,
    enabled :: !Bool,
    samples :: !Natural,
    benchSetup :: TextDocumentIdentifier -> Session setup,
    experiment :: setup -> Experiment
  }

select :: HasConfig => Bench -> Bool
select Bench {name, enabled} =
  enabled && (null mm || name `elem` mm)
  where
    mm = matches ?config

benchWithSetup ::
  String ->
  Natural ->
  (TextDocumentIdentifier -> Session p) ->
  (p -> Experiment) ->
  Bench
benchWithSetup name samples benchSetup experiment = Bench {..}
  where
    enabled = True

bench :: String -> Natural -> Experiment -> Bench
bench name defSamples userExperiment =
  benchWithSetup name defSamples (const $ pure ()) experiment
  where
    experiment () = userExperiment

runBenchmarks :: HasConfig => [Bench] -> IO ()
runBenchmarks allBenchmarks = do
  let benchmarks = [ b{samples = fromMaybe (samples b) (repetitions ?config) }
                   | b <- allBenchmarks
                   , select b ]
  results <- forM benchmarks $ \b@Bench{name} ->
                let run dir = runSessionWithConfig conf (cmd name dir) lspTestCaps dir
                in (b,) <$> runBench run b

  -- output raw data as CSV
  let headers = ["name", "success", "samples", "startup", "setup", "experiment", "maxResidency"]
      rows =
        [ [ name,
            show success,
            show samples,
            show startup,
            show runSetup',
            show runExperiment,
            showMB maxResidency
          ]
          | (Bench {name, samples}, BenchRun {..}) <- results,
            let runSetup' = if runSetup < 0.01 then 0 else runSetup
        ]
      csv = unlines $ map (intercalate ", ") (headers : rows)
  writeFile (outputCSV ?config) csv

  -- print a nice table
  let pads = map (maximum . map length) (transpose (headers : rowsHuman))
      paddedHeaders = zipWith pad pads headers
      outputRow = putStrLn . intercalate " | "
      rowsHuman =
        [ [ name,
            show success,
            show samples,
            showDuration startup,
            showDuration runSetup',
            showDuration runExperiment,
            showMB maxResidency
          ]
          | (Bench {name, samples}, BenchRun {..}) <- results,
            let runSetup' = if runSetup < 0.01 then 0 else runSetup
        ]
  outputRow paddedHeaders
  outputRow $ (map . map) (const '-') paddedHeaders
  forM_ rowsHuman $ \row -> outputRow $ zipWith pad pads row
  where
    gcStats name = escapeSpaces (name <> ".benchmark-gcStats")
    cmd name dir =
      unwords $
        [ ghcide ?config,
          "--lsp",
          "--cwd",
          dir,
          "+RTS",
          "-S" <> gcStats name,
          "-RTS"
        ]
          ++ ghcideOptions ?config
          ++ concat
            [ ["--shake-profiling", path]
              | Just path <- [shakeProfiling ?config]
            ]
    lspTestCaps =
      fullCaps {_window = Just $ WindowClientCapabilities $ Just True}
    conf =
      defaultConfig
        { logStdErr = verbose ?config,
          logMessages = verbose ?config,
          logColor = False,
          messageTimeout = timeoutLsp ?config
        }

data BenchRun = BenchRun
  { startup :: !Seconds,
    runSetup :: !Seconds,
    runExperiment :: !Seconds,
    success :: !Bool,
    maxResidency :: !Int
  }

badRun :: BenchRun
badRun = BenchRun 0 0 0 False 0

waitForProgressDone :: Session ()
waitForProgressDone =
      void(skipManyTill anyMessage message :: Session WorkDoneProgressEndNotification)

runBench :: (?config::Config) => (String -> Session BenchRun -> IO BenchRun) -> Bench -> IO BenchRun
runBench runSess Bench {..} = handleAny (\e -> print e >> return badRun)
  $ runSess dir
  $ do
    doc <- openDoc exampleModulePath "haskell"
    (startup, _) <- duration $ do
      waitForProgressDone
      -- wait again, as the progress is restarted once while loading the cradle
      -- make an edit, to ensure this doesn't block
      changeDoc doc [hygienicEdit]
      waitForProgressDone


    liftIO $ output $ "Running " <> name <> " benchmark"
    (runSetup, userState) <- duration $ benchSetup doc
    let loop 0 = return True
        loop n = do
          (t, res) <- duration $ experiment userState doc
          if not res
            then return False
            else do
              output (showDuration t)
              loop (n -1)

    (runExperiment, success) <- duration $ loop samples

    -- sleep to give ghcide a chance to GC
    liftIO $ threadDelay 1100000

    maxResidency <- liftIO $
        ifM (doesFileExist gcStats)
            (parseMaxResidency <$> readFile gcStats)
            (pure 0)

    return BenchRun {..}
  where
    dir = "bench/example/" <> examplePackage
    gcStats = escapeSpaces (name <> ".benchmark-gcStats")

setup :: HasConfig => IO (IO ())
setup = do
  alreadyExists <- doesDirectoryExist examplesPath
  when alreadyExists $ removeDirectoryRecursive examplesPath
  let path = examplesPath </> examplePackage
  case buildTool ?config of
      Cabal -> do
        callCommand $ "cabal get -v0 " <> examplePackage <> " -d " <> examplesPath
        writeFile
            (path </> "hie.yaml")
            ("cradle: {cabal: {component: " <> show examplePackageName <> "}}")
        -- Need this in case there is a parent cabal.project somewhere
        writeFile
            (path </> "cabal.project")
            "packages: ."
        writeFile
            (path </> "cabal.project.local")
            ""
      Stack -> do
        callCommand $ "stack --silent unpack " <> examplePackage <> " --to " <> examplesPath
        -- Generate the stack descriptor to match the one used to build ghcide
        stack_yaml <- fromMaybe "stack.yaml" <$> getEnv "STACK_YAML"
        stack_yaml_lines <- lines <$> readFile stack_yaml
        writeFile (path </> stack_yaml)
                  (unlines $
                   "packages: [.]" :
                    [ l
                    | l <- stack_yaml_lines
                    , any (`isPrefixOf` l)
                        ["resolver"
                        ,"allow-newer"
                        ,"compiler"]
                    ]
                  )

        writeFile
            (path </> "hie.yaml")
            ("cradle: {stack: {component: " <> show (examplePackageName <> ":lib") <> "}}")

  whenJust (shakeProfiling ?config) $ createDirectoryIfMissing True

  return $ removeDirectoryRecursive examplesPath

--------------------------------------------------------------------------------------------

-- Parse the max residency in RTS -s output
parseMaxResidency :: String -> Int
parseMaxResidency input =
  case find ("maximum residency" `isInfixOf`) (reverse $ lines input) of
    Just l -> read $ filter isDigit $ head (words l)
    Nothing -> -1


escapeSpaces :: String -> String
escapeSpaces = map f
  where
    f ' ' = '_'
    f x = x

pad :: Int -> String -> String
pad n [] = replicate n ' '
pad 0 _ = error "pad"
pad n (x:xx) = x : pad (n-1) xx

showMB :: Int -> String
showMB x = show (x `div` 2^(20::Int)) <> "MB"
