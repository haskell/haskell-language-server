{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE ImplicitParams #-}

{- An automated benchmark built around the simple experiment described in:

  > https://neilmitchell.blogspot.com/2020/05/fixing-space-leaks-in-ghcide.html

  As an example project, it unpacks Cabal-3.2.0.0 in the local filesystem and
  loads the module 'Distribution.Simple'. The rationale for this choice is:

    - It's convenient to download with `cabal unpack Cabal-3.2.0.0`
    - It has very few dependencies, and all are already needed to build ghcide
    - Distribution.Simple has 235 transitive module dependencies, so non trivial

  The experiments are sequences of lsp commands scripted using lsp-test.
  A more refined approach would be to record and replay real IDE interactions,
  once the replay functionality is available in lsp-test.
  A more declarative approach would be to reuse ide-debug-driver:

  > https://github.com/digital-asset/daml/blob/master/compiler/damlc/ide-debug-driver/README.md

  The result of an experiment is a total duration in seconds after a preset
  number of iterations. There is ample room for improvement:
     - Statistical analysis to detect outliers and auto infer the number of iterations needed
     - GC stats analysis (currently -S is printed as part of the experiment)
     - Analyisis of performance over the commit history of the project

  How to run:
     1. `cabal bench`
     2. `cabal exec cabal run ghcide-bench -- -- ghcide-bench-options`

  Note that the package database influences the response times of certain actions,
  e.g. code actions, and therefore the two methods above do not necessarily
  produce the same results.

 -}

import Control.Applicative.Combinators (skipManyTill)
import Control.Concurrent
import Control.Exception.Safe
import Control.Monad.Extra
import Control.Monad.IO.Class
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
import Data.Char (isDigit)

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

main :: IO ()
main = do
  config <- execParser $ info (configP <**> helper) fullDesc
  let ?config = config

  output "starting test"

  cleanUp <- setup

  runBenchmarks
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
      bench "code actions after edit" 10 $ \doc -> do
        changeDoc doc [breakingEdit]
        void (skipManyTill anyMessage message :: Session WorkDoneProgressEndNotification)
        not . null <$> getCodeActions doc (Range identifierP identifierP)
    ]
    `finally` cleanUp

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
    cradle :: !Cradle,
    rtsOptions :: ![String],
    matches :: ![String],
    repetitions :: Maybe Natural,
    ghcide :: FilePath,
    timeoutLsp :: Int,
    examplePackageUsed :: (String, Version, String)
  }
  deriving (Eq, Show)

quiet, verbose :: Config -> Bool
verbose = (== All) . verbosity
quiet   = (== Quiet) . verbosity

data Cradle = Cabal | Stack
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
    <*> flag Cabal Stack (long "stack" <> help "Use a stack cradle")
    <*> many (strOption (long "rts" <> help "additional RTS options for ghcide"))
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
  HasConfig =>
  String ->
  Natural ->
  (TextDocumentIdentifier -> Session p) ->
  (p -> Experiment) ->
  Bench
benchWithSetup name defSamples benchSetup experiment = Bench {..}
  where
    enabled = True
    samples = fromMaybe defSamples (repetitions ?config)

bench :: HasConfig => String -> Natural -> Experiment -> Bench
bench name defSamples userExperiment =
  benchWithSetup name defSamples (const $ pure ()) experiment
  where
    experiment () = userExperiment

runBenchmarks :: HasConfig => [Bench] -> IO ()
runBenchmarks (filter select -> benchmarks) = do
  results <- forM benchmarks $ \b -> (b,) <$> runBench b

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

runBench :: HasConfig => Bench -> IO BenchRun
runBench Bench {..} = handleAny (\e -> print e >> return badRun)
  $ runSessionWithConfig conf cmd lspTestCaps dir
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

    maxResidency <- liftIO $ parseMaxResidency <$> readFile gcStats

    return BenchRun {..}
  where
    gcStats = escapeSpaces (name <> ".benchmark-gcStats")
    cmd =
      unwords $
        [ ghcide ?config,
          "--lsp",
          "--cwd",
          dir,
          "+RTS",
          "-S" <> gcStats
        ]
          ++ rtsOptions ?config
          ++ [ "-RTS"
             ]
          ++ concat
            [ ["--shake-profiling", path]
              | Just path <- [shakeProfiling ?config]
            ]
    dir = "bench/example/" <> examplePackage
    lspTestCaps =
      fullCaps {_window = Just $ WindowClientCapabilities $ Just True}
    conf =
      defaultConfig
        { logStdErr = verbose ?config,
          logMessages = verbose ?config,
          logColor = False,
          messageTimeout = timeoutLsp ?config
        }

setup :: HasConfig => IO (IO ())
setup = do
  alreadyExists <- doesDirectoryExist examplesPath
  when alreadyExists $ removeDirectoryRecursive examplesPath
  callCommand $ "cabal get -v0 " <> examplePackage <> " -d " <> examplesPath
  writeFile
    (examplesPath </> examplePackage </> "hie.yaml")
    exampleCradle

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

exampleCradle :: HasConfig => String
exampleCradle = case cradle ?config of
  Cabal -> "cradle: {cabal: {component: " <> show examplePackageName <> "}}"
  Stack -> "cradle: {stack: {component: " <> show (examplePackageName <> ":lib") <> "}}"

pad :: Int -> String -> String
pad n [] = replicate n ' '
pad 0 _ = error "pad"
pad n (x:xx) = x : pad (n-1) xx

showMB :: Int -> String
showMB x = show (x `div` 2^(20::Int)) <> "MB"
