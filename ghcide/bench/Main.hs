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
     2. `cabal exec <absolute-path-to-ghcide-bench> -- ghcide-bench-options`

  Note that the package database influences the response times of certain actions,
  e.g. code actions, and therefore the two methods above do not necessarily
  produce the same results.

 -}

import Control.Applicative.Combinators
import Control.Concurrent
import Control.Exception.Safe
import Control.Monad.Extra
import Control.Monad.IO.Class
import Data.Aeson
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

-- Points to a string in the target file,
-- convenient for hygienic edits
hygienicP :: Position
hygienicP = Position 854 23

-- Points to the middle of an identifier,
-- convenient for requesting goto-def, hover and completions
identifierP :: Position
identifierP = Position 853 12

main :: IO ()
main = do
  config <- execParser $ info configP fullDesc
  let ?config = config

  output "starting test"

  cleanUp <- setup

  runBenchmarks
    [ ---------------------------------------------------------------------------------------
      bench "hover" 10 $ \doc ->
        isJust <$> getHover doc identifierP,
      ---------------------------------------------------------------------------------------
      bench "getDefinition" 10 $ \doc ->
        not . null <$> getDefinitions doc identifierP,
      ---------------------------------------------------------------------------------------
      bench "documentSymbols" 100 $
        fmap (either (not . null) (not . null)) . getDocumentSymbols,
      ---------------------------------------------------------------------------------------
      bench "documentSymbols after edit" 100 $ \doc -> do
        let change =
              TextDocumentContentChangeEvent
                { _range = Just (Range hygienicP hygienicP),
                  _rangeLength = Nothing,
                  _text = " "
                }
        changeDoc doc [change]
        either (not . null) (not . null) <$> getDocumentSymbols doc,
      ---------------------------------------------------------------------------------------
      bench "completions after edit" 10 $ \doc -> do
        let change =
              TextDocumentContentChangeEvent
                { _range = Just (Range hygienicP hygienicP),
                  _rangeLength = Nothing,
                  _text = " "
                }
        changeDoc doc [change]
        not . null <$> getCompletions doc identifierP,
      ---------------------------------------------------------------------------------------
      benchWithSetup
        "code actions"
        10
        ( \doc -> do
            let p = identifierP
            let change =
                  TextDocumentContentChangeEvent
                    { _range = Just (Range p p),
                      _rangeLength = Nothing,
                      _text = "a"
                    }
            changeDoc doc [change]
            void (skipManyTill anyMessage message :: Session WorkDoneProgressEndNotification)
            return p
        )
        ( \p doc -> do
            not . null <$> getCodeActions doc (Range p p)
        ),
      ---------------------------------------------------------------------------------------
      bench "code actions after edit" 10 $ \doc -> do
        let p = identifierP
        let change =
              TextDocumentContentChangeEvent
                { _range = Just (Range p p),
                  _rangeLength = Nothing,
                  _text = "a"
                }
        changeDoc doc [change]
        void (skipManyTill anyMessage message :: Session WorkDoneProgressEndNotification)
        not . null <$> getCodeActions doc (Range p p)
    ]
    `finally` cleanUp

---------------------------------------------------------------------------------------------

examplePackageName :: String
examplePackageName = "Cabal"

examplePackageVersion :: Version
examplePackageVersion = makeVersion [3, 2, 0, 0]

examplePackage :: String
examplePackage = examplePackageName <> "-" <> showVersion examplePackageVersion

exampleModulePath :: FilePath
exampleModulePath = "Distribution" </> "Simple.hs"

examplesPath :: FilePath
examplesPath = "bench/example"

data Config = Config
  { verbose :: !Bool,
    -- For some reason, the Shake profile files are truncated and won't load
    shakeProfiling :: !(Maybe FilePath),
    outputCSV :: !Bool
  }

type HasConfig = (?config :: Config)

configP :: Parser Config
configP = Config
    <$> (not <$> switch (long "quiet"))
    <*> optional (strOption (long "shake-profiling" <> metavar "PATH"))
    <*> switch (long "csv")

output :: (MonadIO m, HasConfig) => String -> m ()
output = if verbose ?config then liftIO . putStrLn else (\_ -> pure ())

---------------------------------------------------------------------------------------

type Experiment = TextDocumentIdentifier -> Session Bool

data Bench = forall setup.
  Bench
  { name :: !String,
    samples :: !Natural,
    benchSetup :: TextDocumentIdentifier -> Session setup,
    experiment :: setup -> Experiment
  }

bench :: String -> Natural -> Experiment -> Bench
bench name samples userExperiment = Bench {..}
  where
    experiment () = userExperiment
    benchSetup _ = return ()

benchWithSetup ::
  String ->
  Natural ->
  (TextDocumentIdentifier -> Session p) ->
  (p -> Experiment) ->
  Bench
benchWithSetup = Bench

runBenchmarks :: HasConfig => [Bench] -> IO ()
runBenchmarks benchmarks = do
  results <- forM benchmarks $ \b -> (b,) <$> runBench b

  forM_ results $ \(Bench {name, samples}, duration) ->
    output $
      "TOTAL "
        <> name
        <> " = "
        <> showDuration duration
        <> " ("
        <> show samples
        <> " repetitions)"

  when (outputCSV ?config) $ do
    putStrLn $ intercalate ", " $ map name benchmarks
    putStrLn $ intercalate ", " $ map (showDuration . snd) results

runBench :: HasConfig => Bench -> IO Seconds
runBench Bench {..} = handleAny (\e -> print e >> return (-1))
  $ runSessionWithConfig conf cmd lspTestCaps dir
  $ do
    doc <- openDoc exampleModulePath "haskell"
    void (skipManyTill anyMessage message :: Session WorkDoneProgressEndNotification)

    liftIO $ output $ "Running " <> name <> " benchmark"
    userState <- benchSetup doc
    let loop 0 = return True
        loop n = do
          (t, res) <- duration $ experiment userState doc
          if not res
            then return False
            else do
              output (showDuration t)
              loop (n -1)

    (t, res) <- duration $ loop samples

    exitServer
    -- sleeep to give ghcide a chance to print the RTS stats
    liftIO $ threadDelay 50000

    return $ if res then t else -1
  where
    cmd =
      unwords $
        [ "ghcide",
          "--lsp",
          "--cwd",
          dir,
          "+RTS",
          "-S",
          "-RTS"
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
          logMessages = False,
          logColor = False
        }

setup :: HasConfig => IO (IO ())
setup = do
  alreadyExists <- doesDirectoryExist examplesPath
  when alreadyExists $ removeDirectoryRecursive examplesPath
  callCommand $ "cabal get -v0 " <> examplePackage <> " -d " <> examplesPath
  writeFile
    (examplesPath </> examplePackage </> "hie.yaml")
    ("cradle: {cabal: {component: " <> show examplePackageName <> "}}")

  whenJust (shakeProfiling ?config) $ createDirectoryIfMissing True

  return $ removeDirectoryRecursive examplesPath

-- | Asks the server to shutdown and exit politely
exitServer :: Session ()
exitServer = request_ Shutdown (Nothing :: Maybe Value) >> sendNotification Exit ExitParams

--------------------------------------------------------------------------------------------
