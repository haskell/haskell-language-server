{-# LANGUAGE DataKinds          #-}
{-# LANGUAGE GADTs              #-}
{-# LANGUAGE ImplicitParams     #-}
{-# LANGUAGE ImpredicativeTypes #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# OPTIONS_GHC -Wno-deprecations -Wno-unticked-promoted-constructors #-}

module Experiments
( Bench(..)
, BenchRun(..)
, Config(..)
, Verbosity(..)
, CabalStack(..)
, SetupResult(..)
, Example(..)
, experiments
, configP
, defConfig
, output
, setup
, runBench
, exampleToOptions
) where
import           Control.Applicative.Combinators    (skipManyTill)
import           Control.Concurrent.Async           (withAsync)
import           Control.Exception.Safe             (IOException, handleAny,
                                                     try)
import           Control.Lens                       (_Just, (&), (.~), (^.),
                                                     (^?))
import           Control.Lens.Extras                (is)
import           Control.Monad.Extra                (allM, forM, forM_, forever,
                                                     unless, void, when,
                                                     whenJust, (&&^))
import           Control.Monad.IO.Class
import           Data.Aeson                         (Value (Null),
                                                     eitherDecodeStrict',
                                                     toJSON)
import qualified Data.Aeson                         as A
import qualified Data.ByteString                    as BS
import qualified Data.ByteString.Lazy               as BSL
import           Data.Either                        (fromRight)
import           Data.List
import           Data.Maybe
import           Data.Proxy
import           Data.Text                          (Text)
import qualified Data.Text                          as T
import           Data.Version
import           Development.IDE.Plugin.Test
import           Development.IDE.Test.Diagnostic
import           Development.Shake                  (CmdOption (Cwd), cmd_)
import           Experiments.Types
import           Language.LSP.Protocol.Capabilities
import qualified Language.LSP.Protocol.Lens         as L
import           Language.LSP.Protocol.Message
import           Language.LSP.Protocol.Types        hiding (Null,
                                                     SemanticTokenAbsolute (..))
import qualified Language.LSP.Protocol.Types        as LSP
import           Language.LSP.Test
import           Numeric.Natural
import           Options.Applicative
import           System.Directory
import           System.Environment.Blank           (getEnv)
import           System.FilePath                    ((<.>), (</>))
import           System.IO
import           System.Process
import           System.Time.Extra
import           Text.ParserCombinators.ReadP       (readP_to_S)
import           Text.Printf

charEdit :: Position -> TextDocumentContentChangeEvent
charEdit p =
    TextDocumentContentChangeEvent $ InL TextDocumentContentChangePartial
        { _range = Range p p
        , _rangeLength = Nothing
        , _text = "a"
        }

headerEdit :: TextDocumentContentChangeEvent
headerEdit =
    TextDocumentContentChangeEvent $ InL TextDocumentContentChangePartial
        { _range = Range (Position 0 0) (Position 0 0)
        , _rangeLength = Nothing
        , _text = "-- header comment \n"
        }

data DocumentPositions = DocumentPositions {
    -- | A position that can be used to generate non null goto-def and completion responses
    identifierP    :: Maybe Position,
    -- | A position that can be modified without generating a new diagnostic
    stringLiteralP :: !Position,
    -- | The document containing the above positions
    doc            :: !TextDocumentIdentifier
}

allWithIdentifierPos :: MonadFail m => (DocumentPositions -> m Bool) -> [DocumentPositions] -> m Bool
allWithIdentifierPos f docs = case applicableDocs of
    -- fail if there are no documents to benchmark
    []    -> fail "None of the example modules have identifier positions"
    docs' -> allM f docs'
  where
    applicableDocs = filter (isJust . identifierP) docs

experiments :: HasConfig => [Bench]
experiments =
    [
      bench "semanticTokens" $ \docs -> do
        liftIO $ putStrLn "Starting semanticTokens"
        r <- forM docs $ \DocumentPositions{..} -> do
            changeDoc doc [charEdit stringLiteralP]
            waitForProgressStart
            waitForProgressDone
            tks <- getSemanticTokens doc
            case tks ^? LSP._L of
                Just _  -> return True
                Nothing -> return False
        return $ and r,
      ---------------------------------------------------------------------------------------
      bench "hover" $ allWithIdentifierPos $ \DocumentPositions{..} ->
        isJust <$> getHover doc (fromJust identifierP),
      ---------------------------------------------------------------------------------------
      bench "hover after edit" $ \docs -> do
        forM_ docs $ \DocumentPositions{..} ->
          changeDoc doc [charEdit stringLiteralP]
        flip allWithIdentifierPos docs $ \DocumentPositions{..} ->
          isJust <$> getHover doc (fromJust identifierP),
      ---------------------------------------------------------------------------------------
      bench
        "hover after cradle edit"
        (\docs -> do
            hieYamlUri <- getDocUri "hie.yaml"
            liftIO $ appendFile (fromJust $ uriToFilePath hieYamlUri) "##\n"
            sendNotification SMethod_WorkspaceDidChangeWatchedFiles $ DidChangeWatchedFilesParams
             [ FileEvent hieYamlUri FileChangeType_Changed ]
            flip allWithIdentifierPos docs $ \DocumentPositions{..} -> isJust <$> getHover doc (fromJust identifierP)
        ),
      ---------------------------------------------------------------------------------------
      bench "edit" $ \docs -> do
        forM_ docs $ \DocumentPositions{..} -> do
          changeDoc doc [charEdit stringLiteralP]
          -- wait for a fresh build start
          waitForProgressStart
        -- wait for the build to be finished
        output "edit: waitForProgressDone"
        waitForProgressDone
        return True,
      ---------------------------------------------------------------------------------------
      bench "edit-header" $ \docs -> do
        forM_ docs $ \DocumentPositions{..} -> do
          changeDoc doc [headerEdit]
          -- wait for a fresh build start
          waitForProgressStart
        -- wait for the build to be finished
        output "edit: waitForProgressDone"
        waitForProgressDone
        return True,
      ---------------------------------------------------------------------------------------
      bench "getDefinition" $ allWithIdentifierPos $ \DocumentPositions{..} ->
        hasDefinitions <$> getDefinitions doc (fromJust identifierP),
      ---------------------------------------------------------------------------------------
      bench "getDefinition after edit" $ \docs -> do
          forM_ docs $ \DocumentPositions{..} ->
            changeDoc doc [charEdit stringLiteralP]
          flip allWithIdentifierPos docs $ \DocumentPositions{..} ->
            hasDefinitions <$> getDefinitions doc (fromJust identifierP),
      ---------------------------------------------------------------------------------------
      bench "documentSymbols" $ allM $ \DocumentPositions{..} -> do
        fmap (either (not . null) (not . null)) . getDocumentSymbols $ doc,
      ---------------------------------------------------------------------------------------
      bench "documentSymbols after edit" $ \docs -> do
        forM_ docs $ \DocumentPositions{..} ->
          changeDoc doc [charEdit stringLiteralP]
        flip allM docs $ \DocumentPositions{..} ->
          either (not . null) (not . null) <$> getDocumentSymbols doc,
      ---------------------------------------------------------------------------------------
      bench "completions" $ \docs -> do
        flip allWithIdentifierPos docs $ \DocumentPositions{..} ->
          not . null <$> getCompletions doc (fromJust identifierP),
      ---------------------------------------------------------------------------------------
      bench "completions after edit" $ \docs -> do
        forM_ docs $ \DocumentPositions{..} ->
          changeDoc doc [charEdit stringLiteralP]
        flip allWithIdentifierPos docs $ \DocumentPositions{..} ->
          not . null <$> getCompletions doc (fromJust identifierP),
      ---------------------------------------------------------------------------------------
      bench
        "code actions"
        ( \docs -> do
            unless (any (isJust . identifierP) docs) $
                error "None of the example modules is suitable for this experiment"
            not . null . catMaybes <$> forM docs (\DocumentPositions{..} -> do
              forM identifierP $ \p ->
                getCodeActions doc (Range p p))
        ),
      ---------------------------------------------------------------------------------------
      bench
        "code actions after edit"
        ( \docs -> do
            unless (any (isJust . identifierP) docs) $
                error "None of the example modules is suitable for this experiment"
            forM_ docs $ \DocumentPositions{..} -> do
              changeDoc doc [charEdit stringLiteralP]
              waitForProgressStart
            waitForProgressDone
            not . null . catMaybes <$> forM docs (\DocumentPositions{..} -> do
              forM identifierP $ \p ->
                getCodeActions doc (Range p p))
        ),
      ---------------------------------------------------------------------------------------
      bench
        "code actions after cradle edit"
        ( \docs -> do
            hieYamlUri <- getDocUri "hie.yaml"
            liftIO $ appendFile (fromJust $ uriToFilePath hieYamlUri) "##\n"
            sendNotification SMethod_WorkspaceDidChangeWatchedFiles $ DidChangeWatchedFilesParams
             [ FileEvent hieYamlUri FileChangeType_Changed ]
            waitForProgressStart
            waitForProgressStart
            waitForProgressStart -- the Session logic restarts a second time
            waitForProgressDone
            not . all null . catMaybes <$> forM docs (\DocumentPositions{..} -> do
              forM identifierP $ \p ->
                getCodeActions doc (Range p p))
        ),
      ---------------------------------------------------------------------------------------
      bench
        "code lens"
        ( \docs -> not . null <$> forM docs (\DocumentPositions{..} ->
            getCodeLenses doc)
        ),
      ---------------------------------------------------------------------------------------
      bench
        "code lens after edit"
        ( \docs -> do
            forM_ docs $ \DocumentPositions{..} -> do
              changeDoc doc [charEdit stringLiteralP]
              waitForProgressStart
            waitForProgressDone
            not . null <$> forM docs (\DocumentPositions{..} -> do
              getCodeLenses doc)
        ),
      ---------------------------------------------------------------------------------------
      benchWithSetup
        "hole fit suggestions"
        ( mapM_ $ \DocumentPositions{..} -> do
            let edit = TextDocumentContentChangeEvent $ InL TextDocumentContentChangePartial
                  { _range = Range bottom bottom
                  , _rangeLength = Nothing
                  , _text = t
                  }
                bottom = Position maxBound 0
                t = T.unlines
                    [""
                    ,"holef :: [Int] -> [Int]"
                    ,"holef = _"
                    ,""
                    ,"holeg :: [()] -> [()]"
                    ,"holeg = _"
                    ]
            changeDoc doc [edit]
        )
        (\docs -> do
            forM_ docs $ \DocumentPositions{..} ->
              changeDoc doc [charEdit stringLiteralP]
            void waitForDiagnostics
            waitForProgressDone
            flip allM docs $ \DocumentPositions{..} -> do
                bottom <- pred . length . T.lines <$> documentContents doc
                diags <- getCurrentDiagnostics doc
                case requireDiagnostic diags (DiagnosticSeverity_Error, (fromIntegral bottom, 8), "Found hole", Just "GHC-88464", Nothing) of
                    Nothing   -> pure True
                    Just _err -> pure False
        ),
      ---------------------------------------------------------------------------------------
      benchWithSetup
        "eval execute single-line code lens"
        ( mapM_ $ \DocumentPositions{..} -> do
            let edit = TextDocumentContentChangeEvent $ InL TextDocumentContentChangePartial
                  { _range = Range bottom bottom
                  , _rangeLength = Nothing
                  , _text = t
                  }
                bottom = Position maxBound 0
                t = T.unlines
                    [ ""
                    , "-- >>> 1 + 2"
                    ]
            changeDoc doc [edit]
        )
        ( \docs -> do
            not . null <$> forM docs (\DocumentPositions{..} -> do
              lenses <- getCodeLenses doc
              forM_ lenses $ \case
                CodeLens { _command = Just cmd } -> do
                  executeCommand cmd
                  waitForProgressStart
                  waitForProgressDone
                _ -> return ()
              )
        ),
      ---------------------------------------------------------------------------------------
      benchWithSetup
        "eval execute multi-line code lens"
        ( mapM_ $ \DocumentPositions{..} -> do
            let edit = TextDocumentContentChangeEvent $ InL TextDocumentContentChangePartial
                  { _range = Range bottom bottom
                  , _rangeLength = Nothing
                  , _text = t
                  }
                bottom = Position maxBound 0
                t = T.unlines
                    [ ""
                    , "data T = A | B | C | D"
                    , "  deriving (Show, Eq, Ord, Bounded, Enum)"
                    , ""
                    , "{-"
                    , ">>> import Data.List (nub)"
                    , ">>> xs = ([minBound..maxBound] ++ [minBound..maxBound] :: [T])"
                    , ">>> nub xs"
                    , "-}"
                    ]
            changeDoc doc [edit]
        )
        ( \docs -> do
            not . null <$> forM docs (\DocumentPositions{..} -> do
              lenses <- getCodeLenses doc
              forM_ lenses $ \case
                CodeLens { _command = Just cmd } -> do
                  executeCommand cmd
                  waitForProgressStart
                  waitForProgressDone
                _ -> return ()
              )
        )
    ]
    where hasDefinitions (InL (Definition (InL _)))  = True
          hasDefinitions (InL (Definition (InR ls))) = not $ null ls
          hasDefinitions (InR (InL ds))              = not $ null ds
          hasDefinitions (InR (InR LSP.Null))        = False
---------------------------------------------------------------------------------------------

examplesPath :: FilePath
examplesPath = "bench/example"

defConfig :: Config
Success defConfig = execParserPure defaultPrefs (info configP fullDesc) []

quiet, verbose :: Config -> Bool
verbose = (== All) . verbosity
quiet   = (== Quiet) . verbosity

type HasConfig = (?config :: Config)

configP :: Parser Config
configP =
  Config
    <$> (flag' All (short 'v' <> long "verbose")
         <|> flag' Quiet (short 'q' <> long "quiet")
         <|> pure Normal
        )
    <*> optional (strOption (long "shake-profiling" <> metavar "PATH"))
    <*> optional (strOption (long "ot-profiling" <> metavar "DIR" <> help "Enable OpenTelemetry and write eventlog for each benchmark in DIR"))
    <*> strOption (long "csv" <> metavar "PATH" <> value "results.csv" <> showDefault)
    <*> flag Cabal Stack (long "stack" <> help "Use stack (by default cabal is used)")
    <*> many (strOption (long "ghcide-options" <> help "additional options for ghcide"))
    <*> many (strOption (short 's' <> long "select" <> help "select which benchmarks to run"))
    <*> optional (option auto (long "samples" <> metavar "NAT" <> help "override sampling count"))
    <*> strOption (long "ghcide" <> metavar "PATH" <> help "path to ghcide" <> value "ghcide")
    <*> option auto (long "timeout" <> value 60 <> help "timeout for waiting for a ghcide response")
    <*> ( Example
               <$> exampleName
               <*> (ExampleHackage <$> packageP)
               <*> (some moduleOption <|> pure ["src/Distribution/Simple.hs"])
               <*> pure []
      <|> Example
               <$> exampleName
               <*> pathOrScriptP
               <*> some moduleOption
               <*> pure [])
    <*> switch (long "lsp-config" <> help "Read an LSP config payload from standard input")
  where
      moduleOption = strOption (long "example-module" <> metavar "PATH")
      exampleName = strOption (long "example-name" <> metavar "NAME")

      packageP = ExamplePackage
            <$> strOption (long "example-package-name" <> value "Cabal")
            <*> option versionP (long "example-package-version" <> value (makeVersion [3,6,0,0]))
      pathOrScriptP = ExamplePath   <$> strOption (long "example-path")
                  <|> ExampleScript <$> strOption (long "example-script") <*> many (strOption (long "example-script-args" <> help "arguments for the example generation script"))

versionP :: ReadM Version
versionP = maybeReader $ extract . readP_to_S parseVersion
  where
      extract parses = listToMaybe [ res | (res,"") <- parses]

output :: (MonadIO m, HasConfig) => String -> m ()
output = if quiet ?config then (\_ -> pure ()) else liftIO . putStrLn

---------------------------------------------------------------------------------------

type Experiment = [DocumentPositions] -> Session Bool

data Bench =
  Bench
  { name       :: !String,
    enabled    :: !Bool,
    samples    :: !Natural,
    benchSetup :: [DocumentPositions] -> Session (),
    experiment :: Experiment
  }

select :: HasConfig => Bench -> Bool
select Bench {name, enabled} =
  enabled && (null mm || name `elem` mm)
  where
    mm = matches ?config

benchWithSetup ::
  String ->
  ([DocumentPositions] -> Session ()) ->
  Experiment ->
  Bench
benchWithSetup name benchSetup experiment = Bench {..}
  where
    enabled = True
    samples = 100

bench :: String -> Experiment -> Bench
bench name = benchWithSetup name (const $ pure ())

runBenchmarksFun :: HasConfig => FilePath -> [Bench] -> IO ()
runBenchmarksFun dir allBenchmarks = do
  let benchmarks = [ b{samples = fromMaybe 100 (repetitions ?config) }
                   | b <- allBenchmarks
                   , select b ]

  whenJust (otMemoryProfiling ?config) $ \eventlogDir ->
      createDirectoryIfMissing True eventlogDir

  lspConfig <- if Experiments.Types.lspConfig ?config
    then either error id . eitherDecodeStrict' <$> BS.getContents
    else return mempty

  let conf = defaultConfig
        { logStdErr = verbose ?config,
          logMessages = verbose ?config,
          logColor = False,
          Language.LSP.Test.lspConfig = lspConfig,
          messageTimeout = timeoutLsp ?config
        }
  results <- forM benchmarks $ \b@Bench{name} ->  do
    let p = (proc (ghcide ?config) (allArgs name dir))
                { std_in = CreatePipe, std_out = CreatePipe, std_err = CreatePipe }
        run sess = withCreateProcess p $ \(Just inH) (Just outH) (Just errH) pH -> do
                    -- Need to continuously consume to stderr else it gets blocked
                    -- Can't pass NoStream either to std_err
                    hSetBuffering errH NoBuffering
                    hSetBinaryMode errH True
                    let errSinkThread =
                            forever $ hGetLine errH >>= when (verbose ?config). putStrLn
                    withAsync errSinkThread $ \_ -> do
                        runSessionWithHandles' (Just pH) inH outH conf lspTestCaps dir sess
    (b,) <$> runBench run b

  -- output raw data as CSV
  let headers =
        [ "name"
        , "success"
        , "samples"
        , "startup"
        , "setup"
        , "userT"
        , "delayedT"
        , "1stBuildT"
        , "avgPerRespT"
        , "totalT"
        , "rulesBuilt"
        , "rulesChanged"
        , "rulesVisited"
        , "rulesTotal"
        , "ruleEdges"
        , "ghcRebuilds"
        ]
      rows =
        [ [ name,
            show success,
            show samples,
            showMs startup,
            showMs runSetup',
            showMs userWaits,
            showMs delayedWork,
            showMs $ firstResponse+firstResponseDelayed,
            -- Exclude first response as it has a lot of setup time included
            -- Assume that number of requests = number of modules * number of samples
            showMs ((userWaits - firstResponse)/((fromIntegral samples - 1)*modules)),
            showMs runExperiment,
            show rulesBuilt,
            show rulesChanged,
            show rulesVisited,
            show rulesTotal,
            show edgesTotal,
            show rebuildsTotal
          ]
          | (Bench {name, samples}, BenchRun {..}) <- results,
            let runSetup' = if runSetup < 0.01 then 0 else runSetup
                modules = fromIntegral $ length $ exampleModules $ example ?config
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
            showDuration userWaits,
            showDuration delayedWork,
            showDuration firstResponse,
            showDuration runExperiment,
            show rulesBuilt,
            show rulesChanged,
            show rulesVisited,
            show rulesTotal,
            show edgesTotal,
            show rebuildsTotal
          ]
          | (Bench {name, samples}, BenchRun {..}) <- results,
            let runSetup' = if runSetup < 0.01 then 0 else runSetup
        ]
  outputRow paddedHeaders
  outputRow $ (map . map) (const '-') paddedHeaders
  forM_ rowsHuman $ \row -> outputRow $ zipWith pad pads row
  where
    ghcideArgs dir =
        [ "--lsp",
          "--test",
          "--cwd",
          dir
        ]
    allArgs name dir =
        ghcideArgs dir
          ++ concat
             [ [ "+RTS"
               , "-l"
               , "-ol" ++ (dir </> map (\c -> if c == ' ' then '-' else c) name <.> "eventlog")
               , "-RTS"
               ]
             | Just dir <- [otMemoryProfiling ?config]
             ]
          ++ ghcideOptions ?config
          ++ concat
            [ ["--shake-profiling", path] | Just path <- [shakeProfiling ?config]
            ]
          ++ ["--ot-memory-profiling" | Just _ <- [otMemoryProfiling ?config]]
    lspTestCaps =
      fullLatestClientCaps
        & (L.window . _Just) .~ WindowClientCapabilities (Just True) Nothing Nothing
        & (L.textDocument . _Just . L.codeAction . _Just . L.resolveSupport . _Just) .~ (ClientCodeActionResolveOptions ["edit"])
        & (L.textDocument . _Just . L.codeAction . _Just . L.dataSupport . _Just) .~ True

showMs :: Seconds -> String
showMs = printf "%.2f"

data BenchRun = BenchRun
  { startup              :: !Seconds,
    runSetup             :: !Seconds,
    runExperiment        :: !Seconds,
    userWaits            :: !Seconds,
    delayedWork          :: !Seconds,
    firstResponse        :: !Seconds,
    firstResponseDelayed :: !Seconds,
    rulesBuilt           :: !Int,
    rulesChanged         :: !Int,
    rulesVisited         :: !Int,
    rulesTotal           :: !Int,
    edgesTotal           :: !Int,
    rebuildsTotal        :: !Int,
    success              :: !Bool
  }

badRun :: BenchRun
badRun = BenchRun 0 0 0 0 0 0 0 0 0 0 0 0 0 False

waitForProgressStart :: Session ()
waitForProgressStart = void $ do
    skipManyTill anyMessage $ satisfy $ \case
      FromServerMess SMethod_WindowWorkDoneProgressCreate _ -> True
      _                                              -> False

-- | Wait for all progress to be done
-- Needs at least one progress done notification to return
waitForProgressDone :: Session ()
waitForProgressDone = loop
  where
    loop = do
      ~() <- skipManyTill anyMessage $ satisfyMaybe $ \case
        FromServerMess  SMethod_Progress  (TNotificationMessage _ _ (ProgressParams _ v)) | is _workDoneProgressEnd v -> Just ()
        _ -> Nothing
      done <- null <$> getIncompleteProgressSessions
      unless done loop

-- | Wait for the build queue to be empty
waitForBuildQueue :: Session Seconds
waitForBuildQueue = do
    let m = SMethod_CustomMethod (Proxy @"test")
    waitId <- sendRequest m (toJSON WaitForShakeQueue)
    (td, resp) <- duration $ skipManyTill anyMessage $ responseForId m waitId
    case resp of
        TResponseMessage{_result=Right Null} -> return td
        -- assume a ghcide binary lacking the WaitForShakeQueue method
        _                                    -> return 0

runBench ::
  HasConfig =>
  (Session BenchRun -> IO BenchRun) ->
  Bench ->
  IO BenchRun
runBench runSess Bench{..} = handleAny (\e -> print e >> return badRun)
  $ runSess
  $ do
      (startup, docs) <- duration $ do
        (d, docs) <- duration $ setupDocumentContents ?config
        output $ "Setting up document contents took " <> showDuration d
        -- wait again, as the progress is restarted once while loading the cradle
        -- make an edit, to ensure this doesn't block
        let DocumentPositions{..} = head docs
        changeDoc doc [charEdit stringLiteralP]
        waitForProgressDone
        return docs

      liftIO $ output $ "Running " <> name <> " benchmark"
      (runSetup, ()) <- duration $ benchSetup docs
      let loop' (Just timeForFirstResponse) !userWaits !delayedWork 0 = return $ Just (userWaits, delayedWork, timeForFirstResponse)
          loop' timeForFirstResponse !userWaits !delayedWork n = do
            (t, res) <- duration $ experiment docs
            if not res
              then return Nothing
            else do
                output (showDuration t)
                -- Wait for the delayed actions to finish
                td <- waitForBuildQueue
                loop' (timeForFirstResponse <|> Just (t,td)) (userWaits+t) (delayedWork+td) (n -1)
          loop = loop' Nothing

      (runExperiment, result) <- duration $ loop 0 0 samples
      let success = isJust result
          (userWaits, delayedWork, (firstResponse, firstResponseDelayed)) = fromMaybe (0,0,(0,0)) result

      rulesTotal <- length <$> getStoredKeys
      rulesBuilt <- either (const 0) length <$> getBuildKeysBuilt
      rulesChanged <- either (const 0) length <$> getBuildKeysChanged
      rulesVisited <- either (const 0) length <$> getBuildKeysVisited
      edgesTotal   <- fromRight 0 <$> getBuildEdgesCount
      rebuildsTotal <- fromRight 0 <$> getRebuildsCount

      return BenchRun {..}

data SetupResult = SetupResult {
    runBenchmarks :: [Bench] -> IO (),
    -- | Path to the setup benchmark example
    benchDir      :: FilePath,
    cleanUp       :: IO ()
}

callCommandLogging :: HasConfig => String -> IO ()
callCommandLogging cmd = do
    output cmd
    callCommand cmd

simpleCabalCradleContent :: String
simpleCabalCradleContent = "cradle:\n  cabal:\n"

simpleStackCradleContent :: String
simpleStackCradleContent = "cradle:\n  stack:\n"

-- | Setup the benchmark
-- we need to create a hie.yaml file for the examples
-- or the hie.yaml file would be searched in the parent directories recursively
-- implicit-hie is error prone for the example test `lsp-types-2.1.1.0`
-- we are using the simpleCabalCradleContent for the hie.yaml file instead.
-- it works if we have cabal > 3.2.
setup :: HasConfig => IO SetupResult
setup = do
  benchDir <- case exampleDetails(example ?config) of
      ExamplePath examplePath -> do
          let hieYamlPath = examplePath </> "hie.yaml"
          alreadyExists <- doesFileExist hieYamlPath
          unless alreadyExists $ writeFile hieYamlPath simpleCabalCradleContent
          return examplePath
      ExampleScript examplePath' scriptArgs -> do
          let exampleDir = examplesPath </> exampleName (example ?config)
          alreadySetup <- doesDirectoryExist exampleDir
          unless alreadySetup $ do
            createDirectoryIfMissing True exampleDir
            examplePath <- makeAbsolute examplePath'
            cmd_ (Cwd exampleDir) examplePath scriptArgs
            let hieYamlPath = exampleDir </> "hie.yaml"
            alreadyExists <- doesFileExist hieYamlPath
            unless alreadyExists $ writeFile hieYamlPath simpleCabalCradleContent

          return exampleDir
      ExampleHackage ExamplePackage{..} -> do
        let path = examplesPath </> package
            package = packageName <> "-" <> showVersion packageVersion
            hieYamlPath = path </> "hie.yaml"
        alreadySetup <- doesDirectoryExist path
        unless alreadySetup $
          case buildTool ?config of
            Cabal -> do
                let cabalVerbosity = "-v" ++ show (fromEnum (verbose ?config))
                callCommandLogging $ "cabal get " <> cabalVerbosity <> " " <> package <> " -d " <> examplesPath
                let hieYamlPath = path </> "hie.yaml"
                writeFile hieYamlPath simpleCabalCradleContent
                -- Need this in case there is a parent cabal.project somewhere
                writeFile
                    (path </> "cabal.project")
                    "packages: ."
                writeFile
                    (path </> "cabal.project.local")
                    ""
            Stack -> do
                let stackVerbosity = case verbosity ?config of
                        Quiet  -> "--silent"
                        Normal -> ""
                        All    -> "--verbose"
                callCommandLogging $ "stack " <> stackVerbosity <> " unpack " <> package <> " --to " <> examplesPath
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
                writeFile hieYamlPath simpleStackCradleContent
        return path

  whenJust (shakeProfiling ?config) $ createDirectoryIfMissing True

  let cleanUp = case exampleDetails (example ?config) of
        ExampleHackage _  -> removeDirectoryRecursive examplesPath
        ExampleScript _ _ -> removeDirectoryRecursive examplesPath
        ExamplePath _     -> return ()

      runBenchmarks = runBenchmarksFun benchDir

  return SetupResult{..}

setupDocumentContents :: Config -> Session [DocumentPositions]
setupDocumentContents config =
        forM (exampleModules $ example config) $ \m -> do
        doc <- openDoc m "haskell"

        -- Setup the special positions used by the experiments
        lastLine <- fromIntegral . length . T.lines <$> documentContents doc
        changeDoc doc [TextDocumentContentChangeEvent $ InL TextDocumentContentChangePartial
                        { _range = Range (Position lastLine 0) (Position lastLine 0)
                        , _rangeLength = Nothing
                        , _text = T.unlines [ "_hygienic = \"hygienic\"" ]
                        }
                      ]
        let
        -- Points to a string in the target file,
        -- convenient for hygienic edits
            stringLiteralP = Position lastLine 15

        -- Find an identifier defined in another file in this project
        symbols <- getDocumentSymbols doc
        let endOfImports = case symbols of
                Right symbols | Just x <- findEndOfImports symbols -> x
                _ -> error $ "symbols: " <> show symbols
        contents <- documentContents doc
        identifierP <- searchSymbol doc contents endOfImports
        return $ DocumentPositions{..}

findEndOfImports :: [DocumentSymbol] -> Maybe Position
findEndOfImports (DocumentSymbol{_kind = SymbolKind_Module, _name = "imports", _range} : _) =
    Just $ Position (succ $ _line $ _end _range) 4
findEndOfImports [DocumentSymbol{_kind = SymbolKind_File, _children = Just cc}] =
    findEndOfImports cc
findEndOfImports (DocumentSymbol{_range} : _) =
    Just $ _range ^. L.start
findEndOfImports _ = Nothing

--------------------------------------------------------------------------------------------

pad :: Int -> String -> String
pad n []     = replicate n ' '
pad 0 _      = error "pad"
pad n (x:xx) = x : pad (n-1) xx

-- | Search for a position where:
--     - get definition works and returns a uri other than this file
--     - get completions returns a non empty list
searchSymbol :: TextDocumentIdentifier -> T.Text -> Position -> Session (Maybe Position)
searchSymbol doc@TextDocumentIdentifier{_uri} fileContents pos = do
    -- this search is expensive, so we cache the result on disk
    let cachedPath = fromJust (uriToFilePath _uri) <.> "identifierPosition"
    cachedRes <- liftIO $ try @_ @IOException $ A.decode . BSL.fromStrict <$> BS.readFile cachedPath
    case cachedRes of
        Left _ -> do
            result <- loop pos
            liftIO $ BS.writeFile cachedPath $ BSL.toStrict $ A.encode result
            return result
        Right res ->
            return res
  where
      loop pos
        | (fromIntegral $ _line pos) >= lll =
            return Nothing
        | (fromIntegral $ _character pos) >= lengthOfLine (fromIntegral $ _line pos) =
            loop (nextLine pos)
        | otherwise = do
                checks <- checkDefinitions pos &&^ checkCompletions pos
                if checks
                    then return $ Just pos
                    else loop (nextIdent pos)

      nextIdent p = p{_character = _character p + 2}
      nextLine p = Position (_line p + 1) 4

      lengthOfLine n = if n >= lll then 0 else T.length (ll !! n)
      ll = T.lines fileContents
      lll = length ll

      checkDefinitions pos = do
        defs <- getDefinitions doc pos
        case defs of
            (InL (Definition (InR [Location uri _]))) -> return $ uri /= _uri
            _                                         -> return False
      checkCompletions pos =
        not . null <$> getCompletions doc pos


getBuildKeysBuilt :: Session (Either (TResponseError @ClientToServer (Method_CustomMethod "test")) [T.Text])
getBuildKeysBuilt = tryCallTestPlugin GetBuildKeysBuilt

getBuildKeysVisited :: Session (Either (TResponseError @ClientToServer (Method_CustomMethod "test")) [T.Text])
getBuildKeysVisited = tryCallTestPlugin GetBuildKeysVisited

getBuildKeysChanged :: Session (Either (TResponseError @ClientToServer (Method_CustomMethod "test")) [T.Text])
getBuildKeysChanged = tryCallTestPlugin GetBuildKeysChanged

getBuildEdgesCount :: Session (Either (TResponseError @ClientToServer (Method_CustomMethod "test")) Int)
getBuildEdgesCount = tryCallTestPlugin GetBuildEdgesCount

getRebuildsCount :: Session (Either (TResponseError @ClientToServer (Method_CustomMethod "test")) Int)
getRebuildsCount = tryCallTestPlugin GetRebuildsCount

-- Copy&paste from ghcide/test/Development.IDE.Test
getStoredKeys :: Session [Text]
getStoredKeys = callTestPlugin GetStoredKeys

-- Copy&paste from ghcide/test/Development.IDE.Test
tryCallTestPlugin :: (A.FromJSON b) => TestRequest -> Session (Either (TResponseError @ClientToServer (Method_CustomMethod "test")) b)
tryCallTestPlugin cmd = do
    let cm = SMethod_CustomMethod (Proxy @"test")
    waitId <- sendRequest cm (A.toJSON cmd)
    TResponseMessage{_result} <- skipManyTill anyMessage $ responseForId cm waitId
    return $ case _result of
         Left e -> Left e
         Right json -> case A.fromJSON json of
             A.Success a -> Right a
             A.Error e   -> error e

-- Copy&paste from ghcide/test/Development.IDE.Test
callTestPlugin :: (A.FromJSON b) => TestRequest -> Session b
callTestPlugin cmd = do
    res <- tryCallTestPlugin cmd
    case res of
        Left (TResponseError t err _) -> error $ show t <> ": " <> T.unpack err
        Right a                       -> pure a
