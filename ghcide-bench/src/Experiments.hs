{-# LANGUAGE ConstraintKinds           #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE GADTs                     #-}
{-# LANGUAGE ImplicitParams            #-}
{-# LANGUAGE ImpredicativeTypes        #-}
{-# LANGUAGE OverloadedStrings         #-}
{-# LANGUAGE PolyKinds                 #-}
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
import           Control.Applicative.Combinators (skipManyTill)
import           Control.Concurrent.Async        (withAsync)
import           Control.Exception.Safe          (IOException, handleAny, try)
import           Control.Monad.Extra             (allM, forM, forM_, forever,
                                                  unless, void, when, whenJust,
                                                  (&&^))
import           Control.Monad.Fail              (MonadFail)
import           Control.Monad.IO.Class
import           Data.Aeson                      (Value (Null),
                                                  eitherDecodeStrict', toJSON)
import qualified Data.Aeson                      as A
import qualified Data.ByteString                 as BS
import           Data.Either                     (fromRight)
import           Data.List
import           Data.Maybe
import           Data.Text                       (Text)
import qualified Data.Text                       as T
import           Data.Version
import           Development.IDE.Plugin.Test
import           Development.IDE.Test.Diagnostic
import           Development.Shake               (CmdOption (Cwd, FileStdout),
                                                  cmd_)
import           Experiments.Types
import           Language.LSP.Test
import           Language.LSP.Types              hiding
                                                 (SemanticTokenAbsolute (length, line),
                                                  SemanticTokenRelative (length),
                                                  SemanticTokensEdit (_start))
import           Language.LSP.Types.Capabilities
import           Numeric.Natural
import           Options.Applicative
import           System.Directory
import           System.Environment.Blank        (getEnv)
import           System.FilePath                 ((<.>), (</>))
import           System.IO
import           System.Process
import           System.Time.Extra
import           Text.ParserCombinators.ReadP    (readP_to_S)
import           Text.Printf

charEdit :: Position -> TextDocumentContentChangeEvent
charEdit p =
    TextDocumentContentChangeEvent
    { _range = Just (Range p p),
      _rangeLength = Nothing,
      _text = "a"
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
    [ ---------------------------------------------------------------------------------------
      bench "hover" $ allWithIdentifierPos $ \DocumentPositions{..} ->
        isJust <$> getHover doc (fromJust identifierP),
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
      bench "hover after edit" $ \docs -> do
        forM_ docs $ \DocumentPositions{..} ->
          changeDoc doc [charEdit stringLiteralP]
        flip allWithIdentifierPos docs $ \DocumentPositions{..} ->
          isJust <$> getHover doc (fromJust identifierP),
      ---------------------------------------------------------------------------------------
      bench "getDefinition" $ allWithIdentifierPos $ \DocumentPositions{..} ->
        either (not . null) (not . null) . toEither <$> getDefinitions doc (fromJust identifierP),
      ---------------------------------------------------------------------------------------
      bench "getDefinition after edit" $ \docs -> do
          forM_ docs $ \DocumentPositions{..} ->
            changeDoc doc [charEdit stringLiteralP]
          flip allWithIdentifierPos docs $ \DocumentPositions{..} ->
            either (not . null) (not . null) . toEither <$> getDefinitions doc (fromJust identifierP),
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
      benchWithSetup
        "code actions"
        ( \docs -> do
            unless (any (isJust . identifierP) docs) $
                error "None of the example modules is suitable for this experiment"
            forM_ docs $ \DocumentPositions{..} -> do
                forM_ identifierP $ \p -> changeDoc doc [charEdit p]
                waitForProgressStart
            waitForProgressDone
        )
        ( \docs -> not . null . catMaybes <$> forM docs (\DocumentPositions{..} ->
            forM identifierP $ \p ->
              getCodeActions doc (Range p p))
        ),
      ---------------------------------------------------------------------------------------
      benchWithSetup
        "code actions after edit"
        ( \docs -> do
            unless (any (isJust . identifierP) docs) $
                error "None of the example modules is suitable for this experiment"
            forM_ docs $ \DocumentPositions{..} ->
                forM_ identifierP $ \p -> changeDoc doc [charEdit p]
        )
        ( \docs -> do
            forM_ docs $ \DocumentPositions{..} -> do
              changeDoc doc [charEdit stringLiteralP]
              waitForProgressStart
            waitForProgressDone
            not . null . catMaybes <$> forM docs (\DocumentPositions{..} -> do
              forM identifierP $ \p ->
                getCodeActions doc (Range p p))
        ),
      ---------------------------------------------------------------------------------------
      benchWithSetup
        "code actions after cradle edit"
        ( \docs -> do
            forM_ docs $ \DocumentPositions{..} -> do
                forM identifierP $ \p -> do
                    changeDoc doc [charEdit p]
                    waitForProgressStart
            void waitForBuildQueue
        )
        ( \docs -> do
            hieYamlUri <- getDocUri "hie.yaml"
            liftIO $ appendFile (fromJust $ uriToFilePath hieYamlUri) "##\n"
            sendNotification SWorkspaceDidChangeWatchedFiles $ DidChangeWatchedFilesParams $
                List [ FileEvent hieYamlUri FcChanged ]
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
        "hover after cradle edit"
        (\docs -> do
            hieYamlUri <- getDocUri "hie.yaml"
            liftIO $ appendFile (fromJust $ uriToFilePath hieYamlUri) "##\n"
            sendNotification SWorkspaceDidChangeWatchedFiles $ DidChangeWatchedFilesParams $
                List [ FileEvent hieYamlUri FcChanged ]
            flip allWithIdentifierPos docs $ \DocumentPositions{..} -> isJust <$> getHover doc (fromJust identifierP)
        ),
      ---------------------------------------------------------------------------------------
      benchWithSetup
        "hole fit suggestions"
        ( mapM_ $ \DocumentPositions{..} -> do
            let edit :: TextDocumentContentChangeEvent =TextDocumentContentChangeEvent
                  { _range = Just Range {_start = bottom, _end = bottom}
                  , _rangeLength = Nothing, _text = t}
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
                case requireDiagnostic diags (DsError, (fromIntegral bottom, 8), "Found hole", Nothing) of
                    Nothing   -> pure True
                    Just _err -> pure False
        )
    ]

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
    <*> ( Example "name"
               <$> (Right <$> packageP)
               <*> (some moduleOption <|> pure ["src/Distribution/Simple.hs"])
               <*> pure []
         <|>
          Example "name"
                <$> (Left <$> pathP)
                <*> some moduleOption
                <*> pure [])
    <*> switch (long "lsp-config" <> help "Read an LSP config payload from standard input")
  where
      moduleOption = strOption (long "example-module" <> metavar "PATH")

      packageP = ExamplePackage
            <$> strOption (long "example-package-name" <> value "Cabal")
            <*> option versionP (long "example-package-version" <> value (makeVersion [3,6,0,0]))
      pathP = strOption (long "example-path")

versionP :: ReadM Version
versionP = maybeReader $ extract . readP_to_S parseVersion
  where
      extract parses = listToMaybe [ res | (res,"") <- parses]

output :: (MonadIO m, HasConfig) => String -> m ()
output = if quiet?config then (\_ -> pure ()) else liftIO . putStrLn

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
    then either error Just . eitherDecodeStrict' <$> BS.getContents
    else return Nothing

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
      fullCaps {_window = Just $ WindowClientCapabilities (Just True) Nothing Nothing }

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
      FromServerMess SWindowWorkDoneProgressCreate _ -> True
      _                                              -> False

-- | Wait for all progress to be done
-- Needs at least one progress done notification to return
waitForProgressDone :: Session ()
waitForProgressDone = loop
  where
    loop = do
      ~() <- skipManyTill anyMessage $ satisfyMaybe $ \case
        FromServerMess SProgress (NotificationMessage _ _ (ProgressParams _ (End _))) -> Just ()
        _ -> Nothing
      done <- null <$> getIncompleteProgressSessions
      unless done loop

-- | Wait for the build queue to be empty
waitForBuildQueue :: Session Seconds
waitForBuildQueue = do
    let m = SCustomMethod "test"
    waitId <- sendRequest m (toJSON WaitForShakeQueue)
    (td, resp) <- duration $ skipManyTill anyMessage $ responseForId m waitId
    case resp of
        ResponseMessage{_result=Right Null} -> return td
        -- assume a ghcide binary lacking the WaitForShakeQueue method
        _                                   -> return 0

runBench ::
  HasConfig =>
  (Session BenchRun -> IO BenchRun) ->
  Bench ->
  IO BenchRun
runBench runSess b = handleAny (\e -> print e >> return badRun)
  $ runSess
  $ do
    case b of
     Bench{..} -> do
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

setup :: HasConfig => IO SetupResult
setup = do
--   when alreadyExists $ removeDirectoryRecursive examplesPath
  benchDir <- case exampleDetails(example ?config) of
      Left examplePath -> do
          let hieYamlPath = examplePath </> "hie.yaml"
          alreadyExists <- doesFileExist hieYamlPath
          unless alreadyExists $
                cmd_ (Cwd examplePath) (FileStdout hieYamlPath) ("gen-hie"::String)
          return examplePath
      Right ExamplePackage{..} -> do
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
                cmd_ (Cwd path) (FileStdout hieYamlPath) ("gen-hie"::String)
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

                cmd_ (Cwd path) (FileStdout hieYamlPath) ("gen-hie"::String) ["--stack"::String]
        return path

  whenJust (shakeProfiling ?config) $ createDirectoryIfMissing True

  let cleanUp = case exampleDetails(example ?config) of
        Right _ -> removeDirectoryRecursive examplesPath
        Left _  -> return ()

      runBenchmarks = runBenchmarksFun benchDir

  return SetupResult{..}

setupDocumentContents :: Config -> Session [DocumentPositions]
setupDocumentContents config =
        forM (exampleModules $ example config) $ \m -> do
        doc <- openDoc m "haskell"

        -- Setup the special positions used by the experiments
        lastLine <- fromIntegral . length . T.lines <$> documentContents doc
        changeDoc doc [TextDocumentContentChangeEvent
            { _range = Just (Range (Position lastLine 0) (Position lastLine 0))
            , _rangeLength = Nothing
            , _text = T.unlines [ "_hygienic = \"hygienic\"" ]
            }]
        let
        -- Points to a string in the target file,
        -- convenient for hygienic edits
            stringLiteralP = Position lastLine 15

        -- Find an identifier defined in another file in this project
        symbols <- getDocumentSymbols doc
        let endOfImports = case symbols of
                Left symbols | Just x <- findEndOfImports symbols -> x
                _ -> error $ "symbols: " <> show symbols
        contents <- documentContents doc
        identifierP <- searchSymbol doc contents endOfImports
        return $ DocumentPositions{..}

findEndOfImports :: [DocumentSymbol] -> Maybe Position
findEndOfImports (DocumentSymbol{_kind = SkModule, _name = "imports", _range} : _) =
    Just $ Position (succ $ _line $ _end _range) 4
findEndOfImports [DocumentSymbol{_kind = SkFile, _children = Just (List cc)}] =
    findEndOfImports cc
findEndOfImports (DocumentSymbol{_range} : _) =
    Just $ _start _range
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
    cachedRes <- liftIO $ try @_ @IOException $ read <$> readFile cachedPath
    case cachedRes of
        Left _ -> do
            result <- loop pos
            liftIO $ writeFile cachedPath $ show result
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
            (InL [Location uri _]) -> return $ uri /= _uri
            _                      -> return False
      checkCompletions pos =
        not . null <$> getCompletions doc pos


getBuildKeysBuilt :: Session (Either ResponseError [T.Text])
getBuildKeysBuilt = tryCallTestPlugin GetBuildKeysBuilt

getBuildKeysVisited :: Session (Either ResponseError [T.Text])
getBuildKeysVisited = tryCallTestPlugin GetBuildKeysVisited

getBuildKeysChanged :: Session (Either ResponseError [T.Text])
getBuildKeysChanged = tryCallTestPlugin GetBuildKeysChanged

getBuildEdgesCount :: Session (Either ResponseError Int)
getBuildEdgesCount = tryCallTestPlugin GetBuildEdgesCount

getRebuildsCount :: Session (Either ResponseError Int)
getRebuildsCount = tryCallTestPlugin GetRebuildsCount

-- Copy&paste from ghcide/test/Development.IDE.Test
getStoredKeys :: Session [Text]
getStoredKeys = callTestPlugin GetStoredKeys

-- Copy&paste from ghcide/test/Development.IDE.Test
tryCallTestPlugin :: (A.FromJSON b) => TestRequest -> Session (Either ResponseError b)
tryCallTestPlugin cmd = do
    let cm = SCustomMethod "test"
    waitId <- sendRequest cm (A.toJSON cmd)
    ResponseMessage{_result} <- skipManyTill anyMessage $ responseForId cm waitId
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
        Left (ResponseError t err _) -> error $ show t <> ": " <> T.unpack err
        Right a                      -> pure a
