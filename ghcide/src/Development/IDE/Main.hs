{-# OPTIONS_GHC -Wno-orphans #-}
module Development.IDE.Main
(Arguments(..)
,defaultArguments
,Command(..)
,IdeCommand(..)
,isLSP
,commandP
,defaultMain
,testing
,Log(..)
) where

import           Control.Concurrent.Extra                 (Chan, newChan,
                                                           withNumCapabilities,
                                                           writeChan)
import           Control.Concurrent.MVar                  (newEmptyMVar,
                                                           putMVar, tryReadMVar)
import           Control.Concurrent.STM.Stats             (dumpSTMStats)
import           Control.Exception.Safe                   (SomeException,
                                                           catchAny,
                                                           displayException)
import           Control.Monad.Extra                      (concatMapM, unless,
                                                           when)
import           Control.Monad.IO.Class                   (liftIO)
import qualified Data.Aeson                               as J
import           Data.Coerce                              (coerce)
import           Data.Default                             (Default (def))
import           Data.Foldable                            (traverse_)
import           Data.Hashable                            (hashed)
import qualified Data.HashMap.Strict                      as HashMap
import           Data.List.Extra                          (intercalate,
                                                           isPrefixOf, nubOrd,
                                                           partition)
import           Data.Maybe                               (catMaybes, isJust)
import qualified Data.Text                                as T
import           Development.IDE                          (Action,
                                                           Priority (Debug, Error),
                                                           Rules, emptyFilePath,
                                                           hDuplicateTo')
import           Development.IDE.Core.Debouncer           (Debouncer,
                                                           newAsyncDebouncer)
import           Development.IDE.Core.FileStore           (isWatchSupported,
                                                           setSomethingModified)
import           Development.IDE.Core.IdeConfiguration    (IdeConfiguration (..),
                                                           modifyClientSettings,
                                                           registerIdeConfiguration)
import           Development.IDE.Core.OfInterest          (FileOfInterestStatus (OnDisk),
                                                           kick,
                                                           setFilesOfInterest)
import           Development.IDE.Core.Rules               (mainRule)
import qualified Development.IDE.Core.Rules               as Rules
import           Development.IDE.Core.RuleTypes           (GenerateCore (GenerateCore),
                                                           GetHieAst (GetHieAst),
                                                           TypeCheck (TypeCheck))
import           Development.IDE.Core.Service             (initialise,
                                                           runAction)
import qualified Development.IDE.Core.Service             as Service
import           Development.IDE.Core.Shake               (IdeState (shakeExtras),
                                                           IndexQueue,
                                                           shakeSessionInit,
                                                           uses)
import qualified Development.IDE.Core.Shake               as Shake
import           Development.IDE.Graph                    (action)
import           Development.IDE.LSP.LanguageServer       (runLanguageServer,
                                                           setupLSP)
import qualified Development.IDE.LSP.LanguageServer       as LanguageServer
import           Development.IDE.LSP.Server
import           Development.IDE.Main.HeapStats           (withHeapStats)
import qualified Development.IDE.Main.HeapStats           as HeapStats
import qualified Development.IDE.Monitoring.EKG           as EKG
import qualified Development.IDE.Monitoring.OpenTelemetry as OpenTelemetry
import           Development.IDE.Plugin                   (Plugin (pluginHandlers, pluginModifyDynflags, pluginRules))
import           Development.IDE.Plugin.HLS               (asGhcIdePlugin)
import qualified Development.IDE.Plugin.HLS               as PluginHLS
import qualified Development.IDE.Plugin.HLS.GhcIde        as GhcIde
import qualified Development.IDE.Plugin.Test              as Test
import           Development.IDE.Session                  (SessionLoadingOptions,
                                                           getHieDbLoc,
                                                           loadSessionWithOptions,
                                                           retryOnSqliteBusy,
                                                           runWithDb,
                                                           setInitialDynFlags)
import qualified Development.IDE.Session                  as Session
import           Development.IDE.Types.Location           (NormalizedUri,
                                                           toNormalizedFilePath')
import           Development.IDE.Types.Monitoring         (Monitoring)
import           Development.IDE.Types.Options            (IdeGhcSession,
                                                           IdeOptions (optCheckParents, optCheckProject, optReportProgress, optRunSubset),
                                                           IdeTesting (IdeTesting),
                                                           clientSupportsProgress,
                                                           defaultIdeOptions,
                                                           optModifyDynFlags,
                                                           optTesting)
import           Development.IDE.Types.Shake              (WithHieDb, toKey)
import           GHC.Conc                                 (getNumProcessors)
import           GHC.IO.Encoding                          (setLocaleEncoding)
import           GHC.IO.Handle                            (hDuplicate)
import           HIE.Bios.Cradle                          (findCradle)
import qualified HieDb.Run                                as HieDb
import           Ide.Logger                               (Logger,
                                                           Pretty (pretty),
                                                           Priority (Info),
                                                           Recorder,
                                                           WithPriority,
                                                           cmapWithPrio,
                                                           logDebug, logWith,
                                                           nest, vsep, (<+>))
import           Ide.Plugin.Config                        (CheckParents (NeverCheck),
                                                           Config, checkParents,
                                                           checkProject,
                                                           getConfigFromNotification)
import           Ide.PluginUtils                          (allLspCmdIds',
                                                           getProcessID,
                                                           idePluginsToPluginDesc,
                                                           pluginDescToIdePlugins)
import           Ide.Types                                (IdeCommand (IdeCommand),
                                                           IdePlugins,
                                                           PluginDescriptor (PluginDescriptor, pluginCli),
                                                           PluginId (PluginId),
                                                           ipMap, pluginId)
import qualified Language.LSP.Server                      as LSP
import           Numeric.Natural                          (Natural)
import           Options.Applicative                      hiding (action)
import qualified System.Directory.Extra                   as IO
import           System.Exit                              (ExitCode (ExitFailure),
                                                           exitWith)
import           System.FilePath                          (takeExtension,
                                                           takeFileName)
import           System.IO                                (BufferMode (LineBuffering, NoBuffering),
                                                           Handle, hFlush,
                                                           hPutStrLn,
                                                           hSetBuffering,
                                                           hSetEncoding, stderr,
                                                           stdin, stdout, utf8)
import           System.Random                            (newStdGen)
import           System.Time.Extra                        (Seconds, offsetTime,
                                                           showDuration)

data Log
  = LogHeapStats !HeapStats.Log
  | LogLspStart [PluginId]
  | LogLspStartDuration !Seconds
  | LogShouldRunSubset !Bool
  | LogSetInitialDynFlagsException !SomeException
  | LogService Service.Log
  | LogShake Shake.Log
  | LogGhcIde GhcIde.Log
  | LogLanguageServer LanguageServer.Log
  | LogSession Session.Log
  | LogPluginHLS PluginHLS.Log
  | LogRules Rules.Log
  deriving Show

instance Pretty Log where
  pretty = \case
    LogHeapStats msg -> pretty msg
    LogLspStart pluginIds ->
      nest 2 $ vsep
        [ "Starting LSP server..."
        , "If you are seeing this in a terminal, you probably should have run WITHOUT the --lsp option!"
        , "PluginIds:" <+> pretty (coerce @_ @[T.Text] pluginIds)
        ]
    LogLspStartDuration duration ->
      "Started LSP server in" <+> pretty (showDuration duration)
    LogShouldRunSubset shouldRunSubset ->
      "shouldRunSubset:" <+> pretty shouldRunSubset
    LogSetInitialDynFlagsException e ->
      "setInitialDynFlags:" <+> pretty (displayException e)
    LogService msg -> pretty msg
    LogShake msg -> pretty msg
    LogGhcIde msg -> pretty msg
    LogLanguageServer msg -> pretty msg
    LogSession msg -> pretty msg
    LogPluginHLS msg -> pretty msg
    LogRules msg -> pretty msg

data Command
    = Check [FilePath]  -- ^ Typecheck some paths and print diagnostics. Exit code is the number of failures
    | Db {hieOptions ::  HieDb.Options, hieCommand :: HieDb.Command}
     -- ^ Run a command in the hiedb
    | LSP   -- ^ Run the LSP server
    | Custom {ideCommand :: IdeCommand IdeState} -- ^ User defined
    deriving Show

-- TODO move these to hiedb
deriving instance Show HieDb.Command
deriving instance Show HieDb.Options

isLSP :: Command -> Bool
isLSP LSP = True
isLSP _   = False

commandP :: IdePlugins IdeState -> Parser Command
commandP plugins =
    hsubparser(command "typecheck" (info (Check <$> fileCmd) fileInfo)
            <> command "hiedb" (info (Db <$> HieDb.optParser "" True <*> HieDb.cmdParser) hieInfo)
            <> command "lsp" (info (pure LSP) lspInfo)
            <> pluginCommands
            )
  where
    fileCmd = many (argument str (metavar "FILES/DIRS..."))
    lspInfo = fullDesc <> progDesc "Start talking to an LSP client"
    fileInfo = fullDesc <> progDesc "Used as a test bed to check your IDE will work"
    hieInfo = fullDesc <> progDesc "Query .hie files"

    pluginCommands = mconcat
        [ command (T.unpack pId) (Custom <$> p)
        | PluginDescriptor{pluginCli = Just p, pluginId = PluginId pId} <- ipMap plugins
        ]


data Arguments = Arguments
    { argsProjectRoot           :: Maybe FilePath
    , argCommand                :: Command
    , argsLogger                :: IO Logger
    , argsRules                 :: Rules ()
    , argsHlsPlugins            :: IdePlugins IdeState
    , argsGhcidePlugin          :: Plugin Config  -- ^ Deprecated
    , argsSessionLoadingOptions :: SessionLoadingOptions
    , argsIdeOptions            :: Config -> Action IdeGhcSession -> IdeOptions
    , argsLspOptions            :: LSP.Options
    , argsDefaultHlsConfig      :: Config
    , argsGetHieDbLoc           :: FilePath -> IO FilePath -- ^ Map project roots to the location of the hiedb for the project
    , argsDebouncer             :: IO (Debouncer NormalizedUri) -- ^ Debouncer used for diagnostics
    , argsHandleIn              :: IO Handle
    , argsHandleOut             :: IO Handle
    , argsThreads               :: Maybe Natural
    , argsMonitoring            :: IO Monitoring
    }

defaultArguments :: Recorder (WithPriority Log) -> Logger -> IdePlugins IdeState -> Arguments
defaultArguments recorder logger plugins = Arguments
        { argsProjectRoot = Nothing
        , argCommand = LSP
        , argsLogger = pure logger
        , argsRules = mainRule (cmapWithPrio LogRules recorder) def >> action kick
        , argsGhcidePlugin = mempty
        , argsHlsPlugins = pluginDescToIdePlugins (GhcIde.descriptors (cmapWithPrio LogGhcIde recorder)) <> plugins
        , argsSessionLoadingOptions = def
        , argsIdeOptions = \config ghcSession -> (defaultIdeOptions ghcSession)
            { optCheckProject = pure $ checkProject config
            , optCheckParents = pure $ checkParents config
            }
        , argsLspOptions = def {LSP.optCompletionTriggerCharacters = Just "."}
        , argsDefaultHlsConfig = def
        , argsGetHieDbLoc = getHieDbLoc
        , argsDebouncer = newAsyncDebouncer
        , argsThreads = Nothing
        , argsHandleIn = pure stdin
        , argsHandleOut = do
                -- Move stdout to another file descriptor and duplicate stderr
                -- to stdout. This guards against stray prints from corrupting the JSON-RPC
                -- message stream.
                newStdout <- hDuplicate stdout
                stderr `hDuplicateTo'` stdout
                hSetBuffering stdout NoBuffering

                -- Print out a single space to assert that the above redirection works.
                -- This is interleaved with the logger, hence we just print a space here in
                -- order not to mess up the output too much. Verified that this breaks
                -- the language server tests without the redirection.
                putStr " " >> hFlush stdout
                return newStdout
        , argsMonitoring = OpenTelemetry.monitoring <> EKG.monitoring logger 8999
        }


testing :: Recorder (WithPriority Log) -> Logger -> IdePlugins IdeState -> Arguments
testing recorder logger plugins =
  let
    arguments@Arguments{ argsHlsPlugins, argsIdeOptions } =
        defaultArguments recorder logger plugins
    hlsPlugins = pluginDescToIdePlugins $
      idePluginsToPluginDesc argsHlsPlugins
      ++ [Test.blockCommandDescriptor "block-command", Test.plugin]
    ideOptions config sessionLoader =
      let
        defOptions = argsIdeOptions config sessionLoader
      in
        defOptions{ optTesting = IdeTesting True }
  in
    arguments
      { argsHlsPlugins = hlsPlugins
      , argsIdeOptions = ideOptions
      }

defaultMain :: Recorder (WithPriority Log) -> Arguments -> IO ()
defaultMain recorder Arguments{..} = withHeapStats (cmapWithPrio LogHeapStats recorder) fun
 where
  fun = do
    setLocaleEncoding utf8
    pid <- T.pack . show <$> getProcessID
    logger <- argsLogger
    hSetBuffering stderr LineBuffering

    let hlsPlugin = asGhcIdePlugin (cmapWithPrio LogPluginHLS recorder) argsHlsPlugins
        hlsCommands = allLspCmdIds' pid argsHlsPlugins
        plugins = hlsPlugin <> argsGhcidePlugin
        options = argsLspOptions { LSP.optExecuteCommandCommands = LSP.optExecuteCommandCommands argsLspOptions <> Just hlsCommands }
        argsParseConfig = getConfigFromNotification argsHlsPlugins
        rules = argsRules >> pluginRules plugins

    debouncer <- argsDebouncer
    inH <- argsHandleIn
    outH <- argsHandleOut

    numProcessors <- getNumProcessors
    let numCapabilities = max 1 $ maybe (numProcessors `div` 2) fromIntegral argsThreads

    case argCommand of
        LSP -> withNumCapabilities numCapabilities $ do
            ioT <- offsetTime
            logWith recorder Info $ LogLspStart (pluginId <$> ipMap argsHlsPlugins)

            ideStateVar <- newEmptyMVar
            let getIdeState :: LSP.LanguageContextEnv Config -> Maybe FilePath -> WithHieDb -> IndexQueue -> IO IdeState
                getIdeState env rootPath withHieDb hieChan = do
                  traverse_ IO.setCurrentDirectory rootPath
                  t <- ioT
                  logWith recorder Info $ LogLspStartDuration t

                  dir <- maybe IO.getCurrentDirectory return rootPath

                  -- We want to set the global DynFlags right now, so that we can use
                  -- `unsafeGlobalDynFlags` even before the project is configured
                  _mlibdir <-
                      setInitialDynFlags (cmapWithPrio LogSession recorder) dir argsSessionLoadingOptions
                          -- TODO: should probably catch/log/rethrow at top level instead
                          `catchAny` (\e -> logWith recorder Error (LogSetInitialDynFlagsException e) >> pure Nothing)

                  sessionLoader <- loadSessionWithOptions (cmapWithPrio LogSession recorder) argsSessionLoadingOptions dir
                  config <- LSP.runLspT env LSP.getConfig
                  let def_options = argsIdeOptions config sessionLoader

                  -- disable runSubset if the client doesn't support watched files
                  runSubset <- (optRunSubset def_options &&) <$> LSP.runLspT env isWatchSupported
                  logWith recorder Debug $ LogShouldRunSubset runSubset

                  let ideOptions = def_options
                              { optReportProgress = clientSupportsProgress caps
                              , optModifyDynFlags = optModifyDynFlags def_options <> pluginModifyDynflags plugins
                              , optRunSubset = runSubset
                              }
                      caps = LSP.resClientCapabilities env
                  monitoring <- argsMonitoring
                  ide <- initialise
                      (cmapWithPrio LogService recorder)
                      argsDefaultHlsConfig
                      argsHlsPlugins
                      rules
                      (Just env)
                      logger
                      debouncer
                      ideOptions
                      withHieDb
                      hieChan
                      monitoring
                  putMVar ideStateVar ide
                  pure ide

            -- Send everything over a channel, since you need to wait until after initialise before
            -- LspFuncs is available
            clientMsgChan :: Chan ReactorMessage <- newChan

            let setup = setupLSP (cmapWithPrio LogLanguageServer recorder) argsGetHieDbLoc (pluginHandlers plugins) getIdeState clientMsgChan
                -- See Note [Client configuration in Rules]
                onConfigChange cfg = do
                  -- TODO: this is nuts, we're converting back to JSON just to get a fingerprint
                  let cfgObj = J.toJSON cfg
                  let configChangeIO = do
                        mide <- liftIO $ tryReadMVar ideStateVar
                        case mide of
                            Nothing -> pure ()
                            Just ide -> liftIO $ do
                                let msg = T.pack $ show cfg
                                logDebug (Shake.ideLogger ide) $ "Configuration changed: " <> msg
                                modifyClientSettings ide (const $ Just cfgObj)
                                setSomethingModified Shake.VFSUnmodified ide [toKey Rules.GetClientSettings emptyFilePath] "config change"
                  liftIO $ writeChan clientMsgChan $ ReactorNotification configChangeIO


            runLanguageServer (cmapWithPrio LogLanguageServer recorder) options inH outH argsDefaultHlsConfig argsParseConfig onConfigChange setup
            dumpSTMStats
        Check argFiles -> do
          dir <- maybe IO.getCurrentDirectory return argsProjectRoot
          dbLoc <- getHieDbLoc dir
          runWithDb (cmapWithPrio LogSession recorder) dbLoc $ \hiedb hieChan -> do
            -- GHC produces messages with UTF8 in them, so make sure the terminal doesn't error
            hSetEncoding stdout utf8
            hSetEncoding stderr utf8

            putStrLn $ "ghcide setup tester in " ++ dir ++ "."
            putStrLn "Report bugs at https://github.com/haskell/haskell-language-server/issues"

            putStrLn $ "\nStep 1/4: Finding files to test in " ++ dir
            files <- expandFiles (argFiles ++ ["." | null argFiles])
            -- LSP works with absolute file paths, so try and behave similarly
            absoluteFiles <- nubOrd <$> mapM IO.canonicalizePath files
            putStrLn $ "Found " ++ show (length absoluteFiles) ++ " files"

            putStrLn "\nStep 2/4: Looking for hie.yaml files that control setup"
            cradles <- mapM findCradle absoluteFiles
            let ucradles = nubOrd cradles
            let n = length ucradles
            putStrLn $ "Found " ++ show n ++ " cradle" ++ ['s' | n /= 1]
            when (n > 0) $ putStrLn $ "  (" ++ intercalate ", " (catMaybes ucradles) ++ ")"
            putStrLn "\nStep 3/4: Initializing the IDE"
            sessionLoader <- loadSessionWithOptions (cmapWithPrio LogSession recorder) argsSessionLoadingOptions dir
            let def_options = argsIdeOptions argsDefaultHlsConfig sessionLoader
                ideOptions = def_options
                        { optCheckParents = pure NeverCheck
                        , optCheckProject = pure False
                        , optModifyDynFlags = optModifyDynFlags def_options <> pluginModifyDynflags plugins
                        }
            ide <- initialise (cmapWithPrio LogService recorder) argsDefaultHlsConfig argsHlsPlugins rules Nothing logger debouncer ideOptions hiedb hieChan mempty
            shakeSessionInit (cmapWithPrio LogShake recorder) ide
            registerIdeConfiguration (shakeExtras ide) $ IdeConfiguration mempty (hashed Nothing)

            putStrLn "\nStep 4/4: Type checking the files"
            setFilesOfInterest ide $ HashMap.fromList $ map ((,OnDisk) . toNormalizedFilePath') absoluteFiles
            results <- runAction "User TypeCheck" ide $ uses TypeCheck (map toNormalizedFilePath' absoluteFiles)
            _results <- runAction "GetHie" ide $ uses GetHieAst (map toNormalizedFilePath' absoluteFiles)
            _results <- runAction "GenerateCore" ide $ uses GenerateCore (map toNormalizedFilePath' absoluteFiles)
            let (worked, failed) = partition fst $ zip (map isJust results) absoluteFiles
            when (failed /= []) $
                putStr $ unlines $ "Files that failed:" : map ((++) " * " . snd) failed

            let nfiles xs = let n' = length xs in if n' == 1 then "1 file" else show n' ++ " files"
            putStrLn $ "\nCompleted (" ++ nfiles worked ++ " worked, " ++ nfiles failed ++ " failed)"

            unless (null failed) (exitWith $ ExitFailure (length failed))
        Db opts cmd -> do
            root <-  maybe IO.getCurrentDirectory return argsProjectRoot
            dbLoc <- getHieDbLoc root
            hPutStrLn stderr $ "Using hiedb at: " ++ dbLoc
            mlibdir <- setInitialDynFlags (cmapWithPrio LogSession recorder) root def
            rng <- newStdGen
            case mlibdir of
                Nothing     -> exitWith $ ExitFailure 1
                Just libdir -> retryOnSqliteBusy (cmapWithPrio LogSession recorder) rng (HieDb.runCommand libdir opts{HieDb.database = dbLoc} cmd)

        Custom (IdeCommand c) -> do
          root <-  maybe IO.getCurrentDirectory return argsProjectRoot
          dbLoc <- getHieDbLoc root
          runWithDb (cmapWithPrio LogSession recorder) dbLoc $ \hiedb hieChan -> do
            sessionLoader <- loadSessionWithOptions (cmapWithPrio LogSession recorder) argsSessionLoadingOptions "."
            let def_options = argsIdeOptions argsDefaultHlsConfig sessionLoader
                ideOptions = def_options
                    { optCheckParents = pure NeverCheck
                    , optCheckProject = pure False
                    , optModifyDynFlags = optModifyDynFlags def_options <> pluginModifyDynflags plugins
                    }
            ide <- initialise (cmapWithPrio LogService recorder) argsDefaultHlsConfig argsHlsPlugins rules Nothing logger debouncer ideOptions hiedb hieChan mempty
            shakeSessionInit (cmapWithPrio LogShake recorder) ide
            registerIdeConfiguration (shakeExtras ide) $ IdeConfiguration mempty (hashed Nothing)
            c ide

expandFiles :: [FilePath] -> IO [FilePath]
expandFiles = concatMapM $ \x -> do
    b <- IO.doesFileExist x
    if b
        then return [x]
        else do
            let recurse "." = True
                recurse y | "." `isPrefixOf` takeFileName y = False -- skip .git etc
                recurse y = takeFileName y `notElem` ["dist", "dist-newstyle"] -- cabal directories
            files <- filter (\y -> takeExtension y `elem` [".hs", ".lhs"]) <$> IO.listFilesInside (return . recurse) x
            when (null files) $
                fail $ "Couldn't find any .hs/.lhs files inside directory: " ++ x
            return files
