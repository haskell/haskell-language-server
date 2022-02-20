{-# LANGUAGE PackageImports #-}
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
import           Control.Concurrent.Extra              (withNumCapabilities)
import           Control.Concurrent.STM.Stats          (atomically,
                                                        dumpSTMStats)
import           Control.Exception.Safe                (SomeException, catchAny,
                                                        displayException)
import           Control.Monad.Extra                   (concatMapM, unless,
                                                        when)
import qualified Data.Aeson.Encode.Pretty              as A
import           Data.Default                          (Default (def))
import           Data.Foldable                         (traverse_)
import qualified Data.HashMap.Strict                   as HashMap
import           Data.Hashable                         (hashed)
import           Data.List.Extra                       (intercalate, isPrefixOf,
                                                        nub, nubOrd, partition)
import           Data.Maybe                            (catMaybes, isJust)
import qualified Data.Text                             as T
import           Data.Text.Lazy.Encoding               (decodeUtf8)
import qualified Data.Text.Lazy.IO                     as LT
import           Data.Typeable                         (typeOf)
import           Development.IDE                       (Action, GhcVersion (..),
                                                        Priority (Debug), Rules,
                                                        ghcVersion,
                                                        hDuplicateTo')
import           Development.IDE.Core.Debouncer        (Debouncer,
                                                        newAsyncDebouncer)
import           Development.IDE.Core.FileStore        (isWatchSupported,
                                                        makeVFSHandle)
import           Development.IDE.Core.IdeConfiguration (IdeConfiguration (..),
                                                        registerIdeConfiguration)
import           Development.IDE.Core.OfInterest       (FileOfInterestStatus (OnDisk),
                                                        kick,
                                                        setFilesOfInterest)
import           Development.IDE.Core.RuleTypes        (GenerateCore (GenerateCore),
                                                        GetHieAst (GetHieAst),
                                                        GhcSession (GhcSession),
                                                        GhcSessionDeps (GhcSessionDeps),
                                                        TypeCheck (TypeCheck))
import           Development.IDE.Core.Rules            (GhcSessionIO (GhcSessionIO),
                                                        mainRule)
import qualified Development.IDE.Core.Rules            as Rules
import           Development.IDE.Core.Service          (initialise, runAction)
import qualified Development.IDE.Core.Service          as Service
import           Development.IDE.Core.Shake            (IdeState (shakeExtras),
                                                        ShakeExtras (state),
                                                        shakeSessionInit, uses)
import qualified Development.IDE.Core.Shake            as Shake
import           Development.IDE.Core.Tracing          (measureMemory)
import           Development.IDE.Graph                 (action)
import           Development.IDE.LSP.LanguageServer    (runLanguageServer)
import qualified Development.IDE.LSP.LanguageServer    as LanguageServer
import           Development.IDE.Main.HeapStats        (withHeapStats)
import qualified Development.IDE.Main.HeapStats        as HeapStats
import           Development.IDE.Plugin                (Plugin (pluginHandlers, pluginModifyDynflags, pluginRules))
import           Development.IDE.Plugin.HLS            (asGhcIdePlugin)
import qualified Development.IDE.Plugin.HLS            as PluginHLS
import qualified Development.IDE.Plugin.HLS.GhcIde     as GhcIde
import qualified Development.IDE.Plugin.Test           as Test
import           Development.IDE.Session               (SessionLoadingOptions,
                                                        getHieDbLoc,
                                                        loadSessionWithOptions,
                                                        retryOnSqliteBusy,
                                                        runWithDb,
                                                        setInitialDynFlags)
import qualified Development.IDE.Session               as Session
import           Development.IDE.Types.Location        (NormalizedUri,
                                                        toNormalizedFilePath')
import           Development.IDE.Types.Logger          (Logger, Pretty (pretty),
                                                        Priority (Info, Warning),
                                                        Recorder, WithPriority,
                                                        cmapWithPrio, logWith,
                                                        vsep, (<+>))
import           Development.IDE.Types.Options         (IdeGhcSession,
                                                        IdeOptions (optCheckParents, optCheckProject, optReportProgress, optRunSubset),
                                                        IdeTesting (IdeTesting),
                                                        clientSupportsProgress,
                                                        defaultIdeOptions,
                                                        optModifyDynFlags,
                                                        optTesting)
import           Development.IDE.Types.Shake           (fromKeyType)
import           GHC.Conc                              (getNumProcessors)
import           GHC.IO.Encoding                       (setLocaleEncoding)
import           GHC.IO.Handle                         (hDuplicate)
import           HIE.Bios.Cradle                       (findCradle)
import qualified HieDb.Run                             as HieDb
import           Ide.Plugin.Config                     (CheckParents (NeverCheck),
                                                        Config, checkParents,
                                                        checkProject,
                                                        getConfigFromNotification)
import           Ide.Plugin.ConfigUtils                (pluginsToDefaultConfig,
                                                        pluginsToVSCodeExtensionSchema)
import           Ide.PluginUtils                       (allLspCmdIds',
                                                        getProcessID,
                                                        idePluginsToPluginDesc,
                                                        pluginDescToIdePlugins)
import           Ide.Types                             (IdeCommand (IdeCommand),
                                                        IdePlugins,
                                                        PluginDescriptor (PluginDescriptor, pluginCli),
                                                        PluginId (PluginId),
                                                        ipMap)
import qualified Language.LSP.Server                   as LSP
import qualified "list-t" ListT
import           Numeric.Natural                       (Natural)
import           Options.Applicative                   hiding (action)
import qualified StmContainers.Map                     as STM
import qualified System.Directory.Extra                as IO
import           System.Exit                           (ExitCode (ExitFailure),
                                                        exitWith)
import           System.FilePath                       (takeExtension,
                                                        takeFileName)
import           System.IO                             (BufferMode (LineBuffering, NoBuffering),
                                                        Handle, hFlush,
                                                        hPutStrLn,
                                                        hSetBuffering,
                                                        hSetEncoding, stderr,
                                                        stdin, stdout, utf8)
import           System.Random                         (newStdGen)
import           System.Time.Extra                     (Seconds, offsetTime,
                                                        showDuration)
import           Text.Printf                           (printf)

data Log
  = LogHeapStats !HeapStats.Log
  | LogLspStart
  | LogLspStartDuration !Seconds
  | LogShouldRunSubset !Bool
  | LogOnlyPartialGhc9Support
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
    LogHeapStats log -> pretty log
    LogLspStart ->
      vsep
        [ "Staring LSP server..."
        , "If you are seeing this in a terminal, you probably should have run WITHOUT the --lsp option!"]
    LogLspStartDuration duration ->
      "Started LSP server in" <+> pretty (showDuration duration)
    LogShouldRunSubset shouldRunSubset ->
      "shouldRunSubset:" <+> pretty shouldRunSubset
    LogOnlyPartialGhc9Support ->
      "Currently, HLS supports GHC 9 only partially. See [issue #297](https://github.com/haskell/haskell-language-server/issues/297) for more detail."
    LogSetInitialDynFlagsException e ->
      "setInitialDynFlags:" <+> pretty (displayException e)
    LogService log -> pretty log
    LogShake log -> pretty log
    LogGhcIde log -> pretty log
    LogLanguageServer log -> pretty log
    LogSession log -> pretty log
    LogPluginHLS log -> pretty log
    LogRules log -> pretty log

data Command
    = Check [FilePath]  -- ^ Typecheck some paths and print diagnostics. Exit code is the number of failures
    | Db {hieOptions ::  HieDb.Options, hieCommand :: HieDb.Command}
     -- ^ Run a command in the hiedb
    | LSP   -- ^ Run the LSP server
    | PrintExtensionSchema
    | PrintDefaultConfig
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
            <> command "vscode-extension-schema" extensionSchemaCommand
            <> command "generate-default-config" generateDefaultConfigCommand
            <> pluginCommands
            )
  where
    fileCmd = many (argument str (metavar "FILES/DIRS..."))
    lspInfo = fullDesc <> progDesc "Start talking to an LSP client"
    fileInfo = fullDesc <> progDesc "Used as a test bed to check your IDE will work"
    hieInfo = fullDesc <> progDesc "Query .hie files"
    extensionSchemaCommand =
        info (pure PrintExtensionSchema)
             (fullDesc <> progDesc "Print generic config schema for plugins (used in the package.json of haskell vscode extension)")
    generateDefaultConfigCommand =
        info (pure PrintDefaultConfig)
             (fullDesc <> progDesc "Print config supported by the server with default values")

    pluginCommands = mconcat
        [ command (T.unpack pId) (Custom <$> p)
        | (PluginId pId, PluginDescriptor{pluginCli = Just p}) <- ipMap plugins
        ]


data Arguments = Arguments
    { argsProjectRoot           :: Maybe FilePath
    , argsOTMemoryProfiling     :: Bool
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
    }


defaultArguments :: Recorder (WithPriority Log) -> Logger -> Arguments
defaultArguments recorder logger = Arguments
        { argsProjectRoot = Nothing
        , argsOTMemoryProfiling = False
        , argCommand = LSP
        , argsLogger = pure logger
        , argsRules = mainRule (cmapWithPrio LogRules recorder) def >> action kick
        , argsGhcidePlugin = mempty
        , argsHlsPlugins = pluginDescToIdePlugins (GhcIde.descriptors (cmapWithPrio LogGhcIde recorder))
        , argsSessionLoadingOptions = def
        , argsIdeOptions = \config ghcSession -> (defaultIdeOptions ghcSession)
            { optCheckProject = pure $ checkProject config
            , optCheckParents = pure $ checkParents config
            }
        , argsLspOptions = def {LSP.completionTriggerCharacters = Just "."}
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
        }


testing :: Recorder (WithPriority Log) -> Logger -> Arguments
testing recorder logger =
  let
    arguments@Arguments{ argsHlsPlugins, argsIdeOptions } = defaultArguments recorder logger
    hlsPlugins = pluginDescToIdePlugins $
      idePluginsToPluginDesc argsHlsPlugins
      ++ [Test.blockCommandDescriptor "block-command", Test.plugin]
    ideOptions = \config sessionLoader ->
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
  log :: Priority -> Log -> IO ()
  log = logWith recorder

  fun = do
    setLocaleEncoding utf8
    pid <- T.pack . show <$> getProcessID
    logger <- argsLogger
    hSetBuffering stderr LineBuffering

    let hlsPlugin = asGhcIdePlugin (cmapWithPrio LogPluginHLS recorder) argsHlsPlugins
        hlsCommands = allLspCmdIds' pid argsHlsPlugins
        plugins = hlsPlugin <> argsGhcidePlugin
        options = argsLspOptions { LSP.executeCommandCommands = LSP.executeCommandCommands argsLspOptions <> Just hlsCommands }
        argsOnConfigChange = getConfigFromNotification
        rules = argsRules >> pluginRules plugins

    debouncer <- argsDebouncer
    inH <- argsHandleIn
    outH <- argsHandleOut

    numProcessors <- getNumProcessors

    case argCommand of
        PrintExtensionSchema ->
            LT.putStrLn $ decodeUtf8 $ A.encodePretty $ pluginsToVSCodeExtensionSchema argsHlsPlugins
        PrintDefaultConfig ->
            LT.putStrLn $ decodeUtf8 $ A.encodePretty $ pluginsToDefaultConfig argsHlsPlugins
        LSP -> withNumCapabilities (maybe (numProcessors `div` 2) fromIntegral argsThreads) $ do
            t <- offsetTime
            log Info LogLspStart

            runLanguageServer (cmapWithPrio LogLanguageServer recorder) options inH outH argsGetHieDbLoc argsDefaultHlsConfig argsOnConfigChange (pluginHandlers plugins) $ \env vfs rootPath withHieDb hieChan -> do
                traverse_ IO.setCurrentDirectory rootPath
                t <- t
                log Info $ LogLspStartDuration t

                dir <- maybe IO.getCurrentDirectory return rootPath

                -- We want to set the global DynFlags right now, so that we can use
                -- `unsafeGlobalDynFlags` even before the project is configured
                _mlibdir <-
                    setInitialDynFlags (cmapWithPrio LogSession recorder) dir argsSessionLoadingOptions
                        -- TODO: should probably catch/log/rethrow at top level instead
                        `catchAny` (\e -> log Debug (LogSetInitialDynFlagsException e) >> pure Nothing)

                sessionLoader <- loadSessionWithOptions (cmapWithPrio LogSession recorder) argsSessionLoadingOptions dir
                config <- LSP.runLspT env LSP.getConfig
                let def_options = argsIdeOptions config sessionLoader

                -- disable runSubset if the client doesn't support watched files
                runSubset <- (optRunSubset def_options &&) <$> LSP.runLspT env isWatchSupported
                log Debug $ LogShouldRunSubset runSubset

                let options = def_options
                            { optReportProgress = clientSupportsProgress caps
                            , optModifyDynFlags = optModifyDynFlags def_options <> pluginModifyDynflags plugins
                            , optRunSubset = runSubset
                            }
                    caps = LSP.resClientCapabilities env
                -- FIXME: Remove this after GHC 9 gets fully supported
                when (ghcVersion == GHC90) $
                    log Warning LogOnlyPartialGhc9Support
                initialise
                    (cmapWithPrio LogService recorder)
                    argsDefaultHlsConfig
                    rules
                    (Just env)
                    logger
                    debouncer
                    options
                    vfs
                    withHieDb
                    hieChan
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
            files <- nubOrd <$> mapM IO.canonicalizePath files
            putStrLn $ "Found " ++ show (length files) ++ " files"

            putStrLn "\nStep 2/4: Looking for hie.yaml files that control setup"
            cradles <- mapM findCradle files
            let ucradles = nubOrd cradles
            let n = length ucradles
            putStrLn $ "Found " ++ show n ++ " cradle" ++ ['s' | n /= 1]
            when (n > 0) $ putStrLn $ "  (" ++ intercalate ", " (catMaybes ucradles) ++ ")"
            putStrLn "\nStep 3/4: Initializing the IDE"
            vfs <- makeVFSHandle
            sessionLoader <- loadSessionWithOptions (cmapWithPrio LogSession recorder) argsSessionLoadingOptions dir
            let def_options = argsIdeOptions argsDefaultHlsConfig sessionLoader
                options = def_options
                        { optCheckParents = pure NeverCheck
                        , optCheckProject = pure False
                        , optModifyDynFlags = optModifyDynFlags def_options <> pluginModifyDynflags plugins
                        }
            ide <- initialise (cmapWithPrio LogService recorder) argsDefaultHlsConfig rules Nothing logger debouncer options vfs hiedb hieChan
            shakeSessionInit (cmapWithPrio LogShake recorder) ide
            registerIdeConfiguration (shakeExtras ide) $ IdeConfiguration mempty (hashed Nothing)

            putStrLn "\nStep 4/4: Type checking the files"
            setFilesOfInterest ide $ HashMap.fromList $ map ((,OnDisk) . toNormalizedFilePath') files
            results <- runAction "User TypeCheck" ide $ uses TypeCheck (map toNormalizedFilePath' files)
            _results <- runAction "GetHie" ide $ uses GetHieAst (map toNormalizedFilePath' files)
            _results <- runAction "GenerateCore" ide $ uses GenerateCore (map toNormalizedFilePath' files)
            let (worked, failed) = partition fst $ zip (map isJust results) files
            when (failed /= []) $
                putStr $ unlines $ "Files that failed:" : map ((++) " * " . snd) failed

            let nfiles xs = let n = length xs in if n == 1 then "1 file" else show n ++ " files"
            putStrLn $ "\nCompleted (" ++ nfiles worked ++ " worked, " ++ nfiles failed ++ " failed)"

            when argsOTMemoryProfiling $ do
                let values = state $ shakeExtras ide
                let consoleObserver Nothing = return $ \size -> printf "Total: %.2fMB\n" (fromIntegral @Int @Double size / 1e6)
                    consoleObserver (Just k) = return $ \size -> printf "  - %s: %.2fKB\n" (show k) (fromIntegral @Int @Double size / 1e3)

                stateContents <- atomically $ ListT.toList $ STM.listT values
                printf "# Shake value store contents(%d):\n" (length stateContents)
                let keys =
                        nub $
                            typeOf GhcSession :
                            typeOf GhcSessionDeps :
                            [kty | (fromKeyType -> Just (kty,_), _) <- stateContents, kty /= typeOf GhcSessionIO] ++
                            [typeOf GhcSessionIO]
                measureMemory logger [keys] consoleObserver values

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
            vfs <- makeVFSHandle
            sessionLoader <- loadSessionWithOptions (cmapWithPrio LogSession recorder) argsSessionLoadingOptions "."
            let def_options = argsIdeOptions argsDefaultHlsConfig sessionLoader
                options = def_options
                    { optCheckParents = pure NeverCheck
                    , optCheckProject = pure False
                    , optModifyDynFlags = optModifyDynFlags def_options <> pluginModifyDynflags plugins
                    }
            ide <- initialise (cmapWithPrio LogService recorder) argsDefaultHlsConfig rules Nothing logger debouncer options vfs hiedb hieChan
            shakeSessionInit (cmapWithPrio LogShake recorder) ide
            registerIdeConfiguration (shakeExtras ide) $ IdeConfiguration mempty (hashed Nothing)
            c ide

{-# ANN defaultMain ("HLint: ignore Use nubOrd" :: String) #-}


expandFiles :: [FilePath] -> IO [FilePath]
expandFiles = concatMapM $ \x -> do
    b <- IO.doesFileExist x
    if b
        then return [x]
        else do
            let recurse "." = True
                recurse x | "." `isPrefixOf` takeFileName x = False -- skip .git etc
                recurse x = takeFileName x `notElem` ["dist", "dist-newstyle"] -- cabal directories
            files <- filter (\x -> takeExtension x `elem` [".hs", ".lhs"]) <$> IO.listFilesInside (return . recurse) x
            when (null files) $
                fail $ "Couldn't find any .hs/.lhs files inside directory: " ++ x
            return files
