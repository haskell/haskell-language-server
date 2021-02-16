module Development.IDE.Main (Arguments(..), defArguments, defaultMain) where
import Control.Concurrent.Extra (readVar)
import Control.Exception.Safe (
    Exception (displayException),
    catchAny,
 )
import Control.Monad.Extra (concatMapM, unless, when)
import Data.Default (Default (def))
import qualified Data.HashMap.Strict as HashMap
import Data.List.Extra (
    intercalate,
    isPrefixOf,
    nub,
    nubOrd,
    partition,
 )
import Data.Maybe (catMaybes, fromMaybe, isJust)
import qualified Data.Text as T
import Development.IDE (Action, Rules, noLogging)
import Development.IDE.Core.Debouncer (newAsyncDebouncer)
import Development.IDE.Core.FileStore (makeVFSHandle)
import Development.IDE.Core.OfInterest (
    FileOfInterestStatus (OnDisk),
    kick,
    setFilesOfInterest,
 )
import Development.IDE.Core.RuleTypes (
    GenerateCore (GenerateCore),
    GetHieAst (GetHieAst),
    GhcSession (GhcSession),
    GhcSessionDeps (GhcSessionDeps),
    TypeCheck (TypeCheck),
 )
import Development.IDE.Core.Rules (
    GhcSessionIO (GhcSessionIO),
    mainRule,
 )
import Development.IDE.Core.Service (initialise, runAction)
import Development.IDE.Core.Shake (
    IdeState (shakeExtras),
    ShakeExtras (state),
    uses,
 )
import Development.IDE.Core.Tracing (measureMemory)
import Development.IDE.LSP.LanguageServer (runLanguageServer)
import Development.IDE.Plugin (
    Plugin (pluginHandlers, pluginRules),
 )
import Development.IDE.Plugin.HLS (asGhcIdePlugin)
import Development.IDE.Session (SessionLoadingOptions, defaultLoadingOptions, loadSessionWithOptions, setInitialDynFlags, getHieDbLoc, runWithDb)
import Development.IDE.Types.Location (toNormalizedFilePath')
import Development.IDE.Types.Logger (Logger)
import Development.IDE.Types.Options (
    IdeGhcSession,
    IdeOptions (optCheckParents, optCheckProject, optReportProgress),
    clientSupportsProgress,
    defaultIdeOptions,
 )
import Development.IDE.Types.Shake (Key (Key))
import Development.Shake (action)
import HIE.Bios.Cradle (findCradle)
import Ide.Plugin.Config (CheckParents (NeverCheck), Config, getConfigFromNotification)
import Ide.PluginUtils (allLspCmdIds', getProcessID, pluginDescToIdePlugins)
import Ide.Types (IdePlugins)
import qualified Language.LSP.Server as LSP
import qualified System.Directory.Extra as IO
import System.Exit (ExitCode (ExitFailure), exitWith)
import System.FilePath (takeExtension, takeFileName)
import System.IO (hPutStrLn, hSetEncoding, stderr, stdout, utf8)
import System.Time.Extra (offsetTime, showDuration)
import Text.Printf (printf)
import qualified Development.IDE.Plugin.HLS.GhcIde as Ghcide

data Arguments = Arguments
    { argsOTMemoryProfiling :: Bool
    , argFiles :: Maybe [FilePath]   -- ^ Nothing: lsp server ;  Just: typecheck and exit
    , argsLogger :: Logger
    , argsRules :: Rules ()
    , argsHlsPlugins :: IdePlugins IdeState
    , argsGhcidePlugin :: Plugin Config  -- ^ Deprecated
    , argsSessionLoadingOptions :: SessionLoadingOptions
    , argsIdeOptions :: Maybe Config -> Action IdeGhcSession -> IdeOptions
    , argsLspOptions :: LSP.Options
    , argsDefaultHlsConfig :: Config
    , argsGetHieDbLoc :: FilePath -> IO FilePath -- ^ Map project roots to the location of the hiedb for the project
    }

defArguments :: Arguments
defArguments =
    Arguments
        { argsOTMemoryProfiling = False
        , argFiles = Nothing
        , argsLogger = noLogging
        , argsRules = mainRule >> action kick
        , argsGhcidePlugin = mempty
        , argsHlsPlugins = pluginDescToIdePlugins Ghcide.descriptors
        , argsSessionLoadingOptions = defaultLoadingOptions
        , argsIdeOptions = const defaultIdeOptions
        , argsLspOptions = def {LSP.completionTriggerCharacters = Just "."}
        , argsDefaultHlsConfig = def
        , argsGetHieDbLoc = getHieDbLoc
        }

defaultMain :: Arguments -> IO ()
defaultMain Arguments{..} = do
    pid <- T.pack . show <$> getProcessID

    let hlsPlugin = asGhcIdePlugin argsHlsPlugins
        hlsCommands = allLspCmdIds' pid argsHlsPlugins
        plugins = hlsPlugin <> argsGhcidePlugin
        options = argsLspOptions { LSP.executeCommandCommands = Just hlsCommands }
        argsOnConfigChange _ide = pure . getConfigFromNotification argsDefaultHlsConfig

    case argFiles of
        Nothing -> do
            t <- offsetTime
            hPutStrLn stderr "Starting LSP server..."
            hPutStrLn stderr "If you are seeing this in a terminal, you probably should have run ghcide WITHOUT the --lsp option!"
            runLanguageServer options argsGetHieDbLoc argsOnConfigChange (pluginHandlers plugins) $ \env vfs rootPath hiedb hieChan -> do
                t <- t
                hPutStrLn stderr $ "Started LSP server in " ++ showDuration t

                dir <- IO.getCurrentDirectory

                -- We want to set the global DynFlags right now, so that we can use
                -- `unsafeGlobalDynFlags` even before the project is configured
                -- We do it here since haskell-lsp changes our working directory to the correct place ('rootPath')
                -- before calling this function
                _mlibdir <-
                    setInitialDynFlags
                        `catchAny` (\e -> (hPutStrLn stderr $ "setInitialDynFlags: " ++ displayException e) >> pure Nothing)

                sessionLoader <- loadSessionWithOptions argsSessionLoadingOptions $ fromMaybe dir rootPath
                config <- LSP.runLspT env LSP.getConfig
                let options = (argsIdeOptions config sessionLoader)
                            { optReportProgress = clientSupportsProgress caps
                            }
                    rules = argsRules >> pluginRules plugins
                    caps = LSP.resClientCapabilities env
                debouncer <- newAsyncDebouncer
                initialise
                    rules
                    (Just env)
                    argsLogger
                    debouncer
                    options
                    vfs
                    hiedb
                    hieChan
        Just argFiles -> do
          dir <- IO.getCurrentDirectory
          dbLoc <- getHieDbLoc dir
          runWithDb dbLoc $ \hiedb hieChan -> do
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
            debouncer <- newAsyncDebouncer
            sessionLoader <- loadSessionWithOptions argsSessionLoadingOptions dir
            let options = (argsIdeOptions Nothing sessionLoader)
                        { optCheckParents = pure NeverCheck
                        , optCheckProject = pure False
                        }
            ide <- initialise mainRule Nothing argsLogger debouncer options vfs hiedb hieChan

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
                let valuesRef = state $ shakeExtras ide
                values <- readVar valuesRef
                let consoleObserver Nothing = return $ \size -> printf "Total: %.2fMB\n" (fromIntegral @Int @Double size / 1e6)
                    consoleObserver (Just k) = return $ \size -> printf "  - %s: %.2fKB\n" (show k) (fromIntegral @Int @Double size / 1e3)

                printf "# Shake value store contents(%d):\n" (length values)
                let keys =
                        nub $
                            Key GhcSession :
                            Key GhcSessionDeps :
                            [k | (_, k) <- HashMap.keys values, k /= Key GhcSessionIO]
                            ++ [Key GhcSessionIO]
                measureMemory argsLogger [keys] consoleObserver valuesRef

            unless (null failed) (exitWith $ ExitFailure (length failed))
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
