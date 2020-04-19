{-# LANGUAGE CPP, OverloadedStrings, NamedFieldPuns #-}
module TestUtils
  (
    withFileLogging
  , setupBuildToolFiles
  -- , testCommand
  -- , runSingle
  -- , runSingle'
  -- , runSingleReq
  -- , makeRequest
  -- , runIGM
  -- , runIGM'
  , ghcVersion, GhcVersion(..)
  , logFilePath
  , hieCommand
  , hieCommandVomit
  , hieCommandExamplePlugin
  , getHspecFormattedConfig
  -- , testOptions
  , flushStackEnvironment
  , dummyLspFuncs
  )
where

-- import           Control.Concurrent.STM
import           Control.Monad
import           Data.Aeson.Types (typeMismatch)
import           Data.Default
import           Data.List (intercalate)
import           Data.Text (pack)
-- import           Data.Typeable
import           Data.Yaml
-- import qualified Data.Map as Map
import           Data.Maybe
import           Language.Haskell.LSP.Core
import           Language.Haskell.LSP.Types
-- import           Haskell.Ide.Engine.MonadTypes hiding (withProgress, withIndefiniteProgress)
-- import qualified Ide.Cradle as Bios
-- import qualified Ide.Engine.Config as Config
import           System.Directory
import           System.Environment
import           System.FilePath
import qualified System.Log.Logger as L
-- import           Test.Hspec
import           Test.Hspec.Runner
import           Test.Hspec.Core.Formatters
import           Text.Blaze.Renderer.String (renderMarkup)
import           Text.Blaze.Internal
-- import qualified Haskell.Ide.Engine.PluginApi as HIE (BiosOptions, defaultOptions)

-- import HIE.Bios.Types

-- testOptions :: HIE.BiosOptions
-- testOptions = HIE.defaultOptions { cradleOptsVerbosity = Verbose }

-- ---------------------------------------------------------------------


-- testCommand :: (ToJSON a, Typeable b, ToJSON b, Show b, Eq b)
--             => IdePlugins -> FilePath -> IdeGhcM (IdeResult b) -> PluginId -> CommandId -> a -> IdeResult b -> IO ()
-- testCommand testPlugins fp act plugin cmd arg res = do
--   flushStackEnvironment
--   (newApiRes, oldApiRes) <- runIGM testPlugins fp $ do
--     new <- act
--     old <- makeRequest plugin cmd arg
--     return (new, old)
--   newApiRes `shouldBe` res
--   fmap fromDynJSON oldApiRes `shouldBe` fmap Just res

-- runSingle :: IdePlugins -> FilePath -> IdeGhcM (IdeResult b) -> IO (IdeResult b)
-- runSingle = runSingle' id

-- runSingle' :: (Config.Config -> Config.Config) -> IdePlugins -> FilePath -> IdeGhcM (IdeResult b) -> IO (IdeResult b)
-- runSingle' modifyConfig testPlugins fp act = runIGM' modifyConfig testPlugins fp act

-- runSingleReq :: ToJSON a
--              => IdePlugins -> FilePath -> PluginId -> CommandId -> a -> IO (IdeResult DynamicJSON)
-- runSingleReq testPlugins fp plugin com arg = runIGM testPlugins fp (makeRequest plugin com arg)

-- makeRequest :: ToJSON a => PluginId -> CommandId -> a -> IdeGhcM (IdeResult DynamicJSON)
-- makeRequest plugin com arg = runPluginCommand plugin com (toJSON arg)

-- runIGM :: IdePlugins -> FilePath -> IdeGhcM a -> IO a
-- runIGM = runIGM' id

-- runIGM' :: (Config.Config -> Config.Config) -> IdePlugins -> FilePath -> IdeGhcM a -> IO a
-- runIGM' modifyConfig testPlugins fp f = do
--   stateVar <- newTVarIO $ IdeState emptyModuleCache Map.empty Map.empty Nothing
--   crdl <- Bios.findLocalCradle fp
--   mlibdir <- Bios.getProjectGhcLibDir crdl
--   let tmpFuncs :: LspFuncs Config.Config
--       tmpFuncs = dummyLspFuncs
--       lspFuncs :: LspFuncs Config.Config
--       lspFuncs = tmpFuncs { config = (fmap . fmap) modifyConfig (config tmpFuncs)}
--   runIdeGhcM mlibdir testPlugins lspFuncs stateVar f

withFileLogging :: FilePath -> IO a -> IO a
withFileLogging logFile f = do
  let logDir = "./test-logs"
      logPath = logDir </> logFile

  dirExists <- doesDirectoryExist logDir
  unless dirExists $ createDirectory logDir

  exists <- doesFileExist logPath
  when exists $ removeFile logPath

  setupLogger (Just logPath) ["hie"] L.DEBUG

  f

-- ---------------------------------------------------------------------

setupBuildToolFiles :: IO ()
setupBuildToolFiles = do
  forM_ files setupDirectFilesIn

setupDirectFilesIn :: FilePath -> IO ()
setupDirectFilesIn f =
  writeFile (f ++ "hie.yaml") hieYamlCradleDirectContents


-- ---------------------------------------------------------------------

files :: [FilePath]
files =
  [  "./test/testdata/"
   -- , "./test/testdata/addPackageTest/cabal-exe/"
   -- , "./test/testdata/addPackageTest/hpack-exe/"
   -- , "./test/testdata/addPackageTest/cabal-lib/"
   -- , "./test/testdata/addPackageTest/hpack-lib/"
   -- , "./test/testdata/addPragmas/"
   -- , "./test/testdata/badProjects/cabal/"
   -- , "./test/testdata/completion/"
   -- , "./test/testdata/definition/"
   -- , "./test/testdata/gototest/"
   -- , "./test/testdata/redundantImportTest/"
   -- , "./test/testdata/wErrorTest/"
  ]

data GhcVersion
  = GHC88
  | GHC86
  | GHC84
  deriving (Eq,Show)

ghcVersion :: GhcVersion
#if (defined(MIN_VERSION_GLASGOW_HASKELL) && (MIN_VERSION_GLASGOW_HASKELL(8,8,0,0)))
ghcVersion = GHC88
#elif (defined(MIN_VERSION_GLASGOW_HASKELL) && (MIN_VERSION_GLASGOW_HASKELL(8,6,0,0)))
ghcVersion = GHC86
#elif (defined(MIN_VERSION_GLASGOW_HASKELL) && (MIN_VERSION_GLASGOW_HASKELL(8,4,0,0)))
ghcVersion = GHC84
#endif

logFilePath :: String
logFilePath = "hie-" ++ show ghcVersion ++ ".log"

-- | The command to execute the version of hie for the current compiler.
--
-- Both @stack test@ and @cabal new-test@ setup the environment so @hie@ is
-- on PATH. Cabal seems to respond to @build-tool-depends@ specifically while
-- stack just puts all project executables on PATH.
hieCommand :: String
-- hieCommand = "hie --lsp --bios-verbose -d -l test-logs/" ++ logFilePath
-- hieCommand = "haskell-language-server --lsp"
-- hieCommand = "haskell-language-server --lsp --test --shake-profiling=test-logs/" ++ logFilePath
hieCommand = "haskell-language-server --lsp -d -l test-logs/" ++ logFilePath

hieCommandVomit :: String
hieCommandVomit = hieCommand ++ " --vomit"

hieCommandExamplePlugin :: String
hieCommandExamplePlugin = hieCommand ++ " --example"

-- ---------------------------------------------------------------------

hieYamlCradleDirectContents :: String
hieYamlCradleDirectContents = unlines
  [ "# WARNING: THIS FILE IS AUTOGENERATED IN test/utils/TestUtils.hs. IT WILL BE OVERWRITTEN ON EVERY TEST RUN"
  , "cradle:"
  , "  direct:"
  , "    arguments:"
  , "      - -i."
  ]


-- ---------------------------------------------------------------------

getHspecFormattedConfig :: String -> IO Config
getHspecFormattedConfig name = do
  -- https://circleci.com/docs/2.0/env-vars/#built-in-environment-variables
  isCI <- isJust <$> lookupEnv "CI"

  -- Only use the xml formatter on CI since it hides console output
  if isCI
    then do
      let subdir = "test-results" </> name
      createDirectoryIfMissing True subdir

      return $ defaultConfig { configFormatter = Just xmlFormatter
                             , configOutputFile = Right $ subdir </> "results.xml"
                             }
    else return defaultConfig

-- | A Hspec formatter for CircleCI.
-- Originally from https://github.com/LeastAuthority/hspec-jenkins
xmlFormatter :: Formatter
xmlFormatter = silent {
    headerFormatter = do
      writeLine "<?xml version='1.0' encoding='UTF-8'?>"
      writeLine "<testsuite>"
  , exampleSucceeded
  , exampleFailed
  , examplePending
  , footerFormatter = writeLine "</testsuite>"
  }
  where

#if MIN_VERSION_hspec(2,5,0)
    exampleSucceeded path _ =
#else
    exampleSucceeded path =
#endif
      writeLine $ renderMarkup $ testcase path ""

#if MIN_VERSION_hspec(2,5,0)
    exampleFailed path _ err =
#else
    exampleFailed path (Left err) =
      writeLine $ renderMarkup $ testcase path $
        failure ! message (show err) $ ""
    exampleFailed path (Right err) =
#endif
      writeLine $ renderMarkup $ testcase path $
        failure ! message (reasonAsString err) $ ""

#if MIN_VERSION_hspec(2,5,0)
    examplePending path _ reason =
#else
    examplePending path reason =
#endif
      writeLine $ renderMarkup $ testcase path $
        case reason of
          Just desc -> skipped ! message desc  $ ""
          Nothing -> skipped ""

    failure, skipped :: Markup -> Markup
    failure = customParent "failure"
    skipped = customParent "skipped"

    name, className, message :: String -> Attribute
    name = customAttribute "name" . stringValue
    className = customAttribute "classname" . stringValue
    message = customAttribute "message" . stringValue

    testcase :: Path -> Markup -> Markup
    testcase (xs,x) = customParent "testcase" ! name x ! className (intercalate "." xs)

    reasonAsString :: FailureReason -> String
    reasonAsString NoReason = "no reason given"
    reasonAsString (Reason x) = x
    reasonAsString (ExpectedButGot Nothing expected got) = "Expected " ++ expected ++ " but got " ++ got
    reasonAsString (ExpectedButGot (Just src) expected got) = src ++ " expected " ++ expected ++ " but got " ++ got
#if MIN_VERSION_hspec(2,5,0)
    reasonAsString (Error Nothing err ) = show err
    reasonAsString (Error (Just s) err) = s ++ show err
#endif

-- ---------------------------------------------------------------------

flushStackEnvironment :: IO ()
flushStackEnvironment = do
  -- We need to clear these environment variables to prevent
  -- collisions with stack usages
  -- See https://github.com/commercialhaskell/stack/issues/4875
  unsetEnv "GHC_PACKAGE_PATH"
  unsetEnv "GHC_ENVIRONMENT"
  unsetEnv "HASKELL_PACKAGE_SANDBOX"
  unsetEnv "HASKELL_PACKAGE_SANDBOXES"

-- ---------------------------------------------------------------------

dummyLspFuncs :: Default a => LspFuncs a
dummyLspFuncs = LspFuncs { clientCapabilities = def
                         , config = return (Just def)
                         , sendFunc = const (return ())
                         , getVirtualFileFunc = const (return Nothing)
                         , persistVirtualFileFunc = \uri -> return (uriToFilePath (fromNormalizedUri uri))
                         , reverseFileMapFunc = return id
                         , publishDiagnosticsFunc = mempty
                         , flushDiagnosticsBySourceFunc = mempty
                         , getNextReqId = pure (IdInt 0)
                         , rootPath = Nothing
                         , getWorkspaceFolders = return Nothing
                         , withProgress = \_ _ f -> f (const (return ()))
                         , withIndefiniteProgress = \_ _ f -> f
                         }
