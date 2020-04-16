module HlsInstall where

import           Options.Applicative
import           Control.Monad
import           System.Environment                       ( unsetEnv )
import           System.Process
import           System.Environment

import           BuildSystem
import           Stack
import           Cabal
import           Version
import           Env
import           Help

defaultMain :: IO ()
defaultMain = do
  -- unset GHC_PACKAGE_PATH for cabal
  unsetEnv "GHC_PACKAGE_PATH"

  -- used for cabal-based targets
  ghcPaths <- findInstalledGhcs
  let cabalVersions = map fst ghcPaths

  -- used for stack-based targets
  stackVersions <- getHlsVersions

  let versions = if isRunFromStack then stackVersions else cabalVersions
  let toolsVersions = BuildableVersions stackVersions cabalVersions
  let latestVersion = last versions

  let showOptions :: String -> IO ()
      showOptions verb = do
        putStrLn $ "Options:"
        putStrLn $ "    Verbosity level: " ++ verb
  -- general purpose targets
  let submodules = updateSubmodules
  let shortHelp = shortHelpMessage
  let help = helpMessage toolsVersions

  let checkInstalledTool verb = if isRunFromStack then checkStack [verb] else checkCabal_ [verb]

  let buildData verb = do
        showOptions verb
        submodules
        checkInstalledTool verb
        if isRunFromStack then stackBuildData [verb] else cabalBuildData [verb]

  let installVersion verb version = do
        showOptions verb
        submodules
        checkInstalledTool verb
        if isRunFromStack then
          stackInstallHlsWithErrMsg (Just version) [verb]
        else
          cabalInstallHls version [verb]

  let installLatest verb = installVersion verb latestVersion
  let installLatestAndData verb = buildData verb >> installLatest verb

  let installNightly verb = do
        showOptions verb
        stackInstallHlsWithErrMsg Nothing [verb]

  -- cabal specific targets
  let ghcs = do
        -- It throws an error if there is no ghc in $PATH
        checkInstalledGhcs ghcPaths
        showInstalledGhcs ghcPaths

  let icuMacosFixInstall = callProcess "brew" ["install", "icu4c"]
  let icuMacosFixBuild verb =  mapM_ (flip buildIcuMacosFix [verb]) versions

  -- macos specific targets
  let icuMacosFix verb = do
        showOptions verb
        icuMacosFixBuild verb
        icuMacosFixInstall

  args <- getArgs
  case args of
    [] -> shortHelp
    _ -> do
      Options {..} <- execParser (info (optionParser versions) mempty)
      let verb = getVerbosity verbosity
      case cmd of
        IcuMacosFix -> icuMacosFix verb
        ShowInstalledGhcs -> ghcs
        InstallNightly -> installNightly verb
        InstallLatest -> installLatest verb
        InstallLatestWithData -> installLatestAndData verb
        InstallVersion version -> installVersion verb verb
        BuildData -> buildData verb
        CheckInstalledTools -> checkInstalledTool verb
        ShortHelp -> shortHelp
        AllHelp -> help

buildIcuMacosFix :: VersionNumber -> [String] -> IO ()
buildIcuMacosFix version args = execStackWithGhc_
  version $
  [ "build"
  , "text-icu"
  , "--extra-lib-dirs=/usr/local/opt/icu4c/lib"
  , "--extra-include-dirs=/usr/local/opt/icu4c/include"
  ] ++ args

-- | update the submodules that the project is in the state as required by the `stack.yaml` files
updateSubmodules :: IO ()
updateSubmodules = do
  callProcess "git" ["submodule", "sync"]
  callProcess "git" ["submodule", "update", "--init"]

data Cmd
  = IcuMacosFix
  | ShowInstalledGhcs
  | InstallNightly
  | InstallLatest
  | InstallLatestWithData
  | InstallVersion String
  | BuildData
  | CheckInstalledTools
  | ShortHelp
  | AllHelp
  deriving (Show, Eq, Read)

data Verbosity
  = Silent
  | Normal
  | Verbose
  | Debug
  deriving (Show, Eq, Read)

data Options = Options
  { verbosity :: Verbosity
  , cmd :: Cmd
  }

optionParser :: [String] -> Parser Options
optionParser versions = Options
  <$> verbosityParser
  <*> cmdParser
  where
    verbosityParser :: Parser Verbosity
    verbosityParser =
        option (maybeReader parseVerbosity) (long "verbosity" <> short 'v' <> value Normal)

    parseVerbosity "silent" = Just Silent
    parseVerbosity "normal" = Just Normal
    parseVerbosity "verbose" = Just Verbose
    parseVerbosity "debug" = Just Debug
    parseVerbosity _ = Nothing

    cmdParser :: Parser Cmd
    cmdParser = subparser
      (  command "icu-macos-fix" (info (pure IcuMacosFix) mempty)
      <> command "ghcs" (info (pure ShowInstalledGhcs) mempty)
      <> command "dev" (info (pure InstallNightly) mempty)
      <> command "latest" (info (pure InstallLatest) mempty)
      <> command "hls" (info (pure InstallLatestWithData) mempty)
      <> mconcat
          (map (\v -> command ("hls-" ++ v) (info (pure $ InstallVersion v) mempty)) versions)
      <> command "data" (info (pure BuildData) mempty)
      <> command "check" (info (pure CheckInstalledTools) mempty)
      <> command "short-help" (info (pure ShortHelp) mempty)
      <> command "help" (info (pure AllHelp) mempty)
      )


getVerbosity :: Verbosity -> String
getVerbosity verb
  | isRunFromStack = "--verbosity=" ++ stackVerbosity verb
  | otherwise = "-v" ++ cabalVerbosity verb
  where
    cabalVerbosity = \case
      Silent -> "0"
      Normal -> "1"
      Verbose -> "2"
      Debug -> "3"
    stackVerbosity = \case
      Silent -> "silent"
      Normal -> "info"
      Verbose -> "warn"
      Debug -> "debug"
