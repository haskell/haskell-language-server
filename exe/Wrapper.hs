{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE CPP #-}
-- | This module is based on the hie-wrapper.sh script in
-- https://github.com/alanz/vscode-hie-server
module Main where

import           Control.Monad.Extra
import           Data.Char  (isSpace)
import           Data.Default
import           Data.Foldable
import           Data.List
import           Data.Void
import qualified Development.IDE.Session as Session
import qualified HIE.Bios.Environment    as HieBios
import           HIE.Bios.Types
import           Ide.Arguments
import           Ide.Version
import           System.Directory
import           System.Environment
import           System.Exit
import           System.FilePath
import           System.IO
import           System.Info
#ifndef mingw32_HOST_OS
import           System.Posix.Process (executeFile)
import qualified Data.Map.Strict as Map
#else
import           System.Process
#endif

-- ---------------------------------------------------------------------

main :: IO ()
main = do
  -- WARNING: If you write to stdout before runLanguageServer
  --          then the language server will not work
  args <- getArguments "haskell-language-server-wrapper" mempty

  hlsVer <- haskellLanguageServerVersion
  case args of
      ProbeToolsMode -> do
          programsOfInterest <- findProgramVersions
          putStrLn hlsVer
          putStrLn "Tool versions found on the $PATH"
          putStrLn $ showProgramVersionOfInterest programsOfInterest

      VersionMode PrintVersion ->
          putStrLn hlsVer

      VersionMode PrintNumericVersion ->
          putStrLn haskellLanguageServerNumericVersion

      BiosMode PrintCradleType ->
          print =<< findProjectCradle
      PrintLibDir -> do
          cradle <- findProjectCradle' False
          (CradleSuccess libdir) <- HieBios.getRuntimeGhcLibDir cradle
          putStr libdir
      _ -> launchHaskellLanguageServer args

launchHaskellLanguageServer :: Arguments -> IO ()
launchHaskellLanguageServer parsedArgs = do
  case parsedArgs of
    Ghcide GhcideArguments{..} -> whenJust argsCwd setCurrentDirectory
    _                          -> pure ()

  d <- getCurrentDirectory

  -- search for the project cradle type
  cradle <- findProjectCradle

  -- Get the root directory from the cradle
  setCurrentDirectory $ cradleRootDir cradle

  case parsedArgs of
    Ghcide GhcideArguments{..} ->
      when argsProjectGhcVersion $ getRuntimeGhcVersion' cradle >>= putStrLn >> exitSuccess
    _ -> pure ()

  progName <- getProgName
  hPutStrLn stderr $ "Run entered for haskell-language-server-wrapper(" ++ progName ++ ") "
                     ++ hlsVersion
  hPutStrLn stderr $ "Current directory: " ++ d
  hPutStrLn stderr $ "Operating system: " ++ os
  args <- getArgs
  hPutStrLn stderr $ "Arguments: " ++ show args
  hPutStrLn stderr $ "Cradle directory: " ++ cradleRootDir cradle
  hPutStrLn stderr $ "Cradle type: " ++ show (actionName (cradleOptsProg cradle))
  programsOfInterest <- findProgramVersions
  hPutStrLn stderr ""
  hPutStrLn stderr "Tool versions found on the $PATH"
  hPutStrLn stderr $ showProgramVersionOfInterest programsOfInterest
  hPutStrLn stderr ""
  -- Get the ghc version -- this might fail!
  hPutStrLn stderr "Consulting the cradle to get project GHC version..."
  ghcVersion <- getRuntimeGhcVersion' cradle
  hPutStrLn stderr $ "Project GHC version: " ++ ghcVersion

  let
    hlsBin = "haskell-language-server-" ++ ghcVersion
    candidates' = [hlsBin, "haskell-language-server"]
    candidates = map (++ exeExtension) candidates'

  hPutStrLn stderr $ "haskell-language-server exe candidates: " ++ show candidates

  mexes <- traverse findExecutable candidates

  case asum mexes of
    Nothing -> die $ "Cannot find any haskell-language-server exe, looked for: " ++ intercalate ", " candidates
    Just e -> do
      hPutStrLn stderr $ "Launching haskell-language-server exe at:" ++ e
#ifdef mingw32_HOST_OS
      callProcess e args
#else
      let Cradle { cradleOptsProg = CradleAction { runGhcCmd } } = cradle
      -- we need to be compatible with NoImplicitPrelude
      ghcBinary <- (fmap trim <$> runGhcCmd ["-v0", "-package-env=-", "-e", "do e <- System.Environment.getExecutablePath ; System.IO.putStr e"])
        >>= cradleResult "Failed to get project GHC executable path"
      libdir <- HieBios.getRuntimeGhcLibDir cradle
        >>= cradleResult "Failed to get project GHC libdir path"
      env <- Map.fromList <$> getEnvironment
      let newEnv = Map.insert "GHC_BIN" ghcBinary $ Map.insert "GHC_LIBDIR" libdir env
      executeFile e True args (Just (Map.toList newEnv))
#endif


cradleResult :: String -> CradleLoadResult a -> IO a
cradleResult _ (CradleSuccess a) = pure a
cradleResult str (CradleFail e) = die $ str ++ ": " ++ show e
cradleResult str CradleNone = die $ str ++ ": no cradle"

-- | Version of 'getRuntimeGhcVersion' that dies if we can't get it, and also
-- checks to see if the tool is missing if it is one of
getRuntimeGhcVersion' :: Show a => Cradle a -> IO String
getRuntimeGhcVersion' cradle = do

  -- See if the tool is installed
  case actionName (cradleOptsProg cradle) of
    Stack   -> checkToolExists "stack"
    Cabal   -> checkToolExists "cabal"
    Default -> checkToolExists "ghc"
    Direct  -> checkToolExists "ghc"
    _       -> pure ()

  HieBios.getRuntimeGhcVersion cradle >>= cradleResult "Failed to get project GHC version"
  where
    checkToolExists exe = do
      exists <- findExecutable exe
      case exists of
        Just _ -> pure ()
        Nothing ->
          die $ "Cradle requires " ++ exe ++ " but couldn't find it" ++ "\n"
           ++ show cradle

findProjectCradle :: IO (Cradle Void)
findProjectCradle = findProjectCradle' True

findProjectCradle' :: Bool -> IO (Cradle Void)
findProjectCradle' log = do
  d <- getCurrentDirectory

  let initialFp = d </> "a"
  hieYaml <- Session.findCradle def initialFp

  -- Some log messages
  when log $
      case hieYaml of
        Just yaml -> hPutStrLn stderr $ "Found \"" ++ yaml ++ "\" for \"" ++ initialFp ++ "\""
        Nothing -> hPutStrLn stderr "No 'hie.yaml' found. Try to discover the project type!"

  Session.loadCradle def hieYaml d

trim :: String -> String
trim s = case lines s of
  [] -> s
  ls -> dropWhileEnd isSpace $ last ls
