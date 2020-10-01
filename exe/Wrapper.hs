{-# LANGUAGE RecordWildCards #-}
-- | This module is based on the hie-wrapper.sh script in
-- https://github.com/alanz/vscode-hie-server
module Main where

import Control.Monad.Extra
import Data.Foldable
import Data.List
import Data.Void
import HIE.Bios
import HIE.Bios.Environment
import HIE.Bios.Types
import Ide.Arguments
import Ide.Version
import System.Directory
import System.Environment
import System.Exit
import System.FilePath
import System.IO
import System.Info
import System.Process

-- ---------------------------------------------------------------------

main :: IO ()
main = do
  -- WARNING: If you write to stdout before runLanguageServer
  --          then the language server will not work
  args <- getArguments "haskell-language-server-wrapper"

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

      LspMode lspArgs ->
          launchHaskellLanguageServer lspArgs

launchHaskellLanguageServer :: LspArguments -> IO ()
launchHaskellLanguageServer LspArguments{..} = do
  whenJust argsCwd setCurrentDirectory

  d <- getCurrentDirectory

  -- Get the cabal directory from the cradle
  cradle <- findLocalCradle (d </> "a")
  setCurrentDirectory $ cradleRootDir cradle

  when argsProjectGhcVersion $ getRuntimeGhcVersion' cradle >>= putStrLn >> exitSuccess

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
  hPutStrLn stderr $ "Consulting the cradle to get project GHC version..."
  ghcVersion <- getRuntimeGhcVersion' cradle
  hPutStrLn stderr $ "Project GHC version: " ++ ghcVersion

  let
    hlsBin = "haskell-language-server-" ++ ghcVersion
    backupHlsBin =
      case dropWhileEnd (/='.') ghcVersion of
        [] -> "haskell-language-server"
        xs -> "haskell-language-server-" ++ init xs
    candidates' = [hlsBin, backupHlsBin, "haskell-language-server"]
    candidates = map (++ exeExtension) candidates'

  hPutStrLn stderr $ "haskell-language-server exe candidates: " ++ show candidates

  mexes <- traverse findExecutable candidates

  case asum mexes of
    Nothing -> hPutStrLn stderr $ "Cannot find any haskell-language-server exe, looked for: " ++ intercalate ", " candidates
    Just e -> do
      hPutStrLn stderr $ "Launching haskell-language-server exe at:" ++ e
      callProcess e args

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

  ghcVersionRes <- getRuntimeGhcVersion cradle
  case ghcVersionRes of
    CradleSuccess ver -> do
      return ver
    CradleFail error -> die $ "Failed to get project GHC version:" ++ show error
    CradleNone -> die "Failed get project GHC version, since we have a none cradle"
  where
    checkToolExists exe = do
      exists <- findExecutable exe
      case exists of
        Just _ -> pure ()
        Nothing ->
          die $ "Cradle requires " ++ exe ++ " but couldn't find it" ++ "\n"
           ++ show cradle

-- | Find the cradle that the given File belongs to.
--
-- First looks for a "hie.yaml" file in the directory of the file
-- or one of its parents. If this file is found, the cradle
-- is read from the config. If this config does not comply to the "hie.yaml"
-- specification, an error is raised.
--
-- If no "hie.yaml" can be found, the implicit config is used.
-- The implicit config uses different heuristics to determine the type
-- of the project that may or may not be accurate.
findLocalCradle :: FilePath -> IO (Cradle Void)
findLocalCradle fp = do
  cradleConf <- findCradle fp
  crdl       <- case cradleConf of
    Just yaml -> do
      hPutStrLn stderr $ "Found \"" ++ yaml ++ "\" for \"" ++ fp ++ "\""
      loadCradle yaml
    Nothing -> loadImplicitCradle fp
  hPutStrLn stderr $ "Module \"" ++ fp ++ "\" is loaded by Cradle: " ++ show crdl
  return crdl

