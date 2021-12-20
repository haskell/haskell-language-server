{-# LANGUAGE RecordWildCards #-}
-- | This module is based on the hie-wrapper.sh script in
-- https://github.com/alanz/vscode-hie-server
module Main where

import           Control.Monad.Extra
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
import           System.Process

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

  ghcVersionRes <- HieBios.getRuntimeGhcVersion cradle
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

findProjectCradle :: IO (Cradle Void)
findProjectCradle = do
  d <- getCurrentDirectory

  let initialFp = d </> "a"
  hieYaml <- Session.findCradle def initialFp

  -- Some log messages
  case hieYaml of
    Just yaml -> hPutStrLn stderr $ "Found \"" ++ yaml ++ "\" for \"" ++ initialFp ++ "\""
    Nothing -> hPutStrLn stderr "No 'hie.yaml' found. Try to discover the project type!"

  Session.loadCradle def hieYaml d
