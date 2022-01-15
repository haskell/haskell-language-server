{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
-- | This module is based on the hie-wrapper.sh script in
-- https://github.com/alanz/vscode-hie-server
module Main where

import           Control.Monad.Extra
import           Control.Monad.IO.Class
import           Control.Monad.Trans.Except
import           Data.Default
import           Data.Foldable
import           Data.List
import           Data.Maybe
import qualified Data.Text                  as T
import qualified Data.Text.IO               as T
import           Data.Void
import qualified Development.IDE.Session    as Session
import qualified HIE.Bios.Environment       as HieBios
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
import           WrapperLspMain

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

      _ -> launchHaskellLanguageServer args >>= \case
        Right () -> pure ()
        Left err -> do
          T.hPutStrLn stderr "*** Startup ERROR"
          T.hPutStrLn stderr (prettyError err NoShorten)
          case args of
            Ghcide ghcideArguments -> lspMain ghcideArguments (prettyError err Shorten)
            _ -> pure ()


launchHaskellLanguageServer :: Arguments -> IO (Either WrapperSetupError ())
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
      when argsProjectGhcVersion $ do
        runExceptT (getRuntimeGhcVersion' cradle) >>= \case
          Right ghcVersion -> putStrLn ghcVersion >> exitSuccess
          Left err -> T.putStrLn (prettyError err NoShorten) >> exitFailure
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
  runExceptT (getRuntimeGhcVersion' cradle) >>= \case
    Left err -> pure $ Left err
    Right ghcVersion -> do
      hPutStrLn stderr $ "Project GHC version: " ++ ghcVersion

      let
        hlsBin = "haskell-language-server-" ++ ghcVersion
        candidates' = [hlsBin, "haskell-language-server"]
        candidates = map (++ exeExtension) candidates'

      hPutStrLn stderr $ "haskell-language-server exe candidates: " ++ show candidates

      mexes <- traverse findExecutable candidates

      case asum mexes of
        Nothing -> pure $ Left $ NoLanguageServer ghcVersion candidates
        Just e -> do
          hPutStrLn stderr $ "Launching haskell-language-server exe at:" ++ e
          callProcess e args
          pure $ Right ()

-- | Version of 'getRuntimeGhcVersion' that throws a 'WrapperSetupError' if we
-- can't get it, and also checks if run-time tool dependencies are missing.
getRuntimeGhcVersion' :: Cradle Void -> ExceptT WrapperSetupError IO String
getRuntimeGhcVersion' cradle = do

  let cradleName = actionName (cradleOptsProg cradle)
  -- See if the tool is installed
  case cradleName of
    Stack   -> checkToolExists "stack"
    Cabal   -> checkToolExists "cabal"
    Default -> checkToolExists "ghc"
    Direct  -> checkToolExists "ghc"
    _       -> pure ()

  ghcVersionRes <- liftIO $ HieBios.getRuntimeGhcVersion cradle
  case ghcVersionRes of
    CradleSuccess ver -> do
      return ver
    CradleFail error -> throwE $ FailedToObtainGhcVersion cradleName error
    CradleNone -> throwE $ NoneCradleGhcVersion cradleName
  where
    checkToolExists exe = do
      exists <- liftIO $ findExecutable exe
      case exists of
        Just _ -> pure ()
        Nothing -> throwE $ ToolRequirementMissing exe (actionName (cradleOptsProg cradle))

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

data WrapperSetupError
    = FailedToObtainGhcVersion (ActionName Void) CradleError
    | NoneCradleGhcVersion (ActionName Void)
    | NoLanguageServer String [FilePath]
    | ToolRequirementMissing String (ActionName Void)
    deriving (Show)

data Shorten = Shorten | NoShorten

-- | Pretty error message displayable to the future.
-- Extra argument 'Shorten' can be used to shorten error message.
-- Reduces usefulness, but allows us to show the error message via LSP
-- as LSP doesn't allow any newlines and makes it really hard to read
-- the message otherwise.
prettyError :: WrapperSetupError -> Shorten ->  T.Text
prettyError (FailedToObtainGhcVersion name crdlError) shorten =
  "Failed to find the GHC version of this " <> T.pack (show name) <> " project." <>
  case shorten of
    Shorten ->
      "\n" <> T.pack (fromMaybe "" . listToMaybe $ cradleErrorStderr crdlError)
    NoShorten ->
      "\n" <> T.pack (intercalate "\n" (cradleErrorStderr crdlError))
prettyError (NoneCradleGhcVersion name) _ =
  "Failed to get the GHC version of the " <> T.pack (show name) <>
  " project, since we have a none cradle"
prettyError (NoLanguageServer ghcVersion candidates) _ =
  "Failed to find a HLS version for GHC " <> T.pack ghcVersion <>
  "\nExecutable names we failed to find: " <> T.pack (intercalate "," candidates)
prettyError (ToolRequirementMissing toolExe name) _ =
  "This is a " <> T.pack (show name) <> " Project, but we failed to find \"" <>
  T.pack toolExe <> "\" on the $PATH"
