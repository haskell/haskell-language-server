{-# LANGUAGE CPP             #-}
-- | This module is based on the hie-wrapper.sh script in
-- https://github.com/alanz/vscode-hie-server
module Main where

#if __GLASGOW_HASKELL__ < 804
import           Data.Semigroup
#endif
import           Data.Foldable
import           Data.List
import           Data.Version                          (showVersion)
import           HIE.Bios
import           Ide.Cradle (findLocalCradle, logm)
import           Ide.Options
import           Ide.Version
import qualified Language.Haskell.LSP.Core             as Core
import           Options.Applicative.Simple
import qualified Paths_ide                             as Meta
import           System.Directory
import           System.Environment
import           System.FilePath
import           System.Info
import qualified System.Log.Logger as L
import           System.Process

-- ---------------------------------------------------------------------

main :: IO ()
main = do
    let
        numericVersion :: Parser (a -> a)
        numericVersion =
            infoOption
                (showVersion Meta.version)
                (long "numeric-version" <>
                 help "Show only version number")
        compiler :: Parser (a -> a)
        compiler =
            infoOption
                hieGhcDisplayVersion
                (long "compiler" <>
                 help "Show only compiler and version supported")
    -- Parse the options and run
    (global, ()) <-
        simpleOptions
            hieVersion
            "haskell-ide-wrapper - Launch the appropriate haskell-ide for a given project"
            ""
            (numericVersion <*> compiler <*> globalOptsParser)
            empty

    run global

-- ---------------------------------------------------------------------

run :: GlobalOpts -> IO ()
run opts = do
  let mLogFileName = optLogFile opts

      logLevel = if optDebugOn opts
                   then L.DEBUG
                   else L.INFO

  Core.setupLogger mLogFileName ["hie"] logLevel

  maybe (pure ()) setCurrentDirectory $ projectRoot opts


  progName <- getProgName
  logm $  "run entered for haskell-ide-wrapper(" ++ progName ++ ") " ++ hieVersion
  d <- getCurrentDirectory
  logm $ "Current directory:" ++ d
  logm $ "Operating system:" ++ os
  args <- getArgs
  logm $ "args:" ++ show args

  -- Get the cabal directory from the cradle
  cradle <- findLocalCradle (d </> "File.hs")
  let dir = cradleRootDir cradle
  logm $ "Cradle directory:" ++ dir
  setCurrentDirectory dir

  ghcVersion <- getProjectGhcVersion cradle
  logm $ "Project GHC version:" ++ ghcVersion

  let
    hieBin = "haskell-ide-" ++ ghcVersion
    backupHieBin =
      case dropWhileEnd (/='.') ghcVersion of
        [] -> "haskell-ide"
        xs -> "haskell-ide-" ++ init xs
    candidates' = [hieBin, backupHieBin, "haskell-ide"]
    candidates = map (++ exeExtension) candidates'

  logm $ "haskell-ide exe candidates :" ++ show candidates

  mexes <- traverse findExecutable candidates

  case asum mexes of
    Nothing -> logm $ "cannot find any haskell-ide exe, looked for:" ++ intercalate ", " candidates
    Just e -> do
      logm $ "found haskell-ide exe at:" ++ e
      logm $ "args:" ++ show args
      logm "launching ....\n\n\n"
      callProcess e args
      logm "done"

-- ---------------------------------------------------------------------
