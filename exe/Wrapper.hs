{-# LANGUAGE RecordWildCards #-}
-- | This module is based on the hie-wrapper.sh script in
-- https://github.com/alanz/vscode-hie-server
module Main where

import Arguments
import Control.Monad.Extra
import Data.Foldable
import Data.List
import HIE.Bios
import HIE.Bios.Environment
import HIE.Bios.Types
import Ide.Cradle (findLocalCradle)
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
  Arguments{..} <- getArguments "haskell-language-server-wrapper"

  d <- getCurrentDirectory

  -- Get the cabal directory from the cradle
  cradle <- findLocalCradle (d </> "a")
  setCurrentDirectory $ cradleRootDir cradle

  when argsProjectGhcVersion $ getRuntimeGhcVersion' cradle >>= putStrLn >> exitSuccess
  when argsVersion $ haskellLanguageServerVersion >>= putStrLn >> exitSuccess

  whenJust argsCwd setCurrentDirectory

  progName <- getProgName
  hPutStrLn stderr $ "Run entered for haskell-language-server-wrapper(" ++ progName ++ ") "
                     ++ hlsVersion
  hPutStrLn stderr $ "Current directory: " ++ d
  hPutStrLn stderr $ "Operating system: " ++ os
  args <- getArgs
  hPutStrLn stderr $ "Arguments: " ++ show args
  hPutStrLn stderr $ "Cradle directory: " ++ cradleRootDir cradle
  hPutStrLn stderr $ "Cradle type: " ++ show (actionName (cradleOptsProg cradle))

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

-- | Version of 'getRuntimeGhcVersion' that dies if we can't get it
getRuntimeGhcVersion' :: Cradle a -> IO String
getRuntimeGhcVersion' cradle = do
  ghcVersionRes <- getRuntimeGhcVersion cradle
  case ghcVersionRes of
    CradleSuccess ver -> do
      return ver
    CradleFail error -> die $ "Failed to get project GHC version:" ++ show error
    CradleNone -> die "Failed get project GHC version, since we have a none cradle"
