{-# LANGUAGE CPP             #-}
{-# LANGUAGE RecordWildCards #-}
-- | This module is based on the hie-wrapper.sh script in
-- https://github.com/alanz/vscode-hie-server
module Main where

#if __GLASGOW_HASKELL__ < 804
import           Data.Semigroup
#endif

import Arguments
-- import Control.Concurrent.Extra
import Control.Monad.Extra
import           Data.Foldable
import           Data.List
-- import Data.List.Extra
-- import qualified Data.Text as T
-- import qualified Data.Text.IO as T
-- import Development.IDE.Types.Logger
import HIE.Bios
import           Ide.Cradle (findLocalCradle)
import           Ide.Logger (logm)
import           Ide.Version
import           System.Directory
import           System.Environment
import System.Exit
import System.IO
import           System.Info
import           System.Process

-- ---------------------------------------------------------------------

main :: IO ()
main = do
  -- WARNING: If you write to stdout before runLanguageServer
  --          then the language server will not work
  Arguments{..} <- getArguments "haskell-language-server-wrapper"
  
  d <- getCurrentDirectory

  -- Get the cabal directory from the cradle
  cradle <- findLocalCradle d
  let dir = cradleRootDir cradle
  setCurrentDirectory dir

  ghcVersion <- getProjectGhcVersion cradle

  when argsProjectGhcVersion $ putStrLn ghcVersion >> exitSuccess

  if argsVersion then ghcideVersion >>= putStrLn >> exitSuccess
  else hPutStrLn stderr {- see WARNING above -} =<< ghcideVersion

  -- lock to avoid overlapping output on stdout
  -- lock <- newLock
  -- let logger p = Logger $ \pri msg -> when (pri >= p) $ withLock lock $
  --         T.putStrLn $ T.pack ("[" ++ upper (show pri) ++ "] ") <> msg

  whenJust argsCwd setCurrentDirectory

  -- let mLogFileName = optLogFile opts

  --     logLevel = if optDebugOn opts
  --                  then L.DEBUG
  --                  else L.INFO

  -- Core.setupLogger mLogFileName ["hie"] logLevel

  progName <- getProgName
  logm $  "run entered for haskell-language-server-wrapper(" ++ progName ++ ") "
            ++ hlsVersion
  logm $ "Current directory:" ++ d
  logm $ "Operating system:" ++ os
  args <- getArgs
  logm $ "args:" ++ show args
  logm $ "Cradle directory:" ++ dir
  logm $ "Project GHC version:" ++ ghcVersion

  let
    hlsBin = "haskell-language-server-" ++ ghcVersion
    backupHlsBin =
      case dropWhileEnd (/='.') ghcVersion of
        [] -> "haskell-language-server"
        xs -> "haskell-language-server-" ++ init xs
    candidates' = [hlsBin, backupHlsBin, "haskell-language-server"]
    candidates = map (++ exeExtension) candidates'

  logm $ "haskell-language-server exe candidates :" ++ show candidates

  mexes <- traverse findExecutable candidates

  case asum mexes of
    Nothing -> logm $ "cannot find any haskell-language-server exe, looked for:" ++ intercalate ", " candidates
    Just e -> do
      logm $ "found haskell-language-server exe at:" ++ e
      logm $ "args:" ++ show args
      logm "launching ....\n\n\n"
      callProcess e args
      logm "done"

-- ---------------------------------------------------------------------
