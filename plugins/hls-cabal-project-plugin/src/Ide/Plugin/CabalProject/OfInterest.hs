{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE OverloadedStrings #-}

-- | Tracks which @cabal.project@ files are currently open in the editor,
-- so that diagnostics can be produced for them on every shake run.
module Ide.Plugin.CabalProject.OfInterest
  ( ofInterestRules
  , getProjectFilesOfInterestUntracked
  , addProjectFileOfInterest
  , deleteProjectFileOfInterest
  ) where

import           Control.Concurrent.Strict
import           Control.Monad.IO.Class
import           Data.HashMap.Strict        (HashMap)
import qualified Data.HashMap.Strict        as HashMap
import           Development.IDE
import qualified Development.IDE.Core.Shake as Shake

{- | Project files (e.g. @cabal.project@, @cabal.project.freeze@) that are
currently open in the lsp-client.

We need to store the open files to parse them again if we restart the shake
session. Restarting of the shake session happens whenever these files are
modified.
-}
newtype ProjectFilesOfInterest = ProjectFilesOfInterest (Var (HashMap NormalizedFilePath FileOfInterestStatus))

instance Shake.IsIdeGlobal ProjectFilesOfInterest

{- | The rule that initialises the project files of interest state.

Needs to be run on start-up.
-}
ofInterestRules :: Rules ()
ofInterestRules =
  Shake.addIdeGlobal . ProjectFilesOfInterest =<< liftIO (newVar HashMap.empty)

getProjectFilesOfInterestUntracked :: Action (HashMap NormalizedFilePath FileOfInterestStatus)
getProjectFilesOfInterestUntracked = do
  ProjectFilesOfInterest var <- Shake.getIdeGlobalAction
  liftIO $ readVar var

addProjectFileOfInterest :: IdeState -> NormalizedFilePath -> FileOfInterestStatus -> IO ()
addProjectFileOfInterest state f v = do
  ProjectFilesOfInterest var <- Shake.getIdeGlobalState state
  _ <- modifyVar' var $ HashMap.insert f v
  pure ()

deleteProjectFileOfInterest :: IdeState -> NormalizedFilePath -> IO ()
deleteProjectFileOfInterest state f = do
  ProjectFilesOfInterest var <- Shake.getIdeGlobalState state
  _ <- modifyVar' var $ HashMap.delete f
  pure ()
