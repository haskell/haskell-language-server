-- Copyright (c) 2019 The DAML Authors. All rights reserved.
-- SPDX-License-Identifier: Apache-2.0

{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeFamilies      #-}

-- | Utilities and state for the files of interest - those which are currently
--   open in the editor. The rule is 'IsFileOfInterest'
module Development.IDE.Core.OfInterest(
    ofInterestRules,
    getFilesOfInterest,
    getFilesOfInterestUntracked,
    addFileOfInterest,
    deleteFileOfInterest,
    setFilesOfInterest,
    kick, FileOfInterestStatus(..),
    OfInterestVar(..)
    ) where

import           Control.Concurrent.Strict
import           Control.Monad
import           Control.Monad.IO.Class
import           Data.HashMap.Strict                    (HashMap)
import qualified Data.HashMap.Strict                    as HashMap
import qualified Data.Text                              as T
import           Development.IDE.Graph

import qualified Data.ByteString                        as BS
import           Data.Maybe                             (catMaybes)
import           Development.IDE.Core.ProgressReporting
import           Development.IDE.Core.RuleTypes
import           Development.IDE.Core.Shake
import           Development.IDE.Types.Exports
import           Development.IDE.Types.Location
import           Development.IDE.Types.Logger
import           System.Time.Extra                      (sleep)

newtype OfInterestVar = OfInterestVar (Var (HashMap NormalizedFilePath FileOfInterestStatus))
instance IsIdeGlobal OfInterestVar

-- | The rule that initialises the files of interest state.
ofInterestRules :: Rules ()
ofInterestRules = do
    addIdeGlobal . OfInterestVar =<< liftIO (newVar HashMap.empty)
    defineEarlyCutoff $ RuleNoDiagnostics $ \IsFileOfInterest f -> do
        alwaysRerun
        filesOfInterest <- getFilesOfInterestUntracked
        let foi = maybe NotFOI IsFOI $ f `HashMap.lookup` filesOfInterest
            fp  = summarize foi
            res = (Just fp, Just foi)
        return res
    where
    summarize NotFOI                   = BS.singleton 0
    summarize (IsFOI OnDisk)           = BS.singleton 1
    summarize (IsFOI (Modified False)) = BS.singleton 2
    summarize (IsFOI (Modified True))  = BS.singleton 3


------------------------------------------------------------
-- Exposed API

getFilesOfInterest :: IdeState -> IO( HashMap NormalizedFilePath FileOfInterestStatus)
getFilesOfInterest state = do
    OfInterestVar var <- getIdeGlobalState state
    readVar var

-- | Set the files-of-interest - not usually necessary or advisable.
--   The LSP client will keep this information up to date.
setFilesOfInterest :: IdeState -> HashMap NormalizedFilePath FileOfInterestStatus -> IO ()
setFilesOfInterest state files = do
    OfInterestVar var <- getIdeGlobalState state
    writeVar var files

getFilesOfInterestUntracked :: Action (HashMap NormalizedFilePath FileOfInterestStatus)
getFilesOfInterestUntracked = do
    OfInterestVar var <- getIdeGlobalAction
    liftIO $ readVar var

addFileOfInterest :: IdeState -> NormalizedFilePath -> FileOfInterestStatus -> IO ()
addFileOfInterest state f v = do
    OfInterestVar var <- getIdeGlobalState state
    (prev, files) <- modifyVar var $ \dict -> do
        let (prev, new) = HashMap.alterF (, Just v) f dict
        pure (new, (prev, new))
    when (prev /= Just v) $
        recordDirtyKeys (shakeExtras state) IsFileOfInterest [f]
    logDebug (ideLogger state) $
        "Set files of interest to: " <> T.pack (show files)

deleteFileOfInterest :: IdeState -> NormalizedFilePath -> IO ()
deleteFileOfInterest state f = do
    OfInterestVar var <- getIdeGlobalState state
    files <- modifyVar' var $ HashMap.delete f
    recordDirtyKeys (shakeExtras state) IsFileOfInterest [f]
    logDebug (ideLogger state) $ "Set files of interest to: " <> T.pack (show files)


-- | Typecheck all the files of interest.
--   Could be improved
kick :: Action ()
kick = do
    files <- HashMap.keys <$> getFilesOfInterestUntracked
    ShakeExtras{exportsMap, progress} <- getShakeExtras
    liftIO $ progressUpdate progress KickStarted

    -- Update the exports map
    results <- uses GenerateCore files <* uses GetHieAst files
    let mguts = catMaybes results
        !exportsMap' = createExportsMapMg mguts
    void $ liftIO $ modifyVar' exportsMap (exportsMap' <>)

    liftIO $ progressUpdate progress KickCompleted

    -- if idle, perform garbage collection
    liftIO $ sleep 5
    garbageCollectDirtyKeys
