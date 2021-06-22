-- Copyright (c) 2019 The DAML Authors. All rights reserved.
-- SPDX-License-Identifier: Apache-2.0

{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeFamilies      #-}

-- | Utilities and state for the files of interest - those which are currently
--   open in the editor. The rule is 'IsFileOfInterest'
module Development.IDE.Core.OfInterest(
    ofInterestRules,
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
import           Data.HashMap.Strict                          (HashMap)
import qualified Data.HashMap.Strict                          as HashMap
import qualified Data.Text                                    as T
import           Development.IDE.Graph

import           Control.Monad.Trans.Class
import           Control.Monad.Trans.Maybe
import qualified Data.ByteString                              as BS
import           Data.List.Extra                              (nubOrd)
import           Data.Maybe                                   (catMaybes)
import           Development.IDE.Core.ProgressReporting
import           Development.IDE.Core.RuleTypes
import           Development.IDE.Core.Shake
import           Development.IDE.Import.DependencyInformation
import           Development.IDE.Types.Exports
import           Development.IDE.Types.Location
import           Development.IDE.Types.Logger
import           Development.IDE.Types.Options

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
        pure (new, (prev, dict))
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
    ShakeExtras{progress} <- getShakeExtras
    liftIO $ progressUpdate progress KickStarted

    -- Update the exports map for FOIs
    results <- uses GenerateCore files <* uses GetHieAst files

    -- Update the exports map for non FOIs
    -- We can skip this if checkProject is True, assuming they never change under our feet.
    IdeOptions{ optCheckProject = doCheckProject } <- getIdeOptions
    checkProject <- liftIO doCheckProject
    ifaces <- if checkProject then return Nothing else runMaybeT $ do
        deps <- MaybeT $ sequence <$> uses GetDependencies files
        hiResults <- lift $ uses GetModIface (nubOrd $ foldMap transitiveModuleDeps deps)
        return $ map hirModIface $ catMaybes hiResults

    ShakeExtras{exportsMap} <- getShakeExtras
    let mguts = catMaybes results
        !exportsMap' = createExportsMapMg mguts
        !exportsMap'' = maybe mempty createExportsMap ifaces
    void $ liftIO $ modifyVar' exportsMap $ (exportsMap'' <>) . (exportsMap' <>)

    liftIO $ progressUpdate progress KickCompleted
