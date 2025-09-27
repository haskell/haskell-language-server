-- Copyright (c) 2019 The DAML Authors. All rights reserved.
-- SPDX-License-Identifier: Apache-2.0

{-# LANGUAGE TypeFamilies #-}

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
    OfInterestVar(..),
    scheduleGarbageCollection,
    Log(..), doKick
    ) where

import           Control.Concurrent.Strict
import           Control.Monad
import           Control.Monad.IO.Class
import           Data.HashMap.Strict                      (HashMap)
import qualified Data.HashMap.Strict                      as HashMap
import           Data.Proxy
import           Development.IDE.Graph

import           Control.Concurrent.STM.Stats             (atomically,
                                                           modifyTVar')
import           Data.Aeson                               (toJSON)
import qualified Data.ByteString                          as BS
import           Data.Maybe                               (catMaybes)
import           Development.IDE.Core.ProgressReporting
import           Development.IDE.Core.RuleTypes
import           Development.IDE.Core.Shake               hiding (Log)
import qualified Development.IDE.Core.Shake               as Shake
import           Development.IDE.Plugin.Completions.Types
import           Development.IDE.Types.Exports
import           Development.IDE.Types.Location
import           Development.IDE.Types.Options            (IdeTesting (..))
import           Development.IDE.Types.Shake              (toKey, toNoFileKey)
import           GHC.TypeLits                             (KnownSymbol)
import           Ide.Logger                               (Pretty (pretty),
                                                           Priority (..),
                                                           Recorder,
                                                           WithPriority,
                                                           cmapWithPrio,
                                                           logWith)
import qualified Language.LSP.Protocol.Message            as LSP
import qualified Language.LSP.Server                      as LSP

data Log = LogShake Shake.Log
  deriving Show

instance Pretty Log where
  pretty = \case
    LogShake msg -> pretty msg

newtype OfInterestVar = OfInterestVar (Var (HashMap NormalizedFilePath FileOfInterestStatus))

instance IsIdeGlobal OfInterestVar

-- | The rule that initialises the files of interest state.
ofInterestRules :: Recorder (WithPriority Log) -> Rules ()
ofInterestRules recorder = do
    addIdeGlobal . OfInterestVar =<< liftIO (newVar HashMap.empty)
    addIdeGlobal . GarbageCollectVar =<< liftIO (newVar False)
    -- A no-file rule to perform the global kick action
    defineEarlyCutOffNoFile (cmapWithPrio LogShake recorder) $ \Kick -> do
        kick
        pure ("", ())
    defineEarlyCutoff (cmapWithPrio LogShake recorder) $ RuleNoDiagnostics $ \IsFileOfInterest f -> do
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
newtype GarbageCollectVar = GarbageCollectVar (Var Bool)
instance IsIdeGlobal GarbageCollectVar

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

addFileOfInterest :: IdeState -> NormalizedFilePath -> FileOfInterestStatus -> IO [Key]
addFileOfInterest state f v = do
    OfInterestVar var <- getIdeGlobalState state
    (prev, files) <- modifyVar var $ \dict -> do
        let (prev, new) = HashMap.alterF (, Just v) f dict
        pure (new, (prev, new))
    if prev /= Just v
    then do
        logWith (ideLogger state) Debug $
            LogSetFilesOfInterest (HashMap.toList files)
        return [toKey IsFileOfInterest f, toNoFileKey Kick]
    else return []

deleteFileOfInterest :: IdeState -> NormalizedFilePath -> IO [Key]
deleteFileOfInterest state f = do
    OfInterestVar var <- getIdeGlobalState state
    files <- modifyVar' var $ HashMap.delete f
    logWith (ideLogger state) Debug $
        LogSetFilesOfInterest (HashMap.toList files)
    return [toKey IsFileOfInterest f, toNoFileKey Kick]
scheduleGarbageCollection :: IdeState -> IO ()
scheduleGarbageCollection state = do
    GarbageCollectVar var <- getIdeGlobalState state
    writeVar var True

doKick :: Action ()
doKick = useNoFile_ Kick

-- | Typecheck all the files of interest.
--   Could be improved
kick :: Action ()
kick = do
    files <- HashMap.keys <$> getFilesOfInterestUntracked
    ShakeExtras{exportsMap, ideTesting = IdeTesting testing, lspEnv, progress} <- getShakeExtras
    let signal :: KnownSymbol s => Proxy s -> Action ()
        signal msg = when testing $ liftIO $
            mRunLspT lspEnv $
                LSP.sendNotification (LSP.SMethod_CustomMethod msg) $
                toJSON $ map fromNormalizedFilePath files

    signal (Proxy @"kick/start")
    liftIO $ progressUpdate progress ProgressNewStarted

    -- Update the exports map
    results <- uses GenerateCore files
            <* uses GetHieAst files
            -- needed to have non local completions on the first edit
            -- when the first edit breaks the module header
            <* uses NonLocalCompletions files
    let mguts = catMaybes results
    void $ liftIO $ atomically $ modifyTVar' exportsMap (updateExportsMapMg mguts)

    liftIO $ progressUpdate progress ProgressCompleted

    GarbageCollectVar var <- getIdeGlobalAction
    garbageCollectionScheduled <- liftIO $ readVar var
    when garbageCollectionScheduled $ do
        void garbageCollectDirtyKeys
        liftIO $ writeVar var False

    signal (Proxy @"kick/done")
