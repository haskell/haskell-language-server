-- Copyright (c) 2019 The DAML Authors. All rights reserved.
-- SPDX-License-Identifier: Apache-2.0

{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeFamilies      #-}

-- | Utilities and state for the files of interest - those which are currently
--   open in the editor. The useful function is 'getFilesOfInterest'.
module Development.IDE.Core.OfInterest(
    ofInterestRules,
    getFilesOfInterest, setFilesOfInterest, modifyFilesOfInterest,
    kick, FileOfInterestStatus(..),
    OfInterestVar(..)
    ) where

import           Control.Concurrent.Strict
import           Control.DeepSeq
import           Control.Exception
import           Control.Monad
import           Control.Monad.IO.Class
import           Data.Binary
import           Data.HashMap.Strict                          (HashMap)
import qualified Data.HashMap.Strict                          as HashMap
import           Data.Hashable
import qualified Data.Text                                    as T
import           Data.Typeable
import           Development.Shake
import           GHC.Generics

import           Control.Monad.Trans.Class
import           Control.Monad.Trans.Maybe
import qualified Data.ByteString.Lazy                         as LBS
import           Data.List.Extra                              (nubOrd)
import           Data.Maybe                                   (catMaybes)
import           Development.IDE.Core.RuleTypes
import           Development.IDE.Core.Shake
import           Development.IDE.Import.DependencyInformation
import           Development.IDE.Types.Exports
import           Development.IDE.Types.Location
import           Development.IDE.Types.Logger
import           Development.IDE.Types.Options

newtype OfInterestVar = OfInterestVar (Var (HashMap NormalizedFilePath FileOfInterestStatus))
instance IsIdeGlobal OfInterestVar

type instance RuleResult GetFilesOfInterest = HashMap NormalizedFilePath FileOfInterestStatus

data GetFilesOfInterest = GetFilesOfInterest
    deriving (Eq, Show, Typeable, Generic)
instance Hashable GetFilesOfInterest
instance NFData   GetFilesOfInterest
instance Binary   GetFilesOfInterest


-- | The rule that initialises the files of interest state.
ofInterestRules :: Rules ()
ofInterestRules = do
    addIdeGlobal . OfInterestVar =<< liftIO (newVar HashMap.empty)
    defineEarlyCutoff $ RuleNoDiagnostics $ \GetFilesOfInterest _file -> assert (null $ fromNormalizedFilePath _file) $ do
        alwaysRerun
        filesOfInterest <- getFilesOfInterestUntracked
        let !cutoff = LBS.toStrict $ encode $ HashMap.toList filesOfInterest
        pure (Just cutoff, Just filesOfInterest)

-- | Get the files that are open in the IDE.
getFilesOfInterest :: Action (HashMap NormalizedFilePath FileOfInterestStatus)
getFilesOfInterest = useNoFile_ GetFilesOfInterest

------------------------------------------------------------
-- Exposed API

-- | Set the files-of-interest - not usually necessary or advisable.
--   The LSP client will keep this information up to date.
setFilesOfInterest :: IdeState -> HashMap NormalizedFilePath FileOfInterestStatus -> IO ()
setFilesOfInterest state files = modifyFilesOfInterest state (const files)

getFilesOfInterestUntracked :: Action (HashMap NormalizedFilePath FileOfInterestStatus)
getFilesOfInterestUntracked = do
    OfInterestVar var <- getIdeGlobalAction
    liftIO $ readVar var

-- | Modify the files-of-interest - not usually necessary or advisable.
--   The LSP client will keep this information up to date.
modifyFilesOfInterest
  :: IdeState
  -> (HashMap NormalizedFilePath FileOfInterestStatus -> HashMap NormalizedFilePath FileOfInterestStatus)
  -> IO ()
modifyFilesOfInterest state f = do
    OfInterestVar var <- getIdeGlobalState state
    files <- modifyVar' var f
    logDebug (ideLogger state) $ "Set files of interest to: " <> T.pack (show $ HashMap.toList files)

-- | Typecheck all the files of interest.
--   Could be improved
kick :: Action ()
kick = do
    files <- HashMap.keys <$> getFilesOfInterest
    ShakeExtras{progressUpdate} <- getShakeExtras
    liftIO $ progressUpdate KickStarted

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

    liftIO $ progressUpdate KickCompleted

