-- Copyright (c) 2019 The DAML Authors. All rights reserved.
-- SPDX-License-Identifier: Apache-2.0

{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleInstances #-}

-- | Utilities and state for the files of interest - those which are currently
--   open in the editor. The useful function is 'getFilesOfInterest'.
module Development.IDE.Core.OfInterest(
    ofInterestRules,
    getFilesOfInterest, setFilesOfInterest, modifyFilesOfInterest,
    kick, FileOfInterestStatus(..)
    ) where

import Control.Concurrent.Extra
import Data.Binary
import Data.Hashable
import Control.DeepSeq
import GHC.Generics
import Data.Typeable
import qualified Data.ByteString.UTF8 as BS
import Control.Exception
import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HashMap
import qualified Data.Text as T
import Data.Tuple.Extra
import Development.Shake
import Control.Monad

import Development.IDE.Types.Exports
import Development.IDE.Types.Location
import Development.IDE.Types.Logger
import Development.IDE.Core.RuleTypes
import Development.IDE.Core.Shake
import Data.Maybe (catMaybes)
import Data.List.Extra (nubOrd)
import Development.IDE.Import.DependencyInformation
import Control.Monad.Trans.Maybe
import Control.Monad.Trans.Class
import Development.IDE.Types.Options

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
    defineEarlyCutoff $ \GetFilesOfInterest _file -> assert (null $ fromNormalizedFilePath _file) $ do
        alwaysRerun
        filesOfInterest <- getFilesOfInterestUntracked
        pure (Just $ BS.fromString $ show filesOfInterest, ([], Just filesOfInterest))


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
    files <- modifyVar var $ pure . dupe . f
    logDebug (ideLogger state) $ "Set files of interest to: " <> T.pack (show $ HashMap.toList files)

-- | Typecheck all the files of interest.
--   Could be improved
kick :: Action ()
kick = do
    files <- HashMap.keys <$> getFilesOfInterest
    ShakeExtras{progressUpdate} <- getShakeExtras
    liftIO $ progressUpdate KickStarted

    -- Update the exports map for FOIs
    (results, ()) <- par (uses GenerateCore files) (void $ uses GetHieAst files)

    -- Update the exports map for non FOIs
    -- We can skip this if checkProject is True, assuming they never change under our feet.
    IdeOptions{ optCheckProject = doCheckProject } <- getIdeOptions
    checkProject <- liftIO $ doCheckProject
    ifaces <- if checkProject then return Nothing else runMaybeT $ do
        deps <- MaybeT $ sequence <$> uses GetDependencies files
        hiResults <- lift $ uses GetModIface (nubOrd $ foldMap transitiveModuleDeps deps)
        return $ map hirModIface $ catMaybes hiResults

    ShakeExtras{exportsMap} <- getShakeExtras
    let mguts = catMaybes results
        !exportsMap' = createExportsMapMg mguts
        !exportsMap'' = maybe mempty createExportsMap ifaces
    liftIO $ modifyVar_ exportsMap $ evaluate . (exportsMap'' <>) . (exportsMap' <>)

    liftIO $ progressUpdate KickCompleted

