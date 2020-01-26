-- Copyright (c) 2019 The DAML Authors. All rights reserved.
-- SPDX-License-Identifier: Apache-2.0

{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleInstances #-}

-- | Utilities and state for the files of interest - those which are currently
--   open in the editor. The useful function is 'getFilesOfInterest'.
module Development.IDE.Core.OfInterest(
    ofInterestRules,
    getFilesOfInterest, setFilesOfInterest, modifyFilesOfInterest,
    ) where

import Control.Concurrent.Extra
import Data.Binary
import Data.Hashable
import Control.DeepSeq
import GHC.Generics
import Data.Typeable
import qualified Data.ByteString.UTF8 as BS
import Control.Exception
import Data.Set (Set)
import qualified Data.Set as Set
import qualified Data.Text as T
import Data.Tuple.Extra
import Data.Functor
import Development.Shake

import Development.IDE.Types.Location
import Development.IDE.Types.Logger
import Development.IDE.Core.Shake


newtype OfInterestVar = OfInterestVar (Var (Set NormalizedFilePath))
instance IsIdeGlobal OfInterestVar

type instance RuleResult GetFilesOfInterest = Set NormalizedFilePath

data GetFilesOfInterest = GetFilesOfInterest
    deriving (Eq, Show, Typeable, Generic)
instance Hashable GetFilesOfInterest
instance NFData   GetFilesOfInterest
instance Binary   GetFilesOfInterest


-- | The rule that initialises the files of interest state.
ofInterestRules :: Rules ()
ofInterestRules = do
    addIdeGlobal . OfInterestVar =<< liftIO (newVar Set.empty)
    defineEarlyCutoff $ \GetFilesOfInterest _file -> assert (null $ fromNormalizedFilePath _file) $ do
        alwaysRerun
        filesOfInterest <- getFilesOfInterestUntracked
        pure (Just $ BS.fromString $ show filesOfInterest, ([], Just filesOfInterest))


-- | Get the files that are open in the IDE.
getFilesOfInterest :: Action (Set NormalizedFilePath)
getFilesOfInterest = useNoFile_ GetFilesOfInterest



------------------------------------------------------------
-- Exposed API

-- | Set the files-of-interest - not usually necessary or advisable.
--   The LSP client will keep this information up to date.
setFilesOfInterest :: IdeState -> Set NormalizedFilePath -> IO ()
setFilesOfInterest state files = modifyFilesOfInterest state (const files)

getFilesOfInterestUntracked :: Action (Set NormalizedFilePath)
getFilesOfInterestUntracked = do
    OfInterestVar var <- getIdeGlobalAction
    liftIO $ readVar var

-- | Modify the files-of-interest - not usually necessary or advisable.
--   The LSP client will keep this information up to date.
modifyFilesOfInterest :: IdeState -> (Set NormalizedFilePath -> Set NormalizedFilePath) -> IO ()
modifyFilesOfInterest state f = do
    OfInterestVar var <- getIdeGlobalState state
    files <- modifyVar var $ pure . dupe . f
    logDebug (ideLogger state) $ "Set files of interest to: " <> T.pack (show $ Set.toList files)
    void $ shakeRun state []
