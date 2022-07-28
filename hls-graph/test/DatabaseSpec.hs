{-# LANGUAGE OverloadedLists     #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}
module DatabaseSpec where

import           Control.Concurrent.STM
import           Development.IDE.Graph                 (shakeOptions)
import           Development.IDE.Graph.Database        (shakeNewDatabase,
                                                        shakeRunDatabase)
import           Development.IDE.Graph.Internal.Action (apply1)
import           Development.IDE.Graph.Internal.Types
import           Development.IDE.Graph.Rule
import           Example
import qualified StmContainers.Map                     as STM
import           System.Time.Extra                     (timeout)
import           Test.Hspec

spec :: Spec
spec = do
    describe "Evaluation" $ do
        it "detects cycles" $ do
            db <- shakeNewDatabase shakeOptions $ do
                ruleBool
                addRule $ \Rule old mode -> do
                    True <- apply1 (Rule @Bool)
                    return $ RunResult ChangedRecomputeDiff "" ()
            let res = shakeRunDatabase db $ pure $ apply1 (Rule @())
            timeout 1 res `shouldThrow` \StackException{} -> True
