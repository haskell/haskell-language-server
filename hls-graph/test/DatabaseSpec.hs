{-# LANGUAGE OverloadedStrings #-}

module DatabaseSpec where

import           Development.IDE.Graph                 (shakeOptions)
import           Development.IDE.Graph.Database        (shakeNewDatabase,
                                                        shakeRunDatabase)
import           Development.IDE.Graph.Internal.Action (apply1)
import           Development.IDE.Graph.Internal.Rules  (addRule)
import           Development.IDE.Graph.Internal.Types
import           Example
import           System.Time.Extra                     (timeout)
import           Test.Hspec

spec :: Spec
spec = do
    describe "Evaluation" $ do
        it "detects cycles" $ do
            db <- shakeNewDatabase shakeOptions $ do
                ruleBool
                addRule $ \Rule _old _mode -> do
                    True <- apply1 (Rule @Bool)
                    return $ RunResult ChangedRecomputeDiff "" () (return ())
            let res = shakeRunDatabase db $ pure $ apply1 (Rule @())
            timeout 1 res `shouldThrow` \StackException{} -> True
