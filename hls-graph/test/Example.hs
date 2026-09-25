{-# LANGUAGE DeriveAnyClass    #-}
{-# LANGUAGE NoPolyKinds       #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies      #-}
module Example where

import qualified Control.Concurrent            as C
import           Control.Monad                 (when)
import           Control.Monad.IO.Class        (liftIO)
import           Data.Maybe                    (isJust)
import           Development.IDE.Graph
import           Development.IDE.Graph.Classes
import           Development.IDE.Graph.Rule
import           GHC.Generics
import           Type.Reflection               (typeRep)

data Rule a = Rule
    deriving (Eq, Generic, Hashable, NFData)

instance Typeable a => Show (Rule a) where
    show Rule = show $ typeRep @a

type instance RuleResult (Rule a) = a

ruleStep :: Rules ()
ruleStep = addRule $ \(Rule :: Rule ()) _old mode -> do
    case mode of
        RunDependenciesChanged -> return $ RunResult ChangedRecomputeSame "" () (return ())
        RunDependenciesSame -> return $ RunResult ChangedNothing "" () (return ())

ruleUnit :: Rules ()
ruleUnit = addRule $ \(Rule :: Rule ()) _old _mode -> do
    return $ RunResult ChangedRecomputeDiff "" () (return ())

-- | Depends on Rule @()
ruleBool :: Rules ()
ruleBool = addRule $ \Rule _old _mode -> do
    () <- apply1 Rule
    return $ RunResult ChangedRecomputeDiff "" True (return ())


data CondRule = CondRule
    deriving (Eq, Generic, Hashable, NFData, Show)
type instance RuleResult CondRule = Bool


ruleCond :: C.MVar Bool -> Rules ()
ruleCond mv = addRule $ \CondRule _old _mode -> do
    r <- liftIO $ C.modifyMVar mv $ \x -> return (not x, x)
    return $ RunResult ChangedRecomputeDiff "" r (return ())

data BranchedRule = BranchedRule
    deriving (Eq, Generic, Hashable, NFData, Show)
type instance RuleResult BranchedRule = Int

ruleWithCond :: Rules ()
ruleWithCond = addRule $ \BranchedRule _old _mode -> do
    r <- apply1 CondRule
    if r then do
            _ <- apply1 SubBranchRule
            return $ RunResult ChangedRecomputeDiff "" (1 :: Int) (return ())
         else
            return $ RunResult ChangedRecomputeDiff "" (2 :: Int) (return ())

data SubBranchRule = SubBranchRule
    deriving (Eq, Generic, Hashable, NFData, Show)
type instance RuleResult SubBranchRule = Int

ruleSubBranch :: C.MVar Int -> Rules ()
ruleSubBranch mv = addRule $ \SubBranchRule _old _mode -> do
    r <- liftIO $ C.modifyMVar mv $ \x -> return (x+1, x)
    return $ RunResult ChangedRecomputeDiff "" r (return ())

data CountRule = CountRule
    deriving (Eq, Generic, Hashable, NFData, Show)
type instance RuleResult CountRule = Int

data CycleRule = CycleRule Int
    deriving (Eq, Generic, Hashable, NFData, Show)
type instance RuleResult CycleRule = Int

-- | @CycleRule 0@ applies @CycleRule 1@ and then itself, which closes a cycle.
-- Keep that order, so that 1 is 'Running' with an unforced thunk when 0 throws.
-- The other keys are leaves that run @leaf@ when they recompute.
ruleCycleAfterVictim :: IO () -> Rules ()
ruleCycleAfterVictim leaf = addRule $ \(CycleRule n) old _mode -> do
    when (n == 0) $ do
        _ :: [Int] <- apply [CycleRule 1, CycleRule 0]
        pure ()
    when (n == 1) $ do
        _ :: [Int] <- apply [CycleRule 2, CycleRule 3]
        pure ()
    when (n > 1 && isJust old) $ liftIO leaf
    return $ RunResult ChangedRecomputeDiff "" n (return ())
