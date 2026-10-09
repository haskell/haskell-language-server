{-# LANGUAGE DeriveAnyClass    #-}
{-# LANGUAGE NoPolyKinds       #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies      #-}
module Example where

import qualified Control.Concurrent            as C
import           Control.Monad.IO.Class        (liftIO)
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
