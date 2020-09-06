{-# LANGUAGE DeriveGeneric #-}

module IDE.Tactics where

import Refinery.Tactic
import GHC
import OccName
import GHC.Generics
import Data.Function
import Type


newtype CType = CType { unCType :: Type }

instance Eq CType where
  (==) = eqType `on` unCType

instance Ord CType where
  compare = nonDetCmpType `on` unCType


data Judgement = Judgement
  { jHoleId     :: Integer
  , jHypothesis :: [(OccName, CType)]
  , jGoal       :: CType
  }
  deriving (Eq, Ord, Generic)


data TacticError
  = UndefinedHypothesis OccName
  | GoalMismatch String CType
  | UnsolvedSubgoals [Judgement]


type TacticsM = TacticT Judgement (HsType GhcTc) (ProvableT Judgement (Either TacticError))
type RuleM    = RuleT Judgement (HsType GhcTc) (ProvableT Judgement (Either TacticError))

