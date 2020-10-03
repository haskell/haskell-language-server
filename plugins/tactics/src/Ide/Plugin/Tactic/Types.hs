{-# LANGUAGE DeriveFunctor                  #-}
{-# LANGUAGE DeriveGeneric                  #-}
{-# LANGUAGE DerivingStrategies             #-}
{-# LANGUAGE FlexibleInstances              #-}
{-# LANGUAGE GeneralizedNewtypeDeriving     #-}
{-# LANGUAGE MultiParamTypeClasses          #-}
{-# LANGUAGE TypeSynonymInstances           #-}

module Ide.Plugin.Tactic.Types
  ( module Ide.Plugin.Tactic.Types
  , module Ide.Plugin.Tactic.Debug
  , OccName
  , Name
  , Type
  , TyVar
  , Span
  , Range
  ) where

import Control.Monad.Reader
import Data.Function
import Data.Map (Map)
import Data.Set (Set)
import Development.IDE.GHC.Compat
import Development.IDE.Types.Location
import GHC.Generics
import Ide.Plugin.Tactic.Debug
import OccName
import Refinery.Tactic
import Type


------------------------------------------------------------------------------
-- | A wrapper around 'Type' which supports equality and ordering.
newtype CType = CType { unCType :: Type }

instance Eq CType where
  (==) = eqType `on` unCType

instance Ord CType where
  compare = nonDetCmpType `on` unCType


------------------------------------------------------------------------------
data TacticState = TacticState
    { ts_skolems   :: !([TyVar])
    , ts_unifier   :: !(TCvSubst)
    , ts_used_vals :: !(Set OccName)
    }

instance Semigroup TacticState where
  TacticState a1 b1 c1 <> TacticState a2 b2 c2
    = TacticState
        (a1 <> a2)
        (unionTCvSubst b1 b2)
        (c1 <> c2)

instance Monoid TacticState where
  mempty = TacticState mempty emptyTCvSubst mempty


withUsedVals :: (Set OccName -> Set OccName) -> TacticState -> TacticState
withUsedVals f ts = ts
  { ts_used_vals = f $ ts_used_vals ts
  }


------------------------------------------------------------------------------
-- | The current bindings and goal for a hole to be filled by refinery.
data Judgement' a = Judgement
  { _jHypothesis  :: !(Map OccName a)
  , _jDestructed  :: !(Set OccName)
    -- ^ These should align with keys of _jHypothesis
  , _jPatternVals :: !(Set OccName)
    -- ^ These should align with keys of _jHypothesis
  , _jGoal        :: !(a)
  }
  deriving stock (Eq, Ord, Generic, Functor)

type Judgement = Judgement' CType


newtype ExtractM a = ExtractM { unExtractM :: Reader Context a }
    deriving (Functor, Applicative, Monad, MonadReader Context)

------------------------------------------------------------------------------
-- | Orphan instance for producing holes when attempting to solve tactics.
instance MonadExtract (LHsExpr GhcPs) ExtractM where
  hole = pure $ noLoc $ HsVar noExtField $ noLoc $ Unqual $ mkVarOcc "_"


------------------------------------------------------------------------------
-- | Reasons a tactic might fail.
data TacticError
  = UndefinedHypothesis OccName
  | GoalMismatch String CType
  | UnsolvedSubgoals [Judgement]
  | UnificationError CType CType
  | NoProgress
  | NoApplicableTactic
  | AlreadyDestructed OccName
  | IncorrectDataCon DataCon
  deriving stock (Eq)

instance Show TacticError where
    show (UndefinedHypothesis name) =
      occNameString name <> " is not available in the hypothesis."
    show (GoalMismatch tac (CType typ)) =
      mconcat
        [ "The tactic "
        , tac
        , " doesn't apply to goal type "
        , unsafeRender typ
        ]
    show (UnsolvedSubgoals _) =
      "There were unsolved subgoals"
    show (UnificationError (CType t1) (CType t2)) =
        mconcat
          [ "Could not unify "
          , unsafeRender t1
          , " and "
          , unsafeRender t2
          ]
    show NoProgress =
      "Unable to make progress"
    show NoApplicableTactic =
      "No tactic could be applied"
    show (AlreadyDestructed name) =
      "Already destructed " <> unsafeRender name
    show (IncorrectDataCon dcon) =
      "Data con doesn't align with goal type (" <> unsafeRender dcon <> ")"


------------------------------------------------------------------------------
type TacticsM  = TacticT Judgement (LHsExpr GhcPs) TacticError TacticState ExtractM
type RuleM     = RuleT Judgement (LHsExpr GhcPs) TacticError TacticState ExtractM
type Rule      = RuleM (LHsExpr GhcPs)


------------------------------------------------------------------------------
-- | The Reader context of tactics and rules
data Context = Context
  { ctxDefiningFuncs :: [(OccName, CType)]
    -- ^ The functions currently being defined
  , ctxModuleFuncs :: [(OccName, CType)]
    -- ^ Everything defined in the current module
  }
  deriving stock (Eq, Ord)

