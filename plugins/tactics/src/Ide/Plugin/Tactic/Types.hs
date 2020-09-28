{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE OverloadedStrings #-}

module Ide.Plugin.Tactic.Types where

import qualified Data.Map as M
import Data.Map (Map)
import qualified Data.Set as S
import Data.Set (Set)
import qualified Data.Text as T
import           Type
import           Data.Function
import GHC.Generics
import OccName
import Ide.Plugin.Tactic.Debug
import Development.IDE.GHC.Compat
import Refinery.Tactic (RuleT)
import Refinery.Tactic.Internal
import Control.Monad.Reader

------------------------------------------------------------------------------
-- | The list of tactics exposed to the outside world. These are attached to
-- actual tactics via 'commandTactic' and are contextually provided to the
-- editor via 'commandProvider'.
data TacticCommand
  = Auto
  | Split
  | Intro
  | Intros
  | Destruct
  | Homomorphism
  deriving (Eq, Ord, Show, Enum, Bounded)

-- | Generate a title for the command.
tacticTitle :: TacticCommand -> T.Text -> T.Text
tacticTitle Auto _ = "Auto"
tacticTitle Split _ = "Auto"
tacticTitle Intro _ = "Intro"
tacticTitle Intros _ = "Introduce lambda"
tacticTitle Destruct var = "Case split on " <> var
tacticTitle Homomorphism var = "Homomorphic case split on " <> var

------------------------------------------------------------------------------
-- | A wrapper around 'Type' which supports equality and ordering.
newtype CType = CType { unCType :: Type }

instance Eq CType where
  (==) = eqType `on` unCType

instance Ord CType where
  compare = nonDetCmpType `on` unCType


------------------------------------------------------------------------------
data TacticState = TacticState
    { ts_skolems :: [TyVar]
    , ts_unifier :: TCvSubst
    }

instance Semigroup TacticState where
  TacticState a1 b1 <> TacticState a2 b2
    = TacticState (a1 <> a2) (unionTCvSubst b1 b2)

instance Monoid TacticState where
  mempty = TacticState mempty emptyTCvSubst

------------------------------------------------------------------------------
-- | The current bindings and goal for a hole to be filled by refinery.
data Judgement' a = Judgement
  { _jHypothesis :: Map OccName a
  , _jDestructed :: Set OccName
  , _jGoal       :: a
  }
  deriving stock (Eq, Ord, Generic, Functor)

type Judgement = Judgement' CType

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
  deriving stock (Eq, Ord)

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
      "Aleady destructed " <> unsafeRender name


------------------------------------------------------------------------------
type ProvableM = ProvableT Judgement (Reader Context)
type TacticsM  = TacticT Judgement (LHsExpr GhcPs) TacticError TacticState ProvableM
type RuleM     = RuleT Judgement (LHsExpr GhcPs) TacticError TacticState ProvableM
type Rule      = RuleM (LHsExpr GhcPs)


------------------------------------------------------------------------------
-- | The Reader context of tactics and rules
data Context = Context
  { ctxDefiningFuncs :: [(OccName, CType)]
    -- ^ The functions currently being defined
  }
  deriving stock (Eq, Ord)

