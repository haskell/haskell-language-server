{-# LANGUAGE DeriveFunctor              #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE DerivingVia                #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE TypeApplications           #-}
{-# OPTIONS_GHC -fno-warn-orphans       #-}

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

import           Control.Lens                   hiding (Context, (.=))
import           Control.Monad.Reader
import           Control.Monad.State
import           Data.Coerce
import           Data.Function
import           Data.Generics.Product          (field)
import           Data.Set                       (Set)
import           Data.Tree
import           Development.IDE.GHC.Compat     hiding (Node)
import           Development.IDE.GHC.Orphans    ()
import           Development.IDE.Types.Location
import           GHC.Generics
import           Ide.Plugin.Tactic.Debug
import           Ide.Plugin.Tactic.FeatureSet   (FeatureSet)
import           OccName
import           Refinery.Tactic
import           System.IO.Unsafe               (unsafePerformIO)
import           Type
import           UniqSupply                     (UniqSupply, mkSplitUniqSupply,
                                                 takeUniqFromSupply)
import           Unique                         (Uniquable, Unique, getUnique,
                                                 nonDetCmpUnique)


------------------------------------------------------------------------------
-- | A wrapper around 'Type' which supports equality and ordering.
newtype CType = CType { unCType :: Type }

instance Eq CType where
  (==) = eqType `on` unCType

instance Ord CType where
  compare = nonDetCmpType `on` unCType

instance Show CType where
  show  = unsafeRender . unCType

instance Show OccName where
  show  = unsafeRender

instance Show Var where
  show  = unsafeRender

instance Show TCvSubst where
  show  = unsafeRender

instance Show DataCon where
  show  = unsafeRender

instance Show Class where
  show  = unsafeRender

instance Show (HsExpr GhcPs) where
  show  = unsafeRender

instance Show (Pat GhcPs) where
  show  = unsafeRender


------------------------------------------------------------------------------
data TacticState = TacticState
    { ts_skolems         :: !(Set TyVar)
      -- ^ The known skolems.
    , ts_unifier         :: !TCvSubst
      -- ^ The current substitution of univars.
    , ts_used_vals       :: !(Set OccName)
      -- ^ Set of values used by tactics.
    , ts_intro_vals      :: !(Set OccName)
      -- ^ Set of values introduced by tactics.
    , ts_unused_top_vals :: !(Set OccName)
      -- ^ Set of currently unused arguments to the function being defined.
    , ts_recursion_stack :: ![Maybe PatVal]
      -- ^ Stack for tracking whether or not the current recursive call has
      -- used at least one smaller pat val. Recursive calls for which this
      -- value is 'False' are guaranteed to loop, and must be pruned.
    , ts_recursion_count :: !Int
      -- ^ Number of calls to recursion. We penalize each.
    , ts_unique_gen      :: !UniqSupply
    } deriving stock (Show, Generic)

instance Show UniqSupply where
  show _ = "<uniqsupply>"


------------------------------------------------------------------------------
-- | A 'UniqSupply' to use in 'defaultTacticState'
unsafeDefaultUniqueSupply :: UniqSupply
unsafeDefaultUniqueSupply =
  unsafePerformIO $ mkSplitUniqSupply '🚒'
{-# NOINLINE unsafeDefaultUniqueSupply #-}


defaultTacticState :: TacticState
defaultTacticState =
  TacticState
    { ts_skolems         = mempty
    , ts_unifier         = emptyTCvSubst
    , ts_used_vals       = mempty
    , ts_intro_vals      = mempty
    , ts_unused_top_vals = mempty
    , ts_recursion_stack = mempty
    , ts_recursion_count = 0
    , ts_unique_gen      = unsafeDefaultUniqueSupply
    }


------------------------------------------------------------------------------
-- | Generate a new 'Unique'
freshUnique :: MonadState TacticState m => m Unique
freshUnique = do
  (uniq, supply) <- gets $ takeUniqFromSupply . ts_unique_gen
  modify' $! field @"ts_unique_gen" .~ supply
  pure uniq


withRecursionStack
  :: ([Maybe PatVal] -> [Maybe PatVal]) -> TacticState -> TacticState
withRecursionStack f =
  field @"ts_recursion_stack" %~ f

pushRecursionStack :: TacticState -> TacticState
pushRecursionStack = withRecursionStack (Nothing :)

popRecursionStack :: TacticState -> TacticState
popRecursionStack = withRecursionStack tail


withUsedVals :: (Set OccName -> Set OccName) -> TacticState -> TacticState
withUsedVals f =
  field @"ts_used_vals" %~ f


withIntroducedVals :: (Set OccName -> Set OccName) -> TacticState -> TacticState
withIntroducedVals f =
  field @"ts_intro_vals" %~ f


------------------------------------------------------------------------------
-- | Describes where hypotheses came from. Used extensively to prune stupid
-- solutions from the search space.
data Provenance
  = -- | An argument given to the topmost function that contains the current
    -- hole. Recursive calls are restricted to values whose provenance lines up
    -- with the same argument.
    TopLevelArgPrv
      OccName   -- ^ Binding function
      Int       -- ^ Argument Position
    -- | A binding created in a pattern match.
  | PatternMatchPrv PatVal
    -- | A class method from the given context.
  | ClassMethodPrv
      (Uniquely Class)     -- ^ Class
    -- | A binding explicitly written by the user.
  | UserPrv
    -- | The recursive hypothesis. Present only in the context of the recursion
    -- tactic.
  | RecursivePrv
    -- | A hypothesis which has been disallowed for some reason. It's important
    -- to keep these in the hypothesis set, rather than filtering it, in order
    -- to continue tracking downstream provenance.
  | DisallowedPrv DisallowReason Provenance
  deriving stock (Eq, Show, Generic, Ord)


------------------------------------------------------------------------------
-- | Why was a hypothesis disallowed?
data DisallowReason
  = WrongBranch Int
  | Shadowed
  | RecursiveCall
  | AlreadyDestructed
  deriving stock (Eq, Show, Generic, Ord)


------------------------------------------------------------------------------
-- | Provenance of a pattern value.
data PatVal = PatVal
  { pv_scrutinee :: Maybe OccName
    -- ^ Original scrutinee which created this PatVal. Nothing, for lambda
    -- case.
  , pv_ancestry  :: Set OccName
    -- ^ The set of values which had to be destructed to discover this term.
    -- Always contains the scrutinee.
  , pv_datacon   :: Uniquely DataCon
    -- ^ The datacon which introduced this term.
  , pv_position  :: Int
    -- ^ The position of this binding in the datacon's arguments.
  } deriving stock (Eq, Show, Generic, Ord)


------------------------------------------------------------------------------
-- | A wrapper which uses a 'Uniquable' constraint for providing 'Eq' and 'Ord'
-- instances.
newtype Uniquely a = Uniquely { getViaUnique :: a }
  deriving Show via a

instance Uniquable a => Eq (Uniquely a) where
  (==) = (==) `on` getUnique . getViaUnique

instance Uniquable a => Ord (Uniquely a) where
  compare = nonDetCmpUnique `on` getUnique . getViaUnique


newtype Hypothesis a = Hypothesis
  { unHypothesis :: [HyInfo a]
  }
  deriving stock (Functor, Eq, Show, Generic, Ord)
  deriving newtype (Semigroup, Monoid)


------------------------------------------------------------------------------
-- | The provenance and type of a hypothesis term.
data HyInfo a = HyInfo
  { hi_name       :: OccName
  , hi_provenance :: Provenance
  , hi_type       :: a
  }
  deriving stock (Functor, Eq, Show, Generic, Ord)


------------------------------------------------------------------------------
-- | Map a function over the provenance.
overProvenance :: (Provenance -> Provenance) -> HyInfo a -> HyInfo a
overProvenance f (HyInfo name prv ty) = HyInfo name (f prv) ty


------------------------------------------------------------------------------
-- | The current bindings and goal for a hole to be filled by refinery.
data Judgement' a = Judgement
  { _jHypothesis        :: !(Hypothesis a)
  , _jBlacklistDestruct :: !Bool
  , _jWhitelistSplit    :: !Bool
  , _jIsTopHole         :: !Bool
  , _jGoal              :: !a
  }
  deriving stock (Eq, Generic, Functor, Show)

type Judgement = Judgement' CType


newtype ExtractM a = ExtractM { unExtractM :: Reader Context a }
    deriving (Functor, Applicative, Monad, MonadReader Context)

------------------------------------------------------------------------------
-- | Orphan instance for producing holes when attempting to solve tactics.
instance MonadExtract (Trace, LHsExpr GhcPs) ExtractM where
  hole = pure (mempty, noLoc $ HsVar noExtField $ noLoc $ Unqual $ mkVarOcc "_")


------------------------------------------------------------------------------
-- | Reasons a tactic might fail.
data TacticError
  = UndefinedHypothesis OccName
  | GoalMismatch String CType
  | UnsolvedSubgoals [Judgement]
  | UnificationError CType CType
  | NoProgress
  | NoApplicableTactic
  | IncorrectDataCon DataCon
  | RecursionOnWrongParam OccName Int OccName
  | UnhelpfulDestruct OccName
  | UnhelpfulSplit OccName
  | TooPolymorphic
  | NotInScope OccName
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
    show (IncorrectDataCon dcon) =
      "Data con doesn't align with goal type (" <> unsafeRender dcon <> ")"
    show (RecursionOnWrongParam call p arg) =
      "Recursion on wrong param (" <> show call <> ") on arg"
        <> show p <> ": " <> show arg
    show (UnhelpfulDestruct n) =
      "Destructing patval " <> show n <> " leads to no new types"
    show (UnhelpfulSplit n) =
      "Splitting constructor " <> show n <> " leads to no new goals"
    show TooPolymorphic =
      "The tactic isn't applicable because the goal is too polymorphic"
    show (NotInScope name) =
      "Tried to do something with the out of scope name " <> show name


------------------------------------------------------------------------------
type TacticsM  = TacticT Judgement (Trace, LHsExpr GhcPs) TacticError TacticState ExtractM
type RuleM     = RuleT Judgement (Trace, LHsExpr GhcPs) TacticError TacticState ExtractM
type Rule      = RuleM (Trace, LHsExpr GhcPs)

type Trace = Rose String


------------------------------------------------------------------------------
-- | The Reader context of tactics and rules
data Context = Context
  { ctxDefiningFuncs :: [(OccName, CType)]
    -- ^ The functions currently being defined
  , ctxModuleFuncs   :: [(OccName, CType)]
    -- ^ Everything defined in the current module
  , ctxFeatureSet    :: FeatureSet
  }
  deriving stock (Eq, Ord, Show)


------------------------------------------------------------------------------
-- | An empty context
emptyContext :: Context
emptyContext  = Context mempty mempty mempty


newtype Rose a = Rose (Tree a)
  deriving stock (Eq, Functor, Generic)

instance Show (Rose String) where
  show = unlines . dropEveryOther . lines . drawTree . coerce

dropEveryOther :: [a] -> [a]
dropEveryOther []           = []
dropEveryOther [a]          = [a]
dropEveryOther (a : _ : as) = a : dropEveryOther as

instance Semigroup a => Semigroup (Rose a) where
  Rose (Node a as) <> Rose (Node b bs) = Rose $ Node (a <> b) (as <> bs)

instance Monoid a => Monoid (Rose a) where
  mempty = Rose $ Node mempty mempty

rose :: (Eq a, Monoid a) => a -> [Rose a] -> Rose a
rose a [Rose (Node a' rs)] | a' == mempty = Rose $ Node a rs
rose a rs = Rose $ Node a $ coerce rs


------------------------------------------------------------------------------
-- | The results of 'Ide.Plugin.Tactic.Machinery.runTactic'
data RunTacticResults = RunTacticResults
  { rtr_trace       :: Trace
  , rtr_extract     :: LHsExpr GhcPs
  , rtr_other_solns :: [(Trace, LHsExpr GhcPs)]
  , rtr_jdg         :: Judgement
  , rtr_ctx         :: Context
  } deriving Show


data AgdaMatch = AgdaMatch
  { amPats :: [Pat GhcPs]
  , amBody :: HsExpr GhcPs
  }
  deriving (Show)

