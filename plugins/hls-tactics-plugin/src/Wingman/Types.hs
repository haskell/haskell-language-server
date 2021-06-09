{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

{-# OPTIONS_GHC -fno-warn-orphans #-}

module Wingman.Types
  ( module Wingman.Types
  , module Wingman.Debug
  , OccName
  , Name
  , Type
  , TyVar
  , Span
  ) where

import           ConLike (ConLike)
import           Control.Lens hiding (Context)
import           Control.Monad.Reader
import           Control.Monad.State
import           Data.Coerce
import           Data.Function
import           Data.Generics.Product (field)
import           Data.List.NonEmpty (NonEmpty (..))
import           Data.Semigroup
import           Data.Set (Set)
import           Data.Text (Text)
import qualified Data.Text as T
import           Data.Tree
import           Development.IDE (Range)
import           Development.IDE.Core.UseStale
import           Development.IDE.GHC.Compat hiding (Node)
import           Development.IDE.GHC.Orphans ()
import           FamInstEnv (FamInstEnvs)
import           GHC.Generics
import           GHC.SourceGen (var)
import           InstEnv (InstEnvs(..))
import           OccName
import           Refinery.Tactic
import           System.IO.Unsafe (unsafePerformIO)
import           Type (TCvSubst, Var, eqType, nonDetCmpType, emptyTCvSubst)
import           UniqSupply (takeUniqFromSupply, mkSplitUniqSupply, UniqSupply)
import           Unique (nonDetCmpUnique, Uniquable, getUnique, Unique)
import           Wingman.Debug


------------------------------------------------------------------------------
-- | The list of tactics exposed to the outside world. These are attached to
-- actual tactics via 'commandTactic' and are contextually provided to the
-- editor via 'commandProvider'.
data TacticCommand
  = Auto
  | Intros
  | Destruct
  | DestructPun
  | Homomorphism
  | DestructLambdaCase
  | HomomorphismLambdaCase
  | DestructAll
  | UseDataCon
  | Refine
  | BeginMetaprogram
  | RunMetaprogram
  deriving (Eq, Ord, Show, Enum, Bounded)

-- | Generate a title for the command.
tacticTitle :: TacticCommand -> T.Text -> T.Text
tacticTitle = (mappend "Wingman: " .) . go
  where
    go Auto _                   = "Attempt to fill hole"
    go Intros _                 = "Introduce lambda"
    go Destruct var             = "Case split on " <> var
    go DestructPun var          = "Split on " <> var <> " with NamedFieldPuns"
    go Homomorphism var         = "Homomorphic case split on " <> var
    go DestructLambdaCase _     = "Lambda case split"
    go HomomorphismLambdaCase _ = "Homomorphic lambda case split"
    go DestructAll _            = "Split all function arguments"
    go UseDataCon dcon          = "Use constructor " <> dcon
    go Refine _                 = "Refine hole"
    go BeginMetaprogram _       = "Use custom tactic block"
    go RunMetaprogram _         = "Run custom tactic"


------------------------------------------------------------------------------
-- | Plugin configuration for tactics
data Config = Config
  { cfg_max_use_ctor_actions :: Int
  , cfg_timeout_seconds      :: Int
  , cfg_auto_gas             :: Int
  }
  deriving (Eq, Ord, Show)

emptyConfig :: Config
emptyConfig = Config
  { cfg_max_use_ctor_actions = 5
  , cfg_timeout_seconds = 2
  , cfg_auto_gas = 4
  }

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

instance Show Name where
  show  = unsafeRender

instance Show Type where
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

instance Show (HsExpr GhcTc) where
  show  = unsafeRender

instance Show (HsDecl GhcPs) where
  show  = unsafeRender

instance Show (Pat GhcPs) where
  show  = unsafeRender

instance Show (LHsSigType GhcPs) where
  show  = unsafeRender

instance Show TyCon where
  show  = unsafeRender

instance Show ConLike where
  show  = unsafeRender


------------------------------------------------------------------------------
-- | The state that should be shared between subgoals. Extracts move towards
-- the root, judgments move towards the leaves, and the state moves *sideways*.
data TacticState = TacticState
    { ts_skolems         :: !(Set TyVar)
      -- ^ The known skolems.
    , ts_unifier         :: !TCvSubst
    , ts_unique_gen :: !UniqSupply
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
    , ts_unique_gen      = unsafeDefaultUniqueSupply
    }


------------------------------------------------------------------------------
-- | Generate a new 'Unique'
freshUnique :: MonadState TacticState m => m Unique
freshUnique = do
  (uniq, supply) <- gets $ takeUniqFromSupply . ts_unique_gen
  modify' $! field @"ts_unique_gen" .~ supply
  pure uniq


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
      Int       -- ^ of how many arguments total?
    -- | A binding created in a pattern match.
  | PatternMatchPrv PatVal
    -- | A class method from the given context.
  | ClassMethodPrv
      (Uniquely Class)     -- ^ Class
    -- | A binding explicitly written by the user.
  | UserPrv
    -- | A binding explicitly imported by the user.
  | ImportPrv
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
  , pv_datacon   :: Uniquely ConLike
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


-- NOTE(sandy): The usage of list here is mostly for convenience, but if it's
-- ever changed, make sure to correspondingly update
-- 'jAcceptableDestructTargets' so that it correctly identifies newly
-- introduced terms.
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
    deriving newtype (Functor, Applicative, Monad, MonadReader Context)

------------------------------------------------------------------------------
-- | Orphan instance for producing holes when attempting to solve tactics.
instance MonadExtract (Synthesized (LHsExpr GhcPs)) ExtractM where
  hole = pure . pure . noLoc $ var "_"


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
  | TacticPanic String
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
    show (TacticPanic err) =
      "PANIC: " <> err


------------------------------------------------------------------------------
type TacticsM  = TacticT Judgement (Synthesized (LHsExpr GhcPs)) TacticError TacticState ExtractM
type RuleM     = RuleT Judgement (Synthesized (LHsExpr GhcPs)) TacticError TacticState ExtractM
type Rule      = RuleM (Synthesized (LHsExpr GhcPs))

type Trace = Rose String

------------------------------------------------------------------------------
-- | The extract for refinery. Represents a "synthesized attribute" in the
-- context of attribute grammars. In essence, 'Synthesized' describes
-- information we'd like to pass from leaves of the tactics search upwards.
-- This includes the actual AST we've generated (in 'syn_val').
data Synthesized a = Synthesized
  { syn_trace  :: Trace
    -- ^ A tree describing which tactics were used produce the 'syn_val'.
    -- Mainly for debugging when you get the wrong answer, to see the other
    -- things it tried.
  , syn_scoped :: Hypothesis CType
    -- ^ All of the bindings created to produce the 'syn_val'.
  , syn_used_vals :: Set OccName
    -- ^ The values used when synthesizing the 'syn_val'.
  , syn_recursion_count :: Sum Int
    -- ^ The number of recursive calls
  , syn_val    :: a
  }
  deriving (Eq, Show, Functor, Foldable, Traversable, Generic)

mapTrace :: (Trace -> Trace) -> Synthesized a -> Synthesized a
mapTrace f (Synthesized tr sc uv rc a) = Synthesized (f tr) sc uv rc a


------------------------------------------------------------------------------
-- | This might not be lawful, due to the semigroup on 'Trace' maybe not being
-- lawful. But that's only for debug output, so it's not anything I'm concerned
-- about.
instance Applicative Synthesized where
  pure = Synthesized mempty mempty mempty mempty
  Synthesized tr1 sc1 uv1 rc1 f <*> Synthesized tr2 sc2 uv2 rc2 a =
    Synthesized (tr1 <> tr2) (sc1 <> sc2) (uv1 <> uv2) (rc1 <> rc2) $ f a


------------------------------------------------------------------------------
-- | The Reader context of tactics and rules
data Context = Context
  { ctxDefiningFuncs :: [(OccName, CType)]
    -- ^ The functions currently being defined
  , ctxModuleFuncs   :: [(OccName, CType)]
    -- ^ Everything defined in the current module
  , ctxConfig        :: Config
  , ctxKnownThings   :: KnownThings
  , ctxInstEnvs      :: InstEnvs
  , ctxFamInstEnvs   :: FamInstEnvs
  , ctxTheta         :: Set CType
  }

instance Show Context where
  show (Context {..}) = mconcat
    [ "Context "
    , showsPrec 10 ctxDefiningFuncs ""
    , showsPrec 10 ctxModuleFuncs ""
    , showsPrec 10 ctxConfig ""
    , showsPrec 10 ctxTheta ""
    ]


------------------------------------------------------------------------------
-- | Things we'd like to look up, that don't exist in TysWiredIn.
data KnownThings = KnownThings
  { kt_semigroup :: Class
  , kt_monoid    :: Class
  }


------------------------------------------------------------------------------
-- | An empty context
emptyContext :: Context
emptyContext
  = Context
      { ctxDefiningFuncs = mempty
      , ctxModuleFuncs = mempty
      , ctxConfig = emptyConfig
      , ctxKnownThings = error "empty known things from emptyContext"
      , ctxFamInstEnvs = mempty
      , ctxInstEnvs = InstEnvs mempty mempty mempty
      , ctxTheta = mempty
      }


newtype Rose a = Rose (Tree a)
  deriving stock (Eq, Functor, Generic)

instance Show (Rose String) where
  show = unlines . dropEveryOther . lines . drawTree . coerce

dropEveryOther :: [a] -> [a]
dropEveryOther []           = []
dropEveryOther [a]          = [a]
dropEveryOther (a : _ : as) = a : dropEveryOther as

------------------------------------------------------------------------------
-- | This might not be lawful! I didn't check, and it feels sketchy.
instance (Eq a, Monoid a) => Semigroup (Rose a) where
  Rose (Node a as) <> Rose (Node b bs) = Rose $ Node (a <> b) (as <> bs)
  sconcat (a :| as) = rose mempty $ a : as

instance (Eq a, Monoid a) => Monoid (Rose a) where
  mempty = Rose $ Node mempty mempty

rose :: (Eq a, Monoid a) => a -> [Rose a] -> Rose a
rose a [Rose (Node a' rs)] | a' == mempty = Rose $ Node a rs
rose a rs = Rose $ Node a $ coerce rs


------------------------------------------------------------------------------
-- | The results of 'Wingman.Machinery.runTactic'
data RunTacticResults = RunTacticResults
  { rtr_trace       :: Trace
  , rtr_extract     :: LHsExpr GhcPs
  , rtr_subgoals    :: [Judgement]
  , rtr_other_solns :: [Synthesized (LHsExpr GhcPs)]
  , rtr_jdg         :: Judgement
  , rtr_ctx         :: Context
  } deriving Show


data AgdaMatch = AgdaMatch
  { amPats :: [Pat GhcPs]
  , amBody :: HsExpr GhcPs
  }
  deriving (Show)


data UserFacingMessage
  = TacticErrors
  | TimedOut
  | NothingToDo
  | InfrastructureError Text
  deriving Eq

instance Show UserFacingMessage where
  show TacticErrors            = "Wingman couldn't find a solution"
  show TimedOut                = "Wingman timed out while trying to find a solution"
  show NothingToDo             = "Nothing to do"
  show (InfrastructureError t) = "Internal error: " <> T.unpack t


data HoleSort = Hole | Metaprogram T.Text
  deriving (Eq, Ord, Show)

data HoleJudgment = HoleJudgment
  { hj_range     :: Tracked 'Current Range
  , hj_jdg       :: Judgement
  , hj_ctx       :: Context
  , hj_dflags    :: DynFlags
  , hj_hole_sort :: HoleSort
  }

