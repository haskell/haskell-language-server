{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE RecordWildCards      #-}
{-# LANGUAGE UndecidableInstances #-}

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

import           Control.Lens hiding (Context)
import           Control.Monad.Reader
import           Control.Monad.State
import qualified Control.Monad.State.Strict as Strict
import           Data.Coerce
import           Data.Function
import           Data.Generics (mkM, everywhereM, Data, Typeable)
import           Data.Generics.Labels ()
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
import qualified Development.IDE.GHC.Compat.Util as Util
import           Development.IDE.GHC.Orphans ()
import           GHC.Exts (fromString)
import           GHC.Generics
import           GHC.SourceGen (var)
import           Refinery.ProofState
import           Refinery.Tactic.Internal (TacticT(TacticT), RuleT (RuleT))
import           System.IO.Unsafe (unsafePerformIO)
import           Wingman.Debug
import           Data.IORef


------------------------------------------------------------------------------
-- | The list of tactics exposed to the outside world. These are attached to
-- actual tactics via 'commandTactic' and are contextually provided to the
-- editor via 'commandProvider'.
data TacticCommand
  = Intros
  | IntroAndDestruct
  | Destruct
  | DestructPun
  | Homomorphism
  | DestructLambdaCase
  | HomomorphismLambdaCase
  | DestructAll
  | UseDataCon
  | Refine
  deriving (Eq, Ord, Show, Enum, Bounded)

-- | Generate a title for the command.
tacticTitle :: TacticCommand -> T.Text -> T.Text
tacticTitle = (mappend "Wingman: " .) . go
  where
    go Intros _                 = "Introduce lambda"
    go IntroAndDestruct _       = "Introduce and destruct term"
    go Destruct var             = "Case split on " <> var
    go DestructPun var          = "Split on " <> var <> " with NamedFieldPuns"
    go Homomorphism var         = "Homomorphic case split on " <> var
    go DestructLambdaCase _     = "Lambda case split"
    go HomomorphismLambdaCase _ = "Homomorphic lambda case split"
    go DestructAll _            = "Split all function arguments"
    go UseDataCon dcon          = "Use constructor " <> dcon
    go Refine _                 = "Refine hole"


------------------------------------------------------------------------------
-- | Plugin configuration for tactics
data Config = Config
  { cfg_max_use_ctor_actions :: Int
  , cfg_timeout_seconds      :: Int
  }
  deriving (Eq, Ord, Show)

emptyConfig :: Config
emptyConfig = Config
  { cfg_max_use_ctor_actions = 5
  , cfg_timeout_seconds = 2
  }

------------------------------------------------------------------------------
-- | A wrapper around 'Type' which supports equality and ordering.
newtype CType = CType { unCType :: Type }
  deriving stock (Data, Typeable)

instance Eq CType where
  (==) = eqType `on` unCType

instance Ord CType where
  compare = nonDetCmpType `on` unCType

instance Show CType where
  show  = unsafeRender . unCType

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

instance Show LexicalFixity where
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
  unsafePerformIO $ mkSplitUniqSupply 'w'
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
freshUnique :: MonadState TacticState m => m Util.Unique
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
    -- | A binding explicitly written by the user.
  | UserPrv
    -- | The recursive hypothesis. Present only in the context of the recursion
    -- tactic.
  | RecursivePrv
    -- | A hypothesis which has been disallowed for some reason. It's important
    -- to keep these in the hypothesis set, rather than filtering it, in order
    -- to continue tracking downstream provenance.
  | DisallowedPrv DisallowReason Provenance
  deriving stock (Eq, Show, Generic, Ord, Data, Typeable)


------------------------------------------------------------------------------
-- | Why was a hypothesis disallowed?
data DisallowReason
  = WrongBranch Int
  | Shadowed
  | RecursiveCall
  | AlreadyDestructed
  deriving stock (Eq, Show, Generic, Ord, Data, Typeable)


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
  } deriving stock (Eq, Show, Generic, Ord, Data, Typeable)


------------------------------------------------------------------------------
-- | A wrapper which uses a 'Uniquable' constraint for providing 'Eq' and 'Ord'
-- instances.
newtype Uniquely a = Uniquely { getViaUnique :: a }
  deriving Show via a
  deriving stock (Data, Typeable)

instance Util.Uniquable a => Eq (Uniquely a) where
  (==) = (==) `on` Util.getUnique . getViaUnique

instance Util.Uniquable a => Ord (Uniquely a) where
  compare = Util.nonDetCmpUnique `on` Util.getUnique . getViaUnique


-- NOTE(sandy): The usage of list here is mostly for convenience, but if it's
-- ever changed, make sure to correspondingly update
-- 'jAcceptableDestructTargets' so that it correctly identifies newly
-- introduced terms.
newtype Hypothesis a = Hypothesis
  { unHypothesis :: [HyInfo a]
  }
  deriving stock (Functor, Eq, Show, Generic, Ord, Data, Typeable)
  deriving newtype (Semigroup, Monoid)


------------------------------------------------------------------------------
-- | The provenance and type of a hypothesis term.
data HyInfo a = HyInfo
  { hi_name       :: OccName
  , hi_provenance :: Provenance
  , hi_type       :: a
  }
  deriving stock (Functor, Eq, Show, Generic, Ord, Data, Typeable)


------------------------------------------------------------------------------
-- | Map a function over the provenance.
overProvenance :: (Provenance -> Provenance) -> HyInfo a -> HyInfo a
overProvenance f (HyInfo name prv ty) = HyInfo name (f prv) ty


------------------------------------------------------------------------------
-- | The current bindings and goal for a hole to be filled by refinery.
data Judgement' a = Judgement
  { _jHypothesis        :: !(Hypothesis a)
  , _jIsTopHole         :: !Bool
  , _jGoal              :: !a
  , j_coercion          :: TCvSubst
  }
  deriving stock (Generic, Functor, Show)

type Judgement = Judgement' CType


newtype ExtractM a = ExtractM { unExtractM :: ReaderT Config IO a }
    deriving newtype (Functor, Applicative, Monad, MonadReader Config)

------------------------------------------------------------------------------
-- | Used to ensure hole names are unique across invocations of runTactic
globalHoleRef :: IORef Int
globalHoleRef = unsafePerformIO $ newIORef 10
{-# NOINLINE globalHoleRef #-}

instance MonadExtract Int (Synthesized (LHsExpr GhcPs)) TacticError TacticState ExtractM where
  hole = do
    u <- lift $ ExtractM $ lift $
          readIORef globalHoleRef <* modifyIORef' globalHoleRef (+ 1)
    pure
      ( u
      , pure . noLoc $ var $ fromString $ occNameString $ occName $ mkMetaHoleName u
      )

  unsolvableHole _ = hole


instance MonadReader r m => MonadReader r (TacticT jdg ext err s m) where
  ask = TacticT $ lift $ Effect $ asks pure
  local f (TacticT m) = TacticT $ Strict.StateT $ \jdg ->
    Effect $ local f $ pure $ Strict.runStateT m jdg

instance MonadReader r m => MonadReader r (RuleT jdg ext err s m) where
  ask = RuleT $ Effect $ asks Axiom
  local f (RuleT m) = RuleT $ Effect $ local f $ pure m

mkMetaHoleName :: Int -> RdrName
mkMetaHoleName u = mkRdrUnqual $ mkVarOcc $ "_" <> show (Util.mkUnique 'w' u)

instance MetaSubst Int (Synthesized (LHsExpr GhcPs)) where
  -- TODO(sandy): This join is to combine the synthesizeds
  substMeta u val a =  join $ a <&>
    everywhereM (mkM $ \case
      (L _ (HsVar _ (L _ name)))
        | name == mkMetaHoleName u -> val
      (t :: LHsExpr GhcPs) -> pure t)


------------------------------------------------------------------------------
-- | Reasons a tactic might fail.
data TacticError
  = OutOfGas
  | GoalMismatch String CType
  | NoProgress
  | NoApplicableTactic
  | UnhelpfulRecursion
  | UnhelpfulDestruct OccName
  | TooPolymorphic
  | NotInScope OccName
  | TacticPanic String
  deriving (Eq)

instance Show TacticError where
    show OutOfGas = "Auto ran out of gas"
    show (GoalMismatch tac (CType typ)) =
      mconcat
        [ "The tactic "
        , tac
        , " doesn't apply to goal type "
        , unsafeRender typ
        ]
    show NoProgress =
      "Unable to make progress"
    show NoApplicableTactic =
      "No tactic could be applied"
    show UnhelpfulRecursion =
      "Recursion wasn't productive"
    show (UnhelpfulDestruct n) =
      "Destructing patval " <> show n <> " leads to no new types"
    show TooPolymorphic =
      "The tactic isn't applicable because the goal is too polymorphic"
    show (NotInScope name) =
      "Tried to do something with the out of scope name " <> show name
    show (TacticPanic err) =
      "Tactic panic: " <> err


------------------------------------------------------------------------------
type TacticsM  = TacticT Judgement (Synthesized (LHsExpr GhcPs)) TacticError TacticState ExtractM
type RuleM     = RuleT Judgement (Synthesized (LHsExpr GhcPs)) TacticError TacticState ExtractM
type Rule      = RuleM (Synthesized (LHsExpr GhcPs))

------------------------------------------------------------------------------
-- | The extract for refinery. Represents a "synthesized attribute" in the
-- context of attribute grammars. In essence, 'Synthesized' describes
-- information we'd like to pass from leaves of the tactics search upwards.
-- This includes the actual AST we've generated (in 'syn_val').
data Synthesized a = Synthesized
  { syn_val    :: a
  }
  deriving stock (Eq, Show, Functor, Foldable, Traversable, Generic, Data, Typeable)

instance Monad Synthesized where
  return = pure
  Synthesized a >>= f =
    case f a of
      Synthesized b ->
        Synthesized
          { syn_val = b
          }


------------------------------------------------------------------------------
-- | This might not be lawful, due to the semigroup on 'Trace' maybe not being
-- lawful. But that's only for debug output, so it's not anything I'm concerned
-- about.
instance Applicative Synthesized where
  pure = Synthesized
  Synthesized f <*> Synthesized a =
    Synthesized $ f a


------------------------------------------------------------------------------
-- | The results of 'Wingman.Machinery.runTactic'
data RunTacticResults = RunTacticResults
  { rtr_extract     :: LHsExpr GhcPs
  , rtr_jdg         :: Judgement
  , rtr_ctx         :: Config
  , rtr_timed_out   :: Bool
  } deriving Show


data AgdaMatch = AgdaMatch
  { amPats :: [Pat GhcPs]
  , amBody :: HsExpr GhcPs
  }
  deriving (Show)


data UserFacingMessage
  = NotEnoughGas
  | TacticErrors
  | TimedOut
  | NothingToDo
  | InfrastructureError Text
  deriving Eq

instance Show UserFacingMessage where
  show NotEnoughGas = "Wingman ran out of gas when trying to find a solution.  \nTry increasing the `auto_gas` setting."
  show TacticErrors = "Wingman couldn't find a solution"
  show TimedOut     = "Wingman timed out while finding a solution.  \nYou might get a better result if you increase the timeout duration."
  show NothingToDo  = "Nothing to do"
  show (InfrastructureError t) = "Internal error: " <> T.unpack t


data HoleJudgment = HoleJudgment
  { hj_range     :: Tracked 'Current Range
  , hj_jdg       :: Judgement
  , hj_ctx       :: Config
  , hj_dflags    :: DynFlags
  }

