{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE MonoLocalBinds        #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeSynonymInstances  #-}
{-# LANGUAGE ViewPatterns          #-}
{-# LANGUAGE ViewPatterns          #-}

module Ide.Plugin.Tactic.Machinery where

import           Control.Monad.Except (throwError)
import           Control.Monad.State (gets, get, modify, evalStateT)
import           Data.Char
import           Data.Either
import           Data.Function
import           Data.Functor.Identity
import           Data.List
import           Data.Map (Map)
import qualified Data.Map as M
import           Data.Maybe
import           Data.Traversable
import           DataCon
import           Development.IDE.GHC.Compat
import           Development.IDE.Spans.LocalBindings
import           Development.IDE.Types.Location
import           Ide.Plugin.Tactic.GHC
import           DynFlags (unsafeGlobalDynFlags)
import qualified FastString as FS
import           GHC.Generics
import           GHC.SourceGen.Overloaded
import           Ide.Plugin.Tactic.GHC (oneWayUnify)
import           Name
import           Outputable hiding ((<>))
import           Refinery.Tactic
import           SrcLoc
import           TcType
import           Type
import           TysWiredIn (listTyCon, pairTyCon, intTyCon, floatTyCon, doubleTyCon, charTyCon)
import           Unify


data TacticState = TacticState
    { ts_skolems :: [TyVar]
    , ts_unifier :: TCvSubst
    }

initialTacticState :: TacticState
initialTacticState = TacticState
    { ts_skolems = []
    , ts_unifier = emptyTCvSubst
    }

------------------------------------------------------------------------------
-- | Orphan instance for producing holes when attempting to solve tactics.
instance MonadExtract (LHsExpr GhcPs) ProvableM where
  hole = pure $ noLoc $ HsVar noExtField $ noLoc $ Unqual $ mkVarOcc "_"


------------------------------------------------------------------------------
-- | A wrapper around 'Type' which supports equality and ordering.
newtype CType = CType { unCType :: Type }

instance Eq CType where
  (==) = eqType `on` unCType

instance Ord CType where
  compare = nonDetCmpType `on` unCType

substCTy :: TCvSubst -> CType -> CType
substCTy subst = CType . substTy subst . unCType

------------------------------------------------------------------------------
-- | Given a 'SrcSpan' and a 'Bindings', create a hypothesis.
hypothesisFromBindings :: RealSrcSpan -> Bindings -> Map OccName CType
hypothesisFromBindings span bs = buildHypothesis (getLocalScope bs span)


------------------------------------------------------------------------------
-- | Convert a @Set Id@ into a hypothesis.
buildHypothesis :: [(Name, Maybe Type)] -> Map OccName CType
buildHypothesis
  = M.fromList
  . mapMaybe go
  where
    go (n, t)
      | Just ty <- t
      , isAlpha . head . occNameString $ occ = Just (occ, CType ty)
      | otherwise = Nothing
      where
        occ = occName n


------------------------------------------------------------------------------
-- | The current bindings and goal for a hole to be filled by refinery.
data Judgement = Judgement
  { jHypothesis :: Map OccName CType
  , jGoal       :: CType
  }
  deriving (Eq, Ord, Generic)


substJdg :: TCvSubst -> Judgement -> Judgement
substJdg subst (Judgement hys goal) = Judgement (fmap (substCTy subst) hys) (substCTy subst goal)

------------------------------------------------------------------------------
-- | Reasons a tactic might fail.
data TacticError
  = UndefinedHypothesis OccName
  | GoalMismatch String CType
  | UnsolvedSubgoals [Judgement]
  | UnificationError CType CType
  | NoProgress
  | NoApplicableTactic

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

type ProvableM = ProvableT Judgement Identity
type TacticsM  = TacticT Judgement (LHsExpr GhcPs) TacticError TacticState ProvableM
type RuleM     = RuleT Judgement (LHsExpr GhcPs) TacticError TacticState ProvableM
type Rule      = RuleM (LHsExpr GhcPs)


------------------------------------------------------------------------------
-- | Produce a subgoal that must be solved before we can solve the original
-- goal.
newSubgoal
    :: Map OccName CType  -- ^ Available bindings
    -> CType              -- ^ Sub-goal type
    -> RuleM (LHsExpr GhcPs)
newSubgoal hy g = do
    j <- newJudgement hy g
    unifier <- gets ts_unifier
    subgoal $ substJdg unifier j

------------------------------------------------------------------------------
-- | Create a new judgment
newJudgement
    ::  Monad m
    => Map OccName CType  -- ^ Available bindings
    -> CType              -- ^ Sub-goal type
    -> m Judgement
newJudgement hy g = do
  pure $ Judgement hy g


------------------------------------------------------------------------------
-- | Produce a unique, good name for a type.
mkGoodName
    :: [OccName]  -- ^ Bindings in scope; used to ensure we don't shadow anything
    -> Type       -- ^ The type to produce a name for
    -> OccName
mkGoodName in_scope t =
  let tn = mkTyName t
   in mkVarOcc $ case elem (mkVarOcc tn) in_scope of
        True -> tn ++ show (length in_scope)
        False -> tn


------------------------------------------------------------------------------
-- | Like 'mkGoodName' but creates several apart names.
mkManyGoodNames
  :: (Traversable t, Monad m)
  => M.Map OccName a
  -> t Type
  -> m (t OccName)
mkManyGoodNames hy args =
  flip evalStateT (getInScope hy) $ for args $ \at -> do
    in_scope <- Control.Monad.State.get
    let n = mkGoodName in_scope at
    modify (n :)
    pure n


------------------------------------------------------------------------------
-- | Use type information to create a reasonable name.
mkTyName :: Type -> String
-- eg. mkTyName (a -> B) = "fab"
mkTyName (tcSplitFunTys -> ([a@(isFunTy -> False)], b))
  = "f" ++ mkTyName a ++ mkTyName b
-- eg. mkTyName (a -> b -> C) = "f_C"
mkTyName (tcSplitFunTys -> ((_:_), b))
  = "f_" ++ mkTyName b
-- eg. mkTyName (Either A B) = "eab"
mkTyName (splitTyConApp_maybe -> Just (c, args))
  = mkTyConName c ++ foldMap mkTyName args
-- eg. mkTyName a = "a"
mkTyName (getTyVar_maybe-> Just tv)
  = occNameString $ occName tv
-- eg. mkTyName (forall x. y) = "y"
mkTyName (tcSplitSigmaTy-> ((_:_), _, t))
  = mkTyName t
mkTyName _ = "x"


------------------------------------------------------------------------------
-- | Is this a function type?
isFunction :: Type -> Bool
isFunction (tcSplitFunTys -> ((_:_), _)) = True
isFunction _ = False

------------------------------------------------------------------------------
-- | Is this an algebraic type?
algebraicTyCon :: Type -> Maybe TyCon
algebraicTyCon (splitTyConApp_maybe -> Just (tycon, _))
  | tycon == intTyCon    = Nothing
  | tycon == floatTyCon  = Nothing
  | tycon == doubleTyCon = Nothing
  | tycon == charTyCon   = Nothing
  | tycon == funTyCon    = Nothing
  | otherwise = Just tycon
algebraicTyCon _ = Nothing


------------------------------------------------------------------------------
-- | Get a good name for a type constructor.
mkTyConName :: TyCon -> String
mkTyConName tc
  | tc == listTyCon = "l_"
  | tc == pairTyCon = "p_"
  | otherwise = fmap toLower . take 1 . occNameString $ getOccName tc


------------------------------------------------------------------------------
-- | Attempt to generate a term of the right type using in-scope bindings, and
-- a given tactic.
runTactic
    :: Judgement
    -> TacticsM ()       -- ^ Tactic to use
    -> Either [TacticError] (LHsExpr GhcPs)
runTactic jdg t = case partitionEithers $ runProvable $ runTacticT t jdg initialTacticState of
    (errs, []) -> Left $ errs
    (_, solns) ->
      let soln = listToMaybe $ filter (null . snd) solns
      in Right $ fst $ fromMaybe (head solns) soln


------------------------------------------------------------------------------
-- | Which names are in scope?
getInScope :: Map OccName a -> [OccName]
getInScope = M.keys


------------------------------------------------------------------------------
-- | Construct a data con with subgoals for each field.
buildDataCon
    :: Map OccName CType  -- ^ In-scope bindings
    -> DataCon            -- ^ The data con to build
    -> [Type]             -- ^ Type arguments for the data con
    -> RuleM (LHsExpr GhcPs)
buildDataCon hy dc apps = do
  let args = dataConInstArgTys dc apps
  sgs <- traverse (newSubgoal hy . CType) args
  pure
    . noLoc
    . foldl' (@@)
        (HsVar noExtField $ noLoc $ Unqual $ nameOccName $ dataConName dc)
    $ fmap unLoc sgs


-- | We need to make sure that we don't try to unify any skolems.
-- To see why, consider the case:
--
-- uhh :: (Int -> Int) -> a
-- uhh f = _
--
-- If we were to apply 'f', then we would try to unify 'Int' and 'a'.
-- This is fine from the perspective of 'tcUnifyTy', but will cause obvious
-- type errors in our use case. Therefore, we need to ensure that our
-- 'TCvSubst' doesn't try to unify skolems.
checkSkolemUnification :: CType -> CType -> TCvSubst -> RuleM ()
checkSkolemUnification t1 t2 subst = do
    skolems <- gets ts_skolems
    case traverse (lookupTyVar subst) skolems of
        Just _ -> throwError (UnificationError t1 t2)
        Nothing -> pure ()

------------------------------------------------------------------------------
-- | Attempt to unify two types.
unify :: CType -> CType -> RuleM ()
unify t1 t2 =
    case tcUnifyTy (unCType t1) (unCType t2) of
      Just subst -> do
          checkSkolemUnification t1 t2 subst
          modify (\s -> s { ts_unifier = unionTCvSubst subst (ts_unifier s) })
      Nothing -> throwError (UnificationError t1 t2)

oneWayUnifyRule
    :: [TyVar]  -- ^ binders
    -> CType     -- ^ type to instiate
    -> CType     -- ^ at this type
    -> RuleM ()
oneWayUnifyRule binders t1 t2 =
  case oneWayUnify binders (unCType t1) (unCType t2) of
    Just subst -> do
        checkSkolemUnification t1 t2 subst
        modify (\s -> s { ts_unifier = unionTCvSubst subst (ts_unifier s) })
    Nothing -> throwError $ UnificationError t1 t2


------------------------------------------------------------------------------
-- | Convert a DAML compiler Range to a GHC SrcSpan
-- TODO(sandy): this doesn't belong here
rangeToSrcSpan :: String -> Range -> SrcSpan
rangeToSrcSpan file range = RealSrcSpan $ rangeToRealSrcSpan file range

rangeToRealSrcSpan :: String -> Range -> RealSrcSpan
rangeToRealSrcSpan file (Range (Position startLn startCh) (Position endLn endCh)) =
    mkRealSrcSpan
      (mkRealSrcLoc (FS.fsLit file) (startLn + 1) (startCh + 1))
      (mkRealSrcLoc (FS.fsLit file) (endLn + 1) (endCh + 1))

------------------------------------------------------------------------------
-- | Print something
unsafeRender :: Outputable a => a -> String
unsafeRender = unsafeRender' . ppr

unsafeRender' :: SDoc -> String
unsafeRender' = showSDoc unsafeGlobalDynFlags

