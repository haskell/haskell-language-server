{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TupleSections    #-}
{-# LANGUAGE ViewPatterns     #-}

module Ide.Plugin.Tactic.CodeGen where

import           Control.Lens ((+~), (%~), (<>~))
import           Control.Monad.Except
import           Control.Monad.State (MonadState)
import           Control.Monad.State.Class (modify)
import           Data.Generics.Product (field)
import           Data.List
import qualified Data.Map as M
import qualified Data.Set as S
import           Data.Traversable
import           DataCon
import           Development.IDE.GHC.Compat
import           GHC.Exts
import           GHC.SourceGen (RdrNameStr)
import           GHC.SourceGen.Binds
import           GHC.SourceGen.Expr
import           GHC.SourceGen.Overloaded
import           GHC.SourceGen.Pat
import           Ide.Plugin.Tactic.GHC
import           Ide.Plugin.Tactic.Judgements
import           Ide.Plugin.Tactic.Machinery
import           Ide.Plugin.Tactic.Naming
import           Ide.Plugin.Tactic.Types
import           Name
import           Type hiding (Var)


useOccName :: MonadState TacticState m => Judgement -> OccName -> m ()
useOccName jdg name =
  -- Only score points if this is in the local hypothesis
  case M.lookup name $ jLocalHypothesis jdg of
    Just{}  -> modify
             $ (withUsedVals $ S.insert name)
             . (field @"ts_unused_top_vals" %~ S.delete name)
    Nothing -> pure ()


------------------------------------------------------------------------------
-- | Doing recursion incurs a small penalty in the score.
countRecursiveCall :: TacticState -> TacticState
countRecursiveCall = field @"ts_recursion_count" +~ 1


------------------------------------------------------------------------------
-- | Insert some values into the unused top values field. These are
-- subsequently removed via 'useOccName'.
addUnusedTopVals :: MonadState TacticState m => S.Set OccName -> m ()
addUnusedTopVals vals = modify $ field @"ts_unused_top_vals" <>~ vals


destructMatches
    :: (DataCon -> Judgement -> Rule)
       -- ^ How to construct each match
    -> Maybe OccName
       -- ^ Scrutinee
    -> CType
       -- ^ Type being destructed
    -> Judgement
    -> RuleM (Trace, [RawMatch])
destructMatches f scrut t jdg = do
  let hy = jEntireHypothesis jdg
      g  = jGoal jdg
  case splitTyConApp_maybe $ unCType t of
    Nothing -> throwError $ GoalMismatch "destruct" g
    Just (tc, apps) -> do
      let dcs = tyConDataCons tc
      case dcs of
        [] -> throwError $ GoalMismatch "destruct" g
        _ -> fmap unzipTrace $ for dcs $ \dc -> do
          let args = dataConInstOrigArgTys' dc apps
          names <- mkManyGoodNames hy args
          let hy' = zip names $ coerce args
              j = introducingPat scrut dc hy'
                $ withNewGoal g jdg
          (tr, sg) <- f dc j
          modify $ withIntroducedVals $ mappend $ S.fromList names
          pure ( rose ("match " <> show dc <> " {" <>
                          intercalate ", " (fmap show names) <> "}")
                    $ pure tr
               , match [mkDestructPat dc names] $ unLoc sg
               )


------------------------------------------------------------------------------
-- | Produces a pattern for a data con and the names of its fields.
mkDestructPat :: DataCon -> [OccName] -> Pat GhcPs
mkDestructPat dcon names
  | isTupleDataCon dcon =
      tuple pat_args
  | otherwise =
      infixifyPatIfNecessary dcon $
        conP
          (coerceName $ dataConName dcon)
          pat_args
  where
    pat_args = fmap bvar' names


infixifyPatIfNecessary :: DataCon -> Pat GhcPs -> Pat GhcPs
infixifyPatIfNecessary dcon x
  | dataConIsInfix dcon =
      case x of
        ConPatIn op (PrefixCon [lhs, rhs]) ->
          ConPatIn op $ InfixCon lhs rhs
        y -> y
  | otherwise = x



unzipTrace :: [(Trace, a)] -> (Trace, [a])
unzipTrace l =
  let (trs, as) = unzip l
   in (rose mempty trs, as)


-- | Essentially same as 'dataConInstOrigArgTys' in GHC,
--  but only accepts universally quantified types as the second arguments
--  and automatically introduces existentials.
--
-- NOTE: The behaviour depends on GHC's 'dataConInstOrigArgTys'.
--       We need some tweaks if the compiler changes the implementation.
dataConInstOrigArgTys'
  :: DataCon
      -- ^ 'DataCon'structor
  -> [Type]
      -- ^ /Universally/ quantified type arguments to a result type.
      --   It /MUST NOT/ contain any dictionaries, coercion and existentials.
      --
      --   For example, for @MkMyGADT :: b -> MyGADT a c@, we
      --   must pass @[a, c]@ as this argument but not @b@, as @b@ is an existential.
  -> [Type]
      -- ^ Types of arguments to the DataCon with returned type is instantiated with the second argument.
dataConInstOrigArgTys' con uniTys =
  let exvars = dataConExTys con
   in dataConInstOrigArgTys con $
        uniTys ++ fmap mkTyVarTy exvars
      -- Rationale: At least in GHC <= 8.10, 'dataConInstOrigArgTys'
      -- unifies the second argument with DataCon's universals followed by existentials.
      -- If the definition of 'dataConInstOrigArgTys' changes,
      -- this place must be changed accordingly.

------------------------------------------------------------------------------
-- | Combinator for performing case splitting, and running sub-rules on the
-- resulting matches.

destruct' :: (DataCon -> Judgement -> Rule) -> OccName -> Judgement -> Rule
destruct' f term jdg = do
  when (isDestructBlacklisted jdg) $ throwError NoApplicableTactic
  let hy = jHypothesis jdg
  case M.lookup term hy of
    Nothing -> throwError $ UndefinedHypothesis term
    Just (hi_type -> t) -> do
      useOccName jdg term
      (tr, ms)
          <- destructMatches
               f
               (Just term)
               t
               $ disallowing AlreadyDestructed [term] jdg
      pure ( rose ("destruct " <> show term) $ pure tr
           , noLoc $ case' (var' term) ms
           )


------------------------------------------------------------------------------
-- | Combinator for performign case splitting, and running sub-rules on the
-- resulting matches.
destructLambdaCase' :: (DataCon -> Judgement -> Rule) -> Judgement -> Rule
destructLambdaCase' f jdg = do
  when (isDestructBlacklisted jdg) $ throwError NoApplicableTactic
  let g  = jGoal jdg
  case splitFunTy_maybe (unCType g) of
    Just (arg, _) | isAlgType arg ->
      fmap (fmap noLoc lambdaCase) <$>
        destructMatches f Nothing (CType arg) jdg
    _ -> throwError $ GoalMismatch "destructLambdaCase'" g


------------------------------------------------------------------------------
-- | Construct a data con with subgoals for each field.
buildDataCon
    :: Judgement
    -> DataCon            -- ^ The data con to build
    -> [Type]             -- ^ Type arguments for the data con
    -> RuleM (Trace, LHsExpr GhcPs)
buildDataCon jdg dc apps = do
  let args = dataConInstOrigArgTys' dc apps
  (tr, sgs)
      <- fmap unzipTrace
       $ traverse ( \(arg, n) ->
                    newSubgoal
                  . filterSameTypeFromOtherPositions dc n
                  . blacklistingDestruct
                  . flip withNewGoal jdg
                  $ CType arg
                  ) $ zip args [0..]
  pure
    . (rose (show dc) $ pure tr,)
    $ mkCon dc sgs


mkCon :: DataCon -> [LHsExpr GhcPs] -> LHsExpr GhcPs
mkCon dcon (fmap unLoc -> args)
  | isTupleDataCon dcon =
      noLoc $ tuple args
  | dataConIsInfix dcon
  , (lhs : rhs : args') <- args =
      noLoc $ foldl' (@@) (op lhs (coerceName dcon_name) rhs) args'
  | otherwise =
      noLoc $ foldl' (@@) (bvar' $ occName dcon_name) args
  where
    dcon_name = dataConName dcon



coerceName :: HasOccName a => a -> RdrNameStr
coerceName = fromString . occNameString . occName



------------------------------------------------------------------------------
-- | Like 'var', but works over standard GHC 'OccName's.
var' :: Var a => OccName -> a
var' = var . fromString . occNameString


------------------------------------------------------------------------------
-- | Like 'bvar', but works over standard GHC 'OccName's.
bvar' :: BVar a => OccName -> a
bvar' = bvar . fromString . occNameString


------------------------------------------------------------------------------
-- | Get an HsExpr corresponding to a function name.
mkFunc :: String -> HsExpr GhcPs
mkFunc = var' . mkVarOcc


------------------------------------------------------------------------------
-- | Get an HsExpr corresponding to a value name.
mkVal :: String -> HsExpr GhcPs
mkVal = var' . mkVarOcc


------------------------------------------------------------------------------
-- | Like 'op', but easier to call.
infixCall :: String -> HsExpr GhcPs -> HsExpr GhcPs -> HsExpr GhcPs
infixCall s = flip op (fromString s)


------------------------------------------------------------------------------
-- | Like '(@@)', but uses a dollar instead of parentheses.
appDollar :: HsExpr GhcPs -> HsExpr GhcPs -> HsExpr GhcPs
appDollar = infixCall "$"
