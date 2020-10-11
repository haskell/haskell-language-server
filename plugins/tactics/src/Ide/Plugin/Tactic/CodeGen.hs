{-# LANGUAGE TupleSections #-}
{-# LANGUAGE FlexibleContexts #-}
module Ide.Plugin.Tactic.CodeGen where

import           Control.Monad.Except
import           Control.Monad.State (MonadState)
import           Control.Monad.State.Class (modify)
import           Data.List
import qualified Data.Map as M
import qualified Data.Set as S
import           Data.Traversable
import           DataCon
import           Development.IDE.GHC.Compat
import           GHC.Exts
import           GHC.SourceGen.Binds
import           GHC.SourceGen.Expr
import           GHC.SourceGen.Overloaded
import           GHC.SourceGen.Pat
import           Ide.Plugin.Tactic.Judgements
import           Ide.Plugin.Tactic.Machinery
import           Ide.Plugin.Tactic.Naming
import           Ide.Plugin.Tactic.Types
import           Name
import           Type hiding (Var)


useOccName :: MonadState TacticState m => Judgement -> OccName -> m ()
useOccName jdg name =
  case M.lookup name $ jHypothesis jdg of
    Just{}  -> modify $ withUsedVals $ S.insert name
    Nothing -> pure ()


destructMatches
    :: (DataCon -> Judgement -> Rule)
       -- ^ How to construct each match
    -> ([(OccName, CType)] -> Judgement -> Judgement)
       -- ^ How to derive each match judgement
    -> CType
       -- ^ Type being destructed
    -> Judgement
    -> RuleM (Trace, [RawMatch])
destructMatches f f2 t jdg = do
  let hy = jHypothesis jdg
      g  = jGoal jdg
  case splitTyConApp_maybe $ unCType t of
    Nothing -> throwError $ GoalMismatch "destruct" g
    Just (tc, apps) -> do
      let dcs = tyConDataCons tc
      case dcs of
        [] -> throwError $ GoalMismatch "destruct" g
        _ -> fmap unzipTrace $ for dcs $ \dc -> do
          let args = dataConInstArgTys dc apps
          names <- mkManyGoodNames hy args
          let hy' = zip names $ coerce args
              dcon_name = nameOccName $ dataConName dc

          let pat :: Pat GhcPs
              pat = conP (fromString $ occNameString dcon_name)
                  $ fmap bvar' names
              j = f2 hy'
                $ withPositionMapping dcon_name names
                $ introducingPat hy'
                $ withNewGoal g jdg
          (tr, sg) <- f dc j
          modify $ withIntroducedVals $ mappend $ S.fromList names
          pure ( rose ("match " <> show dc <> " {" <>
                          intercalate ", " (fmap show names) <> "}")
                    $ pure tr
               , match [pat] $ unLoc sg
               )


unzipTrace :: [(Trace, a)] -> (Trace, [a])
unzipTrace l =
  let (trs, as) = unzip l
   in (rose mempty trs, as)


------------------------------------------------------------------------------
-- | Combinator for performing case splitting, and running sub-rules on the
-- resulting matches.
destruct' :: (DataCon -> Judgement -> Rule) -> OccName -> Judgement -> Rule
destruct' f term jdg = do
  when (isDestructBlacklisted jdg) $ throwError NoApplicableTactic
  let hy = jHypothesis jdg
  case find ((== term) . fst) $ toList hy of
    Nothing -> throwError $ UndefinedHypothesis term
    Just (_, t) -> do
      useOccName jdg term
      (tr, ms)
          <- destructMatches
               f
               (\cs -> setParents term (fmap fst cs) . destructing term)
               t
               jdg
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
      fmap (fmap noLoc $ lambdaCase) <$>
        destructMatches f (const id) (CType arg) jdg
    _ -> throwError $ GoalMismatch "destructLambdaCase'" g


------------------------------------------------------------------------------
-- | Construct a data con with subgoals for each field.
buildDataCon
    :: Judgement
    -> DataCon            -- ^ The data con to build
    -> [Type]             -- ^ Type arguments for the data con
    -> RuleM (Trace, LHsExpr GhcPs)
buildDataCon jdg dc apps = do
  let args = dataConInstArgTys dc apps
      dcon_name = nameOccName $ dataConName dc
  (tr, sgs)
      <- fmap unzipTrace
       $ traverse ( \(arg, n) ->
                    newSubgoal
                  . filterSameTypeFromOtherPositions dcon_name n
                  . blacklistingDestruct
                  . flip withNewGoal jdg
                  $ CType arg
                  ) $ zip args [0..]
  pure
    . (rose (show dc) $ pure tr,)
    . noLoc
    . foldl' (@@)
        (HsVar noExtField $ noLoc $ Unqual $ nameOccName $ dataConName dc)
    $ fmap unLoc sgs


------------------------------------------------------------------------------
-- | Like 'var', but works over standard GHC 'OccName's.
var' :: Var a => OccName -> a
var' = var . fromString . occNameString


------------------------------------------------------------------------------
-- | Like 'bvar', but works over standard GHC 'OccName's.
bvar' :: BVar a => OccName -> a
bvar' = bvar . fromString . occNameString

