{-# LANGUAGE FlexibleContexts #-}
module Ide.Plugin.Tactic.CodeGen where

import Control.Monad.Except
import Data.List
import Data.Traversable
import DataCon
import Development.IDE.GHC.Compat
import GHC.Exts
import GHC.SourceGen.Binds
import GHC.SourceGen.Expr
import GHC.SourceGen.Overloaded
import GHC.SourceGen.Pat
import Ide.Plugin.Tactic.Judgements
import Ide.Plugin.Tactic.Machinery
import Ide.Plugin.Tactic.Naming
import Ide.Plugin.Tactic.Types
import Name
import Type hiding (Var)


destructMatches
    :: (DataCon -> Judgement -> Rule)
       -- ^ How to construct each match
    -> (Judgement -> Judgement)
       -- ^ How to derive each match judgement
    -> CType
       -- ^ Type being destructed
    -> Judgement
    -> RuleM [RawMatch]
destructMatches f f2 t jdg = do
  let hy = jHypothesis jdg
      g  = jGoal jdg
  case splitTyConApp_maybe $ unCType t of
    Nothing -> throwError $ GoalMismatch "destruct" g
    Just (tc, apps) -> do
      let dcs = tyConDataCons tc
      case dcs of
        [] -> throwError $ GoalMismatch "destruct" g
        _ -> for dcs $ \dc -> do
          let args = dataConInstArgTys dc apps
          names <- mkManyGoodNames hy args

          let pat :: Pat GhcPs
              pat = conP (fromString $ occNameString $ nameOccName $ dataConName dc)
                  $ fmap bvar' names
              j = f2
                $ introducingPat (zip names $ coerce args)
                $ withNewGoal g jdg
          sg <- f dc j
          pure $ match [pat] $ unLoc sg


------------------------------------------------------------------------------
-- | Combinator for performing case splitting, and running sub-rules on the
-- resulting matches.
destruct' :: (DataCon -> Judgement -> Rule) -> OccName -> Judgement -> Rule
destruct' f term jdg = do
  let hy = jHypothesis jdg
  case find ((== term) . fst) $ toList hy of
    Nothing -> throwError $ UndefinedHypothesis term
    Just (_, t) ->
      fmap noLoc $ case' (var' term) <$>
        destructMatches f (destructing term) t jdg


------------------------------------------------------------------------------
-- | Combinator for performign case splitting, and running sub-rules on the
-- resulting matches.
destructLambdaCase' :: (DataCon -> Judgement -> Rule) -> Judgement -> Rule
destructLambdaCase' f jdg = do
  let g  = jGoal jdg
  case splitFunTy_maybe (unCType g) of
    Just (arg, _) | isAlgType arg ->
      fmap noLoc $ lambdaCase <$>
        destructMatches f id (CType arg) jdg
    _ -> throwError $ GoalMismatch "destructLambdaCase'" g


------------------------------------------------------------------------------
-- | Construct a data con with subgoals for each field.
buildDataCon
    :: Judgement
    -> DataCon            -- ^ The data con to build
    -> [Type]             -- ^ Type arguments for the data con
    -> RuleM (LHsExpr GhcPs)
buildDataCon jdg dc apps = do
  let args = dataConInstArgTys dc apps
  sgs <- traverse (newSubgoal . flip withNewGoal jdg . CType) args
  pure
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
