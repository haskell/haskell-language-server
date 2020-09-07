{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeSynonymInstances  #-}
{-# LANGUAGE ViewPatterns          #-}

module Ide.Tactics
  ( module Ide.Tactics
  , runTactic
  ) where

import           Control.Monad.Except (throwError)
import           Control.Monad.State
import           Data.List
import qualified Data.Map as M
import           Data.Traversable
import           DataCon
import           GHC
import           GHC.Exts
import           GHC.SourceGen.Binds
import           GHC.SourceGen.Expr
import           GHC.SourceGen.Overloaded
import           GHC.SourceGen.Pat
import           Ide.TacticMachinery
import           Name
import           Refinery.Tactic
import           TyCoRep
import           Type


assumption :: TacticsM ()
assumption = rule $ \(Judgement hy g) ->
  case find ((== g) . snd) $ toList hy of
    Just (v, _) -> pure $ noLoc $ HsVar NoExt $ noLoc $ Unqual v
    Nothing -> throwError $ GoalMismatch "assumption" g


intro :: TacticsM ()
intro = rule $ \(Judgement hy g) ->
  case unCType g of
    (FunTy a b) -> do
      v <- pure $ mkGoodName (getInScope hy) a
      sg <- newSubgoal (M.singleton v (CType a) <> hy) $ CType b
      pure $ noLoc $ lambda [VarPat noExt $ noLoc $ Unqual v] $ unLoc sg
    _ -> throwError $ GoalMismatch "intro" g


destruct' :: (DataCon -> Judgement -> Rule) -> OccName -> TacticsM ()
destruct' f term = rule $ \(Judgement hy g) -> do
  case find ((== term) . fst) $ toList hy of
    Nothing -> throwError $ UndefinedHypothesis term
    Just (_, t) ->
      case splitTyConApp_maybe $ unCType t of
        Nothing -> throwError $ GoalMismatch "destruct" g
        Just (tc, apps) -> do
          fmap noLoc
              $ case' (HsVar NoExt $ noLoc $ Unqual term)
              <$> do
            for (tyConDataCons tc) $ \dc -> do
              let args = dataConInstArgTys dc apps
              names <- flip evalStateT (getInScope hy) $ for args $ \at -> do
                in_scope <- Control.Monad.State.get
                let n = mkGoodName in_scope at
                modify (n :)
                pure n

              let pat :: Pat GhcPs
                  pat = conP (fromString $ occNameString $ nameOccName $ dataConName dc)
                      $ fmap (bvar . fromString . occNameString) names

              j <- newJudgement (M.fromList (zip names (fmap CType args)) <> hy) g
              sg <- f dc j
              pure $ match [pat] $ unLoc sg


destruct :: OccName -> TacticsM ()
destruct = destruct' $ const subgoal

homo :: OccName -> TacticsM ()
homo = destruct' $ \dc (Judgement hy (CType g)) ->
  buildDataCon hy dc (snd $ splitAppTys g)


apply :: TacticsM ()
apply = rule $ \(Judgement hy g) -> do
  case find ((== Just g) . fmap (CType . snd) . splitFunTy_maybe . unCType . snd) $ toList hy of
    Just (func, CType ty) -> do
      let (args, _) = splitFunTys ty
      sgs <- traverse (newSubgoal hy . CType) args
      pure . noLoc
           . foldl' (@@) (HsVar NoExt $ noLoc $ Unqual func)
           $ fmap unLoc sgs
    Nothing -> throwError $ GoalMismatch "apply" g


split :: TacticsM ()
split = rule $ \(Judgement hy g) ->
  case splitTyConApp_maybe $ unCType g of
    Just (tc, apps) ->
      case tyConDataCons tc of
        [dc] -> buildDataCon hy dc apps
        _ -> throwError $ GoalMismatch "split" g
    Nothing -> throwError $ GoalMismatch "split" g


deepen :: Int -> TacticsM ()
deepen 0 = pure ()
deepen depth = do
  one
  deepen $ depth - 1

auto :: TacticsM ()
auto = (intro >> auto)
   <!> (assumption >> auto)
   <!> (split >> auto)
   <!> (apply >> auto)
   <!> pure ()

one :: TacticsM ()
one = intro <!> assumption <!> split <!> apply <!> pure ()

