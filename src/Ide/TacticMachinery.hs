{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeSynonymInstances  #-}
{-# LANGUAGE ViewPatterns          #-}

module Ide.TacticMachinery where

import Control.Arrow
import Data.Char
import Data.Function
import Data.List
import DataCon
import GHC
import GHC.Generics
import GHC.SourceGen.Overloaded
import Name
import Refinery.Tactic
import TcType
import Type


newtype CType = CType { unCType :: Type }

instance Eq CType where
  (==) = eqType `on` unCType

instance Ord CType where
  compare = nonDetCmpType `on` unCType


data Judgement = Judgement
  { jHypothesis :: [(OccName, CType)]
  , jGoal       :: CType
  }
  deriving (Eq, Ord, Generic)


data TacticError
  = UndefinedHypothesis OccName
  | GoalMismatch String CType
  | UnsolvedSubgoals [Judgement]


type ProvableM = ProvableT Judgement (Either TacticError)
type TacticsM = TacticT Judgement (LHsExpr GhcPs) ProvableM
type RuleM    = RuleT Judgement (LHsExpr GhcPs) ProvableM
type Rule = RuleM (LHsExpr GhcPs)


newSubgoal :: [(OccName, CType)] -> CType -> RuleM (LHsExpr GhcPs)
newSubgoal hy g = subgoal =<< newJudgement hy g


newJudgement
    :: ( Monad m)
    => [(OccName, CType)]
    -> CType
    -> m Judgement
newJudgement hy g = do
  pure $ Judgement hy g


mkGoodName :: [OccName] -> Type -> OccName
mkGoodName in_scope t =
  let tn = mkTyName t
   in mkVarOcc $ case elem (mkVarOcc tn) in_scope of
        True -> tn ++ show (length in_scope)
        False -> tn


mkTyName :: Type -> String
mkTyName (tcSplitFunTys -> ([a@(isFunTy -> False)], b))
  = "f" ++ mkTyName a ++ mkTyName b
mkTyName (tcSplitFunTys -> ((_:_), b))
  = "f_" ++ mkTyName b
mkTyName (splitTyConApp_maybe -> Just (c, args))
  = mkTyConName c ++ foldMap mkTyName args
mkTyName (getTyVar_maybe-> Just tv)
  = occNameString $ occName tv
mkTyName (tcSplitSigmaTy-> ((_:_), _, t))
  = mkTyName t
mkTyName _ = "x"


mkTyConName :: TyCon -> String
mkTyConName = fmap toLower . take 1 . occNameString . getOccName


tacticActual
    :: TacticsM ()
    -> Judgement
    -> Either TacticError (LHsExpr GhcPs)
tacticActual t = fmap fst . runProvableT . runTacticT t


runTactic
    :: Type
    -> [(OccName, Type)]
    -> TacticsM ()
    -> Maybe (LHsExpr GhcPs)
runTactic ty hy t
  = hush
  . tacticActual t
  . Judgement (fmap (second CType) hy)
  $ CType ty


hush :: Either a b -> Maybe b
hush = either (const Nothing) Just


instance MonadExtract (LHsExpr GhcPs) ProvableM where
  hole = pure $ noLoc $ HsUnboundVar NoExt $ TrueExprHole $ mkVarOcc "_"


getInScope :: [(OccName, a)] -> [OccName]
getInScope = fmap fst


buildDataCon
    :: [(OccName, CType)]
    -> DataCon
    -> [Type]
    -> RuleM (LHsExpr GhcPs)
buildDataCon hy dc apps = do
  let args = dataConInstArgTys dc apps
  sgs <- traverse (newSubgoal hy . CType) args
  pure . noLoc
       . foldl' (@@) (HsVar NoExt $ noLoc $ Unqual $ nameOccName $ dataConName dc)
       $ fmap unLoc sgs

