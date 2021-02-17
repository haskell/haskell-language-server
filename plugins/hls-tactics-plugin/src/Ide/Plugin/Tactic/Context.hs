{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase       #-}

module Ide.Plugin.Tactic.Context where

import           Bag
import           Control.Arrow
import           Control.Monad.Reader
import           Data.List
import           Data.Maybe (mapMaybe)
import           Data.Set (Set)
import qualified Data.Set as S
import           Development.IDE.GHC.Compat
import           Ide.Plugin.Tactic.GHC (tacticsThetaTy)
import           Ide.Plugin.Tactic.Machinery (methodHypothesis)
import           Ide.Plugin.Tactic.Types
import           OccName
import           TcRnTypes
import           TcType (substTy, tcSplitSigmaTy)
import           Unify (tcUnifyTy)
import Ide.Plugin.Tactic.FeatureSet (FeatureSet)


mkContext :: FeatureSet -> [(OccName, CType)] -> TcGblEnv -> Context
mkContext features locals tcg = Context
  { ctxDefiningFuncs = locals
  , ctxModuleFuncs = fmap splitId
                   . (getFunBindId =<<)
                   . fmap unLoc
                   . bagToList
                   $ tcg_binds tcg
  , ctxFeatureSet = features
  }


------------------------------------------------------------------------------
-- | Find all of the class methods that exist from the givens in the context.
contextMethodHypothesis :: Context -> Hypothesis CType
contextMethodHypothesis ctx
  = Hypothesis
  . excludeForbiddenMethods
  . join
  . concatMap
      ( mapMaybe methodHypothesis
      . tacticsThetaTy
      . unCType
      )
  . mapMaybe (definedThetaType ctx)
  . fmap fst
  $ ctxDefiningFuncs ctx


------------------------------------------------------------------------------
-- | Many operations are defined in typeclasses for performance reasons, rather
-- than being a true part of the class. This function filters out those, in
-- order to keep our hypothesis space small.
excludeForbiddenMethods :: [HyInfo a] -> [HyInfo a]
excludeForbiddenMethods = filter (not . flip S.member forbiddenMethods . hi_name)
  where
    forbiddenMethods :: Set OccName
    forbiddenMethods = S.map mkVarOcc $ S.fromList
      [ -- monadfail
        "fail"
      ]


------------------------------------------------------------------------------
-- | Given the name of a function that exists in 'ctxDefiningFuncs', get its
-- theta type.
definedThetaType :: Context -> OccName -> Maybe CType
definedThetaType ctx name = do
  (_, CType mono) <- find ((== name) . fst) $ ctxDefiningFuncs ctx
  (_, CType poly) <- find ((== name) . fst) $ ctxModuleFuncs ctx
  let (_, _, poly') = tcSplitSigmaTy poly
  subst <- tcUnifyTy poly' mono
  pure $ CType $ substTy subst $ snd $ splitForAllTys poly


splitId :: Id -> (OccName, CType)
splitId = occName &&& CType . idType


getFunBindId :: HsBindLR GhcTc GhcTc -> [Id]
getFunBindId (AbsBinds _ _ _ abes _ _ _)
  = abes >>= \case
      ABE _ poly _ _ _ -> pure poly
      _ -> []
getFunBindId _ = []


getCurrentDefinitions :: MonadReader Context m => m [(OccName, CType)]
getCurrentDefinitions = asks ctxDefiningFuncs

getModuleHypothesis :: MonadReader Context m => m [(OccName, CType)]
getModuleHypothesis = asks ctxModuleFuncs
