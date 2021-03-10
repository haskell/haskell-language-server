module Main where

import BasicTypes
import Criterion.Main
import GHC (LHsExpr, GhcPs, alphaTyVars)
import GhcPlugins (mkOccName, mkTupleTy, mkVarOcc, mkTyVarTy, mkFunTys, mkListTy, mkFunTy)
import Wingman.Auto
import Wingman.Judgements
import Wingman.Machinery (runTactic)
import Wingman.Types


main :: IO ()
main = defaultMain
  [ bgroup "auto"
    [ bench "foldr" $ nfAppIO print $ runAuto $
        (ty_a --> ty_b --> ty_b) --> ty_b --> mkListTy ty_a --> ty_b
    ]
  ]


ctx :: Context
ctx = Context
  { ctxDefiningFuncs = [(mkVarOcc "hello", CType $ mkTupleTy Boxed [])]
  , ctxModuleFuncs = []
  , ctxFeatureSet = mempty
  }


ty_a, ty_b, ty_c :: Type
(ty_a : ty_b : ty_c : _) = fmap mkTyVarTy alphaTyVars


(-->) :: Type -> Type -> Type
(-->) = mkFunTy
infixr 1 -->


goal :: Type -> Judgement
goal = mkFirstJudgement mempty True


runAuto :: Type -> Either [TacticError] (LHsExpr GhcPs)
runAuto g = fmap rtr_extract $ runTactic ctx (goal g) auto

