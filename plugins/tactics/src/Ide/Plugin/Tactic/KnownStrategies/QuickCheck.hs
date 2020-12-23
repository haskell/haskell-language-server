{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE LambdaCase #-}

module Ide.Plugin.Tactic.KnownStrategies.QuickCheck where

import Control.Monad.Except (MonadError(throwError))
import Data.Bool (bool)
import Data.List (partition)
import DataCon ( DataCon, dataConName )
import Development.IDE.GHC.Compat (HsExpr, GhcPs, noLoc)
import GHC.Exts ( IsString(fromString) )
import GHC.List ( foldl' )
import GHC.SourceGen (int)
import GHC.SourceGen.Binds ( match, valBind )
import GHC.SourceGen.Expr ( case', lambda, let' )
import GHC.SourceGen.Overloaded ( App((@@)), HasList(list) )
import GHC.SourceGen.Pat ( conP )
import Ide.Plugin.Tactic.CodeGen
import Ide.Plugin.Tactic.Judgements (jGoal)
import Ide.Plugin.Tactic.Machinery (tracePrim)
import Ide.Plugin.Tactic.Types
import OccName (occNameString,  mkVarOcc, HasOccName(occName) )
import Refinery.Tactic (goal,  rule )
import TyCon (tyConName,  TyCon, tyConDataCons )
import Type ( splitTyConApp_maybe )
import Data.Generics (mkQ, everything)


deriveArbitrary :: TacticsM ()
deriveArbitrary = do
  ty <- jGoal <$> goal
  case splitTyConApp_maybe $ unCType ty of
    Just (gen_tc, [splitTyConApp_maybe -> Just (tc, apps)])
        | occNameString (occName $ tyConName gen_tc) == "Gen" -> do
      rule $ \_ -> do
        let dcs = tyConDataCons tc
            (terminal, big) = partition ((== 0) . genRecursiveCount)
                        $ fmap (mkGenerator tc apps) dcs
            terminal_expr = mkVal "terminal"
            oneof_expr = mkVal "oneof"
        pure
          ( tracePrim "deriveArbitrary"
          , noLoc $
              let' [valBind (fromString "terminal") $ list $ fmap genExpr terminal] $
                appDollar (mkFunc "sized") $ lambda [bvar' (mkVarOcc "n")] $
                  case' (infixCall "<=" (mkVal "n") (int 1))
                    [ match [conP (fromString "True") []] $
                        oneof_expr @@ terminal_expr
                    , match [conP (fromString "False") []] $
                        appDollar oneof_expr $
                          infixCall "<>"
                            (list $ fmap genExpr big)
                            terminal_expr
                    ]
          )
    _ -> throwError $ GoalMismatch "deriveArbitrary" ty



data Generator = Generator
  { genRecursiveCount :: Integer
  , genExpr :: HsExpr GhcPs
  }


mkGenerator :: TyCon -> [Type] -> DataCon -> Generator
mkGenerator tc apps dc = do
  let dc_expr   = var' $ occName $ dataConName dc
      args = dataConInstOrigArgTys' dc apps
      num_recursive_calls = sum $ fmap (bool 0 1 . isRecursiveValue tc) args
      mkArbitrary = mkArbitraryCall tc num_recursive_calls
  Generator num_recursive_calls $ case args of
    []  -> mkFunc "pure" @@ dc_expr
    (a : as) ->
      foldl'
        (infixCall "<*>")
        (infixCall "<$>" dc_expr $ mkArbitrary a)
        (fmap mkArbitrary as)


isRecursiveValue :: TyCon -> Type -> Bool
isRecursiveValue recursive_tc =
  everything (||) $ mkQ False (== recursive_tc)


mkArbitraryCall :: TyCon -> Integer -> Type -> HsExpr GhcPs
mkArbitraryCall recursive_tc n ty =
  let arbitrary = mkFunc "arbitrary"
   in case isRecursiveValue recursive_tc ty of
        True ->
          mkFunc "scale"
            @@ bool (mkFunc "flip" @@ mkFunc "div" @@ int n)
                    (mkFunc "subtract" @@ int 1)
                    (n == 1)
            @@ arbitrary
        False -> arbitrary

