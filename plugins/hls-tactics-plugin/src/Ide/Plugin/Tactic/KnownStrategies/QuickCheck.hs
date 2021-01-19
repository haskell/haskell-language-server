{-# LANGUAGE ViewPatterns #-}

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


------------------------------------------------------------------------------
-- | Known tactic for deriving @arbitrary :: Gen a@. This tactic splits the
-- type's data cons into terminal and inductive cases, and generates code that
-- produces a terminal if the QuickCheck size parameter is <=1, or any data con
-- otherwise. It correctly scales recursive parameters, ensuring termination.
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


------------------------------------------------------------------------------
-- | Helper data type for the generator of a specific data con.
data Generator = Generator
  { genRecursiveCount :: Integer
  , genExpr :: HsExpr GhcPs
  }


------------------------------------------------------------------------------
-- | Make a 'Generator' for a given tycon instantiated with the given @[Type]@.
mkGenerator :: TyCon -> [Type] -> DataCon -> Generator
mkGenerator tc apps dc = do
  let dc_expr   = var' $ occName $ dataConName dc
      args = dataConInstOrigArgTys' dc apps
      num_recursive_calls = sum $ fmap (bool 0 1 . doesTypeContain tc) args
      mkArbitrary = mkArbitraryCall tc num_recursive_calls
  Generator num_recursive_calls $ case args of
    []  -> mkFunc "pure" @@ dc_expr
    (a : as) ->
      foldl'
        (infixCall "<*>")
        (infixCall "<$>" dc_expr $ mkArbitrary a)
        (fmap mkArbitrary as)


------------------------------------------------------------------------------
-- | Check if the given 'TyCon' exists anywhere in the 'Type'.
doesTypeContain :: TyCon -> Type -> Bool
doesTypeContain recursive_tc =
  everything (||) $ mkQ False (== recursive_tc)


------------------------------------------------------------------------------
-- | Generate the correct sort of call to @arbitrary@. For recursive calls, we
-- need to scale down the size parameter, either by a constant factor of 1 if
-- it's the only recursive parameter, or by @`div` n@ where n is the number of
-- recursive parameters. For all other types, just call @arbitrary@ directly.
mkArbitraryCall :: TyCon -> Integer -> Type -> HsExpr GhcPs
mkArbitraryCall recursive_tc n ty =
  let arbitrary = mkFunc "arbitrary"
   in case doesTypeContain recursive_tc ty of
        True ->
          mkFunc "scale"
            @@ bool (mkFunc "flip" @@ mkFunc "div" @@ int n)
                    (mkFunc "subtract" @@ int 1)
                    (n == 1)
            @@ arbitrary
        False -> arbitrary
