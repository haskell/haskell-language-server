{-# LANGUAGE ViewPatterns #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module UnificationSpec where

import           Control.Arrow
import           Data.Bool (bool)
import           Data.Functor ((<&>))
import           Data.Maybe (mapMaybe)
import qualified Data.Set as S
import           Data.Traversable
import           Data.Tuple (swap)
import           TcType (substTy, tcGetTyVar_maybe)
import           Test.Hspec
import           Test.QuickCheck
import           Type (mkTyVarTy)
import           TysPrim (alphaTyVars)
import           TysWiredIn (mkBoxedTupleTy)
import           Wingman.Machinery
import           Wingman.Types


spec :: Spec
spec = describe "unification" $ do
  it "should be able to unify univars with skolems on either side of the equality" $ do
    property $ do
      -- Pick some number of unification vars and skolem
      n <- choose (1, 1)
      let (skolems, take n -> univars) = splitAt n $ fmap mkTyVarTy alphaTyVars
      -- Randomly pair them
      skolem_uni_pairs <-
        for (zip skolems univars) randomSwap
      let (lhs, rhs)
            = mkBoxedTupleTy *** mkBoxedTupleTy
            $ unzip skolem_uni_pairs
      pure $
        counterexample (show skolems) $
        counterexample (show lhs) $
        counterexample (show rhs) $
          case tryUnifyUnivarsButNotSkolems
                 (S.fromList $ mapMaybe tcGetTyVar_maybe skolems)
                 (CType lhs)
                 (CType rhs) of
            Just subst ->
              -- For each pair, running the unification over the univar should
              -- result in the skolem
              conjoin $ zip univars skolems <&> \(uni, skolem) ->
                let substd = substTy subst uni
                 in counterexample (show substd) $
                    counterexample (show skolem) $
                      CType substd === CType skolem
            Nothing -> True === False


randomSwap :: (a, a) -> Gen (a, a)
randomSwap ab = do
  which <- arbitrary
  pure $ bool swap id which ab


