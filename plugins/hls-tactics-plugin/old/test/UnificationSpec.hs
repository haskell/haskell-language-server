{-# LANGUAGE CPP          #-}
{-# LANGUAGE ViewPatterns #-}

{-# OPTIONS_GHC -fno-warn-orphans #-}

module UnificationSpec where

import           Control.Arrow
import           Control.Monad (replicateM, join)
import           Control.Monad.State (evalState)
import           Data.Bool (bool)
import           Data.Functor ((<&>))
import           Data.Maybe (mapMaybe)
import qualified Data.Set as S
import           Data.Traversable
import           Data.Tuple (swap)
import           Development.IDE.GHC.Compat.Core (substTy, mkBoxedTupleTy)
import           Test.Hspec
import           Test.QuickCheck
import           Wingman.GHC
import           Wingman.Machinery (newUnivar)
import           Wingman.Types

#if __GLASGOW_HASKELL__ >= 900
import GHC.Tc.Utils.TcType (tcGetTyVar_maybe)
#else
import TcType  (tcGetTyVar_maybe)
#endif


spec :: Spec
spec = describe "unification" $ do
  it "should be able to unify univars with skolems on either side of the equality" $ do
    property $ do
      -- Pick some number of unification vars and skolem
      n <- choose (1, 20)
      let (skolems, take n -> univars)
            = splitAt n
            $ flip evalState defaultTacticState
            $ replicateM (n * 2) newUnivar
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
              conjoin $ join $
                [ -- For each pair, running the unification over the univar should
                  -- result in the skolem
                  zip univars skolems <&> \(uni, skolem) ->
                    let substd = substTy subst uni
                    in counterexample (show substd) $
                        counterexample (show skolem) $
                          CType substd === CType skolem

                  -- And also, no two univars should equal to one another
                  -- before or after substitution.
                , zip univars (tail univars) <&> \(uni1, uni2) ->
                    let uni1_sub = substTy subst uni1
                        uni2_sub = substTy subst uni2
                    in counterexample (show uni1) $
                        counterexample (show uni2) $
                          CType uni1 =/= CType uni2 .&&.
                          CType uni1_sub =/= CType uni2_sub
                ]
            Nothing -> True === False


randomSwap :: (a, a) -> Gen (a, a)
randomSwap ab = do
  which <- arbitrary
  pure $ bool swap id which ab


