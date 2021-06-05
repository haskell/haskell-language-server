{-# OPTIONS_GHC -fno-warn-orphans #-}

module AutoTupleSpec where

import           Data.Either                  (isRight)
import           Wingman.Judgements (mkFirstJudgement)
import           Wingman.Machinery
import           Wingman.Tactics    (auto')
import           Wingman.Types
import           OccName                      (mkVarOcc)
import           Test.Hspec
import           Test.QuickCheck
import           Type                         (mkTyVarTy)
import           TysPrim                      (alphaTyVars)
import           TysWiredIn                   (mkBoxedTupleTy)


spec :: Spec
spec = describe "auto for tuple" $ do
  it "should always be able to discover an auto solution" $ do
    property $ do
      -- Pick some number of variables
      n <- choose (1, 7)
      let vars = fmap mkTyVarTy $ take n alphaTyVars
      -- Pick a random ordering
      in_vars  <- shuffle vars
      -- Randomly associate them into tuple types
      in_type  <- mkBoxedTupleTy
                . fmap mkBoxedTupleTy
              <$> randomGroups in_vars
      out_type <- mkBoxedTupleTy
                . fmap mkBoxedTupleTy
              <$> randomGroups vars
      pure $
          -- We should always be able to find a solution
          runTactic
            emptyContext
            (mkFirstJudgement
              emptyContext
              (Hypothesis $ pure $ HyInfo (mkVarOcc "x") UserPrv $ CType in_type)
              True
              out_type)
            (auto' $ n * 2) `shouldSatisfy` isRight


randomGroups :: [a] -> Gen [[a]]
randomGroups [] = pure []
randomGroups as = do
  n <- choose (1, length as)
  (:) <$> pure (take n as)
      <*> randomGroups (drop n as)

