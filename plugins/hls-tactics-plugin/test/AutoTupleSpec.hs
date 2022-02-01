{-# LANGUAGE NumDecimals #-}

{-# OPTIONS_GHC -fno-warn-orphans #-}

module AutoTupleSpec where

import Control.Monad (replicateM)
import Control.Monad.State (evalState)
import Data.Either (isRight)
import Development.IDE.GHC.Compat.Core ( mkVarOcc, mkBoxedTupleTy )
import System.IO.Unsafe
import Test.Hspec
import Test.QuickCheck
import Wingman.Judgements (mkFirstJudgement)
import Wingman.Machinery
import Wingman.Tactics (auto')
import Wingman.Types


spec :: Spec
spec = describe "auto for tuple" $ do
  it "should always be able to discover an auto solution" $ do
    property $ do
      -- Pick some number of variables
      n <- choose (1, 7)
      let vars = flip evalState defaultTacticState
               $ replicateM n newUnivar
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
          unsafePerformIO
            (runTactic
              2e6
              emptyContext
              (mkFirstJudgement
                emptyContext
                (Hypothesis $ pure $ HyInfo (mkVarOcc "x") UserPrv $ CType in_type)
                True
                out_type)
              (auto' $ n * 2)) `shouldSatisfy` isRight


randomGroups :: [a] -> Gen [[a]]
randomGroups [] = pure []
randomGroups as = do
  n <- choose (1, length as)
  (:) <$> pure (take n as)
      <*> randomGroups (drop n as)

