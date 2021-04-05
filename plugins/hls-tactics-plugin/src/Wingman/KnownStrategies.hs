module Wingman.KnownStrategies where

import Control.Monad.Error.Class
import OccName (mkVarOcc, mkMethodOcc)
import Refinery.Tactic
import Wingman.Context (getCurrentDefinitions, getKnownInstance)
import Wingman.KnownStrategies.QuickCheck (deriveArbitrary)
import Wingman.Machinery (tracing)
import Wingman.Tactics
import Wingman.Types
import Wingman.Judgements (jGoal)
import Data.Foldable (for_)


knownStrategies :: TacticsM ()
knownStrategies = choice
  [ known "fmap" deriveFmap
  , known "mempty" deriveMempty
  , known "arbitrary" deriveArbitrary
  ]


known :: String -> TacticsM () -> TacticsM ()
known name t = do
  getCurrentDefinitions >>= \case
    [(def, _)] | def == mkVarOcc name ->
      tracing ("known " <> name) t
    _ -> throwError NoApplicableTactic


deriveFmap :: TacticsM ()
deriveFmap = do
  try intros
  overAlgebraicTerms homo
  choice
    [ overFunctions apply >> auto' 2
    , assumption
    , recursion
    ]


deriveMempty :: TacticsM ()
deriveMempty = do
  split
  g <- goal
  minst <- getKnownInstance kt_monoid [unCType $ jGoal g]
  for_ minst $ \inst ->
    applyMethod inst $ mkVarOcc "mempty"


