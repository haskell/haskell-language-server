module Wingman.KnownStrategies where

import Data.Foldable (for_)
import Development.IDE.GHC.Compat.Core
import Refinery.Tactic
import Wingman.Judgements (jGoal)
import Wingman.KnownStrategies.QuickCheck (deriveArbitrary)
import Wingman.Machinery (tracing, getKnownInstance, getCurrentDefinitions)
import Wingman.Tactics
import Wingman.Types


knownStrategies :: TacticsM ()
knownStrategies = choice
  [ known "fmap" deriveFmap
  , known "mempty" deriveMempty
  , known "arbitrary" deriveArbitrary
  , known "<>" deriveMappend
  , known "mappend" deriveMappend
  ]


known :: String -> TacticsM () -> TacticsM ()
known name t = do
  getCurrentDefinitions >>= \case
    [(def, _)] | def == mkVarOcc name ->
      tracing ("known " <> name) t
    _ -> failure NoApplicableTactic


deriveFmap :: TacticsM ()
deriveFmap = do
  try intros
  overAlgebraicTerms homo
  choice
    [ overFunctions (apply Saturated) >> auto' 2
    , assumption
    , recursion
    ]


------------------------------------------------------------------------------
-- | We derive mappend by binding the arguments, introducing the constructor,
-- and then calling mappend recursively. At each recursive call, we filter away
-- any binding that isn't in an analogous position.
--
-- The recursive call first attempts to use an instance in scope. If that fails,
-- it falls back to trying a theta method from the hypothesis with the correct
-- name.
deriveMappend :: TacticsM ()
deriveMappend = do
  try intros
  destructAll
  split
  g <- goal
  minst <- getKnownInstance (mkClsOcc "Semigroup")
         . pure
         . unCType
         $ jGoal g
  for_ minst $ \(cls, df) -> do
    restrictPositionForApplication
      (applyMethod cls df $ mkVarOcc "<>")
      assumption
  try $
    restrictPositionForApplication
      (applyByName $ mkVarOcc "<>")
      assumption


------------------------------------------------------------------------------
-- | We derive mempty by introducing the constructor, and then trying to
-- 'mempty' everywhere. This smaller 'mempty' might come from an instance in
-- scope, or it might come from the hypothesis theta.
deriveMempty :: TacticsM ()
deriveMempty = do
  split
  g <- goal
  minst <- getKnownInstance (mkClsOcc "Monoid") [unCType $ jGoal g]
  for_ minst $ \(cls, df) -> do
    applyMethod cls df $ mkVarOcc "mempty"
  try assumption

