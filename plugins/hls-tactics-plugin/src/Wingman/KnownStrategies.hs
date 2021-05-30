module Wingman.KnownStrategies where

import Control.Monad.Error.Class
import OccName (mkVarOcc)
import Refinery.Tactic
import Wingman.Context (getCurrentDefinitions, getKnownInstance)
import Wingman.KnownStrategies.QuickCheck (deriveArbitrary)
import Wingman.Machinery (tracing)
import Wingman.Tactics
import Wingman.Types
import Wingman.Judgements (jGoal)
import Data.Foldable (for_)
import Wingman.FeatureSet
import Control.Applicative (empty)
import Control.Monad.Reader.Class (asks)


knownStrategies :: TacticsM ()
knownStrategies = choice
  [ known "fmap" deriveFmap
  , known "mempty" deriveMempty
  , known "arbitrary" deriveArbitrary
  , featureGuard FeatureKnownMonoid $ known "<>" deriveMappend
  , featureGuard FeatureKnownMonoid $ known "mappend" deriveMappend
  ]


------------------------------------------------------------------------------
-- | Guard a tactic behind a feature.
featureGuard :: Feature -> TacticsM a -> TacticsM a
featureGuard feat t = do
  fs <- asks $ cfg_feature_set . ctxConfig
  case hasFeature feat fs of
    True -> t
    False -> empty


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


------------------------------------------------------------------------------
-- | We derive mappend by binding the arguments, introducing the constructor,
-- and then calling mappend recursively. At each recursive call, we filter away
-- any binding that isn't in an analogous position.
--
-- The recursive call first attempts to use an instace in scope. If that fails,
-- it fals back to trying a theta method from the hypothesis with the correct
-- name.
deriveMappend :: TacticsM ()
deriveMappend = do
  try intros
  destructAll
  split
  g <- goal
  minst <- getKnownInstance kt_semigroup
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
  minst <- getKnownInstance kt_monoid [unCType $ jGoal g]
  for_ minst $ \(cls, df) -> do
    applyMethod cls df $ mkVarOcc "mempty"
  try assumption

