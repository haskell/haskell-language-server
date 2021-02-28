{-# LANGUAGE DeriveAnyClass     #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE ViewPatterns       #-}
{-# OPTIONS_GHC -Wall           #-}

module Ide.Plugin.Tactic.LanguageServer.TacticProviders
  ( commandProvider
  , commandTactic
  , tcCommandId
  , TacticParams (..)
  ) where

import           Control.Monad
import           Control.Monad.Error.Class (MonadError (throwError))
import           Data.Aeson
import           Data.Bool (bool)
import           Data.Coerce
import qualified Data.Map as M
import           Data.Maybe
import           Data.Monoid
import qualified Data.Text as T
import           Data.Traversable
import           DataCon (dataConName, dataConCannotMatch)
import           Development.IDE.GHC.Compat
import           GHC.Generics
import           GHC.LanguageExtensions.Type (Extension (LambdaCase))
import           Ide.Plugin.Tactic.Auto
import           Ide.Plugin.Tactic.FeatureSet
import           Ide.Plugin.Tactic.GHC
import           Ide.Plugin.Tactic.Judgements
import           Ide.Plugin.Tactic.Tactics
import           Ide.Plugin.Tactic.TestTypes
import           Ide.Plugin.Tactic.Types
import           Ide.PluginUtils
import           Ide.Types
import           Language.LSP.Types
import           OccName
import           Prelude hiding (span)
import           Refinery.Tactic (goal)


------------------------------------------------------------------------------
-- | A mapping from tactic commands to actual tactics for refinery.
commandTactic :: TacticCommand -> OccName -> TacticsM ()
commandTactic Auto                   = const auto
commandTactic Intros                 = const intros
commandTactic Destruct               = useNameFromHypothesis destruct
commandTactic Homomorphism           = useNameFromHypothesis homo
commandTactic DestructLambdaCase     = const destructLambdaCase
commandTactic HomomorphismLambdaCase = const homoLambdaCase
commandTactic UseDataCon             = userSplit


------------------------------------------------------------------------------
-- | Mapping from tactic commands to their contextual providers. See 'provide',
-- 'filterGoalType' and 'filterBindingType' for the nitty gritty.
commandProvider :: TacticCommand -> TacticProvider
commandProvider Auto  = provide Auto ""
commandProvider Intros =
  filterGoalType isFunction $
    provide Intros ""
commandProvider Destruct =
  filterBindingType destructFilter $ \occ _ ->
    provide Destruct $ T.pack $ occNameString occ
commandProvider Homomorphism =
  filterBindingType homoFilter $ \occ _ ->
    provide Homomorphism $ T.pack $ occNameString occ
commandProvider DestructLambdaCase =
  requireExtension LambdaCase $
    filterGoalType (isJust . lambdaCaseable) $
      provide DestructLambdaCase ""
commandProvider HomomorphismLambdaCase =
  requireExtension LambdaCase $
    filterGoalType ((== Just True) . lambdaCaseable) $
      provide HomomorphismLambdaCase ""
commandProvider UseDataCon =
  withConfig $ \cfg ->
    requireFeature FeatureUseDataCon $
      filterTypeProjection
          ( guardLength (<= cfg_max_use_ctor_actions cfg)
          . useCtorFilter
          ) $ \dcon ->
        provide UseDataCon
          . T.pack
          . occNameString
          . occName
          $ dataConName dcon


------------------------------------------------------------------------------
-- | Return an empty list if the given predicate doesn't hold over the length
guardLength :: (Int -> Bool) -> [a] -> [a]
guardLength f as = bool [] as $ f $ length as


------------------------------------------------------------------------------
-- | A 'TacticProvider' is a way of giving context-sensitive actions to the LS
-- UI.
type TacticProvider
     = DynFlags
    -> Config
    -> PluginId
    -> Uri
    -> Range
    -> Judgement
    -> IO [Command |? CodeAction]


data TacticParams = TacticParams
    { tp_file     :: Uri    -- ^ Uri of the file to fill the hole in
    , tp_range    :: Range  -- ^ The range of the hole
    , tp_var_name :: T.Text
    }
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToJSON, FromJSON)


------------------------------------------------------------------------------
-- | Restrict a 'TacticProvider', making sure it appears only when the given
-- 'Feature' is in the feature set.
requireFeature :: Feature -> TacticProvider -> TacticProvider
requireFeature f tp dflags cfg plId uri range jdg = do
  case hasFeature f $ cfg_feature_set cfg of
    True  -> tp dflags cfg plId uri range jdg
    False -> pure []


------------------------------------------------------------------------------
-- | Restrict a 'TacticProvider', making sure it appears only when the given
-- predicate holds for the goal.
requireExtension :: Extension -> TacticProvider -> TacticProvider
requireExtension ext tp dflags cfg plId uri range jdg =
  case xopt ext dflags of
    True  -> tp dflags cfg plId uri range jdg
    False -> pure []


------------------------------------------------------------------------------
-- | Restrict a 'TacticProvider', making sure it appears only when the given
-- predicate holds for the goal.
filterGoalType :: (Type -> Bool) -> TacticProvider -> TacticProvider
filterGoalType p tp dflags cfg plId uri range jdg =
  case p $ unCType $ jGoal jdg of
    True  -> tp dflags cfg plId uri range jdg
    False -> pure []


------------------------------------------------------------------------------
-- | Multiply a 'TacticProvider' for each binding, making sure it appears only
-- when the given predicate holds over the goal and binding types.
filterBindingType
    :: (Type -> Type -> Bool)  -- ^ Goal and then binding types.
    -> (OccName -> Type -> TacticProvider)
    -> TacticProvider
filterBindingType p tp dflags cfg plId uri range jdg =
  let hy = jHypothesis jdg
      g  = jGoal jdg
   in fmap join $ for (unHypothesis hy) $ \hi ->
        let ty = unCType $ hi_type hi
         in case p (unCType g) ty of
              True  -> tp (hi_name hi) ty dflags cfg plId uri range jdg
              False -> pure []


------------------------------------------------------------------------------
-- | Multiply a 'TacticProvider' by some feature projection out of the goal
-- type. Used e.g. to crete a code action for every data constructor.
filterTypeProjection
    :: (Type -> [a])  -- ^ Features of the goal to look into further
    -> (a -> TacticProvider)
    -> TacticProvider
filterTypeProjection p tp dflags cfg plId uri range jdg =
  fmap join $ for (p $ unCType $ jGoal jdg) $ \a ->
      tp a dflags cfg plId uri range jdg


------------------------------------------------------------------------------
-- | Get access to the 'Config' when building a 'TacticProvider'.
withConfig :: (Config -> TacticProvider) -> TacticProvider
withConfig tp dflags cfg plId uri range jdg = tp cfg dflags cfg plId uri range jdg



------------------------------------------------------------------------------
-- | Lift a function over 'HyInfo's to one that takes an 'OccName' and tries to
-- look it up in the hypothesis.
useNameFromHypothesis :: (HyInfo CType -> TacticsM a) -> OccName -> TacticsM a
useNameFromHypothesis f name = do
  hy <- jHypothesis <$> goal
  case M.lookup name $ hyByName hy of
    Just hi -> f hi
    Nothing -> throwError $ NotInScope name


------------------------------------------------------------------------------
-- | Terminal constructor for providing context-sensitive tactics. Tactics
-- given by 'provide' are always available.
provide :: TacticCommand -> T.Text -> TacticProvider
provide tc name _ _ plId uri range _ = do
  let title = tacticTitle tc name
      params = TacticParams { tp_file = uri , tp_range = range , tp_var_name = name }
      cmd = mkLspCommand plId (tcCommandId tc) title (Just [toJSON params])
  pure
    $ pure
    $ InR
    $ CodeAction title (Just CodeActionQuickFix) Nothing Nothing Nothing Nothing
    $ Just cmd


------------------------------------------------------------------------------
-- | Construct a 'CommandId'
tcCommandId :: TacticCommand -> CommandId
tcCommandId c = coerce $ T.pack $ "tactics" <> show c <> "Command"


------------------------------------------------------------------------------
-- | We should show homos only when the goal type is the same as the binding
-- type, and that both are usual algebraic types.
homoFilter :: Type -> Type -> Bool
homoFilter (algebraicTyCon -> Just t1) (algebraicTyCon -> Just t2) = t1 == t2
homoFilter _ _                                                     = False


------------------------------------------------------------------------------
-- | We should show destruct for bindings only when those bindings have usual
-- algebraic types.
destructFilter :: Type -> Type -> Bool
destructFilter _ (algebraicTyCon -> Just _) = True
destructFilter _ _                          = False


------------------------------------------------------------------------------
-- | Only show data cons in "Use constructor" if they can unify with the goal
useCtorFilter :: Type -> [DataCon]
useCtorFilter ty
  | Just (dcs, apps) <- tacticsGetDataCons ty = do
      dc <- dcs
      guard $ not $ dataConCannotMatch apps dc
      pure dc
useCtorFilter _ = []

