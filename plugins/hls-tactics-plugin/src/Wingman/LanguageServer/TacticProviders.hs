{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

module Wingman.LanguageServer.TacticProviders
  ( commandProvider
  , commandTactic
  , tcCommandId
  , TacticParams (..)
  , TacticProviderData (..)
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
import           DataCon (dataConName)
import           Development.IDE.GHC.Compat
import           GHC.Generics
import           GHC.LanguageExtensions.Type (Extension (LambdaCase))
import           Wingman.Auto
import           Wingman.FeatureSet
import           Wingman.GHC
import           Wingman.Judgements
import           Wingman.Tactics
import           Wingman.Types
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
commandTactic DestructAll            = const destructAll
commandTactic UseDataCon             = userSplit
commandTactic Refine                 = const refine


------------------------------------------------------------------------------
-- | The LSP kind
tacticKind :: TacticCommand -> T.Text
tacticKind Auto                   = "fillHole"
tacticKind Intros                 = "introduceLambda"
tacticKind Destruct               = "caseSplit"
tacticKind Homomorphism           = "homomorphicCaseSplit"
tacticKind DestructLambdaCase     = "lambdaCase"
tacticKind HomomorphismLambdaCase = "homomorphicLambdaCase"
tacticKind DestructAll            = "splitFuncArgs"
tacticKind UseDataCon             = "useConstructor"
tacticKind Refine                 = "refine"


------------------------------------------------------------------------------
-- | Whether or not this code action is preferred -- ostensibly refers to
-- whether or not we can bind it to a key in vs code?
tacticPreferred :: TacticCommand -> Bool
tacticPreferred Auto                   = True
tacticPreferred Intros                 = True
tacticPreferred Destruct               = True
tacticPreferred Homomorphism           = False
tacticPreferred DestructLambdaCase     = False
tacticPreferred HomomorphismLambdaCase = False
tacticPreferred DestructAll            = True
tacticPreferred UseDataCon             = True
tacticPreferred Refine                 = True


mkTacticKind :: TacticCommand -> CodeActionKind
mkTacticKind =
  CodeActionUnknown . mappend "refactor.wingman." . tacticKind


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
commandProvider DestructAll =
  requireFeature FeatureDestructAll $
    withJudgement $ \jdg ->
      case _jIsTopHole jdg && jHasBoundArgs jdg of
        True  -> provide DestructAll ""
        False -> mempty
commandProvider UseDataCon =
  withConfig $ \cfg ->
    requireFeature FeatureUseDataCon $
      filterTypeProjection
          ( guardLength (<= cfg_max_use_ctor_actions cfg)
          . fromMaybe []
          . fmap fst
          . tacticsGetDataCons
          ) $ \dcon ->
        provide UseDataCon
          . T.pack
          . occNameString
          . occName
          $ dataConName dcon
commandProvider Refine =
  requireFeature FeatureRefineHole $
    provide Refine ""


------------------------------------------------------------------------------
-- | Return an empty list if the given predicate doesn't hold over the length
guardLength :: (Int -> Bool) -> [a] -> [a]
guardLength f as = bool [] as $ f $ length as


------------------------------------------------------------------------------
-- | A 'TacticProvider' is a way of giving context-sensitive actions to the LS
-- UI.
type TacticProvider
     = TacticProviderData
    -> IO [Command |? CodeAction]

data TacticProviderData = TacticProviderData
  { tpd_dflags :: DynFlags
  , tpd_config :: Config
  , tpd_plid   :: PluginId
  , tpd_uri    :: Uri
  , tpd_range  :: Range
  , tpd_jdg    :: Judgement
  }


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
requireFeature f tp tpd =
  case hasFeature f $ cfg_feature_set $ tpd_config tpd of
    True  -> tp tpd
    False -> pure []


------------------------------------------------------------------------------
-- | Restrict a 'TacticProvider', making sure it appears only when the given
-- predicate holds for the goal.
requireExtension :: Extension -> TacticProvider -> TacticProvider
requireExtension ext tp tpd =
  case xopt ext $ tpd_dflags tpd of
    True  -> tp tpd
    False -> pure []


------------------------------------------------------------------------------
-- | Restrict a 'TacticProvider', making sure it appears only when the given
-- predicate holds for the goal.
filterGoalType :: (Type -> Bool) -> TacticProvider -> TacticProvider
filterGoalType p tp tpd =
  case p $ unCType $ jGoal $ tpd_jdg tpd of
    True  -> tp tpd
    False -> pure []


------------------------------------------------------------------------------
-- | Restrict a 'TacticProvider', making sure it appears only when the given
-- predicate holds for the goal.
withJudgement :: (Judgement -> TacticProvider) -> TacticProvider
withJudgement tp tpd = tp (tpd_jdg tpd) tpd


------------------------------------------------------------------------------
-- | Multiply a 'TacticProvider' for each binding, making sure it appears only
-- when the given predicate holds over the goal and binding types.
filterBindingType
    :: (Type -> Type -> Bool)  -- ^ Goal and then binding types.
    -> (OccName -> Type -> TacticProvider)
    -> TacticProvider
filterBindingType p tp tpd =
  let jdg = tpd_jdg tpd
      hy  = jHypothesis jdg
      g   = jGoal jdg
   in fmap join $ for (unHypothesis hy) $ \hi ->
        let ty = unCType $ hi_type hi
         in case p (unCType g) ty of
              True  -> tp (hi_name hi) ty tpd
              False -> pure []


------------------------------------------------------------------------------
-- | Multiply a 'TacticProvider' by some feature projection out of the goal
-- type. Used e.g. to crete a code action for every data constructor.
filterTypeProjection
    :: (Type -> [a])  -- ^ Features of the goal to look into further
    -> (a -> TacticProvider)
    -> TacticProvider
filterTypeProjection p tp tpd =
  fmap join $ for (p $ unCType $ jGoal $ tpd_jdg tpd) $ \a ->
      tp a tpd


------------------------------------------------------------------------------
-- | Get access to the 'Config' when building a 'TacticProvider'.
withConfig :: (Config -> TacticProvider) -> TacticProvider
withConfig tp tpd = tp (tpd_config tpd) tpd



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
provide tc name TacticProviderData{..} = do
  let title = tacticTitle tc name
      params = TacticParams { tp_file = tpd_uri , tp_range = tpd_range , tp_var_name = name }
      cmd = mkLspCommand tpd_plid (tcCommandId tc) title (Just [toJSON params])
  pure
    $ pure
    $ InR
    $ CodeAction
        { _title       = title
        , _kind        = Just $ mkTacticKind tc
        , _diagnostics = Nothing
        , _isPreferred = Just $ tacticPreferred tc
        , _disabled    = Nothing
        , _edit        = Nothing
        , _command     = Just cmd
        , _xdata       = Nothing
        }


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

