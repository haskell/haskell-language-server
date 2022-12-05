{-# LANGUAGE CPP               #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Wingman.LanguageServer.TacticProviders
  ( commandProvider
  , commandTactic
  , TacticProviderData (..)
  ) where

import           Control.Monad
import           Data.Bool (bool)
import           Data.Coerce
import           Data.Maybe
import           Data.Monoid
import qualified Data.Set as S
import qualified Data.Text as T
import           Development.IDE.GHC.Compat
import           Ide.Types
import           Language.LSP.Types hiding (SemanticTokenAbsolute (..), SemanticTokenRelative (..))
import           Prelude hiding (span)
import           Wingman.AbstractLSP.Types
import           Wingman.GHC
import           Wingman.Judgements
import           Wingman.Machinery (useNameFromHypothesis, uncoveredDataCons)
import           Wingman.Tactics
import           Wingman.Types


------------------------------------------------------------------------------
-- | A mapping from tactic commands to actual tactics for refinery.
commandTactic :: TacticCommand -> T.Text -> TacticsM ()
commandTactic Intros                 = const intros
commandTactic IntroAndDestruct       = const introAndDestruct
commandTactic Destruct               = useNameFromHypothesis destruct . mkVarOcc . T.unpack
commandTactic DestructPun            = useNameFromHypothesis destructPun . mkVarOcc . T.unpack
commandTactic Homomorphism           = useNameFromHypothesis homo . mkVarOcc . T.unpack
commandTactic DestructLambdaCase     = const destructLambdaCase
commandTactic HomomorphismLambdaCase = const homoLambdaCase
commandTactic DestructAll            = const destructAll
commandTactic UseDataCon             = userSplit . mkVarOcc . T.unpack
commandTactic Refine                 = const refine


------------------------------------------------------------------------------
-- | The LSP kind
tacticKind :: TacticCommand -> T.Text
tacticKind Intros                 = "introduceLambda"
tacticKind IntroAndDestruct       = "introduceAndDestruct"
tacticKind Destruct               = "caseSplit"
tacticKind DestructPun            = "caseSplitPun"
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
tacticPreferred Intros                 = True
tacticPreferred IntroAndDestruct       = True
tacticPreferred Destruct               = True
tacticPreferred DestructPun            = False
tacticPreferred Homomorphism           = True
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
commandProvider Intros =
  filterGoalType isFunction $
    provide Intros ""
commandProvider IntroAndDestruct =
  filterGoalType (liftLambdaCase False (\_ -> isJust . algebraicTyCon)) $
    provide IntroAndDestruct ""
commandProvider Destruct =
  filterBindingType destructFilter $ \occ _ ->
    provide Destruct $ T.pack $ occNameString occ
commandProvider DestructPun =
    filterBindingType destructPunFilter $ \occ _ ->
      provide DestructPun $ T.pack $ occNameString occ
commandProvider Homomorphism =
  filterBindingType homoFilter $ \occ _ ->
    provide Homomorphism $ T.pack $ occNameString occ
commandProvider DestructLambdaCase =
  requireExtension LambdaCase $
    filterGoalType (isJust . lambdaCaseable) $
      provide DestructLambdaCase ""
commandProvider HomomorphismLambdaCase =
  requireExtension LambdaCase $
    filterGoalType (liftLambdaCase False homoFilter) $
      provide HomomorphismLambdaCase ""
commandProvider DestructAll =
    withJudgement $ \jdg ->
      case _jIsTopHole jdg && jHasBoundArgs jdg of
        True  -> provide DestructAll ""
        False -> mempty
commandProvider UseDataCon =
  withConfig $ \cfg ->
    filterTypeProjection
        ( guardLength (<= cfg_max_use_ctor_actions cfg)
        . maybe [] fst
        . tacticsGetDataCons
        ) $ \dcon ->
      provide UseDataCon
        . T.pack
        . occNameString
        . occName
        $ dataConName dcon
commandProvider Refine =
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
    -> [(Metadata, T.Text)]


data TacticProviderData = TacticProviderData
  { tpd_lspEnv :: LspEnv
  , tpd_jdg    :: Judgement
  }



------------------------------------------------------------------------------
-- | Restrict a 'TacticProvider', making sure it appears only when the given
-- predicate holds for the goal.
requireExtension :: Extension -> TacticProvider -> TacticProvider
requireExtension ext tp tpd =
  case xopt ext $ le_dflags $ tpd_lspEnv tpd of
    True  -> tp tpd
    False -> []


------------------------------------------------------------------------------
-- | Restrict a 'TacticProvider', making sure it appears only when the given
-- predicate holds for the goal.
filterGoalType :: (Type -> Bool) -> TacticProvider -> TacticProvider
filterGoalType p tp tpd =
  case p $ unCType $ jGoal $ tpd_jdg tpd of
    True  -> tp tpd
    False -> []


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
      hy  = jLocalHypothesis jdg
      g   = jGoal jdg
   in unHypothesis hy >>= \hi ->
        let ty = unCType $ hi_type hi
         in case p (unCType g) ty of
              True  -> tp (hi_name hi) ty tpd
              False -> []


------------------------------------------------------------------------------
-- | Multiply a 'TacticProvider' by some feature projection out of the goal
-- type. Used e.g. to crete a code action for every data constructor.
filterTypeProjection
    :: (Type -> [a])  -- ^ Features of the goal to look into further
    -> (a -> TacticProvider)
    -> TacticProvider
filterTypeProjection p tp tpd =
  (p $ unCType $ jGoal $ tpd_jdg tpd) >>= \a ->
      tp a tpd


------------------------------------------------------------------------------
-- | Get access to the 'Config' when building a 'TacticProvider'.
withConfig :: (Config -> TacticProvider) -> TacticProvider
withConfig tp tpd = tp (le_config $ tpd_lspEnv tpd) tpd


------------------------------------------------------------------------------
-- | Terminal constructor for providing context-sensitive tactics. Tactics
-- given by 'provide' are always available.
provide :: TacticCommand -> T.Text -> TacticProvider
provide tc name _ =
  pure (Metadata (tacticTitle tc name) (mkTacticKind tc) (tacticPreferred tc), name)


------------------------------------------------------------------------------
-- | Construct a 'CommandId'
tcCommandId :: TacticCommand -> CommandId
tcCommandId c = coerce $ T.pack $ "tactics" <> show c <> "Command"


------------------------------------------------------------------------------
-- | We should show homos only when the goal type is the same as the binding
-- type, and that both are usual algebraic types.
homoFilter :: Type -> Type -> Bool
homoFilter codomain domain =
  case uncoveredDataCons domain codomain of
    Just s -> S.null s
    _ -> False


------------------------------------------------------------------------------
-- | Lift a function of (codomain, domain) over a lambda case.
liftLambdaCase :: r -> (Type -> Type -> r) -> Type -> r
liftLambdaCase nil f t =
  case tacticsSplitFunTy t of
    (_, _, arg : _, res) -> f res $ scaledThing arg
    _ -> nil



------------------------------------------------------------------------------
-- | We should show destruct for bindings only when those bindings have usual
-- algebraic types.
destructFilter :: Type -> Type -> Bool
destructFilter _ (algebraicTyCon -> Just _) = True
destructFilter _ _ = False


------------------------------------------------------------------------------
-- | We should show destruct punning for bindings only when those bindings have
-- usual algebraic types, and when any of their data constructors are records.
destructPunFilter :: Type -> Type -> Bool
destructPunFilter _ (algebraicTyCon -> Just tc) =
  not . all (null . dataConFieldLabels) $ tyConDataCons tc
destructPunFilter _ _ = False


instance IsContinuationSort TacticCommand where
  toCommandId = tcCommandId

