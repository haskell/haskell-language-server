{-# LANGUAGE CPP               #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

module Wingman.LanguageServer.TacticProviders
  ( commandProvider
  , commandTactic
  , tcCommandId
  , TacticParams (..)
  , TacticProviderData (..)
  , useNameFromHypothesis
  ) where

import           Control.Monad
import           Control.Monad.Reader (runReaderT)
import           Data.Aeson
import           Data.Bool (bool)
import           Data.Coerce
import           Data.Maybe
import           Data.Monoid
import qualified Data.Text as T
import           Data.Traversable
import           DataCon (dataConName)
import           Development.IDE.Core.UseStale (Tracked, Age(..))
import           Development.IDE.GHC.Compat
import           GHC.Generics
import           GHC.LanguageExtensions.Type (Extension (LambdaCase))
import           Ide.PluginUtils
import           Ide.Types
import           Language.LSP.Types
import           OccName
import           Prelude hiding (span)
import           Wingman.Auto
import           Wingman.GHC
import           Wingman.Judgements
import           Wingman.Machinery (useNameFromHypothesis)
import           Wingman.Metaprogramming.Lexer (ParserContext)
import           Wingman.Metaprogramming.Parser (parseMetaprogram)
import           Wingman.Tactics
import           Wingman.Types


------------------------------------------------------------------------------
-- | A mapping from tactic commands to actual tactics for refinery.
commandTactic :: ParserContext -> TacticCommand -> T.Text -> IO (TacticsM ())
commandTactic _ Auto                   = pure . const auto
commandTactic _ Intros                 = pure . const intros
commandTactic _ Destruct               = pure . useNameFromHypothesis destruct . mkVarOcc . T.unpack
commandTactic _ DestructPun            = pure . useNameFromHypothesis destructPun . mkVarOcc . T.unpack
commandTactic _ Homomorphism           = pure . useNameFromHypothesis homo . mkVarOcc . T.unpack
commandTactic _ DestructLambdaCase     = pure . const destructLambdaCase
commandTactic _ HomomorphismLambdaCase = pure . const homoLambdaCase
commandTactic _ DestructAll            = pure . const destructAll
commandTactic _ UseDataCon             = pure . userSplit . mkVarOcc . T.unpack
commandTactic _ Refine                 = pure . const refine
commandTactic _ BeginMetaprogram       = pure . const metaprogram
commandTactic c RunMetaprogram         = flip runReaderT c . parseMetaprogram


------------------------------------------------------------------------------
-- | The LSP kind
tacticKind :: TacticCommand -> T.Text
tacticKind Auto                   = "fillHole"
tacticKind Intros                 = "introduceLambda"
tacticKind Destruct               = "caseSplit"
tacticKind DestructPun            = "caseSplitPun"
tacticKind Homomorphism           = "homomorphicCaseSplit"
tacticKind DestructLambdaCase     = "lambdaCase"
tacticKind HomomorphismLambdaCase = "homomorphicLambdaCase"
tacticKind DestructAll            = "splitFuncArgs"
tacticKind UseDataCon             = "useConstructor"
tacticKind Refine                 = "refine"
tacticKind BeginMetaprogram       = "beginMetaprogram"
tacticKind RunMetaprogram         = "runMetaprogram"


------------------------------------------------------------------------------
-- | Whether or not this code action is preferred -- ostensibly refers to
-- whether or not we can bind it to a key in vs code?
tacticPreferred :: TacticCommand -> Bool
tacticPreferred Auto                   = True
tacticPreferred Intros                 = True
tacticPreferred Destruct               = True
tacticPreferred DestructPun            = False
tacticPreferred Homomorphism           = False
tacticPreferred DestructLambdaCase     = False
tacticPreferred HomomorphismLambdaCase = False
tacticPreferred DestructAll            = True
tacticPreferred UseDataCon             = True
tacticPreferred Refine                 = True
tacticPreferred BeginMetaprogram       = False
tacticPreferred RunMetaprogram         = True


mkTacticKind :: TacticCommand -> CodeActionKind
mkTacticKind =
  CodeActionUnknown . mappend "refactor.wingman." . tacticKind


------------------------------------------------------------------------------
-- | Mapping from tactic commands to their contextual providers. See 'provide',
-- 'filterGoalType' and 'filterBindingType' for the nitty gritty.
commandProvider :: TacticCommand -> TacticProvider
commandProvider Auto  =
  requireHoleSort (== Hole) $
  provide Auto ""
commandProvider Intros =
  requireHoleSort (== Hole) $
  filterGoalType isFunction $
    provide Intros ""
commandProvider Destruct =
  requireHoleSort (== Hole) $
  filterBindingType destructFilter $ \occ _ ->
    provide Destruct $ T.pack $ occNameString occ
commandProvider DestructPun =
  requireHoleSort (== Hole) $
    filterBindingType destructPunFilter $ \occ _ ->
      provide DestructPun $ T.pack $ occNameString occ
commandProvider Homomorphism =
  requireHoleSort (== Hole) $
  filterBindingType homoFilter $ \occ _ ->
    provide Homomorphism $ T.pack $ occNameString occ
commandProvider DestructLambdaCase =
  requireHoleSort (== Hole) $
  requireExtension LambdaCase $
    filterGoalType (isJust . lambdaCaseable) $
      provide DestructLambdaCase ""
commandProvider HomomorphismLambdaCase =
  requireHoleSort (== Hole) $
  requireExtension LambdaCase $
    filterGoalType ((== Just True) . lambdaCaseable) $
      provide HomomorphismLambdaCase ""
commandProvider DestructAll =
  requireHoleSort (== Hole) $
    withJudgement $ \jdg ->
      case _jIsTopHole jdg && jHasBoundArgs jdg of
        True  -> provide DestructAll ""
        False -> mempty
commandProvider UseDataCon =
  requireHoleSort (== Hole) $
  withConfig $ \cfg ->
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
  requireHoleSort (== Hole) $
    provide Refine ""
commandProvider BeginMetaprogram =
  requireGHC88OrHigher $
  requireHoleSort (== Hole) $
    provide BeginMetaprogram ""
commandProvider RunMetaprogram =
  requireGHC88OrHigher $
  withMetaprogram $ \mp ->
    provide RunMetaprogram mp


requireGHC88OrHigher :: TacticProvider -> TacticProvider
requireGHC88OrHigher tp tpd =
#if __GLASGOW_HASKELL__ >= 808
  tp tpd
#else
  mempty
#endif


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
  , tpd_range  :: Tracked 'Current Range
  , tpd_jdg    :: Judgement
  , tpd_hole_sort :: HoleSort
  }


data TacticParams = TacticParams
    { tp_file     :: Uri    -- ^ Uri of the file to fill the hole in
    , tp_range    :: Tracked 'Current Range  -- ^ The range of the hole
    , tp_var_name :: T.Text
    }
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToJSON, FromJSON)


requireHoleSort :: (HoleSort -> Bool) -> TacticProvider -> TacticProvider
requireHoleSort p tp tpd =
  case p $ tpd_hole_sort tpd of
    True  -> tp tpd
    False -> pure []

withMetaprogram :: (T.Text -> TacticProvider) -> TacticProvider
withMetaprogram tp tpd =
  case tpd_hole_sort tpd of
    Metaprogram mp -> tp mp tpd
    _ -> pure []


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
      hy  = jLocalHypothesis jdg
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


------------------------------------------------------------------------------
-- | We should show destruct punning for bindings only when those bindings have
-- usual algebraic types, and when any of their data constructors are records.
destructPunFilter :: Type -> Type -> Bool
destructPunFilter _ (algebraicTyCon -> Just tc) =
  any (not . null . dataConFieldLabels) $ tyConDataCons tc
destructPunFilter _ _                          = False

