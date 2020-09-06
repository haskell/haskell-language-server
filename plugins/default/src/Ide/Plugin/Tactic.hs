{-# LANGUAGE DeriveAnyClass    #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections     #-}
{-# LANGUAGE TypeApplications  #-}
{-# LANGUAGE ViewPatterns      #-}

-- | A plugin that uses tactics to synthesize code
module Ide.Plugin.Tactic
  ( descriptor
  , tacticTitle
  , TacticCommand (..)
  ) where

import           Control.Monad
import           Control.Monad.Trans
import           Control.Monad.Trans.Maybe
import           Data.Aeson
import           Data.Coerce
import qualified Data.Map as M
import           Data.Maybe
import qualified Data.Text as T
import           Data.Traversable
import           Development.IDE.Core.PositionMapping
import           Development.IDE.Core.RuleTypes (TcModuleResult (tmrModule), TypeCheck (..), GhcSession(..), GetHieAst (..), refMap)
import           Development.IDE.Core.Service (runAction)
import           Development.IDE.Core.Shake (useWithStale, IdeState (..))
import           Development.IDE.GHC.Compat
import           Development.IDE.GHC.Error (srcSpanToRange)
import           Development.IDE.GHC.Util (hscEnv)
import           Development.Shake (Action)
import           GHC.Generics (Generic)
import           HscTypes (hsc_dflags)
import           Ide.LocalBindings (bindings, mostSpecificSpan, holify)
import           Ide.Plugin (mkLspCommand)
import           Ide.Plugin.Tactic.Machinery
import           Ide.Plugin.Tactic.Tactics
import           Ide.TreeTransform (transform, graft, useAnnotatedSource)
import           Ide.Types
import           Language.Haskell.LSP.Core (clientCapabilities)
import           Language.Haskell.LSP.Types
import           OccName
import           Type


descriptor :: PluginId -> PluginDescriptor
descriptor plId = (defaultPluginDescriptor plId)
    { pluginCommands
        = fmap (\tc ->
            PluginCommand
              (tcCommandId tc)
              (tacticDesc $ tcCommandName tc)
              (tacticCmd $ commandTactic tc))
              enabledTactics
    , pluginCodeActionProvider = Just codeActionProvider
    }

tacticDesc :: T.Text -> T.Text
tacticDesc name = "fill the hole using the " <> name <> " tactic"

------------------------------------------------------------------------------
-- | The list of tactics exposed to the outside world. These are attached to
-- actual tactics via 'commandTactic' and are contextually provided to the
-- editor via 'commandProvider'.
data TacticCommand
  = Auto
  | Split
  | Intro
  | Intros
  | Destruct
  | Homomorphism
  deriving (Eq, Ord, Show, Enum, Bounded)


enabledTactics :: [TacticCommand]
enabledTactics = [Intros, Destruct, Homomorphism]


------------------------------------------------------------------------------
-- | A 'TacticProvider' is a way of giving context-sensitive actions to the LS
-- UI.
type TacticProvider = PluginId -> Uri -> Range -> Judgement -> IO [CAResult]


------------------------------------------------------------------------------
-- | Construct a 'CommandId'
tcCommandId :: TacticCommand -> CommandId
tcCommandId c = coerce $ T.pack $ "tactics" <> show c <> "Command"


------------------------------------------------------------------------------
-- | The name of the command for the LS.
tcCommandName :: TacticCommand -> T.Text
tcCommandName = T.pack . show


------------------------------------------------------------------------------
-- | Generate a title for the command.
tacticTitle :: TacticCommand -> T.Text -> T.Text
tacticTitle Auto _ = "Auto"
tacticTitle Split _ = "Auto"
tacticTitle Intro _ = "Intro"
tacticTitle Intros _ = "Introduce lambda"
tacticTitle Destruct var = "Case split on " <> var
tacticTitle Homomorphism var = "Homomorphic case split on " <> var


------------------------------------------------------------------------------
-- | Mapping from tactic commands to their contextual providers. See 'provide',
-- 'filterGoalType' and 'filterBindingType' for the nitty gritty.
commandProvider :: TacticCommand -> TacticProvider
commandProvider Auto  = provide Auto ""
commandProvider Split = provide Split ""
commandProvider Intro =
  filterGoalType isFunction $
    provide Intro ""
commandProvider Intros =
  filterGoalType isFunction $
    provide Intros ""
commandProvider Destruct =
  filterBindingType destructFilter $ \occ _ ->
    provide Destruct $ T.pack $ occNameString occ
commandProvider Homomorphism =
  filterBindingType homoFilter $ \occ _ ->
    provide Homomorphism $ T.pack $ occNameString occ


------------------------------------------------------------------------------
-- | A mapping from tactic commands to actual tactics for refinery.
commandTactic :: TacticCommand -> OccName -> TacticsM ()
commandTactic Auto         = const auto
commandTactic Split        = const split
commandTactic Intro        = const intro
commandTactic Intros       = const intros
commandTactic Destruct     = destruct
commandTactic Homomorphism = homo


------------------------------------------------------------------------------
-- | We should show homos only when the goal type is the same as the binding
-- type, and that both are usual algebraic types.
homoFilter :: Type -> Type -> Bool
homoFilter (algebraicTyCon -> Just t1) (algebraicTyCon -> Just t2) = t1 == t2
homoFilter _ _ = False


------------------------------------------------------------------------------
-- | We should show destruct for bindings only when those bindings have usual
-- algebraic types.
destructFilter :: Type -> Type -> Bool
destructFilter _ (algebraicTyCon -> Just _) = True
destructFilter _ _ = False


runIde :: IdeState -> Action a -> IO a
runIde state = runAction "tactic" state


codeActionProvider :: CodeActionProvider
codeActionProvider _conf state plId (TextDocumentIdentifier uri) range _ctx
  | Just nfp <- uriToNormalizedFilePath $ toNormalizedUri uri =
      fromMaybeT (Right $ List []) $ do
        (pos, mss, jdg) <- MaybeT $ judgmentForHole state nfp range
        case mss of
          L span' (HsUnboundVar _ _) -> do
            resulting_range <-
              liftMaybe $ toCurrentRange pos =<< srcSpanToRange span'
            actions <- lift $
              -- This foldMap is over the function monoid.
              foldMap commandProvider enabledTactics
                plId
                uri
                resulting_range
                jdg
            pure $ Right $ List actions
          _ -> pure $ Right $ codeActions []
codeActionProvider _ _ _ _ _ _ = pure $ Right $ codeActions []


codeActions :: [CodeAction] -> List CAResult
codeActions = List . fmap CACodeAction


------------------------------------------------------------------------------
-- | Terminal constructor for providing context-sensitive tactics. Tactics
-- given by 'provide' are always available.
provide :: TacticCommand -> T.Text -> TacticProvider
provide tc name plId uri range _ = do
  let title = tacticTitle tc name
      params = TacticParams { file = uri , range = range , var_name = name }
  cmd <- mkLspCommand plId (tcCommandId tc) title (Just [toJSON params])
  pure
    $ pure
    $ CACodeAction
    $ CodeAction title (Just CodeActionQuickFix) Nothing Nothing
    $ Just cmd


------------------------------------------------------------------------------
-- | Restrict a 'TacticProvider', making sure it appears only when the given
-- predicate holds for the goal.
filterGoalType :: (Type -> Bool) -> TacticProvider -> TacticProvider
filterGoalType p tp plId uri range jdg@(Judgement _ (CType g)) =
  case p g of
    True  -> tp plId uri range jdg
    False -> pure []


------------------------------------------------------------------------------
-- | Multiply a 'TacticProvider' for each binding, making sure it appears only
-- when the given predicate holds over the goal and binding types.
filterBindingType
    :: (Type -> Type -> Bool)  -- ^ Goal and then binding types.
    -> (OccName -> Type -> TacticProvider)
    -> TacticProvider
filterBindingType p tp plId uri range jdg@(Judgement hys (CType g)) =
  fmap join $ for (M.toList hys) $ \(occ, CType ty) ->
    case p g ty of
      True  -> tp occ ty plId uri range jdg
      False -> pure []


data TacticParams = TacticParams
    { file :: Uri -- ^ Uri of the file to fill the hole in
    , range :: Range -- ^ The range of the hole
    , var_name :: T.Text
    }
  deriving (Show, Eq, Generic, ToJSON, FromJSON)


------------------------------------------------------------------------------
-- | Find the last typechecked module, and find the most specific span, as well
-- as the judgement at the given range.
judgmentForHole
    :: IdeState
    -> NormalizedFilePath
    -> Range
    -> IO (Maybe (PositionMapping, LHsExpr GhcTc, Judgement))
judgmentForHole state nfp range = runMaybeT $ do
  (tmr, pos) <- MaybeT $ runIde state $ useWithStale TypeCheck nfp
  range' <- liftMaybe $ fromCurrentRange pos range
  let span = rangeToSrcSpan (fromNormalizedFilePath nfp) range'
      mod = tmrModule tmr

  (mss@(L span' (HsVar _ (L _ v))))
    <- liftMaybe $ mostSpecificSpan @_ @GhcTc span (tm_typechecked_source mod)
  rss <-
    liftMaybe $ case span' of
      RealSrcSpan rss -> Just rss
      _               -> Nothing

  (har, _) <- MaybeT $ runIde state $ useWithStale GetHieAst nfp
  let refs = refMap har
      binds2 = bindings refs

  let goal = varType v
      hyps = hypothesisFromBindings rss binds2
  pure (pos, holify binds2 mss, Judgement hyps $ CType goal)


tacticCmd :: (OccName -> TacticsM ()) -> CommandFunction TacticParams
tacticCmd tac lf state (TacticParams uri range var_name)
  | Just nfp <- uriToNormalizedFilePath $ toNormalizedUri uri =
      fromMaybeT (Right Null, Nothing) $ do
        (pos, _, jdg) <- MaybeT $ judgmentForHole state nfp range
        -- Ok to use the stale 'ModIface', since all we need is its 'DynFlags'
        -- which don't change very often.
        (hscenv, _) <- MaybeT $ runIde state $ useWithStale GhcSession nfp
        let dflags = hsc_dflags $ hscEnv hscenv
        pm <- MaybeT $ useAnnotatedSource "tacticsCmd" state nfp
        case runTactic jdg
              $ tac
              $ mkVarOcc
              $ T.unpack var_name of
          Left err ->
            pure $ (, Nothing)
              $ Left
              $ ResponseError InvalidRequest (T.pack $ show err) Nothing
          Right res -> do
            range' <- liftMaybe $ toCurrentRange pos range
            let span = rangeToSrcSpan (fromNormalizedFilePath nfp) range'
                g = graft span res
            let response = transform dflags (clientCapabilities lf) uri g pm
            pure
              ( Right Null
              , Just (WorkspaceApplyEdit, ApplyWorkspaceEditParams response)
              )
tacticCmd _ _ _ _ =
  pure ( Left $ ResponseError InvalidRequest (T.pack "Bad URI") Nothing
       , Nothing
       )


fromMaybeT :: Functor m => a -> MaybeT m a -> m a
fromMaybeT def = fmap (fromMaybe def) . runMaybeT

liftMaybe :: Monad m => Maybe a -> MaybeT m a
liftMaybe a = MaybeT $ pure a

