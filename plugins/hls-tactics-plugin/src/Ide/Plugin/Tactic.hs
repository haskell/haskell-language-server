{-# LANGUAGE DeriveAnyClass      #-}
{-# LANGUAGE DeriveGeneric       #-}
{-# LANGUAGE GADTs               #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE NumDecimals         #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections       #-}
{-# LANGUAGE TypeApplications    #-}
{-# LANGUAGE ViewPatterns        #-}

-- | A plugin that uses tactics to synthesize code
module Ide.Plugin.Tactic
  ( descriptor
  , tacticTitle
  , TacticCommand (..)
  ) where

import           Bag (listToBag, bagToList)
import           Control.Arrow
import           Control.Monad
import           Control.Monad.Error.Class (MonadError(throwError))
import           Control.Monad.Trans
import           Control.Monad.Trans.Maybe
import           Data.Aeson
import           Data.Bool (bool)
import           Data.Coerce
import           Data.Data (Data)
import           Data.Functor ((<&>))
import           Data.Generics.Aliases (mkQ)
import           Data.Generics.Schemes (everything)
import           Data.Map (Map)
import qualified Data.Map as M
import           Data.Maybe
import           Data.Monoid
import qualified Data.Set as S
import qualified Data.Text as T
import           Data.Traversable
import           Development.IDE.Core.PositionMapping
import           Development.IDE.Core.RuleTypes
import           Development.IDE.Core.Service (runAction)
import           Development.IDE.Core.Shake (useWithStale, IdeState (..))
import           Development.IDE.GHC.Compat
import           Development.IDE.GHC.Error (realSrcSpanToRange)
import           Development.IDE.GHC.ExactPrint
import           Development.IDE.Spans.LocalBindings (getDefiningBindings)
import           Development.Shake (Action)
import qualified FastString
import           GHC.Generics (Generic)
import           GHC.LanguageExtensions.Type (Extension (LambdaCase))
import           Ide.Plugin.Tactic.Auto
import           Ide.Plugin.Tactic.CaseSplit
import           Ide.Plugin.Tactic.Context
import           Ide.Plugin.Tactic.GHC
import           Ide.Plugin.Tactic.Judgements
import           Ide.Plugin.Tactic.Range
import           Ide.Plugin.Tactic.Tactics
import           Ide.Plugin.Tactic.TestTypes
import           Ide.Plugin.Tactic.Types
import           Ide.PluginUtils
import           Ide.Types
import           Language.LSP.Server
import           Language.LSP.Types
import           OccName
import           Prelude hiding (span)
import           Refinery.Tactic (goal)
import           SrcLoc (containsSpan)
import           System.Timeout
import           TcRnTypes (tcg_binds)


descriptor :: PluginId -> PluginDescriptor IdeState
descriptor plId = (defaultPluginDescriptor plId)
    { pluginCommands
        = fmap (\tc ->
            PluginCommand
              (tcCommandId tc)
              (tacticDesc $ tcCommandName tc)
              (tacticCmd $ commandTactic tc))
              [minBound .. maxBound]
    , pluginHandlers = mkPluginHandler STextDocumentCodeAction codeActionProvider
    }

tacticDesc :: T.Text -> T.Text
tacticDesc name = "fill the hole using the " <> name <> " tactic"

------------------------------------------------------------------------------
-- | A 'TacticProvider' is a way of giving context-sensitive actions to the LS
-- UI.
type TacticProvider = DynFlags -> PluginId -> Uri -> Range -> Judgement -> IO [Command |? CodeAction]


------------------------------------------------------------------------------
-- | Construct a 'CommandId'
tcCommandId :: TacticCommand -> CommandId
tcCommandId c = coerce $ T.pack $ "tactics" <> show c <> "Command"


------------------------------------------------------------------------------
-- | The name of the command for the LS.
tcCommandName :: TacticCommand -> T.Text
tcCommandName = T.pack . show

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


------------------------------------------------------------------------------
-- | A mapping from tactic commands to actual tactics for refinery.
commandTactic :: TacticCommand -> OccName -> TacticsM ()
commandTactic Auto         = const auto
commandTactic Intros       = const intros
commandTactic Destruct     = useNameFromHypothesis destruct
commandTactic Homomorphism = useNameFromHypothesis homo
commandTactic DestructLambdaCase     = const destructLambdaCase
commandTactic HomomorphismLambdaCase = const homoLambdaCase


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


codeActionProvider :: PluginMethodHandler IdeState TextDocumentCodeAction
codeActionProvider state plId (CodeActionParams _ _ (TextDocumentIdentifier uri) range _ctx)
  | Just nfp <- uriToNormalizedFilePath $ toNormalizedUri uri =
      liftIO $ fromMaybeT (Right $ List []) $ do
        (_, jdg, _, dflags) <- judgementForHole state nfp range
        actions <- lift $
          -- This foldMap is over the function monoid.
          foldMap commandProvider [minBound .. maxBound]
            dflags
            plId
            uri
            range
            jdg
        pure $ Right $ List actions
codeActionProvider _ _ _ = pure $ Right $ codeActions []


codeActions :: [CodeAction] -> List (Command |? CodeAction)
codeActions = List . fmap InR


------------------------------------------------------------------------------
-- | Terminal constructor for providing context-sensitive tactics. Tactics
-- given by 'provide' are always available.
provide :: TacticCommand -> T.Text -> TacticProvider
provide tc name _ plId uri range _ = do
  let title = tacticTitle tc name
      params = TacticParams { tp_file = uri , tp_range = range , tp_var_name = name }
      cmd = mkLspCommand plId (tcCommandId tc) title (Just [toJSON params])
  pure
    $ pure
    $ InR
    $ CodeAction title (Just CodeActionQuickFix) Nothing Nothing Nothing Nothing
    $ Just cmd


------------------------------------------------------------------------------
-- | Restrict a 'TacticProvider', making sure it appears only when the given
-- predicate holds for the goal.
requireExtension :: Extension -> TacticProvider -> TacticProvider
requireExtension ext tp dflags plId uri range jdg =
  case xopt ext dflags of
    True  -> tp dflags plId uri range jdg
    False -> pure []


------------------------------------------------------------------------------
-- | Restrict a 'TacticProvider', making sure it appears only when the given
-- predicate holds for the goal.
filterGoalType :: (Type -> Bool) -> TacticProvider -> TacticProvider
filterGoalType p tp dflags plId uri range jdg =
  case p $ unCType $ jGoal jdg of
    True  -> tp dflags plId uri range jdg
    False -> pure []


------------------------------------------------------------------------------
-- | Multiply a 'TacticProvider' for each binding, making sure it appears only
-- when the given predicate holds over the goal and binding types.
filterBindingType
    :: (Type -> Type -> Bool)  -- ^ Goal and then binding types.
    -> (OccName -> Type -> TacticProvider)
    -> TacticProvider
filterBindingType p tp dflags plId uri range jdg =
  let hy = jHypothesis jdg
      g  = jGoal jdg
   in fmap join $ for (unHypothesis hy) $ \hi ->
        let ty = unCType $ hi_type hi
         in case p (unCType g) ty of
              True  -> tp (hi_name hi) ty dflags plId uri range jdg
              False -> pure []


data TacticParams = TacticParams
    { tp_file :: Uri -- ^ Uri of the file to fill the hole in
    , tp_range :: Range -- ^ The range of the hole
    , tp_var_name :: T.Text
    }
  deriving (Show, Eq, Generic, ToJSON, FromJSON)


------------------------------------------------------------------------------
-- | Find the last typechecked module, and find the most specific span, as well
-- as the judgement at the given range.
judgementForHole
    :: IdeState
    -> NormalizedFilePath
    -> Range
    -> MaybeT IO (Range, Judgement, Context, DynFlags)
judgementForHole state nfp range = do
  (asts, amapping) <- MaybeT $ runIde state $ useWithStale GetHieAst nfp
  range' <- liftMaybe $ fromCurrentRange amapping range

  (binds, _) <- MaybeT $ runIde state $ useWithStale GetBindings nfp

  -- Ok to use the stale 'ModIface', since all we need is its 'DynFlags'
  -- which don't change very often.
  ((modsum,_), _) <- MaybeT $ runIde state $ useWithStale GetModSummaryWithoutTimestamps nfp
  let dflags = ms_hspp_opts modsum

  case asts of
    (HAR _ hf _ _ kind) -> do
      (rss, g) <- liftMaybe $ join $ listToMaybe $ M.elems $ flip M.mapWithKey (getAsts hf) $ \fs ast ->
          case selectSmallestContaining (rangeToRealSrcSpan (FastString.unpackFS fs) range') ast of
            Nothing -> Nothing
            Just ast' -> do
              let info = nodeInfo ast'
              ty <- listToMaybe $ nodeType info
              guard $ ("HsUnboundVar","HsExpr") `S.member` nodeAnnotations info
              pure (nodeSpan ast', ty)

      resulting_range <- liftMaybe $ toCurrentRange amapping $ realSrcSpanToRange rss
      (tcmod, _) <- MaybeT $ runIde state $ useWithStale TypeCheck nfp
      let tcg  = tmrTypechecked tcmod
          tcs = tcg_binds tcg
          ctx = mkContext
                  (mapMaybe (sequenceA . (occName *** coerce))
                    $ getDefiningBindings binds rss)
                  tcg
          top_provs = getRhsPosVals rss tcs
          local_hy = spliceProvenance top_provs
                   $ hypothesisFromBindings rss binds
          cls_hy = contextMethodHypothesis ctx
      case kind of
        HieFromDisk _hf' ->
          fail "Need a fresh hie file"
        HieFresh ->
          pure ( resulting_range
               , mkFirstJudgement
                   (local_hy <> cls_hy)
                   (isRhsHole rss tcs)
                   g
               , ctx
               , dflags
               )

spliceProvenance
    :: Map OccName Provenance
    -> Hypothesis a
    -> Hypothesis a
spliceProvenance provs x =
  Hypothesis $ flip fmap (unHypothesis x) $ \hi ->
    overProvenance (maybe id const $ M.lookup (hi_name hi) provs) hi


tacticCmd :: (OccName -> TacticsM ()) -> CommandFunction IdeState TacticParams
tacticCmd tac state (TacticParams uri range var_name)
  | Just nfp <- uriToNormalizedFilePath $ toNormalizedUri uri = do
      clientCapabilities <- getClientCapabilities
      res <- liftIO $ fromMaybeT (Right Nothing) $ do
        (range', jdg, ctx, dflags) <- judgementForHole state nfp range
        let span = rangeToRealSrcSpan (fromNormalizedFilePath nfp) range'
        pm <- MaybeT $ useAnnotatedSource "tacticsCmd" state nfp

        x <- lift $ timeout 2e8 $ pure $
          case runTactic ctx jdg $ tac $ mkVarOcc $ T.unpack var_name of
            Left err ->
              Left $ mkErr InvalidRequest $ T.pack $ show err
            Right rtr ->
              mkWorkspaceEdits rtr jdg span ctx dflags clientCapabilities uri pm
        pure $ joinNote (mkErr InvalidRequest "timed out") x

      case res of
        Left err -> pure $ Left err
        Right medit -> do
          forM_ medit $ \edit ->
            sendRequest SWorkspaceApplyEdit (ApplyWorkspaceEditParams Nothing edit) (\_ -> pure ())
          pure $ Right Null
tacticCmd _ _ _ =
  pure $ Left $ mkErr InvalidRequest "Bad URI"

mkErr :: ErrorCode -> T.Text -> ResponseError
mkErr code err = ResponseError code err Nothing


joinNote :: e -> Maybe (Either e a) -> Either e a
joinNote e Nothing = Left e
joinNote _ (Just a) = a


------------------------------------------------------------------------------
-- | Turn a 'RunTacticResults' into concrete edits to make in the source
-- document.
mkWorkspaceEdits rtr jdg span ctx dflags clientCapabilities uri pm = do
  let g = graftHole jdg (RealSrcSpan span) ctx rtr
      response = transform dflags clientCapabilities uri g pm
   in case response of
        Right res -> Right $ Just res
        Left err -> Left $ mkErr InternalError $ T.pack err


------------------------------------------------------------------------------
-- | Graft a 'RunTacticResults' into the correct place in an AST. Correctly
-- deals with top-level holes, in which we might need to fiddle with the
-- 'Match's that bind variables.
graftHole
  :: Judgement' a2
  -> SrcSpan
  -> Context
  -> RunTacticResults
  -> Graft (Either String) ParsedSource
graftHole jdg span ctx rtr
  | _jIsTopHole jdg
      = graftSmallestDeclsWithM span
      $ graftDecl span
      $ \pats ->
        splitToDecl (fst $ last $ ctxDefiningFuncs ctx)
      $ iterateSplit
      $ mkFirstAgda (fmap unXPat pats)
      $ unLoc
      $ rtr_extract rtr
graftHole jdg span _ rtr
  = graftWithoutParentheses span
    -- Parenthesize the extract iff we're not in a top level hole
  $ bool maybeParensAST id (_jIsTopHole jdg)
  $ rtr_extract rtr


------------------------------------------------------------------------------
-- | Merge in the 'Match'es of a 'FunBind' into a 'HsDecl'. Used to perform
-- agda-style case splitting in which we need to separate one 'Match' into
-- many, without affecting any matches which might exist but don't need to be
-- split.
mergeFunBindMatches
    :: ([Pat GhcPs] -> LHsDecl GhcPs)
    -> SrcSpan
    -> HsBind GhcPs
    -> Either String (HsBind GhcPs)
mergeFunBindMatches make_decl span (fb@FunBind {fun_matches = mg@MG {mg_alts = L alts_src alts}}) =
  pure $
    fb
      { fun_matches = mg
        { mg_alts = L alts_src $ do
            alt@(L alt_src match) <- alts
            case span `isSubspanOf` alt_src of
              True -> do
                let pats = fmap fromPatCompatPs $ m_pats match
                    (L _ (ValD _ (FunBind {fun_matches = MG {mg_alts = L _ to_add}}))) =
                        make_decl pats
                to_add
              False -> pure alt
        }
      }
mergeFunBindMatches _ _ _ = Left "mergeFunBindMatches: called on something that isnt a funbind"


noteT :: String -> TransformT (Either String) a
noteT = lift . Left

------------------------------------------------------------------------------
-- | Helper function to route 'mergeFunBindMatches' into the right place in an
-- AST --- correctly dealing with inserting into instance declarations.
graftDecl
    :: SrcSpan
    -> ([Pat GhcPs] -> LHsDecl GhcPs)
    -> LHsDecl GhcPs
    -> TransformT (Either String) (Maybe [LHsDecl GhcPs])
graftDecl span
    make_decl
    (L src (ValD ext fb))
  = either noteT (pure . Just . pure . L src . ValD ext) $
      mergeFunBindMatches make_decl span fb
-- TODO(sandy): add another case for default methods in class definitions
graftDecl span
    make_decl
    (L src (InstD ext cid@ClsInstD{cid_inst = cidi@ClsInstDecl{cid_sigs = _sigs, cid_binds = binds}}))
  = do
      binds' <-
        for (bagToList binds) $ \b@(L bsrc bind) -> do
          case bind of
            fb@FunBind{}
              | span `isSubspanOf` bsrc -> either noteT (pure . L bsrc) $ mergeFunBindMatches make_decl span fb
            _ -> pure b

      pure $ Just $ pure $ L src $ InstD ext $ cid
        { cid_inst = cidi
          { cid_binds = listToBag binds'
          }
        }
graftDecl span _ x = do
  traceMX "biggest" $ unsafeRender $ locateBiggest @(Match GhcPs (LHsExpr GhcPs)) span x
  traceMX "first" $ unsafeRender $ locateFirst @(Match GhcPs (LHsExpr GhcPs)) x
  noteT "graftDecl: don't know about this AST form"



fromMaybeT :: Functor m => a -> MaybeT m a -> m a
fromMaybeT def = fmap (fromMaybe def) . runMaybeT


liftMaybe :: Monad m => Maybe a -> MaybeT m a
liftMaybe a = MaybeT $ pure a


------------------------------------------------------------------------------
-- | Is this hole immediately to the right of an equals sign?
isRhsHole :: RealSrcSpan -> TypecheckedSource -> Bool
isRhsHole rss tcs = everything (||) (mkQ False $ \case
  TopLevelRHS _ _ (L (RealSrcSpan span) _) -> containsSpan rss span
  _ -> False
  ) tcs


------------------------------------------------------------------------------
-- | Compute top-level position vals of a function
getRhsPosVals :: RealSrcSpan -> TypecheckedSource -> Map OccName Provenance
getRhsPosVals rss tcs
  = M.fromList
  $ join
  $ maybeToList
  $ getFirst
  $ everything (<>) (mkQ mempty $ \case
      TopLevelRHS name ps
          (L (RealSrcSpan span)  -- body with no guards and a single defn
            (HsVar _ (L _ hole)))
        | containsSpan rss span  -- which contains our span
        , isHole $ occName hole  -- and the span is a hole
        -> First $ do
            patnames <- traverse getPatName ps
            pure $ zip patnames $ [0..] <&> TopLevelArgPrv name
      _ -> mempty
  ) tcs


locateBiggest :: (Data r, Data a) => SrcSpan -> a -> Maybe r
locateBiggest ss x = getFirst $ everything (<>)
  ( mkQ mempty $ \case
    L span r | ss `isSubspanOf` span -> pure r
    _ -> mempty
  )x

locateFirst :: (Data r, Data a) => a -> Maybe r
locateFirst x = getFirst $ everything (<>)
  ( mkQ mempty $ \case
    r -> pure r
  ) x

