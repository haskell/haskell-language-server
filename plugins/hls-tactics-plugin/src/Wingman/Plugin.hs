{-# LANGUAGE OverloadedStrings #-}

-- | A plugin that uses tactics to synthesize code
module Wingman.Plugin
  ( descriptor
  , tacticTitle
  , TacticCommand (..)
  ) where

import           Bag (bagToList, listToBag)
import           Control.Exception (evaluate)
import           Control.Monad
import           Control.Monad.Trans
import           Control.Monad.Trans.Maybe
import           Data.Aeson
import           Data.Bifunctor (first)
import           Data.Data (Data)
import           Data.Foldable (for_)
import           Data.Generics.Aliases (mkQ)
import           Data.Generics.Schemes (everything)
import           Data.Maybe
import           Data.Monoid
import qualified Data.Text as T
import           Data.Traversable
import           Development.IDE.Core.Shake (IdeState (..))
import           Development.IDE.GHC.Compat
import           Development.IDE.GHC.ExactPrint
import           Ide.Types
import           Language.LSP.Server
import           Language.LSP.Types
import           Language.LSP.Types.Capabilities
import           OccName
import           Prelude hiding (span)
import           System.Timeout
import           Wingman.CaseSplit
import           Wingman.GHC
import           Wingman.LanguageServer
import           Wingman.LanguageServer.TacticProviders
import           Wingman.Machinery (scoreSolution)
import           Wingman.Range
import           Wingman.Tactics
import           Wingman.Types


descriptor :: PluginId -> PluginDescriptor IdeState
descriptor plId = (defaultPluginDescriptor plId)
    { pluginCommands
        = fmap (\tc ->
            PluginCommand
              (tcCommandId tc)
              (tacticDesc $ tcCommandName tc)
              (tacticCmd $ commandTactic tc))
              [minBound .. maxBound]
    , pluginHandlers =
        mkPluginHandler STextDocumentCodeAction codeActionProvider
    }



codeActionProvider :: PluginMethodHandler IdeState TextDocumentCodeAction
codeActionProvider state plId (CodeActionParams _ _ (TextDocumentIdentifier uri) range _ctx)
  | Just nfp <- uriToNormalizedFilePath $ toNormalizedUri uri = do
      cfg <- getTacticConfig $ shakeExtras state
      liftIO $ fromMaybeT (Right $ List []) $ do
        (_, jdg, _, dflags) <- judgementForHole state nfp range $ cfg_feature_set cfg
        actions <- lift $
          -- This foldMap is over the function monoid.
          foldMap commandProvider [minBound .. maxBound]
            dflags
            cfg
            plId
            uri
            range
            jdg
        pure $ Right $ List actions
codeActionProvider _ _ _ = pure $ Right $ List []


showUserFacingMessage
    :: MonadLsp cfg m
    => UserFacingMessage
    -> m (Either ResponseError a)
showUserFacingMessage ufm = do
  showLspMessage $ mkShowMessageParams ufm
  pure $ Left $ mkErr InternalError $ T.pack $ show ufm


tacticCmd :: (OccName -> TacticsM ()) -> CommandFunction IdeState TacticParams
tacticCmd tac state (TacticParams uri range var_name)
  | Just nfp <- uriToNormalizedFilePath $ toNormalizedUri uri = do
      features <- getFeatureSet $ shakeExtras state
      ccs <- getClientCapabilities
      res <- liftIO $ runMaybeT $ do
        (range', jdg, ctx, dflags) <- judgementForHole state nfp range features
        let span = rangeToRealSrcSpan (fromNormalizedFilePath nfp) range'
        pm <- MaybeT $ useAnnotatedSource "tacticsCmd" state nfp

        timingOut 2e8 $ join $
          case runTactic ctx jdg $ tac $ mkVarOcc $ T.unpack var_name of
            Left _ -> Left TacticErrors
            Right rtr ->
              case rtr_extract rtr of
                L _ (HsVar _ (L _ rdr)) | isHole (occName rdr) ->
                  Left NothingToDo
                _ -> pure $ mkWorkspaceEdits span dflags ccs uri pm rtr

      case res of
        Nothing -> do
          showUserFacingMessage TimedOut
        Just (Left ufm) -> do
          showUserFacingMessage ufm
        Just (Right edit) -> do
          _ <- sendRequest
            SWorkspaceApplyEdit
            (ApplyWorkspaceEditParams Nothing edit)
            (const $ pure ())
          pure $ Right Null
tacticCmd _ _ _ =
  pure $ Left $ mkErr InvalidRequest "Bad URI"


timingOut
    :: Int  -- ^ Time in microseconds
    -> a    -- ^ Computation to run
    -> MaybeT IO a
timingOut t m = MaybeT $ timeout t $ evaluate m


mkErr :: ErrorCode -> T.Text -> ResponseError
mkErr code err = ResponseError code err Nothing


------------------------------------------------------------------------------
-- | Turn a 'RunTacticResults' into concrete edits to make in the source
-- document.
mkWorkspaceEdits
    :: RealSrcSpan
    -> DynFlags
    -> ClientCapabilities
    -> Uri
    -> Annotated ParsedSource
    -> RunTacticResults
    -> Either UserFacingMessage WorkspaceEdit
mkWorkspaceEdits span dflags ccs uri pm rtr = do
  for_ (rtr_other_solns rtr) $ \soln -> do
    traceMX "other solution" soln
    traceMX "with score" $ scoreSolution soln (rtr_jdg rtr) []
  traceMX "solution" $ rtr_extract rtr
  let g = graftHole (RealSrcSpan span) rtr
      response = transform dflags ccs uri g pm
   in first (InfrastructureError . T.pack) response


------------------------------------------------------------------------------
-- | Graft a 'RunTacticResults' into the correct place in an AST. Correctly
-- deals with top-level holes, in which we might need to fiddle with the
-- 'Match's that bind variables.
graftHole
    :: SrcSpan
    -> RunTacticResults
    -> Graft (Either String) ParsedSource
graftHole span rtr
  | _jIsTopHole (rtr_jdg rtr)
      = graftSmallestDeclsWithM span
      $ graftDecl span $ \pats ->
        splitToDecl (fst $ last $ ctxDefiningFuncs $ rtr_ctx rtr)
      $ iterateSplit
      $ mkFirstAgda (fmap unXPat pats)
      $ unLoc
      $ rtr_extract rtr
graftHole span rtr
  = graft span
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
mergeFunBindMatches make_decl span
    (fb@FunBind {fun_matches = mg@MG {mg_alts = L alts_src alts}}) =
  pure $ fb
    { fun_matches = mg
      { mg_alts = L alts_src $ do
          alt@(L alt_src match) <- alts
          case span `isSubspanOf` alt_src of
            True -> do
              let pats = fmap fromPatCompatPs $ m_pats match
                  L _ (ValD _ (FunBind {fun_matches = MG
                        {mg_alts = L _ to_add}})) = make_decl pats
              to_add
            False -> pure alt
      }
    }
mergeFunBindMatches _ _ _ =
  Left "mergeFunBindMatches: called on something that isnt a funbind"


throwError :: String -> TransformT (Either String) a
throwError = lift . Left


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
  = either throwError (pure . Just . pure . L src . ValD ext) $
      mergeFunBindMatches make_decl span fb
-- TODO(sandy): add another case for default methods in class definitions
graftDecl span
    make_decl
    (L src (InstD ext
      cid@ClsInstD{cid_inst =
        cidi@ClsInstDecl{cid_sigs = _sigs, cid_binds = binds}}))
  = do
      binds' <-
        for (bagToList binds) $ \b@(L bsrc bind) -> do
          case bind of
            fb@FunBind{} | span `isSubspanOf` bsrc ->
              either throwError (pure . L bsrc) $
                mergeFunBindMatches make_decl span fb
            _ -> pure b

      pure $ Just $ pure $ L src $ InstD ext $ cid
        { cid_inst = cidi
          { cid_binds = listToBag binds'
          }
        }
graftDecl span _ x = do
  traceMX "biggest" $
    unsafeRender $
      locateBiggest @(Match GhcPs (LHsExpr GhcPs)) span x
  traceMX "first" $
    unsafeRender $
      locateFirst @(Match GhcPs (LHsExpr GhcPs)) x
  throwError "graftDecl: don't know about this AST form"


fromMaybeT :: Functor m => a -> MaybeT m a -> m a
fromMaybeT def = fmap (fromMaybe def) . runMaybeT


locateBiggest :: (Data r, Data a) => SrcSpan -> a -> Maybe r
locateBiggest ss x = getFirst $ everything (<>)
  ( mkQ mempty $ \case
    L span r | ss `isSubspanOf` span -> pure r
    _                                -> mempty
  ) x


locateFirst :: (Data r, Data a) => a -> Maybe r
locateFirst x = getFirst $ everything (<>)
  ( mkQ mempty $ \case
    r -> pure r
  ) x


