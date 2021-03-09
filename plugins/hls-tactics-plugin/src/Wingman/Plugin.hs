{-# LANGUAGE OverloadedStrings #-}

-- | A plugin that uses tactics to synthesize code
module Wingman.Plugin
  ( descriptor
  , tacticTitle
  , TacticCommand (..)
  ) where

import           Control.Exception (evaluate)
import           Control.Monad
import           Control.Monad.Trans
import           Control.Monad.Trans.Maybe
import           Data.Aeson
import           Data.Bifunctor (first)
import           Data.Foldable (for_)
import           Data.Generics.Schemes (everywhereM)
import           Data.Maybe
import           Data.Proxy (Proxy(..))
import qualified Data.Text as T
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
  for_ (rtr_other_solns rtr) $ traceMX "other solution"
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
      = genericGraftWithSmallestM (Proxy @(Located [LMatch GhcPs (LHsExpr GhcPs)])) span $ \dflags ->
        everywhereM
          $ mkBindListT $ graftDecl dflags span $ \pats ->
            splitToDecl (fst $ last $ ctxDefiningFuncs $ rtr_ctx rtr)
          $ iterateSplit
          $ mkFirstAgda (fmap unXPat pats)
          $ unLoc
          $ rtr_extract rtr
graftHole span rtr
  = graft span
  $ rtr_extract rtr


------------------------------------------------------------------------------
-- | Helper function to route 'mergeFunBindMatches' into the right place in an
-- AST --- correctly dealing with inserting into instance declarations.
graftDecl
    :: DynFlags
    -> SrcSpan
    -> ([Pat GhcPs] -> LHsDecl GhcPs)
    -> LMatch GhcPs (LHsExpr GhcPs)
    -> TransformT (Either String) [LMatch GhcPs (LHsExpr GhcPs)]
graftDecl dflags dst make_decl (L src (AMatch _ pats _))
  | dst `isSubspanOf` src = do
      L _ dec <- annotateDecl dflags $ make_decl pats
      case dec of
        ValD _ (FunBind { fun_matches = MG { mg_alts = L _ alts@(_:_)}
                  }) -> do
          pure alts
        _ -> undefined
graftDecl _ _ _ x = pure $ pure x


fromMaybeT :: Functor m => a -> MaybeT m a -> m a
fromMaybeT def = fmap (fromMaybe def) . runMaybeT

