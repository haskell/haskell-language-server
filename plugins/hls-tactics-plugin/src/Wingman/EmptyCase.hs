{-# LANGUAGE OverloadedStrings #-}

{-# LANGUAGE NoMonoLocalBinds  #-}

module Wingman.EmptyCase where

import           Control.Applicative (empty)
import           Control.Monad
import           Control.Monad.Except (runExcept)
import           Control.Monad.Trans
import           Control.Monad.Trans.Maybe
import           Data.Aeson
import           Data.Generics.Aliases (mkQ, GenericQ)
import           Data.Generics.Schemes (everything)
import           Data.Maybe
import           Data.Monoid
import qualified Data.Text as T
import           Data.Traversable
import           Development.IDE (hscEnv)
import           Development.IDE (realSrcSpanToRange)
import           Development.IDE.Core.RuleTypes
import           Development.IDE.Core.Shake (IdeState (..))
import           Development.IDE.Core.UseStale
import           Development.IDE.GHC.Compat
import           Development.IDE.GHC.ExactPrint
import           Development.IDE.Spans.LocalBindings (getLocalScope)
import           Ide.Types
import           Language.LSP.Server
import           Language.LSP.Types
import           OccName
import           Prelude hiding (span)
import           Prelude hiding (span)
import           TcRnTypes (tcg_binds)
import           Wingman.CodeGen (destructionFor)
import           Wingman.GHC
import           Wingman.Judgements
import           Wingman.LanguageServer
import           Wingman.Types


------------------------------------------------------------------------------
-- | The 'CommandId' for the empty case completion.
emptyCaseLensCommandId :: CommandId
emptyCaseLensCommandId = CommandId "wingman.emptyCase"


------------------------------------------------------------------------------
-- | A command function that just applies a 'WorkspaceEdit'.
workspaceEditHandler :: CommandFunction IdeState WorkspaceEdit
workspaceEditHandler _ideState wedit = do
  _ <- sendRequest SWorkspaceApplyEdit (ApplyWorkspaceEditParams Nothing wedit) (\_ -> pure ())
  return $ Right Null


------------------------------------------------------------------------------
-- | Provide the "empty case completion" code lens
codeLensProvider :: PluginMethodHandler IdeState TextDocumentCodeLens
codeLensProvider state plId (CodeLensParams _ _ (TextDocumentIdentifier uri))
  | Just nfp <- uriToNormalizedFilePath $ toNormalizedUri uri = do
      let stale a = runStaleIde "codeLensProvider" state nfp a

      cfg <- getTacticConfig plId
      ccs <- getClientCapabilities
      liftIO $ fromMaybeT (Right $ List []) $ do
        dflags <- getIdeDynflags state nfp
        TrackedStale pm _ <- stale GetAnnotatedParsedSource
        TrackedStale binds bind_map <- stale GetBindings
        holes <- emptyCaseScrutinees state nfp

        fmap (Right . List) $ for holes $ \(ss, ty) -> do
          binds_ss <- liftMaybe $ mapAgeFrom bind_map ss
          let bindings = getLocalScope (unTrack binds) $ unTrack binds_ss
              range = realSrcSpanToRange $ unTrack ss
          matches <-
            liftMaybe $
              destructionFor
                (foldMap (hySingleton . occName . fst) bindings)
                ty
          edits <- liftMaybe $ hush $
                mkWorkspaceEdits dflags ccs uri (unTrack pm) $
                  graftMatchGroup (RealSrcSpan $ unTrack ss) $
                    noLoc matches

          pure $
            CodeLens range
              (Just
                $ mkLspCommand
                    plId
                    emptyCaseLensCommandId
                    (mkEmptyCaseLensDesc ty)
                $ Just $ pure $ toJSON $ edits
              )
              Nothing
codeLensProvider _ _ _ = pure $ Right $ List []


------------------------------------------------------------------------------
-- | The description for the empty case lens.
mkEmptyCaseLensDesc :: Type -> T.Text
mkEmptyCaseLensDesc ty =
  "Wingman: Complete case constructors (" <> T.pack (unsafeRender ty) <> ")"


------------------------------------------------------------------------------
-- | Silence an error.
hush :: Either e a -> Maybe a
hush (Left _) = Nothing
hush (Right a) = Just a


------------------------------------------------------------------------------
-- | Graft a 'RunTacticResults' into the correct place in an AST. Correctly
-- deals with top-level holes, in which we might need to fiddle with the
-- 'Match's that bind variables.
graftMatchGroup
    :: SrcSpan
    -> Located [LMatch GhcPs (LHsExpr GhcPs)]
    -> Graft (Either String) ParsedSource
graftMatchGroup ss l =
  hoistGraft (runExcept . runExceptString) $ graftExprWithM ss $ \case
    L span (HsCase ext scrut mg@_) -> do
      pure $ Just $ L span $ HsCase ext scrut $ mg { mg_alts = l }
    (_ :: LHsExpr GhcPs) -> pure Nothing


fromMaybeT :: Functor m => a -> MaybeT m a -> m a
fromMaybeT def = fmap (fromMaybe def) . runMaybeT


------------------------------------------------------------------------------
-- | Find the last typechecked module, and find the most specific span, as well
-- as the judgement at the given range.
emptyCaseScrutinees
    :: IdeState
    -> NormalizedFilePath
    -> MaybeT IO [(Tracked 'Current RealSrcSpan, Type)]
emptyCaseScrutinees state nfp = do
    let stale a = runStaleIde "emptyCaseScrutinees" state nfp a

    TrackedStale tcg tcg_map <- fmap (fmap tmrTypechecked) $ stale TypeCheck
    let tcg' = unTrack tcg
    hscenv <- stale GhcSessionDeps

    let scrutinees = traverse (emptyCaseQ . tcg_binds) tcg
    for scrutinees $ \aged@(unTrack -> (ss, scrutinee)) -> do
      ty <- MaybeT $ typeCheck (hscEnv $ untrackedStaleValue hscenv) tcg' scrutinee
      case ss of
        RealSrcSpan r   -> do
          rss' <- liftMaybe $ mapAgeTo tcg_map $ unsafeCopyAge aged r
          pure (rss', ty)
        UnhelpfulSpan _ -> empty


------------------------------------------------------------------------------
-- | Get the 'SrcSpan' and scrutinee of every empty case.
emptyCaseQ :: GenericQ [(SrcSpan, HsExpr GhcTc)]
emptyCaseQ = everything (<>) $ mkQ mempty $ \case
  L new_span (Case scrutinee []) -> pure (new_span, scrutinee)
  (_ :: LHsExpr GhcTc) -> mempty

