{-# LANGUAGE OverloadedStrings #-}

-- | A plugin that uses tactics to synthesize code
module Wingman.EmptyCase where

import           Control.Monad
import           Control.Monad.Trans
import           Control.Monad.Trans.Maybe
import           Data.Aeson
import           Data.Maybe
import qualified Data.Text as T
import           Data.Traversable (for)
import           Development.IDE (realSrcSpanToRange, GetBindings(..))
import           Development.IDE.Core.Shake (IdeState (..))
import           Development.IDE.Core.UseStale (TrackedStale(..), unTrack, mapAgeFrom)
import           Development.IDE.GHC.Compat
import           Development.IDE.GHC.ExactPrint
import           Development.IDE.Spans.LocalBindings (getLocalScope)
import           Ide.Types
import           Language.LSP.Server
import           Language.LSP.Types
import           OccName
import           Prelude hiding (span)
import           Wingman.CodeGen (destructionFor)
import           Wingman.GHC
import           Wingman.Judgements (hySingleton)
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
      cfg <- getTacticConfig plId
      ccs <- getClientCapabilities
      liftIO $ fromMaybeT (Right $ List []) $ do
        dflags <- getIdeDynflags state nfp

        TrackedStale pm _ <- runStaleIde state nfp GetAnnotatedParsedSource
        TrackedStale binds bind_map <- runStaleIde state nfp GetBindings
        holes <- completionForHole  state nfp $ cfg_feature_set cfg

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
  "Complete case constructors (" <> T.pack (unsafeRender ty) <> ")"


------------------------------------------------------------------------------
-- | Silence an error.
hush :: Either e a -> Maybe a
hush (Left _) = Nothing
hush (Right a) = Just a


instance MonadFail (Either String) where
  fail = Left

------------------------------------------------------------------------------
-- | Graft a 'RunTacticResults' into the correct place in an AST. Correctly
-- deals with top-level holes, in which we might need to fiddle with the
-- 'Match's that bind variables.
graftMatchGroup
    :: SrcSpan
    -> Located [LMatch GhcPs (LHsExpr GhcPs)]
    -> Graft (Either String) ParsedSource
graftMatchGroup ss l = graftExprWithM ss $ \case
  L span (HsCase ext scrut mg@_) -> do
    pure $ Just $ L span $ HsCase ext scrut $ mg { mg_alts = l }
  (_ :: LHsExpr GhcPs) -> pure Nothing


fromMaybeT :: Functor m => a -> MaybeT m a -> m a
fromMaybeT def = fmap (fromMaybe def) . runMaybeT

