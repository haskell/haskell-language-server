{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TypeFamilies      #-}

{-# LANGUAGE NoMonoLocalBinds  #-}

module Wingman.EmptyCase where

import           Control.Applicative (empty)
import           Control.Monad
import           Control.Monad.Except (runExcept)
import           Control.Monad.Trans
import           Control.Monad.Trans.Maybe
import           Data.Generics.Aliases (mkQ, GenericQ)
import           Data.Generics.Schemes (everything)
import           Data.Maybe
import           Data.Monoid
import qualified Data.Text as T
import           Data.Traversable
import           Development.IDE (hscEnv, realSrcSpanToRange)
import           Development.IDE.Core.RuleTypes
import           Development.IDE.Core.Shake (IdeState (..))
import           Development.IDE.Core.UseStale
import           Development.IDE.GHC.Compat hiding (empty, EmptyCase)
import           Development.IDE.GHC.ExactPrint
import           Development.IDE.Spans.LocalBindings (getLocalScope)
import           Ide.Types
import           Language.LSP.Server
import           Language.LSP.Types
import           Prelude hiding (span)
import           Wingman.AbstractLSP.Types
import           Wingman.CodeGen (destructionFor)
import           Wingman.GHC
import           Wingman.Judgements
import           Wingman.LanguageServer
import           Wingman.Types


data EmptyCaseT = EmptyCaseT

instance IsContinuationSort EmptyCaseT where
  toCommandId _ = CommandId "wingman.emptyCase"

instance IsTarget EmptyCaseT where
  type TargetArgs EmptyCaseT = ()
  fetchTargetArgs _ = pure ()

emptyCaseInteraction :: Interaction
emptyCaseInteraction = Interaction $
  Continuation @EmptyCaseT @EmptyCaseT @WorkspaceEdit EmptyCaseT
    (SynthesizeCodeLens $ \LspEnv{..} _ -> do
      let FileContext{..} = le_fileContext
      nfp <- getNfp fc_uri

      let stale a = runStaleIde "codeLensProvider" le_ideState nfp a

      ccs <- lift getClientCapabilities
      TrackedStale pm _ <- mapMaybeT liftIO $ stale GetAnnotatedParsedSource
      TrackedStale binds bind_map <- mapMaybeT liftIO $ stale GetBindings
      holes <- mapMaybeT liftIO $ emptyCaseScrutinees le_ideState nfp

      for holes $ \(ss, ty) -> do
        binds_ss <- liftMaybe $ mapAgeFrom bind_map ss
        let bindings = getLocalScope (unTrack binds) $ unTrack binds_ss
            range = realSrcSpanToRange $ unTrack ss
        matches <-
          liftMaybe $
            destructionFor
              (foldMap (hySingleton . occName . fst) bindings)
              ty
        edits <- liftMaybe $ hush $
              mkWorkspaceEdits le_dflags ccs fc_uri (unTrack pm) $
                graftMatchGroup (RealSrcSpan (unTrack ss) Nothing) $
                  noLoc matches
        pure
          ( range
          , Metadata
              (mkEmptyCaseLensDesc ty)
              (CodeActionUnknown "refactor.wingman.completeEmptyCase")
              False
          , edits
          )
    )
    (\ _ _ _ we -> pure $ pure $ RawEdit we)


scrutinzedType :: EmptyCaseSort Type -> Maybe Type
scrutinzedType (EmptyCase ty) = pure  ty
scrutinzedType (EmptyLamCase ty) =
  case tacticsSplitFunTy ty of
    (_, _, tys, _) -> listToMaybe $ fmap scaledThing tys


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
    L span (HsCase ext scrut mg) -> do
      pure $ Just $ L span $ HsCase ext scrut $ mg { mg_alts = l }
    L span (HsLamCase ext mg) -> do
      pure $ Just $ L span $ HsLamCase ext $ mg { mg_alts = l }
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
    fmap catMaybes $ for scrutinees $ \aged@(unTrack -> (ss, scrutinee)) -> do
      ty <- MaybeT
          . fmap (scrutinzedType <=< sequence)
          . traverse (typeCheck (hscEnv $ untrackedStaleValue hscenv) tcg')
          $ scrutinee
      case null $ tacticsGetDataCons ty of
        True -> pure empty
        False ->
          case ss of
            RealSrcSpan r _ -> do
              rss' <- liftMaybe $ mapAgeTo tcg_map $ unsafeCopyAge aged r
              pure $ Just (rss', ty)
            UnhelpfulSpan _ -> empty

data EmptyCaseSort a
  = EmptyCase a
  | EmptyLamCase a
  deriving (Eq, Ord, Show, Functor, Foldable, Traversable)

------------------------------------------------------------------------------
-- | Get the 'SrcSpan' and scrutinee of every empty case.
emptyCaseQ :: GenericQ [(SrcSpan, EmptyCaseSort (HsExpr GhcTc))]
emptyCaseQ = everything (<>) $ mkQ mempty $ \case
  L new_span (Case scrutinee []) -> pure (new_span, EmptyCase scrutinee)
  L new_span expr@(LamCase []) -> pure (new_span, EmptyLamCase expr)
  (_ :: LHsExpr GhcTc) -> mempty

