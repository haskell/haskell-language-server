{-# LANGUAGE CPP                   #-}
{-# LANGUAGE DerivingStrategies    #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE MagicHash             #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE PatternSynonyms       #-}
{-# LANGUAGE RecordWildCards       #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE ViewPatterns          #-}

module Ide.Plugin.Splice (descriptor) where

import           Control.Applicative                   (Alternative ((<|>)))
import           Control.Arrow                         (Arrow (first))
import           Control.Exception                     (SomeException)
import qualified Control.Foldl                         as L
import           Control.Lens                          (Identity (..), ix, view,
                                                        (%~), (<&>), (^.))
import           Control.Monad                         (forM, guard, unless)
import           Control.Monad.Error.Class             (MonadError (throwError))
import           Control.Monad.Extra                   (eitherM)
import qualified Control.Monad.Fail                    as Fail
import           Control.Monad.IO.Unlift               (MonadIO (..),
                                                        askRunInIO)
import           Control.Monad.Trans.Class             (MonadTrans (lift))
import           Control.Monad.Trans.Except            (ExceptT (..),
                                                        runExceptT)
import           Control.Monad.Trans.Maybe
import           Data.Aeson                            hiding (Null)
import qualified Data.Bifunctor                        as B (first)
import           Data.Function
import           Data.Generics
import qualified Data.Kind                             as Kinds
import           Data.List                             (sortOn)
import           Data.Maybe                            (fromMaybe, listToMaybe,
                                                        mapMaybe)
import qualified Data.Text                             as T
import           Development.IDE
import           Development.IDE.Core.FileStore            (getVersionedTextDoc)
import           Development.IDE.Core.PluginUtils
import           Development.IDE.GHC.Compat            as Compat
import           Development.IDE.GHC.Compat.ExactPrint
import qualified Development.IDE.GHC.Compat.Util       as Util
import           Development.IDE.GHC.ExactPrint
import           GHC.Exts
import qualified GHC.Runtime.Loader                as Loader
import qualified GHC.Types.Error                       as Error
import           Ide.Plugin.Error                      (PluginError (PluginInternalError))
import           Ide.Plugin.Splice.Types
import           Ide.Types
import qualified Language.LSP.Protocol.Lens            as J
import           Language.LSP.Protocol.Message
import           Language.LSP.Protocol.Types

#if !MIN_VERSION_base(4,20,0)
import           Data.Foldable                         (Foldable (foldl'))
#endif

#if MIN_VERSION_ghc(9,4,1)
import           GHC.Data.Bag                          (Bag)
#endif

#if MIN_VERSION_ghc(9,9,0)
import           GHC.Parser.Annotation                 (EpAnn (..))
#else
import           GHC.Parser.Annotation                 (SrcSpanAnn' (..))
#endif


descriptor :: PluginId -> PluginDescriptor IdeState
descriptor plId =
    (defaultPluginDescriptor plId "Provides a code action to evaluate a TemplateHaskell splice")
        { pluginCommands = commands
        , pluginHandlers = mkPluginHandler SMethod_TextDocumentCodeAction codeAction
        }

commands :: [PluginCommand IdeState]
commands =
    [ PluginCommand expandInplaceId inplaceCmdName $ expandTHSplice Inplace
    -- , PluginCommand expandCommentedId commentedCmdName $ expandTHSplice Commented
    ]

newtype SubSpan = SubSpan {runSubSpan :: SrcSpan}

instance Eq SubSpan where
    (==) = (==) `on` runSubSpan

instance Ord SubSpan where
    (<=) = coerce isSubspanOf

expandTHSplice ::
    -- | Inplace?
    ExpandStyle ->
    CommandFunction IdeState ExpandSpliceParams
expandTHSplice _eStyle ideState _ params@ExpandSpliceParams {..} = ExceptT $ do
    clientCapabilities <- pluginGetClientCapabilities
    rio <- askRunInIO
    let reportEditor :: ReportEditor
        reportEditor msgTy msgs = liftIO $ rio $ pluginSendNotification SMethod_WindowShowMessage (ShowMessageParams msgTy (T.unlines msgs))
        expandManually :: NormalizedFilePath -> ExceptT PluginError IO WorkspaceEdit
        expandManually fp = do
            mresl <-
                liftIO $ runAction "expandTHSplice.fallback.TypeCheck (stale)" ideState $ useWithStale TypeCheck fp
            (TcModuleResult {..}, _) <-
                maybe
                (throwError $ PluginInternalError "Splice expansion: Type-checking information not found in cache.\nYou can once delete or replace the macro with placeholder, convince the type checker and then revert to original (erroneous) macro and expand splice again."
                )
                pure mresl
            reportEditor
                MessageType_Warning
                [ "Expansion in type-checking phase failed;"
                , "trying to expand manually, but note that it is less rigorous."
                ]
            pm <- runActionE "expandTHSplice.fallback.GetParsedModule" ideState $
                        useE GetParsedModule fp
            (ps, hscEnv, _dflags) <- setupHscEnv ideState fp pm

            manualCalcEdit
                clientCapabilities
                reportEditor
                range
                ps
                hscEnv
                tmrTypechecked
                spliceSpan
                _eStyle
                params

        withTypeChecked fp TcModuleResult {..} = do
            (ps, _hscEnv, dflags) <- setupHscEnv ideState fp tmrParsed
            let Splices {..} = tmrTopLevelSplices
            let exprSuperSpans =
                    listToMaybe $ findSubSpansDesc srcSpan exprSplices
                _patSuperSpans =
                    listToMaybe $ findSubSpansDesc srcSpan patSplices
                typeSuperSpans =
                    listToMaybe $ findSubSpansDesc srcSpan typeSplices
                declSuperSpans =
                    listToMaybe $ findSubSpansDesc srcSpan declSplices

                graftSpliceWith ::
                    forall ast.
                    HasSplice AnnListItem ast =>
                    Maybe (SrcSpan, LocatedAn AnnListItem (ast GhcPs)) ->
                    Maybe (Either String WorkspaceEdit)
                graftSpliceWith expandeds =
                    expandeds <&> \(_, expanded) ->
                        transform
                            dflags
                            clientCapabilities
                            verTxtDocId
                            (graft (RealSrcSpan spliceSpan Nothing) expanded)
                            ps
            maybe (throwError $ PluginInternalError "No splice information found") (either (throwError . PluginInternalError . T.pack) pure) $
                case spliceContext of
                    Expr -> graftSpliceWith exprSuperSpans
                    Pat ->

                        graftSpliceWith _patSuperSpans

                    HsType -> graftSpliceWith typeSuperSpans
                    HsDecl ->
                        declSuperSpans <&> \(_, expanded) ->
                            transform
                                dflags
                                clientCapabilities
                                verTxtDocId
                                (graftDecls (RealSrcSpan spliceSpan Nothing) expanded)
                                ps
                                <&>
                                -- FIXME: Why ghc-exactprint sweeps preceding comments?
                                adjustToRange (verTxtDocId ^. J.uri) range

    res <- liftIO $ runMaybeT $ do

            fp <- MaybeT $ pure $ uriToNormalizedFilePath $ toNormalizedUri (verTxtDocId ^. J.uri)
            eedits <-
                ( lift . runExceptT . withTypeChecked fp
                        =<< MaybeT
                            (runAction "expandTHSplice.TypeCheck" ideState $ use TypeCheck fp)
                    )
                    <|> lift (runExceptT $ expandManually fp)

            case eedits of
                Left err -> do
                    reportEditor
                        MessageType_Error
                        [T.pack $ "Error during expanding splice: " <> show (pretty err)]
                    pure (Left err)
                Right edits ->
                    pure (Right edits)
    case res of
      Nothing -> pure $ Right $ InR Null
      Just (Left err) -> pure $ Left err
      Just (Right edit) -> do
        _ <- pluginSendRequest SMethod_WorkspaceApplyEdit (ApplyWorkspaceEditParams Nothing edit) (\_ -> pure ())
        pure $ Right $ InR Null

    where
        range = realSrcSpanToRange spliceSpan
        srcSpan = RealSrcSpan spliceSpan Nothing


setupHscEnv
    :: IdeState
    -> NormalizedFilePath
    -> ParsedModule
    -> ExceptT PluginError IO (ParsedSource, HscEnv, DynFlags)
setupHscEnv ideState fp pm = do
    hscEnvEq <- runActionE "expandTHSplice.fallback.ghcSessionDeps" ideState $
                    useE GhcSessionDeps fp
    let ps = annotateParsedSource pm
        hscEnv0 = hscEnv hscEnvEq
        modSum = pm_mod_summary pm
    hscEnv <- liftIO $ setupDynFlagsForGHCiLike hscEnv0 $ ms_hspp_opts modSum
    pure (ps, hscEnv, hsc_dflags hscEnv)

setupDynFlagsForGHCiLike :: HscEnv -> DynFlags -> IO HscEnv
setupDynFlagsForGHCiLike env dflags = do
    let dflags3 = setInterpreterLinkerOptions dflags
        platform = targetPlatform dflags3
        dflags3a = setWays hostFullWays dflags3
        dflags3b =
            foldl' gopt_set dflags3a $
                concatMap (wayGeneralFlags platform) hostFullWays
        dflags3c =
            foldl' gopt_unset dflags3b $
                concatMap (wayUnsetGeneralFlags platform) hostFullWays
        dflags4 =
            dflags3c
                `gopt_set` Opt_ImplicitImportQualified
                `gopt_set` Opt_IgnoreOptimChanges
                `gopt_set` Opt_IgnoreHpcChanges
                `gopt_unset` Opt_DiagnosticsShowCaret
    Loader.initializePlugins (hscSetFlags dflags4 env)

adjustToRange :: Uri -> Range -> WorkspaceEdit -> WorkspaceEdit
adjustToRange uri ran (WorkspaceEdit mhult mlt x) =
    WorkspaceEdit (adjustWS <$> mhult) (fmap adjustDoc <$> mlt) x
    where
        adjustTextEdits :: Traversable f => f TextEdit -> f TextEdit
        adjustTextEdits eds =
            let minStart =
                    case L.fold (L.premap (view J.range) L.minimum) eds of
                        Nothing -> error "impossible"
                        Just v  -> v
            in adjustLine minStart <$> eds

        adjustATextEdits :: Traversable f => f (TextEdit |? AnnotatedTextEdit) -> f (TextEdit |? AnnotatedTextEdit)
        adjustATextEdits = fmap $ \case
          InL t -> InL $ runIdentity $ adjustTextEdits (Identity t)
          InR AnnotatedTextEdit{_range, _newText, _annotationId} ->
            let oldTE = TextEdit{_range,_newText}
              in let TextEdit{_range,_newText} = runIdentity $ adjustTextEdits (Identity oldTE)
                in InR $ AnnotatedTextEdit{_range,_newText,_annotationId}

        adjustWS = ix uri %~ adjustTextEdits
        adjustDoc :: DocumentChange -> DocumentChange
        adjustDoc (InR es) = InR es
        adjustDoc (InL es)
            | es ^. J.textDocument . J.uri == uri =
                InL $ es & J.edits %~ adjustATextEdits
            | otherwise = InL es

        adjustLine :: Range -> TextEdit -> TextEdit
        adjustLine bad =
            J.range %~ \r ->
                if r == bad then ran else bad

-- Define a pattern to get hold of a `SrcSpan` from the location part of a
-- `GenLocated`. In GHC >= 9.2 this will be a SrcSpanAnn', with annotations;
-- earlier it will just be a plain `SrcSpan`.
{-# COMPLETE AsSrcSpan #-}
#if MIN_VERSION_ghc(9,9,0)
pattern AsSrcSpan :: SrcSpan -> EpAnn ann
pattern AsSrcSpan locA <- (getLoc -> locA)
#else
pattern AsSrcSpan :: SrcSpan -> SrcSpanAnn' a
pattern AsSrcSpan locA <- SrcSpanAnn {locA}
#endif

findSubSpansDesc :: SrcSpan -> [(LHsExpr GhcTc, a)] -> [(SrcSpan, a)]
findSubSpansDesc srcSpan =
    sortOn (Down . SubSpan . fst)
        . mapMaybe
            ( \(L (AsSrcSpan spn) _, e) -> do
                guard (spn `isSubspanOf` srcSpan)
                pure (spn, e)
            )

data SpliceClass where
    OneToOneAST :: HasSplice AnnListItem ast => Proxy# ast -> SpliceClass
    IsHsDecl :: SpliceClass

#if MIN_VERSION_ghc(9,5,0)
data HsSpliceCompat pass
  = UntypedSplice (HsUntypedSplice pass)
  | TypedSplice (LHsExpr pass)
#endif


class (Outputable (ast GhcRn), ASTElement l (ast GhcPs)) => HasSplice l ast where
    type SpliceOf ast :: Kinds.Type -> Kinds.Type
    matchSplice :: Proxy# ast -> ast GhcPs -> Maybe (SpliceOf ast GhcPs)
    expandSplice :: Proxy# ast -> SpliceOf ast GhcPs -> RnM (Either (ast GhcPs) (ast GhcRn), FreeVars)

instance HasSplice AnnListItem HsExpr where
#if MIN_VERSION_ghc(9,5,0)
    type SpliceOf HsExpr = HsSpliceCompat
    matchSplice _ (HsUntypedSplice _ spl) = Just (UntypedSplice spl)
    matchSplice _ (HsTypedSplice _ spl)   = Just (TypedSplice spl)
#else
    type SpliceOf HsExpr = HsSplice
    matchSplice _ (HsSpliceE _ spl) = Just spl
#endif
    matchSplice _ _                 = Nothing
#if MIN_VERSION_ghc(9,5,0)
    expandSplice _ (UntypedSplice e) = fmap (first Right) $ rnUntypedSpliceExpr e
    expandSplice _ (TypedSplice e) = fmap (first Right) $ rnTypedSplice e
#else
    expandSplice _ = fmap (first Right) . rnSpliceExpr
#endif

instance HasSplice AnnListItem Pat where
#if MIN_VERSION_ghc(9,5,0)
    type SpliceOf Pat = HsUntypedSplice
#else
    type SpliceOf Pat = HsSplice
#endif
    matchSplice _ (SplicePat _ spl) = Just spl
    matchSplice _ _                 = Nothing
    expandSplice _ =
#if MIN_VERSION_ghc(9,5,0)
      fmap (first (Left . unLoc . utsplice_result . snd )) .
#endif
      rnSplicePat


instance HasSplice AnnListItem HsType where
#if MIN_VERSION_ghc(9,5,0)
    type SpliceOf HsType = HsUntypedSplice
#else
    type SpliceOf HsType = HsSplice
#endif
    matchSplice _ (HsSpliceTy _ spl) = Just spl
    matchSplice _ _                  = Nothing
    expandSplice _ = fmap (first Right) . rnSpliceType

classifyAST :: SpliceContext -> SpliceClass
classifyAST = \case
    Expr   -> OneToOneAST @HsExpr proxy#
    HsDecl -> IsHsDecl
    Pat    -> OneToOneAST @Pat proxy#
    HsType -> OneToOneAST @HsType proxy#

type ReportEditor = forall m. MonadIO m => MessageType -> [T.Text] -> m ()

manualCalcEdit ::
    ClientCapabilities ->
    ReportEditor ->
    Range ->
    ParsedSource ->
    HscEnv ->
    TcGblEnv ->
    RealSrcSpan ->
    ExpandStyle ->
    ExpandSpliceParams ->
    ExceptT PluginError IO WorkspaceEdit
manualCalcEdit clientCapabilities reportEditor ran ps hscEnv typechkd srcSpan _eStyle ExpandSpliceParams {..} = do
    (warns, resl) <-
        ExceptT $ do
            (msgs, eresl) <-
                initTcWithGbl hscEnv typechkd srcSpan $
                    case classifyAST spliceContext of
                        IsHsDecl -> fmap (fmap $ adjustToRange (verTxtDocId ^. J.uri) ran) $
                            flip (transformM dflags clientCapabilities verTxtDocId) ps $
                                graftDeclsWithM (RealSrcSpan srcSpan Nothing) $ \case
                                    (L _spn (SpliceD _ (SpliceDecl _ (L _ spl) _))) -> do
                                        eExpr <-
                                            eitherM (fail . show) pure
                                                $ TransformT $ lift
                                                    ( lift $
                                                        Util.try @_ @SomeException $
                                                            (fst <$> rnTopSpliceDecls spl)
                                                    )
                                        pure $ Just eExpr
                                    _ -> pure Nothing
                        OneToOneAST astP ->
                            flip (transformM dflags clientCapabilities verTxtDocId) ps $
                                graftWithM (RealSrcSpan srcSpan Nothing) $ \case
                                    (L _spn (matchSplice astP -> Just spl)) -> do
                                        eExpr <-
                                            eitherM (fail . show) pure
                                                $ TransformT $ lift
                                                    ( lift $
                                                        Util.try @_ @SomeException $
                                                            (fst <$> expandSplice astP spl)
                                                    )
                                        Just <$> case eExpr of
                                            Left x  -> pure $ L _spn x
                                            Right y -> unRenamedE dflags y
                                    _ -> pure Nothing
            let (warns, errs) =
                                (Error.getWarningMessages msgs, Error.getErrorMessages msgs)
            pure $ (warns,) <$> maybe (throwError $ PluginInternalError $ T.pack $ showErrors errs)
                                    (B.first (PluginInternalError . T.pack)) eresl

    unless
        (null warns)
        $ reportEditor
            MessageType_Warning
            [ "Warning during expanding: "
            , ""
            , T.pack (showErrors warns)
            ]
    pure resl
    where
        dflags = hsc_dflags hscEnv

#if MIN_VERSION_ghc(9,4,1)
        showErrors = showBag
#else
        showErrors = show
#endif

#if MIN_VERSION_ghc(9,4,1)
showBag :: Error.Diagnostic a => Bag (Error.MsgEnvelope a) -> String
showBag = show . fmap (fmap toDiagnosticMessage)

toDiagnosticMessage :: forall a. Error.Diagnostic a => a -> Error.DiagnosticMessage
toDiagnosticMessage message =
    Error.DiagnosticMessage
        { diagMessage = Error.diagnosticMessage
#if MIN_VERSION_ghc(9,5,0)
                          (Error.defaultDiagnosticOpts @a)
#endif
                          message

        , diagReason  = Error.diagnosticReason  message
        , diagHints   = Error.diagnosticHints   message
        }
#endif

-- | FIXME:  Is thereAny "clever" way to do this exploiting TTG?
unRenamedE ::
    forall ast m l.
    (Fail.MonadFail m, HasSplice l ast) =>
    DynFlags ->
    ast GhcRn ->
    TransformT m (LocatedAn l (ast GhcPs))
unRenamedE dflags expr = do
    uniq <- show <$> uniqueSrcSpanT
    expr' <-
        either (fail . showErrors) pure $
        parseAST @_ @(ast GhcPs) dflags uniq $
            showSDoc dflags $ ppr expr
    pure expr'
  where
#if MIN_VERSION_ghc(9,4,1)
    showErrors = showBag . Error.getMessages
#else
    showErrors = show
#endif

data SearchResult r =
    Continue | Stop | Here r
    deriving (Read, Show, Eq, Ord, Data)

fromSearchResult :: SearchResult a -> Maybe a
fromSearchResult (Here r) = Just r
fromSearchResult _        = Nothing

-- TODO: workaround when HieAst unavailable (e.g. when the module itself errors)
-- TODO: Declaration Splices won't appear in HieAst; perhaps we must just use Parsed/Renamed ASTs?
codeAction :: PluginMethodHandler IdeState Method_TextDocumentCodeAction
codeAction state plId (CodeActionParams _ _ docId ran _) = do
    verTxtDocId <- liftIO $ runAction "splice.codeAction.getVersionedTextDoc" state $ getVersionedTextDoc docId
    liftIO $ fmap (fromMaybe ( InL [])) $
        runMaybeT $ do
            fp <- MaybeT $ pure $ uriToNormalizedFilePath $ toNormalizedUri theUri
            ParsedModule {..} <-
                MaybeT . runAction "splice.codeAction.GitHieAst" state $
                    use GetParsedModule fp
            let spn = rangeToRealSrcSpan fp ran
                mouterSplice = something' (detectSplice spn) pm_parsed_source
            mcmds <- forM mouterSplice $
                \(spliceSpan, spliceContext) ->
                    forM expandStyles $ \(_, (title, cmdId)) -> do
                        let params = ExpandSpliceParams {verTxtDocId, ..}
                            act = mkLspCommand plId cmdId title (Just [toJSON params])
                        pure $
                            InR $
                                CodeAction title (Just CodeActionKind_RefactorRewrite) Nothing Nothing Nothing Nothing (Just act) Nothing

            pure $ InL $ fromMaybe mempty mcmds
    where
        theUri = docId ^. J.uri
        detectSplice ::
            RealSrcSpan ->
            GenericQ (SearchResult (RealSrcSpan, SpliceContext))
        detectSplice spn =
          let
            spanIsRelevant x = RealSrcSpan spn Nothing `isSubspanOf` x
          in
            mkQ
                Continue
                ( \case
                    (L (AsSrcSpan l@(RealSrcSpan spLoc _)) expr :: LHsExpr GhcPs)
                        | spanIsRelevant l ->
                            case expr of
#if MIN_VERSION_ghc(9,5,0)
                                HsTypedSplice{}   -> Here (spLoc, Expr)
                                HsUntypedSplice{} -> Here (spLoc, Expr)
#else
                                HsSpliceE {}      -> Here (spLoc, Expr)
#endif
                                _                 -> Continue
                    _ -> Stop
                )
                `extQ` \case
                    (L (AsSrcSpan l@(RealSrcSpan spLoc _)) pat :: LPat GhcPs)
                        | spanIsRelevant l ->
                            case pat of
                                SplicePat{} -> Here (spLoc, Pat)
                                _           -> Continue
                    _ -> Stop
                `extQ` \case
                    (L (AsSrcSpan l@(RealSrcSpan spLoc _)) ty :: LHsType GhcPs)
                        | spanIsRelevant l ->
                            case ty of
                                HsSpliceTy {} -> Here (spLoc, HsType)
                                _             -> Continue
                    _ -> Stop
                `extQ` \case
                    (L (AsSrcSpan l@(RealSrcSpan spLoc _)) decl :: LHsDecl GhcPs)
                        | spanIsRelevant l ->
                            case decl of
                                SpliceD {} -> Here (spLoc, HsDecl)
                                _          -> Continue
                    _ -> Stop

-- | Like 'something', but performs top-down searching, cutoffs when 'Stop' received,
--   and picks innermost result.
something' :: forall a. GenericQ (SearchResult a) -> GenericQ (Maybe a)
something' f =  go
    where
        go :: GenericQ (Maybe a)
        go x =
            case f x of
              Stop -> Nothing
              resl -> foldl' (flip (<|>)) (fromSearchResult resl) (gmapQ go x)
