{-# LANGUAGE CPP                   #-}
{-# LANGUAGE DeriveDataTypeable    #-}
{-# LANGUAGE DerivingStrategies    #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE MagicHash             #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE RecordWildCards       #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TupleSections         #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE ViewPatterns          #-}
{-# LANGUAGE NamedFieldPuns        #-}

module Ide.Plugin.Splice
    ( descriptor,
    )
where

import           Control.Applicative             (Alternative ((<|>)))
import           Control.Arrow
import qualified Control.Foldl                   as L
import           Control.Lens                    (ix, view, (%~), (<&>), (^.), Identity(..))
import           Control.Monad
import           Control.Monad.Extra             (eitherM)
import qualified Control.Monad.Fail              as Fail
import           Control.Monad.IO.Unlift
import           Control.Monad.Trans.Class
import           Control.Monad.Trans.Except
import           Control.Monad.Trans.Maybe
import           Data.Aeson
import           Data.Foldable                   (Foldable (foldl'))
import           Data.Function
import           Data.Generics
import qualified Data.Kind                       as Kinds
import           Data.List                       (sortOn)
import           Data.Maybe                      (fromMaybe, listToMaybe,
                                                  mapMaybe)
import qualified Data.Text                       as T
import           Development.IDE
import           Development.IDE.GHC.Compat      hiding (getLoc)
import           Development.IDE.GHC.ExactPrint
import           Exception
import           GHC.Exts
import           GhcMonad
import           GhcPlugins                      hiding (Var, getLoc, (<>))
import           Ide.Plugin.Splice.Types
import           Ide.Types
import           Language.Haskell.GHC.ExactPrint (setPrecedingLines,
                                                  uniqueSrcSpanT)
import           Language.LSP.Server
import           Language.LSP.Types
import           Language.LSP.Types.Capabilities
import qualified Language.LSP.Types.Lens         as J
import           RnSplice
import           TcRnMonad

descriptor :: PluginId -> PluginDescriptor IdeState
descriptor plId =
    (defaultPluginDescriptor plId)
        { pluginCommands = commands
        , pluginHandlers = mkPluginHandler STextDocumentCodeAction codeAction
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
expandTHSplice _eStyle ideState params@ExpandSpliceParams {..} = do
    clientCapabilities <- getClientCapabilities
    rio <- askRunInIO
    let reportEditor :: ReportEditor
        reportEditor msgTy msgs = liftIO $ rio $ sendNotification SWindowShowMessage (ShowMessageParams msgTy (T.unlines msgs))
        expandManually fp = do
            mresl <-
                liftIO $ runAction "expandTHSplice.fallback.TypeCheck (stale)" ideState $ useWithStale TypeCheck fp
            (TcModuleResult {..}, _) <-
                maybe
                (throwE "Splice expansion: Type-checking information not found in cache.\nYou can once delete or replace the macro with placeholder, convince the type checker and then revert to original (errornous) macro and expand splice again."
                )
                pure mresl
            reportEditor
                MtWarning
                [ "Expansion in type-chcking phase failed;"
                , "trying to expand manually, but note taht it is less rigorous."
                ]
            pm <-
                liftIO $
                    runAction "expandTHSplice.fallback.GetParsedModule" ideState $
                        use_ GetParsedModule fp
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
#if __GLASGOW_HASKELL__ == 808
                    fmap (second dL) $
#endif
                    listToMaybe $ findSubSpansDesc srcSpan patSplices
                typeSuperSpans =
                    listToMaybe $ findSubSpansDesc srcSpan typeSplices
                declSuperSpans =
                    listToMaybe $ findSubSpansDesc srcSpan declSplices

                graftSpliceWith ::
                    forall ast.
                    HasSplice ast =>
                    Maybe (SrcSpan, Located (ast GhcPs)) ->
                    Maybe (Either String WorkspaceEdit)
                graftSpliceWith expandeds =
                    expandeds <&> \(_, expanded) ->
                        transform
                            dflags
                            clientCapabilities
                            uri
                            (graft (RealSrcSpan spliceSpan) expanded)
                            ps
            maybe (throwE "No splice information found") (either throwE pure) $
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
                                uri
                                (graftDecls (RealSrcSpan spliceSpan) expanded)
                                ps
                                <&>
                                -- FIXME: Why ghc-exactprint sweeps preceeding comments?
                                adjustToRange uri range

    res <- liftIO $ runMaybeT $ do

            fp <- MaybeT $ pure $ uriToNormalizedFilePath $ toNormalizedUri uri
            eedits <-
                ( lift . runExceptT . withTypeChecked fp
                        =<< MaybeT
                            (runAction "expandTHSplice.TypeCheck" ideState $ use TypeCheck fp)
                    )
                    <|> lift (runExceptT $ expandManually fp)

            case eedits of
                Left err -> do
                    reportEditor
                        MtError
                        ["Error during expanding splice: " <> T.pack err]
                    pure (Left $ responseError $ T.pack err)
                Right edits ->
                    pure (Right edits)
    case res of
      Nothing -> pure $ Right Null
      Just (Left err) -> pure $ Left err
      Just (Right edit) -> do
        _ <- sendRequest SWorkspaceApplyEdit (ApplyWorkspaceEditParams Nothing edit) (\_ -> pure ())
        pure $ Right Null

    where
        range = realSrcSpanToRange spliceSpan
        srcSpan = RealSrcSpan spliceSpan


setupHscEnv
    :: IdeState
    -> NormalizedFilePath
    -> ParsedModule
    -> ExceptT String IO (Annotated ParsedSource, HscEnv, DynFlags)
setupHscEnv ideState fp pm = do
    hscEnvEq <-
        liftIO $
            runAction "expandTHSplice.fallback.ghcSessionDeps" ideState $
                use_ GhcSessionDeps fp
    let ps = annotateParsedSource pm
        hscEnv0 = hscEnvWithImportPaths hscEnvEq
        modSum = pm_mod_summary pm
    df' <- liftIO $ setupDynFlagsForGHCiLike hscEnv0 $ ms_hspp_opts modSum
    let hscEnv = hscEnv0 { hsc_dflags = df' }
    pure (ps, hscEnv, df')

setupDynFlagsForGHCiLike :: HscEnv -> DynFlags -> IO DynFlags
setupDynFlagsForGHCiLike env dflags = do
    let dflags3 =
            dflags
                { hscTarget = HscInterpreted
                , ghcMode = CompManager
                , ghcLink = LinkInMemory
                }
        platform = targetPlatform dflags3
        dflags3a = updateWays $ dflags3 {ways = interpWays}
        dflags3b =
            foldl gopt_set dflags3a $
                concatMap (wayGeneralFlags platform) interpWays
        dflags3c =
            foldl gopt_unset dflags3b $
                concatMap (wayUnsetGeneralFlags platform) interpWays
        dflags4 =
            dflags3c
                `gopt_set` Opt_ImplicitImportQualified
                `gopt_set` Opt_IgnoreOptimChanges
                `gopt_set` Opt_IgnoreHpcChanges
                `gopt_unset` Opt_DiagnosticsShowCaret
    initializePlugins env dflags4

adjustToRange :: Uri -> Range -> WorkspaceEdit -> WorkspaceEdit
adjustToRange uri ran (WorkspaceEdit mhult mlt x) =
    WorkspaceEdit (adjustWS <$> mhult) (fmap adjustDoc <$> mlt) x
    where
        adjustTextEdits :: Traversable f => f TextEdit -> f TextEdit
        adjustTextEdits eds =
            let Just minStart =
                    L.fold
                        (L.premap (view J.range) L.minimum)
                        eds
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

findSubSpansDesc :: SrcSpan -> [(LHsExpr GhcTc, a)] -> [(SrcSpan, a)]
findSubSpansDesc srcSpan =
    sortOn (Down . SubSpan . fst)
        . mapMaybe
            ( \(L spn _, e) -> do
                guard (spn `isSubspanOf` srcSpan)
                pure (spn, e)
            )

data SpliceClass where
    OneToOneAST :: HasSplice ast => Proxy# ast -> SpliceClass
    IsHsDecl :: SpliceClass

class (Outputable (ast GhcRn), ASTElement (ast GhcPs)) => HasSplice ast where
    type SpliceOf ast :: Kinds.Type -> Kinds.Type
    type SpliceOf ast = HsSplice
    matchSplice :: Proxy# ast -> ast GhcPs -> Maybe (SpliceOf ast GhcPs)
    expandSplice :: Proxy# ast -> SpliceOf ast GhcPs -> RnM (Either (ast GhcPs) (ast GhcRn), FreeVars)

instance HasSplice HsExpr where
    matchSplice _ (HsSpliceE _ spl) = Just spl
    matchSplice _ _                 = Nothing
    expandSplice _ = fmap (first Right) . rnSpliceExpr

instance HasSplice Pat where
    matchSplice _ (SplicePat _ spl) = Just spl
    matchSplice _ _                 = Nothing
    expandSplice _ = rnSplicePat


instance HasSplice HsType where
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
    Annotated ParsedSource ->
    HscEnv ->
    TcGblEnv ->
    RealSrcSpan ->
    ExpandStyle ->
    ExpandSpliceParams ->
    ExceptT String IO WorkspaceEdit
manualCalcEdit clientCapabilities reportEditor ran ps hscEnv typechkd srcSpan _eStyle ExpandSpliceParams {..} = do
    (warns, resl) <-
        ExceptT $ do
            ((warns, errs), eresl) <-
                initTcWithGbl hscEnv typechkd srcSpan $
                    case classifyAST spliceContext of
                        IsHsDecl -> fmap (fmap $ adjustToRange uri ran) $
                            flip (transformM dflags clientCapabilities uri) ps $
                                graftDeclsWithM (RealSrcSpan srcSpan) $ \case
                                    (L _spn (SpliceD _ (SpliceDecl _ (L _ spl) _))) -> do
                                        eExpr <-
                                            eitherM (fail . show) pure
                                                $ lift
                                                    ( lift $
                                                        gtry @_ @SomeException $
                                                            (fst <$> rnTopSpliceDecls spl)
                                                    )
                                        pure $ Just eExpr
                                    _ -> pure Nothing
                        OneToOneAST astP ->
                            flip (transformM dflags clientCapabilities uri) ps $
                                graftWithM (RealSrcSpan srcSpan) $ \case
                                    (L _spn (matchSplice astP -> Just spl)) -> do
                                        eExpr <-
                                            eitherM (fail . show) pure
                                                $ lift
                                                    ( lift $
                                                        gtry @_ @SomeException $
                                                            (fst <$> expandSplice astP spl)
                                                    )
                                        Just <$> either (pure . L _spn) (unRenamedE dflags) eExpr
                                    _ -> pure Nothing
            pure $ (warns,) <$> fromMaybe (Left $ show errs) eresl

    unless
        (null warns)
        $ reportEditor
            MtWarning
            [ "Warning during expanding: "
            , ""
            , T.pack (show warns)
            ]
    pure resl
    where
        dflags = hsc_dflags hscEnv

-- | FIXME:  Is thereAny "clever" way to do this exploiting TTG?
unRenamedE ::
    forall ast m.
    (Fail.MonadFail m, HasSplice ast) =>
    DynFlags ->
    ast GhcRn ->
    TransformT m (Located (ast GhcPs))
unRenamedE dflags expr = do
    uniq <- show <$> uniqueSrcSpanT
    (anns, expr') <-
        either (fail . show) pure $
            parseAST @(ast GhcPs) dflags uniq $
                showSDoc dflags $ ppr expr
    let _anns' = setPrecedingLines expr' 0 1 anns
    pure expr'

data SearchResult r =
    Continue | Stop | Here r
    deriving (Read, Show, Eq, Ord, Data, Typeable)

fromSearchResult :: SearchResult a -> Maybe a
fromSearchResult (Here r) = Just r
fromSearchResult _        = Nothing

-- TODO: workaround when HieAst unavailable (e.g. when the module itself errors)
-- TODO: Declaration Splices won't appear in HieAst; perhaps we must just use Parsed/Renamed ASTs?
codeAction :: PluginMethodHandler IdeState TextDocumentCodeAction
codeAction state plId (CodeActionParams _ _ docId ran _) = liftIO $
    fmap (maybe (Right $ List []) Right) $
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
                        let params = ExpandSpliceParams {uri = theUri, ..}
                            act = mkLspCommand plId cmdId title (Just [toJSON params])
                        pure $
                            InR $
                                CodeAction title (Just CodeActionRefactorRewrite) Nothing Nothing Nothing Nothing (Just act) Nothing

            pure $ maybe mempty List mcmds
    where
        theUri = docId ^. J.uri
        detectSplice ::
            RealSrcSpan ->
            GenericQ (SearchResult (RealSrcSpan, SpliceContext))
        detectSplice spn =
            mkQ
                Continue
                ( \case
                    (L l@(RealSrcSpan spLoc) expr :: LHsExpr GhcPs)
                        | RealSrcSpan spn `isSubspanOf` l ->
                            case expr of
                                HsSpliceE {} -> Here (spLoc, Expr)
                                _            -> Continue
                    _ -> Stop
                )
                `extQ` \case
#if __GLASGOW_HASKELL__ == 808
                    (dL @(Pat GhcPs) -> L l@(RealSrcSpan spLoc) pat :: Located (Pat GhcPs))
#else
                    (L l@(RealSrcSpan spLoc) pat :: LPat GhcPs)
#endif
                        | RealSrcSpan spn `isSubspanOf` l ->
                            case pat of
                                SplicePat{} -> Here (spLoc, Pat)
                                _           -> Continue
                    _ -> Stop
                `extQ` \case
                    (L l@(RealSrcSpan spLoc) ty :: LHsType GhcPs)
                        | RealSrcSpan spn `isSubspanOf` l ->
                            case ty of
                                HsSpliceTy {} -> Here (spLoc, HsType)
                                _             -> Continue
                    _ -> Stop
                `extQ` \case
                    (L l@(RealSrcSpan spLoc) decl :: LHsDecl GhcPs)
                        | RealSrcSpan spn `isSubspanOf` l ->
                            case decl of
                                SpliceD {} -> Here (spLoc, HsDecl)
                                _          -> Continue
                    _ -> Stop

-- | Like 'something', but performs top-down searching, cutoffs when 'Stop' received,
--   and picks inenrmost result.
something' :: forall a. GenericQ (SearchResult a) -> GenericQ (Maybe a)
something' f =  go
    where
        go :: GenericQ (Maybe a)
        go x =
            case f x of
              Stop -> Nothing
              resl -> foldl' (flip (<|>)) (fromSearchResult resl) (gmapQ go x)
