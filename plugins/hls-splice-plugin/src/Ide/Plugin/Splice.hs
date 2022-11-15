{-# LANGUAGE CPP                   #-}
{-# LANGUAGE DeriveDataTypeable    #-}
{-# LANGUAGE DerivingStrategies    #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE MagicHash             #-}
{-# LANGUAGE NamedFieldPuns        #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE RecordWildCards       #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TupleSections         #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE ViewPatterns          #-}
{-# LANGUAGE PatternSynonyms       #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}

module Ide.Plugin.Splice
    ( descriptor,
    )
where

import           Control.Applicative             (Alternative ((<|>)))
import           Control.Arrow
import           Control.Exception
import qualified Control.Foldl                   as L
import           Control.Lens                    (Identity (..), ix, view, (%~),
                                                  (<&>), (^.))
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
import           Development.IDE.GHC.Compat      as Compat hiding (getLoc)
import           Development.IDE.GHC.Compat.ExactPrint
import qualified Development.IDE.GHC.Compat.Util as Util
import           Development.IDE.GHC.ExactPrint
import           GHC.Exts
#if __GLASGOW_HASKELL__ >= 902
import           GHC.Parser.Annotation (SrcSpanAnn'(..))
import qualified GHC.Types.Error as Error
#endif
import           Ide.Plugin.Splice.Types
import           Ide.Types
import           Language.Haskell.GHC.ExactPrint (uniqueSrcSpanT)
import           Language.LSP.Server
import           Language.LSP.Types
import           Language.LSP.Types.Capabilities
import qualified Language.LSP.Types.Lens         as J

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
                (throwE "Splice expansion: Type-checking information not found in cache.\nYou can once delete or replace the macro with placeholder, convince the type checker and then revert to original (erroneous) macro and expand splice again."
                )
                pure mresl
            reportEditor
                MtWarning
                [ "Expansion in type-checking phase failed;"
                , "trying to expand manually, but note that it is less rigorous."
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
                            uri
                            (graft (RealSrcSpan spliceSpan Nothing) expanded)
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
                                (graftDecls (RealSrcSpan spliceSpan Nothing) expanded)
                                ps
                                <&>
                                -- FIXME: Why ghc-exactprint sweeps preceding comments?
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
        srcSpan = RealSrcSpan spliceSpan Nothing


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
    hscEnv <- liftIO $ setupDynFlagsForGHCiLike hscEnv0 $ ms_hspp_opts modSum
    pure (ps, hscEnv, hsc_dflags hscEnv)

setupDynFlagsForGHCiLike :: HscEnv -> DynFlags -> IO HscEnv
setupDynFlagsForGHCiLike env dflags = do
    let dflags3 = setInterpreterLinkerOptions dflags
        platform = targetPlatform dflags3
        dflags3a = setWays hostFullWays dflags3
        dflags3b =
            foldl gopt_set dflags3a $
                concatMap (wayGeneralFlags platform) hostFullWays
        dflags3c =
            foldl gopt_unset dflags3b $
                concatMap (wayUnsetGeneralFlags platform) hostFullWays
        dflags4 =
            dflags3c
                `gopt_set` Opt_ImplicitImportQualified
                `gopt_set` Opt_IgnoreOptimChanges
                `gopt_set` Opt_IgnoreHpcChanges
                `gopt_unset` Opt_DiagnosticsShowCaret
    initializePlugins (hscSetFlags dflags4 env)

adjustToRange :: Uri -> Range -> WorkspaceEdit -> WorkspaceEdit
adjustToRange uri ran (WorkspaceEdit mhult mlt x) =
    WorkspaceEdit (adjustWS <$> mhult) (fmap adjustDoc <$> mlt) x
    where
        adjustTextEdits :: Traversable f => f TextEdit -> f TextEdit
        adjustTextEdits eds =
            let minStart =
                    case L.fold (L.premap (view J.range) L.minimum) eds of
                        Nothing -> error "impossible"
                        Just v -> v
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
#if __GLASGOW_HASKELL__ >= 902
pattern AsSrcSpan :: SrcSpan -> SrcSpanAnn' a
pattern AsSrcSpan locA <- SrcSpanAnn {locA}
#else
pattern AsSrcSpan :: SrcSpan -> SrcSpan
pattern AsSrcSpan loc <- loc
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

class (Outputable (ast GhcRn), ASTElement l (ast GhcPs)) => HasSplice l ast where
    type SpliceOf ast :: Kinds.Type -> Kinds.Type
    type SpliceOf ast = HsSplice
    matchSplice :: Proxy# ast -> ast GhcPs -> Maybe (SpliceOf ast GhcPs)
    expandSplice :: Proxy# ast -> SpliceOf ast GhcPs -> RnM (Either (ast GhcPs) (ast GhcRn), FreeVars)

instance HasSplice AnnListItem HsExpr where
    matchSplice _ (HsSpliceE _ spl) = Just spl
    matchSplice _ _                 = Nothing
    expandSplice _ = fmap (first Right) . rnSpliceExpr

instance HasSplice AnnListItem Pat where
    matchSplice _ (SplicePat _ spl) = Just spl
    matchSplice _ _                 = Nothing
    expandSplice _ = rnSplicePat


instance HasSplice AnnListItem HsType where
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
            (msgs, eresl) <-
                initTcWithGbl hscEnv typechkd srcSpan $
                    case classifyAST spliceContext of
                        IsHsDecl -> fmap (fmap $ adjustToRange uri ran) $
                            flip (transformM dflags clientCapabilities uri) ps $
                                graftDeclsWithM (RealSrcSpan srcSpan Nothing) $ \case
                                    (L _spn (SpliceD _ (SpliceDecl _ (L _ spl) _))) -> do
                                        eExpr <-
                                            eitherM (fail . show) pure
                                                $ lift
                                                    ( lift $
                                                        Util.try @_ @SomeException $
                                                            (fst <$> rnTopSpliceDecls spl)
                                                    )
                                        pure $ Just eExpr
                                    _ -> pure Nothing
                        OneToOneAST astP ->
                            flip (transformM dflags clientCapabilities uri) ps $
                                graftWithM (RealSrcSpan srcSpan Nothing) $ \case
                                    (L _spn (matchSplice astP -> Just spl)) -> do
                                        eExpr <-
                                            eitherM (fail . show) pure
                                                $ lift
                                                    ( lift $
                                                        Util.try @_ @SomeException $
                                                            (fst <$> expandSplice astP spl)
                                                    )
                                        Just <$> case eExpr of
                                            Left x -> pure $ L _spn x
                                            Right y -> unRenamedE dflags y
                                    _ -> pure Nothing
            let (warns, errs) =
#if __GLASGOW_HASKELL__ >= 902
                                (Error.getWarningMessages msgs, Error.getErrorMessages msgs)
#else
                                msgs
#endif
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
    forall ast m l.
    (Fail.MonadFail m, HasSplice l ast) =>
    DynFlags ->
    ast GhcRn ->
    TransformT m (LocatedAn l (ast GhcPs))
unRenamedE dflags expr = do
    uniq <- show <$> uniqueSrcSpanT
#if __GLASGOW_HASKELL__ >= 902
    expr' <-
#else
    (_anns, expr') <-
#endif
        either (fail . show) pure $
        parseAST @_ @(ast GhcPs) dflags uniq $
            showSDoc dflags $ ppr expr
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
          let
            spanIsRelevant x = RealSrcSpan spn Nothing `isSubspanOf` x
          in
            mkQ
                Continue
                ( \case
                    (L (AsSrcSpan l@(RealSrcSpan spLoc _)) expr :: LHsExpr GhcPs)
                        | spanIsRelevant l ->
                            case expr of
                                HsSpliceE {} -> Here (spLoc, Expr)
                                _            -> Continue
                    _ -> Stop
                )
                `extQ` \case
#if __GLASGOW_HASKELL__ == 808
                    (dL @(Pat GhcPs) -> L l@(RealSrcSpan spLoc _) pat :: Located (Pat GhcPs))
#else
                    (L (AsSrcSpan l@(RealSrcSpan spLoc _)) pat :: LPat GhcPs)
#endif
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
