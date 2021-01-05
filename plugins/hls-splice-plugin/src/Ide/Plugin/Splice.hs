{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ViewPatterns #-}

module Ide.Plugin.Splice
    ( descriptor,
    )
where

import Control.Applicative (Alternative ((<|>)))
import Control.Arrow
import qualified Control.Foldl as L
import Control.Lens (ix, view, (%~), (<&>), (^.))
import Control.Monad
import qualified Control.Monad.Fail as Fail
import Control.Monad.Trans.Class
import Control.Monad.Trans.Except
import Control.Monad.Trans.Maybe
import Data.Aeson
import Data.Function
import Data.Generics
import qualified Data.Kind as Kinds
import Data.List (sortOn)
import Data.Maybe (fromMaybe, listToMaybe, mapMaybe)
import qualified Data.Text as T
import Development.IDE
import Development.IDE.GHC.Compat hiding (getLoc)
import Exception
import GHC.Exts
import GhcMonad
import GhcPlugins hiding (Var, getLoc, (<>))
import Ide.Plugin.Splice.Types
import Ide.PluginUtils (mkLspCommand, responseError)
import Ide.TreeTransform
import Ide.Types
import Language.Haskell.GHC.ExactPrint (TransformT, setPrecedingLines, uniqueSrcSpanT)
import Language.Haskell.LSP.Core
import Language.Haskell.LSP.Messages
import Language.Haskell.LSP.Types
import qualified Language.Haskell.LSP.Types.Lens as J
import Retrie.ExactPrint (Annotated)
import RnSplice
import TcRnMonad
import Data.Foldable (Foldable(foldl'))

descriptor :: PluginId -> PluginDescriptor IdeState
descriptor plId =
    (defaultPluginDescriptor plId)
        { pluginCommands = commands
        , pluginCodeActionProvider = Just codeAction
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
expandTHSplice _eStyle lsp ideState params@ExpandSpliceParams {..} =
    fmap (fromMaybe defaultResult) $
        runMaybeT $ do

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
                        lsp
                        MtError
                        ["Error during expanding splice: " <> T.pack err]
                    pure (Left $ responseError $ T.pack err, Nothing)
                Right edits ->
                    pure
                        ( Right Null
                        , Just (WorkspaceApplyEdit, ApplyWorkspaceEditParams edits)
                        )
    where
        range = realSrcSpanToRange spliceSpan
        srcSpan = RealSrcSpan spliceSpan
        defaultResult = (Right Null, Nothing)
        expandManually fp = do
            mresl <-
                liftIO $ runAction "expandTHSplice.fallback.TypeCheck (stale)" ideState $ useWithStale TypeCheck fp
            (TcModuleResult {..}, _) <-
                maybe
                (throwE "Splice expansion: Type-checking information not found in cache.\nYou can once delete or replace the macro with placeholder, convince the type checker and then revert to original (errornous) macro and expand splice again."
                )
                pure mresl
            reportEditor
                lsp
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
                lsp
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
                            (clientCapabilities lsp)
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
                                (clientCapabilities lsp)
                                uri
                                (graftDecls (RealSrcSpan spliceSpan) expanded)
                                ps
                                <&>
                                -- FIXME: Why ghc-exactprint sweeps preceeding comments?
                                adjustToRange uri range

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
adjustToRange uri ran (WorkspaceEdit mhult mlt) =
    WorkspaceEdit (adjustWS <$> mhult) (fmap adjustDoc <$> mlt)
    where
        adjustTextEdits :: Traversable f => f TextEdit -> f TextEdit
        adjustTextEdits eds =
            let Just minStart =
                    L.fold
                        (L.premap (view J.range) L.minimum)
                        eds
             in adjustLine minStart <$> eds
        adjustWS = ix uri %~ adjustTextEdits
        adjustDoc es
            | es ^. J.textDocument . J.uri == uri =
                es & J.edits %~ adjustTextEdits
            | otherwise = es

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
    matchSplice _ _ = Nothing
    expandSplice _ = fmap (first Right) . rnSpliceExpr

instance HasSplice Pat where
    matchSplice _ (SplicePat _ spl) = Just spl
    matchSplice _ _ = Nothing
    expandSplice _ = rnSplicePat


instance HasSplice HsType where
    matchSplice _ (HsSpliceTy _ spl) = Just spl
    matchSplice _ _ = Nothing
    expandSplice _ = fmap (first Right) . rnSpliceType

classifyAST :: SpliceContext -> SpliceClass
classifyAST = \case
    Expr -> OneToOneAST @HsExpr proxy#
    HsDecl -> IsHsDecl
    Pat -> OneToOneAST @Pat proxy#
    HsType -> OneToOneAST @HsType proxy#

reportEditor :: MonadIO m => LspFuncs a -> MessageType -> [T.Text] -> m ()
reportEditor lsp msgTy msgs =
    liftIO $
        sendFunc lsp $
            NotShowMessage $
                NotificationMessage "2.0" WindowShowMessage $
                    ShowMessageParams msgTy $
                        T.unlines msgs

manualCalcEdit ::
    LspFuncs a ->
    Range ->
    Annotated ParsedSource ->
    HscEnv ->
    TcGblEnv ->
    RealSrcSpan ->
    ExpandStyle ->
    ExpandSpliceParams ->
    ExceptT String IO WorkspaceEdit
manualCalcEdit lsp ran ps hscEnv typechkd srcSpan _eStyle ExpandSpliceParams {..} = do
    (warns, resl) <-
        ExceptT $ do
            ((warns, errs), eresl) <-
                initTcWithGbl hscEnv typechkd srcSpan $
                    case classifyAST spliceContext of
                        IsHsDecl -> fmap (fmap $ adjustToRange uri ran) $
                            flip (transformM dflags (clientCapabilities lsp) uri) ps $
                                graftDeclsWithM (RealSrcSpan srcSpan) $ \case
                                    (L _spn (SpliceD _ (SpliceDecl _ (L _ spl) _))) -> do
                                        eExpr <-
                                            either (fail . show) pure
                                                =<< lift
                                                    ( lift $
                                                        gtry @_ @SomeException $
                                                            (fst <$> rnTopSpliceDecls spl)
                                                    )
                                        pure $ Just eExpr
                                    _ -> pure Nothing
                        OneToOneAST astP ->
                            flip (transformM dflags (clientCapabilities lsp) uri) ps $
                                graftWithM (RealSrcSpan srcSpan) $ \case
                                    (L _spn (matchSplice astP -> Just spl)) -> do
                                        eExpr <-
                                            either (fail . show) pure
                                                =<< lift
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
            lsp
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
fromSearchResult _ = Nothing

-- TODO: workaround when HieAst unavailable (e.g. when the module itself errors)
-- TODO: Declaration Splices won't appear in HieAst; perhaps we must just use Parsed/Renamed ASTs?
codeAction :: CodeActionProvider IdeState
codeAction _ state plId docId ran _ =
    fmap (maybe (Right $ List []) Right) $
        runMaybeT $ do
            fp <- MaybeT $ pure $ uriToNormalizedFilePath $ toNormalizedUri theUri
            ParsedModule {..} <-
                MaybeT . runAction "splice.codeAction.GitHieAst" state $
                    use GetParsedModule fp
            let spn =
                    rangeToRealSrcSpan ran $
                        fromString $
                            fromNormalizedFilePath fp
                mouterSplice = something' (detectSplice spn) pm_parsed_source
            mcmds <- forM mouterSplice $
                \(spliceSpan, spliceContext) ->
                    forM expandStyles $ \(_, (title, cmdId)) -> do
                        let params = ExpandSpliceParams {uri = theUri, ..}
                        act <- liftIO $ mkLspCommand plId cmdId title (Just [toJSON params])
                        pure $
                            CACodeAction $
                                CodeAction title (Just CodeActionRefactorRewrite) Nothing Nothing (Just act)

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
                                _ -> Continue
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
                                _ -> Continue
                    _ -> Stop
                `extQ` \case
                    (L l@(RealSrcSpan spLoc) ty :: LHsType GhcPs)
                        | RealSrcSpan spn `isSubspanOf` l ->
                            case ty of
                                HsSpliceTy {} -> Here (spLoc, HsType)
                                _ -> Continue
                    _ -> Stop
                `extQ` \case
                    (L l@(RealSrcSpan spLoc) decl :: LHsDecl GhcPs)
                        | RealSrcSpan spn `isSubspanOf` l ->
                            case decl of
                                SpliceD {} -> Here (spLoc, HsDecl)
                                _ -> Continue
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

posToRealSrcLoc :: Position -> FastString -> RealSrcLoc
posToRealSrcLoc pos fs = mkRealSrcLoc fs (line + 1) (col + 1)
    where
        line = _line pos
        col = _character pos

rangeToRealSrcSpan :: Range -> FastString -> RealSrcSpan
rangeToRealSrcSpan ran fs =
    mkRealSrcSpan
        (posToRealSrcLoc (_start ran) fs)
        (posToRealSrcLoc (_end ran) fs)
