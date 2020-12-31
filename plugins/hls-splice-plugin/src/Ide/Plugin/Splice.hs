{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ViewPatterns #-}

module Ide.Plugin.Splice
    ( descriptor,
    )
where

import Control.Applicative (Alternative ((<|>)))
import Control.Lens ((<&>), (^.))
import Control.Monad
import qualified Control.Monad.Fail as Fail
import Control.Monad.Trans.Class
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
import TcRnMonad

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
expandTHSplice _eStyle lsp ideState ExpandSpliceParams {..} =
    fmap (fromMaybe defaultResult) $
        runMaybeT $ do
            let srcSpan = RealSrcSpan spliceSpan
            fp <- MaybeT $ pure $ uriToNormalizedFilePath $ toNormalizedUri uri
            TcModuleResult {..} <-
                MaybeT $
                    runAction "expandTHSplice.TypeCheck" ideState $
                        use TypeCheck fp
            let ps = annotateParsedSource tmrParsed
                Splices {..} = tmrTopLevelSplices
            hscEnvEq <-
                lift $
                    runAction "expandTHSplice.ghcSessionDeps" ideState $
                        use_ GhcSessionDeps fp
            let dflags = hsc_dflags $ hscEnv hscEnvEq
                exprSuperSpans =
                    listToMaybe $ findSubSpansDesc srcSpan exprSplices
                patSuperSpans =
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
            let eedits = join . maybe (Left "No splcie information found") Right $
                    case spliceContext of
                        Expr -> graftSpliceWith exprSuperSpans
                        Pat -> graftSpliceWith patSuperSpans
                        HsType -> graftSpliceWith typeSuperSpans
                        HsDecl ->
                            -- FIXME: It seems multiline edit results in wrong Edit
                            -- emited by @transform@.
                            -- It will eat preceding comments and spaces!
                            declSuperSpans <&> \(_, expanded) ->
                                transform
                                    dflags
                                    (clientCapabilities lsp)
                                    uri
                                    (graftMany (RealSrcSpan spliceSpan) expanded)
                                    ps
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
        defaultResult = (Right Null, Nothing)

findSubSpansDesc :: SrcSpan -> [(LHsExpr GhcTc, a)] -> [(SrcSpan, a)]
findSubSpansDesc srcSpan =
    sortOn (Down . SubSpan . fst)
        . mapMaybe
            ( \(L spn _, e) -> do
                guard (spn `isSubspanOf` srcSpan)
                pure (spn, e)
            )

class (Outputable (ast GhcRn), ASTElement (ast GhcPs)) => HasSplice ast where
    type SpliceOf ast :: Kinds.Type -> Kinds.Type
    type SpliceOf ast = HsSplice
    matchSplice :: Proxy# ast -> ast GhcPs -> Maybe (SpliceOf ast GhcPs)

instance HasSplice HsExpr where
    matchSplice _ (HsSpliceE _ spl) = Just spl
    matchSplice _ _ = Nothing

instance HasSplice Pat where
    matchSplice _ (SplicePat _ spl) = Just spl
    matchSplice _ _ = Nothing

instance HasSplice HsType where
    matchSplice _ (HsSpliceTy _ spl) = Just spl
    matchSplice _ _ = Nothing

reportEditor :: MonadIO m => LspFuncs a -> MessageType -> [T.Text] -> m ()
reportEditor lsp msgTy msgs =
    liftIO $
        sendFunc lsp $
            NotShowMessage $
                NotificationMessage "2.0" WindowShowMessage $
                    ShowMessageParams msgTy $
                        T.unlines msgs

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
            GenericQ (Maybe (RealSrcSpan, SpliceContext))
        detectSplice spn =
            mkQ
                Nothing
                ( \case
                    (L l@(RealSrcSpan spLoc) HsSpliceE {} :: LHsExpr GhcPs)
                        | RealSrcSpan spn `isSubspanOf` l -> Just (spLoc, Expr)
                    _ -> Nothing
                )
                `extQ` \case
                    (L l@(RealSrcSpan spLoc) SplicePat {} :: LPat GhcPs)
                        | RealSrcSpan spn `isSubspanOf` l -> Just (spLoc, Pat)
                    _ -> Nothing
                `extQ` \case
                    (L l@(RealSrcSpan spLoc) HsSpliceTy {} :: LHsType GhcPs)
                        | RealSrcSpan spn `isSubspanOf` l -> Just (spLoc, HsType)
                    _ -> Nothing
                `extQ` \case
                    (L l@(RealSrcSpan spLoc) SpliceD {} :: LHsDecl GhcPs)
                        | RealSrcSpan spn `isSubspanOf` l -> Just (spLoc, HsDecl)
                    _ -> Nothing

-- | Like 'something', but performs in bottom-up manner.
something' :: forall a. GenericQ (Maybe a) -> GenericQ (Maybe a)
something' = everything (flip (<|>))

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
