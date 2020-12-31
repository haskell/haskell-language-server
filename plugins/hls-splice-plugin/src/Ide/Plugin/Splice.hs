{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ViewPatterns #-}

module Ide.Plugin.Splice
    ( descriptor,
    )
where

import Control.Lens (ifoldMap, (^.))
import Control.Monad
import qualified Control.Monad.Fail as Fail
import Control.Monad.Trans.Class
import Control.Monad.Trans.Maybe
import Control.Monad.Trans.Writer.CPS
import Data.Aeson
import qualified Data.DList as DL
import Data.Function
import qualified Data.Kind as Kinds
import Data.List (sortOn)
import Data.Maybe (fromMaybe, listToMaybe, mapMaybe)
import Data.Monoid (Ap (..))
import Data.Set (Set)
import qualified Data.Set as Set
import qualified Data.Text as T
import Development.IDE
import Development.IDE.Core.PositionMapping
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

--
expandInplaceId, expandCommentedId :: CommandId
expandInplaceId = "expandTHSpliceInplace"
expandCommentedId = "expandTHSpliceCommented"

inplaceCmdName :: T.Text
inplaceCmdName = "expand TemplateHaskell Splice (in-place)"

commentedCmdName :: T.Text
commentedCmdName = "expand TemplateHaskell Splice (comented-out)"

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
                srcSpan =
                    RealSrcSpan $
                        rangeToRealSrcSpan range $ fromString $ fromNormalizedFilePath fp
                exprSuperSpans =
                    listToMaybe $ findSuperSpansAsc srcSpan exprSplices
                patSuperSpans =
                    listToMaybe $ findSuperSpansAsc srcSpan patSplices
                typeSuperSpans =
                    listToMaybe $ findSuperSpansAsc srcSpan typeSplices

                graftSpliceWith ::
                    forall ast.
                    HasSplice ast =>
                    Maybe (SrcSpan, Located (ast GhcPs)) ->
                    MaybeT IO (Maybe (Either String WorkspaceEdit))
                graftSpliceWith expandeds = forM expandeds $ \(loc, expanded) ->
                    transformM
                        dflags
                        (clientCapabilities lsp)
                        uri
                        ( graftWithSmallestM
                            loc
                            $ \case
                                (L _ (matchSplice @ast proxy# -> Just {})) -> pure $ Just expanded
                                _ -> pure Nothing
                        )
                        ps
            eedits <-
                join . maybe (Left "No splcie information found") Right <$> case spliceContext of
                    Expr -> graftSpliceWith exprSuperSpans
                    Pat -> graftSpliceWith patSuperSpans
                    HsType -> graftSpliceWith typeSuperSpans
            case eedits of
                Left err -> do
                    reportEditor lsp MtError
                        ["Error during expanding splice: " <> T.pack err]
                    pure (Left $ responseError $ T.pack err, Nothing)
                Right edits -> pure
                    ( Right Null
                    , Just (WorkspaceApplyEdit, ApplyWorkspaceEditParams edits)
                    )
    where
        defaultResult = (Right Null, Nothing)

findSuperSpansAsc :: SrcSpan -> [(LHsExpr GhcTc, a)] -> [(SrcSpan, a)]
findSuperSpansAsc srcSpan =
    sortOn (SubSpan . fst)
        . mapMaybe
            ( \(L spn _, e) -> do
                guard (srcSpan `isSubspanOf` spn)
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

-- | FIXME:  Is thereAny "clever" way to do this exploiting TTG?
reportEditor :: MonadIO m => LspFuncs a -> MessageType -> [T.Text] -> m ()
reportEditor lsp msgTy msgs =
    liftIO $
        sendFunc lsp $
            NotShowMessage $
                NotificationMessage "2.0" WindowShowMessage $
                    ShowMessageParams msgTy $
                        T.unlines msgs

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
    -- modifyAnnsT $ mappend anns'
    pure expr'

-- TODO: workaround when HieAst unavailable (e.g. when the module itself errors)
-- TODO: Declaration Splices won't appear in HieAst; perhaps we must just use Parsed/Renamed ASTs?
codeAction :: CodeActionProvider IdeState
codeAction _ state plId docId range0 _ =
    fmap (maybe (Right $ List []) Right) $
        runMaybeT $ do
            fp <- MaybeT $ pure $ uriToNormalizedFilePath $ toNormalizedUri theUri
            (getAsts . hieAst -> asts, posMap) <-
                MaybeT . runAction "splice.codeAction.GitHieAst" state $
                    useWithStale GetHieAst fp
            ran' <-
                MaybeT $
                    pure $
                        fromCurrentRange posMap range0
            fmap (List . DL.toList) $
                execWriterT $
                    getAp $ ifoldMap (fmap Ap . go ran') asts
    where
        theUri = docId ^. J.uri
        go ran' fs ast =
            forM_ (smallestContainingSatisfying (rangeToRealSrcSpan ran' fs) isSpliceNode ast) $
                \Node {..} -> do
                    let NodeInfo {..} = nodeInfo
                        spCxt
                            | ("SplicePat", "Pat") `Set.member` nodeAnnotations =
                                Just Pat
                            | ("HsSpliceE", "HsExpr") `Set.member` nodeAnnotations = Just Expr
                            | ("HsSpliceTy", "HsType") `Set.member` nodeAnnotations = Just HsType
                            {- FIXME:  HsDecl needs different treatment
                            | ("SpliceD", "HsDecl") `Set.member` nodeAnnotations = Just HsDecl
                            -}
                            | otherwise = Nothing
                    forM_ spCxt $ \spliceContext -> forM_ expandStyles $ \(_style, title, cmdId) -> do
                        let range = realSrcSpanToRange nodeSpan
                            params = ExpandSpliceParams {uri = theUri, ..}
                        act <-
                            liftIO $
                                mkLspCommand plId cmdId title (Just [toJSON params])
                        tell $
                            DL.singleton $
                                CACodeAction $
                                    CodeAction title (Just CodeActionRefactorRewrite) Nothing Nothing (Just act)

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

isSpliceNode :: HieAST Type -> Bool
isSpliceNode Node {..} =
    not $
        Set.null $
            spliceAnns
                `Set.intersection` nodeAnnotations nodeInfo

spliceAnns :: Set (FastString, FastString)
spliceAnns =
    Set.fromList
        [ ("SplicePat", "Pat")
        , ("HsSpliceE", "HsExpr")
        , ("HsSpliceTy", "HsType")
        -- , ("SpliceD", "HsDecl") -- FIXME: HsDecl
        ]

expandStyles :: [(ExpandStyle, T.Text, CommandId)]
expandStyles =
    [ (Inplace, inplaceCmdName, expandInplaceId)
    -- , (Commented, commentedCmdName, expandCommentedId)
    ]
