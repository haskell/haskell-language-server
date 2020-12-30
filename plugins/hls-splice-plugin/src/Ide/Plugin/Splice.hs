{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiWayIf #-}
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

import Control.Exception (SomeException)
import Control.Lens (ifoldMap, (%~), (<&>), (^.))
import Control.Lens.At
import Control.Monad
import qualified Control.Monad.Fail as Fail
import Control.Monad.Trans.Class
import Control.Monad.Trans.Maybe
import Control.Monad.Trans.Writer.CPS
import Data.Aeson
import qualified Data.DList as DL
import Data.Maybe (fromMaybe)
import Data.Monoid (Ap (..))
import Data.Set (Set)
import qualified Data.Set as Set
import Data.String
import qualified Data.Text as T
import Development.IDE
import Development.IDE.Core.PositionMapping
import Development.IDE.GHC.Compat hiding (getLoc)
import Exception (gtry)
import GhcMonad
import GhcPlugins hiding (Var, getLoc, (<>))
import Ide.Plugin
import Ide.Plugin.Splice.Types
    ( ExpandSpliceParams (..),
      ExpandStyle (..),
      SpliceContext (Expr, HsDecl, HsType, Pat),
    )
import Ide.TreeTransform
import Ide.Types
import Language.Haskell.GHC.ExactPrint (Annotation (..), TransformT, getEntryDPT, modifyAnnsT, setPrecedingLines, uniqueSrcSpanT)
import qualified Language.Haskell.GHC.ExactPrint.Parsers as Exact
import Language.Haskell.GHC.ExactPrint.Types (Comment (Comment), mkAnnKey)
import Language.Haskell.LSP.Core
import Language.Haskell.LSP.Messages
import Language.Haskell.LSP.Types
import qualified Language.Haskell.LSP.Types.Lens as J
import RnSplice
import TcRnMonad

descriptor :: PluginId -> PluginDescriptor
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

commands :: [PluginCommand]
commands =
    [ PluginCommand expandInplaceId inplaceCmdName $ expandTHSplice Inplace
    , PluginCommand expandCommentedId commentedCmdName $ expandTHSplice Commented
    ]

expandTHSplice ::
    -- | Inplace?
    ExpandStyle ->
    CommandFunction ExpandSpliceParams
expandTHSplice eStyle lsp ideState params@ExpandSpliceParams {..} =
    fmap (fromMaybe defaultResult) $
        runMaybeT $ do
            fp <- MaybeT $ pure $ uriToNormalizedFilePath $ toNormalizedUri uri
            TcModuleResult {tmrParsed = pm, ..} <-
                MaybeT $
                    runAction "expandTHSplice.TypeCheck" ideState $
                        use TypeCheck fp
            ps <-
                MaybeT $
                    useAnnotatedSource
                        "expandTHSplice.AnnotedSource"
                        ideState
                        fp
            hscEnvEq <-
                lift $
                    runAction "expandTHSplice.ghcSessionDeps" ideState $
                        use_ GhcSessionDeps fp
            let hscEnv0 = hscEnvWithImportPaths hscEnvEq
                modSum = pm_mod_summary pm
            hscEnv <- lift $
                evalGhcEnv hscEnv0 $ do
                    env <- getSession
                    df <- liftIO $ setupDynFlagsForGHCiLike env $ ms_hspp_opts modSum

                    let impPaths = fromMaybe (importPaths df) (envImportPaths hscEnvEq)

                    -- Set the modified flags in the session
                    _lp <- setSessionDynFlags df {importPaths = impPaths}

                    -- copy the package state to the interactive DynFlags
                    idflags <- getInteractiveDynFlags
                    setInteractiveDynFlags $
                        idflags
                            { pkgState = pkgState df
                            , pkgDatabase = pkgDatabase df
                            , packageFlags = packageFlags df
                            , useColor = Never
                            , canUseColor = False
                            }
                    env' <- getSession
                    -- setTargets [thisModuleTarget]
                    resl <- load LoadAllTargets
                    case resl of
                        Succeeded -> do
                            setContext [IIModule $ moduleName $ ms_mod modSum]
                                `gcatch` \(_ :: SomeException) -> pure ()
                            getSession
                        Failed -> pure env'
            let dflags = hsc_dflags hscEnv
                srcSpan = rangeToRealSrcSpan range $ fromString $ fromNormalizedFilePath fp
            ((warns, errs), mEdits) <- liftIO $
                initTcWithGbl hscEnv tmrTypechecked srcSpan $
                    case spliceContext of
                        Expr -> flip (transformM dflags (clientCapabilities lsp) uri) ps $
                            graftWithSmallestM (RealSrcSpan srcSpan) $ \case
                                inp@(L _spn (HsSpliceE _ spl)) -> do
                                    eExpr <- lift $ gtry @_ @SomeException (fst <$> rnSpliceExpr spl)
                                    case (eExpr, eStyle) of
                                        (Left exc, _) ->
                                            lift $
                                                Nothing
                                                    <$ reportEditor
                                                        lsp
                                                        MtError
                                                        [ "Error during expanding splice"
                                                        , ""
                                                        , T.pack (show exc)
                                                        ]
                                        (Right expr', Inplace) ->
                                            Just <$> unRenamedE dflags expr'
                                        (Right expr', Commented) -> do
                                            let expanded = showSDoc dflags $ ppr expr'
                                            dPos <- getEntryDPT inp
                                            uniq <- uniqueSrcSpanT
                                            modifyAnnsT $
                                                ix (mkAnnKey inp)
                                                    %~ \ann ->
                                                        ann
                                                            { annFollowingComments =
                                                                (Comment expanded uniq Nothing, dPos) :
                                                                annFollowingComments ann
                                                            }
                                            pure $ Just inp
                                _ -> pure Nothing
                        HsDecl -> undefined
                        Pat -> undefined
                        HsType -> undefined
            unless (null errs) $
                reportEditor
                    lsp
                    MtError
                    [ "Error during expanding splice:"
                    , T.pack $ show errs
                    ]
            guard $ null errs
            unless (null warns) $
                reportEditor
                    lsp
                    MtWarning
                    [ "Warning during expanding splice:"
                    , T.pack $ show warns
                    ]
            pure
                ( Right Null
                , mEdits <&> \edits ->
                    (WorkspaceApplyEdit, ApplyWorkspaceEditParams edits)
                )
    where
        defaultResult = (Right Null, Nothing)

-- | FIXME:  Is thereAny "clever" way to do this exploiting TTG?
reportEditor :: MonadIO m => LspFuncs a -> MessageType -> [T.Text] -> m ()
reportEditor lsp msgTy msgs =
    liftIO $
        sendFunc lsp $
            NotShowMessage $
                NotificationMessage "2.0" WindowShowMessage $
                    ShowMessageParams msgTy $
                        T.unlines msgs

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

unRenamedE ::
    Fail.MonadFail m =>
    DynFlags ->
    HsExpr GhcRn ->
    TransformT m (LHsExpr GhcPs)
unRenamedE dflags expr = do
    uniq <- show <$> uniqueSrcSpanT
    (anns, expr') <-
        either (fail . show) pure $
            Exact.parseExpr dflags uniq $
                showSDoc dflags $ ppr expr
    let _anns' = setPrecedingLines expr' 0 1 anns
    -- modifyAnnsT $ mappend anns'
    pure expr'

-- TODO: workaround when HieAst unavailable (e.g. when the module itself errors)
-- TODO: Declaration Splices won't appear in HieAst; perhaps we must just use Parsed/Renamed ASTs?
codeAction :: CodeActionProvider
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
        go ran' fs ast = do
            forM_ (smallestContainingSatisfying (rangeToRealSrcSpan ran' fs) isSpliceNode ast) $
                \Node {..} -> do
                    let NodeInfo {..} = nodeInfo
                        spCxt
                            | ("SplicePat", "Pat") `Set.member` nodeAnnotations =
                                Just Pat
                            | ("HsSpliceE", "HsExpr") `Set.member` nodeAnnotations = Just Expr
                            | ("HsSpliceTy", "HsType") `Set.member` nodeAnnotations = Just HsType
                            | ("SpliceD", "HsDecl") `Set.member` nodeAnnotations = Just HsDecl
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
        , ("SpliceD", "HsDecl")
        ]

expandStyles :: [(ExpandStyle, T.Text, CommandId)]
expandStyles =
    [ (Inplace, inplaceCmdName, expandInplaceId)
    , (Commented, commentedCmdName, expandCommentedId)
    ]
