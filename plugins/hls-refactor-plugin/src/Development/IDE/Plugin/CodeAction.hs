-- Copyright (c) 2019 The DAML Authors. All rights reserved.
-- SPDX-License-Identifier: Apache-2.0
{-# LANGUAGE GADTs #-}

module Development.IDE.Plugin.CodeAction
    (
    mkExactprintPluginDescriptor,
    iePluginDescriptor,
    typeSigsPluginDescriptor,
    bindingsPluginDescriptor,
    fillHolePluginDescriptor,
    extendImportPluginDescriptor,
    -- * For testing
    matchRegExMultipleImports
    ) where

import           Control.Applicative                               ((<|>))
import           Control.Arrow                                     (second,
                                                                    (&&&),
                                                                    (>>>))
import           Control.Concurrent.STM.Stats                      (atomically)
import           Control.Monad.Extra
import           Control.Monad.IO.Class
import           Control.Monad.Trans.Maybe
import           Data.Aeson
import           Data.Char
import qualified Data.DList                                        as DL
import           Data.Function
import           Data.Functor
import qualified Data.HashMap.Strict                               as Map
import qualified Data.HashSet                                      as Set
import           Data.List.Extra
import           Data.List.NonEmpty                                (NonEmpty ((:|)))
import qualified Data.List.NonEmpty                                as NE
import qualified Data.Map.Strict                                   as M
import           Data.Maybe
import           Data.Ord                                          (comparing)
import qualified Data.Set                                          as S
import qualified Data.Text                                         as T
import qualified Data.Text.Utf16.Rope                              as Rope
import           Data.Tuple.Extra                                  (first)
import           Development.IDE.Core.Rules
import           Development.IDE.Core.RuleTypes
import           Development.IDE.Core.Service
import           Development.IDE.Core.Shake                        hiding (Log)
import           Development.IDE.GHC.Compat                        hiding
                                                                   (ImplicitPrelude)
import           Development.IDE.GHC.Compat.ExactPrint
import           Development.IDE.GHC.Compat.Util
import           Development.IDE.GHC.Error
import           Development.IDE.GHC.ExactPrint
import qualified Development.IDE.GHC.ExactPrint                    as E
import           Development.IDE.GHC.Util                          (printOutputable,
                                                                    printRdrName)
import           Development.IDE.Plugin.CodeAction.Args
import           Development.IDE.Plugin.CodeAction.ExactPrint
import           Development.IDE.Plugin.CodeAction.PositionIndexed
import           Development.IDE.Plugin.CodeAction.Util
import           Development.IDE.Plugin.Completions.Types
import           Development.IDE.Plugin.TypeLenses                 (suggestSignature)
import           Development.IDE.Types.Exports
import           Development.IDE.Types.Location
import           Development.IDE.Types.Logger                      hiding
                                                                   (group)
import           Development.IDE.Types.Options
import           GHC.Exts                                          (fromList)
import qualified GHC.LanguageExtensions                            as Lang
import           Ide.PluginUtils                                   (makeDiffTextEdit,
                                                                    subRange)
import           Ide.Types
import qualified Language.LSP.Server                               as LSP
import           Language.LSP.Types                                (ApplyWorkspaceEditParams (..),
                                                                    CodeAction (..),
                                                                    CodeActionContext (CodeActionContext, _diagnostics),
                                                                    CodeActionKind (CodeActionQuickFix, CodeActionUnknown),
                                                                    CodeActionParams (CodeActionParams),
                                                                    Command,
                                                                    Diagnostic (..),
                                                                    List (..),
                                                                    MessageType (..),
                                                                    ResponseError,
                                                                    SMethod (..),
                                                                    ShowMessageParams (..),
                                                                    TextDocumentIdentifier (TextDocumentIdentifier),
                                                                    TextEdit (TextEdit, _range),
                                                                    UInt,
                                                                    WorkspaceEdit (WorkspaceEdit, _changeAnnotations, _changes, _documentChanges),
                                                                    type (|?) (InR),
                                                                    uriToFilePath)
import           Language.LSP.VFS                                  (VirtualFile,
                                                                    _file_text)
import qualified Text.Fuzzy.Parallel                               as TFP
import           Text.Regex.TDFA                                   (mrAfter,
                                                                    (=~), (=~~))
#if MIN_VERSION_ghc(9,2,1)
import           Data.Either.Extra                                 (maybeToEither)
import           GHC.Types.SrcLoc                                  (generatedSrcSpan)
import           Language.Haskell.GHC.ExactPrint                   (noAnnSrcSpanDP1,
                                                                    runTransformT)
#endif
#if MIN_VERSION_ghc(9,2,0)
import           Control.Monad.Except                              (lift)
import           Debug.Trace
import           GHC                                               (AddEpAnn (AddEpAnn),
                                                                    Anchor (anchor_op),
                                                                    AnchorOperation (..),
                                                                    AnnsModule (am_main),
                                                                    DeltaPos (..),
                                                                    EpAnn (..),
                                                                    EpaLocation (..),
                                                                    LEpaComment,
                                                                    LocatedA,
                                                                    SrcSpanAnn' (SrcSpanAnn),
                                                                    SrcSpanAnnA,
                                                                    SrcSpanAnnN,
                                                                    TrailingAnn (..),
                                                                    addTrailingAnnToA,
                                                                    emptyComments,
                                                                    noAnn)
import           GHC.Hs                                            (IsUnicodeSyntax (..))
import           Language.Haskell.GHC.ExactPrint.Transform         (d1)

#else
import           Language.Haskell.GHC.ExactPrint.Types             (Annotation (annsDP),
                                                                    DeltaPos,
                                                                    KeywordId (G),
                                                                    deltaRow,
                                                                    mkAnnKey)
#endif

-------------------------------------------------------------------------------------------------

-- | Generate code actions.
codeAction
    :: IdeState
    -> PluginId
    -> CodeActionParams
    -> LSP.LspM c (Either ResponseError (List (Command |? CodeAction)))
codeAction state _ (CodeActionParams _ _ (TextDocumentIdentifier uri) _range CodeActionContext{_diagnostics=List xs}) = do
  contents <- LSP.getVirtualFile $ toNormalizedUri uri
  liftIO $ do
    let text = Rope.toText . (_file_text :: VirtualFile -> Rope.Rope) <$> contents
        mbFile = toNormalizedFilePath' <$> uriToFilePath uri
    diag <- atomically $ fmap (\(_, _, d) -> d) . filter (\(p, _, _) -> mbFile == Just p) <$> getDiagnostics state
    (join -> parsedModule) <- runAction "GhcideCodeActions.getParsedModule" state $ getParsedModule `traverse` mbFile
    let
      actions = caRemoveRedundantImports parsedModule text diag xs uri
               <> caRemoveInvalidExports parsedModule text diag xs uri
    pure $ Right $ List actions

-------------------------------------------------------------------------------------------------

iePluginDescriptor :: Recorder (WithPriority E.Log) -> PluginId -> PluginDescriptor IdeState
iePluginDescriptor recorder plId =
  let old =
        mkGhcideCAsPlugin [
            wrap suggestExportUnusedTopBinding
          , wrap suggestModuleTypo
          , wrap suggestFixConstructorImport
#if !MIN_VERSION_ghc(9,3,0)
          , wrap suggestExtendImport
          , wrap suggestImportDisambiguation
          , wrap suggestNewOrExtendImportForClassMethod
          , wrap suggestHideShadow
#endif
          , wrap suggestNewImport
          ]
          plId
   in mkExactprintPluginDescriptor recorder $ old {pluginHandlers = pluginHandlers old <> mkPluginHandler STextDocumentCodeAction codeAction }

typeSigsPluginDescriptor :: Recorder (WithPriority E.Log) -> PluginId -> PluginDescriptor IdeState
typeSigsPluginDescriptor recorder plId = mkExactprintPluginDescriptor recorder $
  mkGhcideCAsPlugin [
      wrap $ suggestSignature True
    , wrap suggestFillTypeWildcard
    , wrap suggestAddTypeAnnotationToSatisfyConstraints
#if !MIN_VERSION_ghc(9,3,0)
    , wrap removeRedundantConstraints
    , wrap suggestConstraint
#endif
    ]
    plId

bindingsPluginDescriptor :: Recorder (WithPriority E.Log) ->  PluginId -> PluginDescriptor IdeState
bindingsPluginDescriptor recorder plId = mkExactprintPluginDescriptor recorder $
  mkGhcideCAsPlugin [
      wrap suggestReplaceIdentifier
#if !MIN_VERSION_ghc(9,3,0)
    , wrap suggestImplicitParameter
#endif
    , wrap suggestNewDefinition
#if MIN_VERSION_ghc(9,2,1)
    , wrap suggestAddArgument
#endif
    , wrap suggestDeleteUnusedBinding
    ]
    plId

fillHolePluginDescriptor :: Recorder (WithPriority E.Log) -> PluginId -> PluginDescriptor IdeState
fillHolePluginDescriptor recorder plId = mkExactprintPluginDescriptor recorder (mkGhcideCAPlugin (wrap suggestFillHole) plId)

extendImportPluginDescriptor :: Recorder (WithPriority E.Log) -> PluginId -> PluginDescriptor IdeState
extendImportPluginDescriptor recorder plId = mkExactprintPluginDescriptor recorder $ (defaultPluginDescriptor plId)
  { pluginCommands = [extendImportCommand] }


-- | Add the ability for a plugin to call GetAnnotatedParsedSource
mkExactprintPluginDescriptor :: Recorder (WithPriority E.Log) -> PluginDescriptor a -> PluginDescriptor a
mkExactprintPluginDescriptor recorder desc = desc { pluginRules = pluginRules desc >> getAnnotatedParsedSourceRule recorder }

-------------------------------------------------------------------------------------------------


extendImportCommand :: PluginCommand IdeState
extendImportCommand =
  PluginCommand (CommandId extendImportCommandId) "additional edits for a completion" extendImportHandler

extendImportHandler :: CommandFunction IdeState ExtendImport
extendImportHandler ideState edit@ExtendImport {..} = do
  res <- liftIO $ runMaybeT $ extendImportHandler' ideState edit
  whenJust res $ \(nfp, wedit@WorkspaceEdit {_changes}) -> do
    let (_, List (head -> TextEdit {_range})) = fromJust $ _changes >>= listToMaybe . Map.toList
        srcSpan = rangeToSrcSpan nfp _range
    LSP.sendNotification SWindowShowMessage $
      ShowMessageParams MtInfo $
        "Import "
          <> maybe ("‘" <> newThing) (\x -> "‘" <> x <> " (" <> newThing <> ")") thingParent
          <> "’ from "
          <> importName
          <> " (at "
          <> printOutputable srcSpan
          <> ")"
    void $ LSP.sendRequest SWorkspaceApplyEdit (ApplyWorkspaceEditParams Nothing wedit) (\_ -> pure ())
  return $ Right Null

extendImportHandler' :: IdeState -> ExtendImport -> MaybeT IO (NormalizedFilePath, WorkspaceEdit)
extendImportHandler' ideState ExtendImport {..}
  | Just fp <- uriToFilePath doc,
    nfp <- toNormalizedFilePath' fp =
    do
      (ModSummaryResult {..}, ps, contents) <- MaybeT $ liftIO $
        runAction "extend import" ideState $
          runMaybeT $ do
            -- We want accurate edits, so do not use stale data here
            msr <- MaybeT $ use GetModSummaryWithoutTimestamps nfp
            ps <- MaybeT $ use GetAnnotatedParsedSource nfp
            (_, contents) <- MaybeT $ use GetFileContents nfp
            return (msr, ps, contents)
      let df = ms_hspp_opts msrModSummary
          wantedModule = mkModuleName (T.unpack importName)
          wantedQual = mkModuleName . T.unpack <$> importQual
          existingImport = find (isWantedModule wantedModule wantedQual) msrImports
      case existingImport of
        Just imp -> do
            fmap (nfp,) $ liftEither $
              rewriteToWEdit df doc
#if !MIN_VERSION_ghc(9,2,0)
                (annsA ps)
#endif
                $
                  extendImport (T.unpack <$> thingParent) (T.unpack newThing) (makeDeltaAst imp)

        Nothing -> do
            let n = newImport importName sym importQual False
                sym = if isNothing importQual then Just it else Nothing
                it = case thingParent of
                  Nothing -> newThing
                  Just p  -> p <> "(" <> newThing <> ")"
            t <- liftMaybe $ snd <$> newImportToEdit n ps (fromMaybe "" contents)
            return (nfp, WorkspaceEdit {_changes=Just (GHC.Exts.fromList [(doc,List [t])]), _documentChanges=Nothing, _changeAnnotations=Nothing})
  | otherwise =
    mzero

isWantedModule :: ModuleName -> Maybe ModuleName -> GenLocated l (ImportDecl GhcPs) -> Bool
isWantedModule wantedModule Nothing (L _ it@ImportDecl{ideclName, ideclHiding = Just (False, _)}) =
    not (isQualifiedImport it) && unLoc ideclName == wantedModule
isWantedModule wantedModule (Just qual) (L _ ImportDecl{ideclAs, ideclName, ideclHiding = Just (False, _)}) =
    unLoc ideclName == wantedModule && (wantedModule == qual || (unLoc . reLoc <$> ideclAs) == Just qual)
isWantedModule _ _ _ = False


liftMaybe :: Monad m => Maybe a -> MaybeT m a
liftMaybe a = MaybeT $ pure a

liftEither :: Monad m => Either e a -> MaybeT m a
liftEither (Left _)  = mzero
liftEither (Right x) = return x

-------------------------------------------------------------------------------------------------

findSigOfDecl :: p ~ GhcPass p0 => (IdP p -> Bool) -> [LHsDecl p] -> Maybe (Sig p)
findSigOfDecl pred decls =
  listToMaybe
    [ sig
      | L _ (SigD _ sig@(TypeSig _ idsSig _)) <- decls,
        any (pred . unLoc) idsSig
    ]

findSigOfDeclRanged :: forall p p0 . p ~ GhcPass p0 => Range -> [LHsDecl p] -> Maybe (Sig p)
findSigOfDeclRanged range decls = do
  dec <- findDeclContainingLoc (_start range) decls
  case dec of
     L _ (SigD _ sig@TypeSig {})     -> Just sig
     L _ (ValD _ (bind :: HsBind p)) -> findSigOfBind range bind
     _                               -> Nothing

findSigOfBind :: forall p p0. p ~ GhcPass p0 => Range -> HsBind p -> Maybe (Sig p)
findSigOfBind range bind =
    case bind of
      FunBind {} -> findSigOfLMatch (unLoc $ mg_alts (fun_matches bind))
      _          -> Nothing
  where
    findSigOfLMatch :: [LMatch p (LHsExpr p)] -> Maybe (Sig p)
    findSigOfLMatch ls = do
      match <- findDeclContainingLoc (_start range) ls
      let grhs = m_grhss $ unLoc match
#if !MIN_VERSION_ghc(9,2,0)
          span = getLoc $ reLoc $ grhssLocalBinds grhs
      if _start range `isInsideSrcSpan` span
        then findSigOfBinds range (unLoc (grhssLocalBinds grhs)) -- where clause
        else do
          grhs <- findDeclContainingLoc (_start range) (map reLocA $ grhssGRHSs grhs)
          case unLoc grhs of
            GRHS _ _ bd -> findSigOfExpr (unLoc bd)
            _           -> Nothing
#else
      msum
        [findSigOfBinds range (grhssLocalBinds grhs) -- where clause
        , do
#if MIN_VERSION_ghc(9,3,0)
          grhs <- findDeclContainingLoc (_start range) (grhssGRHSs grhs)
#else
          grhs <- findDeclContainingLoc (_start range) (map reLocA $ grhssGRHSs grhs)
#endif
          case unLoc grhs of
            GRHS _ _ bd -> findSigOfExpr (unLoc bd)
        ]
#endif

    findSigOfExpr :: HsExpr p -> Maybe (Sig p)
    findSigOfExpr = go
      where
#if MIN_VERSION_ghc(9,3,0)
        go (HsLet _ _ binds _ _) = findSigOfBinds range binds
#else
        go (HsLet _ binds _) = findSigOfBinds range binds
#endif
        go (HsDo _ _ stmts) = do
          stmtlr <- unLoc <$> findDeclContainingLoc (_start range) (unLoc stmts)
          case stmtlr of
            LetStmt _ lhsLocalBindsLR -> findSigOfBinds range lhsLocalBindsLR
            _                         -> Nothing
        go _ = Nothing

findSigOfBinds :: p ~ GhcPass p0 => Range -> HsLocalBinds p -> Maybe (Sig p)
findSigOfBinds range = go
  where
    go (HsValBinds _ (ValBinds _ binds lsigs)) =
        case unLoc <$> findDeclContainingLoc (_start range) lsigs of
          Just sig' -> Just sig'
          Nothing -> do
            lHsBindLR <- findDeclContainingLoc (_start range) (bagToList binds)
            findSigOfBind range (unLoc lHsBindLR)
    go _ = Nothing

findInstanceHead :: (Outputable (HsType p), p ~ GhcPass p0) => DynFlags -> String -> [LHsDecl p] -> Maybe (LHsType p)
findInstanceHead df instanceHead decls =
  listToMaybe
#if !MIN_VERSION_ghc(9,2,0)
    [ hsib_body
      | L _ (InstD _ (ClsInstD _ ClsInstDecl {cid_poly_ty = HsIB {hsib_body}})) <- decls,
        showSDoc df (ppr hsib_body) == instanceHead
    ]
#else
    [ hsib_body
      | L _ (InstD _ (ClsInstD _ ClsInstDecl {cid_poly_ty = (unLoc -> HsSig {sig_body = hsib_body})})) <- decls,
        showSDoc df (ppr hsib_body) == instanceHead
    ]
#endif

#if MIN_VERSION_ghc(9,2,0)
findDeclContainingLoc :: Foldable t => Position -> t (GenLocated (SrcSpanAnn' a) e) -> Maybe (GenLocated (SrcSpanAnn' a) e)
#else
findDeclContainingLoc :: Foldable t => Position -> t (GenLocated SrcSpan e) -> Maybe (GenLocated SrcSpan e)
#endif
findDeclContainingLoc loc = find (\(L l _) -> loc `isInsideSrcSpan` locA l)

-- Single:
-- This binding for ‘mod’ shadows the existing binding
--   imported from ‘Prelude’ at haskell-language-server/ghcide/src/Development/IDE/Plugin/CodeAction.hs:10:8-40
--   (and originally defined in ‘GHC.Real’)typecheck(-Wname-shadowing)
-- Multi:
--This binding for ‘pack’ shadows the existing bindings
--  imported from ‘Data.ByteString’ at B.hs:6:1-22
--  imported from ‘Data.ByteString.Lazy’ at B.hs:8:1-27
--  imported from ‘Data.Text’ at B.hs:7:1-16
#if !MIN_VERSION_ghc(9,3,0)
suggestHideShadow :: Annotated ParsedSource -> T.Text -> Maybe TcModuleResult -> Maybe HieAstResult -> Diagnostic -> [(T.Text, [Either TextEdit Rewrite])]
suggestHideShadow ps fileContents mTcM mHar Diagnostic {_message, _range}
  | Just [identifier, modName, s] <-
      matchRegexUnifySpaces
        _message
        "This binding for ‘([^`]+)’ shadows the existing binding imported from ‘([^`]+)’ at ([^ ]*)" =
    suggests identifier modName s
  | Just [identifier] <-
      matchRegexUnifySpaces
        _message
        "This binding for ‘([^`]+)’ shadows the existing bindings",
    Just matched <- allMatchRegexUnifySpaces _message "imported from ‘([^’]+)’ at ([^ ]*)",
    mods <- [(modName, s) | [_, modName, s] <- matched],
    result <- nubOrdBy (compare `on` fst) $ mods >>= uncurry (suggests identifier),
    hideAll <- ("Hide " <> identifier <> " from all occurrence imports", concatMap snd result) =
    result <> [hideAll]
  | otherwise = []
  where
    L _ HsModule {hsmodImports} = astA ps

    suggests identifier modName s
      | Just tcM <- mTcM,
        Just har <- mHar,
        [s'] <- [x | (x, "") <- readSrcSpan $ T.unpack s],
        isUnusedImportedId tcM har (T.unpack identifier) (T.unpack modName) (RealSrcSpan s' Nothing),
        mDecl <- findImportDeclByModuleName hsmodImports $ T.unpack modName,
        title <- "Hide " <> identifier <> " from " <> modName =
        if modName == "Prelude" && null mDecl
          then maybeToList $ (\(_, te) -> (title, [Left te])) <$> newImportToEdit (hideImplicitPreludeSymbol identifier) ps fileContents
          else maybeToList $ (title,) . pure . pure . hideSymbol (T.unpack identifier) <$> mDecl
      | otherwise = []
#endif

findImportDeclByModuleName :: [LImportDecl GhcPs] -> String -> Maybe (LImportDecl GhcPs)
findImportDeclByModuleName decls modName = flip find decls $ \case
  (L _ ImportDecl {..}) -> modName == moduleNameString (unLoc ideclName)
  _                     -> error "impossible"

isTheSameLine :: SrcSpan -> SrcSpan -> Bool
isTheSameLine s1 s2
  | Just sl1 <- getStartLine s1,
    Just sl2 <- getStartLine s2 =
    sl1 == sl2
  | otherwise = False
  where
    getStartLine x = srcLocLine . realSrcSpanStart <$> realSpan x

isUnusedImportedId :: TcModuleResult -> HieAstResult -> String -> String -> SrcSpan -> Bool
isUnusedImportedId
  TcModuleResult {tmrTypechecked = TcGblEnv {tcg_imports = ImportAvails {imp_mods}}}
  HAR {refMap}
  identifier
  modName
  importSpan
    | occ <- mkVarOcc identifier,
      impModsVals <- importedByUser . concat $ moduleEnvElts imp_mods,
      Just rdrEnv <-
        listToMaybe
          [ imv_all_exports
            | ImportedModsVal {..} <- impModsVals,
              imv_name == mkModuleName modName,
              isTheSameLine imv_span importSpan
          ],
      [GRE {gre_name = name}] <- lookupGlobalRdrEnv rdrEnv occ,
      importedIdentifier <- Right name,
      refs <- M.lookup importedIdentifier refMap =
      maybe True (not . any (\(_, IdentifierDetails {..}) -> identInfo == S.singleton Use)) refs
    | otherwise = False

suggestRemoveRedundantImport :: ParsedModule -> Maybe T.Text -> Diagnostic -> [(T.Text, [TextEdit])]
suggestRemoveRedundantImport ParsedModule{pm_parsed_source = L _  HsModule{hsmodImports}} contents Diagnostic{_range=_range,..}
--     The qualified import of ‘many’ from module ‘Control.Applicative’ is redundant
    | Just [_, bindings] <- matchRegexUnifySpaces _message "The( qualified)? import of ‘([^’]*)’ from module [^ ]* is redundant"
    , Just (L _ impDecl) <- find (\(L (locA -> l) _) -> _start _range `isInsideSrcSpan` l && _end _range `isInsideSrcSpan` l ) hsmodImports
    , Just c <- contents
    , ranges <- map (rangesForBindingImport impDecl . T.unpack) (T.splitOn ", " bindings)
    , ranges' <- extendAllToIncludeCommaIfPossible False (indexedByPosition $ T.unpack c) (concat ranges)
    , not (null ranges')
    = [( "Remove " <> bindings <> " from import" , [ TextEdit r "" | r <- ranges' ] )]

-- File.hs:16:1: warning:
--     The import of `Data.List' is redundant
--       except perhaps to import instances from `Data.List'
--     To import instances alone, use: import Data.List()
    | _message =~ ("The( qualified)? import of [^ ]* is redundant" :: String)
        = [("Remove import", [TextEdit (extendToWholeLineIfPossible contents _range) ""])]
    | otherwise = []


-- Note [Removing imports is preferred]
-- It's good to prefer the remove imports code action because an unused import
-- is likely to be removed and less likely the warning will be disabled.
-- Therefore actions to remove a single or all redundant imports should be
-- preferred, so that the client can prioritize them higher.
caRemoveRedundantImports :: Maybe ParsedModule -> Maybe T.Text -> [Diagnostic] -> [Diagnostic] -> Uri -> [Command |? CodeAction]
caRemoveRedundantImports m contents digs ctxDigs uri
  | Just pm <- m,
    r <- join $ map (\d -> repeat d `zip` suggestRemoveRedundantImport pm contents d) digs,
    allEdits <- [ e | (_, (_, edits)) <- r, e <- edits],
    caRemoveAll <- removeAll allEdits,
    ctxEdits <- [ x | x@(d, _) <- r, d `elem` ctxDigs],
    not $ null ctxEdits,
    caRemoveCtx <- map (\(d, (title, tedit)) -> removeSingle title tedit d) ctxEdits
      = caRemoveCtx ++ [caRemoveAll]
  | otherwise = []
  where
    removeSingle title tedit diagnostic = mkCA title (Just CodeActionQuickFix) Nothing [diagnostic] WorkspaceEdit{..} where
        _changes = Just $ Map.singleton uri $ List tedit
        _documentChanges = Nothing
        _changeAnnotations = Nothing
    removeAll tedit = InR $ CodeAction{..} where
        _changes = Just $ Map.singleton uri $ List tedit
        _title = "Remove all redundant imports"
        _kind = Just CodeActionQuickFix
        _diagnostics = Nothing
        _documentChanges = Nothing
        _edit = Just WorkspaceEdit{..}
        -- See Note [Removing imports is preferred]
        _isPreferred = Just True
        _command = Nothing
        _disabled = Nothing
        _xdata = Nothing
        _changeAnnotations = Nothing

caRemoveInvalidExports :: Maybe ParsedModule -> Maybe T.Text -> [Diagnostic] -> [Diagnostic] -> Uri -> [Command |? CodeAction]
caRemoveInvalidExports m contents digs ctxDigs uri
  | Just pm <- m,
    Just txt <- contents,
    txt' <- indexedByPosition $ T.unpack txt,
    r <- mapMaybe (groupDiag pm) digs,
    r' <- map (\(t,d,rs) -> (t,d,extend txt' rs)) r,
    caRemoveCtx <- mapMaybe removeSingle r',
    allRanges <- nubOrd $ [ range | (_,_,ranges) <- r, range <- ranges],
    allRanges' <- extend txt' allRanges,
    Just caRemoveAll <- removeAll allRanges',
    ctxEdits <- [ x | x@(_, d, _) <- r, d `elem` ctxDigs],
    not $ null ctxEdits
      = caRemoveCtx ++ [caRemoveAll]
  | otherwise = []
  where
    extend txt ranges = extendAllToIncludeCommaIfPossible True txt ranges

    groupDiag pm dig
      | Just (title, ranges) <- suggestRemoveRedundantExport pm dig
      = Just (title, dig, ranges)
      | otherwise = Nothing

    removeSingle (_, _, []) = Nothing
    removeSingle (title, diagnostic, ranges) = Just $ InR $ CodeAction{..} where
        tedit = concatMap (\r -> [TextEdit r ""]) $ nubOrd ranges
        _changes = Just $ Map.singleton uri $ List tedit
        _title = title
        _kind = Just CodeActionQuickFix
        _diagnostics = Just $ List [diagnostic]
        _documentChanges = Nothing
        _edit = Just WorkspaceEdit{..}
        _command = Nothing
        -- See Note [Removing imports is preferred]
        _isPreferred = Just True
        _disabled = Nothing
        _xdata = Nothing
        _changeAnnotations = Nothing
    removeAll [] = Nothing
    removeAll ranges = Just $ InR $ CodeAction{..} where
        tedit = concatMap (\r -> [TextEdit r ""]) ranges
        _changes = Just $ Map.singleton uri $ List tedit
        _title = "Remove all redundant exports"
        _kind = Just CodeActionQuickFix
        _diagnostics = Nothing
        _documentChanges = Nothing
        _edit = Just WorkspaceEdit{..}
        _command = Nothing
        -- See Note [Removing imports is preferred]
        _isPreferred = Just True
        _disabled = Nothing
        _xdata = Nothing
        _changeAnnotations = Nothing

suggestRemoveRedundantExport :: ParsedModule -> Diagnostic -> Maybe (T.Text, [Range])
suggestRemoveRedundantExport ParsedModule{pm_parsed_source = L _ HsModule{..}} Diagnostic{..}
  | msg <- unifySpaces _message
  , Just export <- hsmodExports
  , Just exportRange <- getLocatedRange $ reLoc export
  , exports <- unLoc export
  , Just (removeFromExport, !ranges) <- fmap (getRanges exports . notInScope) (extractNotInScopeName msg)
                         <|> (,[_range]) <$> matchExportItem msg
                         <|> (,[_range]) <$> matchDupExport msg
  , subRange _range exportRange
    = Just ("Remove ‘" <> removeFromExport <> "’ from export", ranges)
  where
    matchExportItem msg = regexSingleMatch msg "The export item ‘([^’]+)’"
    matchDupExport msg = regexSingleMatch msg "Duplicate ‘([^’]+)’ in export list"
    getRanges exports txt = case smallerRangesForBindingExport exports (T.unpack txt) of
      []     -> (txt, [_range])
      ranges -> (txt, ranges)
suggestRemoveRedundantExport _ _ = Nothing

suggestDeleteUnusedBinding :: ParsedModule -> Maybe T.Text -> Diagnostic -> [(T.Text, [TextEdit])]
suggestDeleteUnusedBinding
  ParsedModule{pm_parsed_source = L _ HsModule{hsmodDecls}}
  contents
  Diagnostic{_range=_range,..}
-- Foo.hs:4:1: warning: [-Wunused-binds] Defined but not used: ‘f’
    | Just [name] <- matchRegexUnifySpaces _message ".*Defined but not used: ‘([^ ]+)’"
    , Just indexedContent <- indexedByPosition . T.unpack <$> contents
      = let edits = flip TextEdit "" <$> relatedRanges indexedContent (T.unpack name)
        in ([("Delete ‘" <> name <> "’", edits) | not (null edits)])
    | otherwise = []
    where
      relatedRanges indexedContent name =
        concatMap (findRelatedSpans indexedContent name . reLoc) hsmodDecls
      toRange = realSrcSpanToRange
      extendForSpaces = extendToIncludePreviousNewlineIfPossible

      findRelatedSpans :: PositionIndexedString -> String -> Located (HsDecl GhcPs) -> [Range]
      findRelatedSpans
        indexedContent
        name
        (L (RealSrcSpan l _) (ValD _ (extractNameAndMatchesFromFunBind -> Just (lname, matches)))) =
        case lname of
          (L nLoc _name) | isTheBinding nLoc ->
            let findSig (L (RealSrcSpan l _) (SigD _ sig)) = findRelatedSigSpan indexedContent name l sig
                findSig _ = []
            in
              extendForSpaces indexedContent (toRange l) :
              concatMap (findSig . reLoc) hsmodDecls
          _ -> concatMap (findRelatedSpanForMatch indexedContent name) matches
      findRelatedSpans _ _ _ = []

      extractNameAndMatchesFromFunBind
        :: HsBind GhcPs
        -> Maybe (Located (IdP GhcPs), [LMatch GhcPs (LHsExpr GhcPs)])
      extractNameAndMatchesFromFunBind
        FunBind
          { fun_id=lname
          , fun_matches=MG {mg_alts=L _ matches}
          } = Just (reLoc lname, matches)
      extractNameAndMatchesFromFunBind _ = Nothing

      findRelatedSigSpan :: PositionIndexedString -> String -> RealSrcSpan -> Sig GhcPs -> [Range]
      findRelatedSigSpan indexedContent name l sig =
        let maybeSpan = findRelatedSigSpan1 name sig
        in case maybeSpan of
          Just (_span, True) -> pure $ extendForSpaces indexedContent $ toRange l -- a :: Int
          Just (RealSrcSpan span _, False) -> pure $ toRange span -- a, b :: Int, a is unused
          _ -> []

      -- Second of the tuple means there is only one match
      findRelatedSigSpan1 :: String -> Sig GhcPs -> Maybe (SrcSpan, Bool)
      findRelatedSigSpan1 name (TypeSig _ lnames _) =
        let maybeIdx = findIndex (\(L _ id) -> isSameName id name) lnames
        in case maybeIdx of
            Nothing -> Nothing
            Just _ | length lnames == 1 -> Just (getLoc $ reLoc $ head lnames, True)
            Just idx ->
              let targetLname = getLoc $ reLoc $ lnames !! idx
                  startLoc = srcSpanStart targetLname
                  endLoc = srcSpanEnd targetLname
                  startLoc' = if idx == 0
                              then startLoc
                              else srcSpanEnd . getLoc . reLoc $ lnames !! (idx - 1)
                  endLoc' = if idx == 0 && idx < length lnames - 1
                            then srcSpanStart . getLoc . reLoc $ lnames !! (idx + 1)
                            else endLoc
              in Just (mkSrcSpan startLoc' endLoc', False)
      findRelatedSigSpan1 _ _ = Nothing

      -- for where clause
      findRelatedSpanForMatch
        :: PositionIndexedString
        -> String
        -> LMatch GhcPs (LHsExpr GhcPs)
        -> [Range]
      findRelatedSpanForMatch
        indexedContent
        name
        (L _ Match{m_grhss=GRHSs{grhssLocalBinds}}) = do
        let go bag lsigs =
                if isEmptyBag bag
                then []
                else concatMap (findRelatedSpanForHsBind indexedContent name lsigs) bag
#if !MIN_VERSION_ghc(9,2,0)
        case grhssLocalBinds of
          (L _ (HsValBinds _ (ValBinds _ bag lsigs))) -> go bag lsigs
          _                                           -> []
#else
        case grhssLocalBinds of
          (HsValBinds _ (ValBinds _ bag lsigs)) -> go bag lsigs
          _                                     -> []
#endif
      findRelatedSpanForMatch _ _ _ = []

      findRelatedSpanForHsBind
        :: PositionIndexedString
        -> String
        -> [LSig GhcPs]
        -> LHsBind GhcPs
        -> [Range]
      findRelatedSpanForHsBind
        indexedContent
        name
        lsigs
        (L (locA -> (RealSrcSpan l _)) (extractNameAndMatchesFromFunBind -> Just (lname, matches))) =
        if isTheBinding (getLoc lname)
        then
          let findSig (L (RealSrcSpan l _) sig) = findRelatedSigSpan indexedContent name l sig
              findSig _ = []
          in extendForSpaces indexedContent (toRange l) : concatMap (findSig . reLoc) lsigs
        else concatMap (findRelatedSpanForMatch indexedContent name) matches
      findRelatedSpanForHsBind _ _ _ _ = []

      isTheBinding :: SrcSpan -> Bool
      isTheBinding span = srcSpanToRange span == Just _range

      isSameName :: IdP GhcPs -> String -> Bool
      isSameName x name = T.unpack (printOutputable x) == name

data ExportsAs = ExportName | ExportPattern | ExportFamily | ExportAll
  deriving (Eq)

getLocatedRange :: HasSrcSpan a => a -> Maybe Range
getLocatedRange = srcSpanToRange . getLoc

suggestExportUnusedTopBinding :: Maybe T.Text -> ParsedModule -> Diagnostic -> Maybe (T.Text, TextEdit)
suggestExportUnusedTopBinding srcOpt ParsedModule{pm_parsed_source = L _ HsModule{..}} Diagnostic{..}
-- Foo.hs:4:1: warning: [-Wunused-top-binds] Defined but not used: ‘f’
-- Foo.hs:5:1: warning: [-Wunused-top-binds] Defined but not used: type constructor or class ‘F’
-- Foo.hs:6:1: warning: [-Wunused-top-binds] Defined but not used: data constructor ‘Bar’
  | Just source <- srcOpt
  , Just [_, name] <-
      matchRegexUnifySpaces
        _message
        ".*Defined but not used: (type constructor or class |data constructor )?‘([^ ]+)’"
  , Just (exportType, _) <-
      find (matchWithDiagnostic _range . snd)
      . mapMaybe (\(L l b) -> if isTopLevel (locA l) then exportsAs b else Nothing)
      $ hsmodDecls
  , Just exports       <- fmap (fmap reLoc) . reLoc <$> hsmodExports
  , Just exportsEndPos <- _end <$> getLocatedRange exports
  , let name'          = printExport exportType name
        sep            = exportSep source $ map getLocatedRange <$> exports
        exportName     = case sep of
          Nothing -> (if needsComma source exports then ", " else "") <> name'
          Just  s -> s <> name'
        exportsEndPos' = exportsEndPos { _character = pred $ _character exportsEndPos }
        insertPos      = fromMaybe exportsEndPos' $ case (sep, unLoc exports) of
          (Just _, exports'@(_:_)) -> fmap _end . getLocatedRange $ last exports'
          _                        -> Nothing
  = Just ("Export ‘" <> name <> "’", TextEdit (Range insertPos insertPos) exportName)
  | otherwise = Nothing
  where
    exportSep :: T.Text -> Located [Maybe Range] -> Maybe T.Text
    exportSep src (L (RealSrcSpan _ _) xs@(_ : tl@(_ : _))) =
      case mapMaybe (\(e, s) -> (,) <$> e <*> s) $ zip (fmap _end <$> xs) (fmap _start <$> tl) of
        []     -> Nothing
        bounds -> Just smallestSep
          where
            smallestSep
              = snd
              $ minimumBy (comparing fst)
              $ map (T.length &&& id)
              $ nubOrd
              $ map (\(prevEnd, nextStart) -> textInRange (Range prevEnd nextStart) src) bounds
    exportSep _   _ = Nothing

    -- We get the last export and the closing bracket and check for comma in that range.
    needsComma :: T.Text -> Located [Located (IE GhcPs)] -> Bool
    needsComma _ (L _ []) = False
    needsComma source (L (RealSrcSpan l _) exports) =
      let closeParen = _end $ realSrcSpanToRange l
          lastExport = fmap _end . getLocatedRange $ last exports
      in
      case lastExport of
        Just lastExport ->
          not $ T.any (== ',') $ textInRange (Range lastExport closeParen) source
        _ -> False
    needsComma _ _ = False

    opLetter :: T.Text
    opLetter = ":!#$%&*+./<=>?@\\^|-~"

    parenthesizeIfNeeds :: Bool -> T.Text -> T.Text
    parenthesizeIfNeeds needsTypeKeyword x
      | T.any (c ==) opLetter = (if needsTypeKeyword then "type " else "") <> "(" <> x <> ")"
      | otherwise = x
      where
        c = T.head x

    matchWithDiagnostic :: Range -> Located (IdP GhcPs) -> Bool
    matchWithDiagnostic Range{_start=l,_end=r} x =
      let loc = fmap _start . getLocatedRange $ x
       in loc >= Just l && loc <= Just r

    printExport :: ExportsAs -> T.Text -> T.Text
    printExport ExportName x    = parenthesizeIfNeeds False x
    printExport ExportPattern x = "pattern " <> x
    printExport ExportFamily x  = parenthesizeIfNeeds True x
    printExport ExportAll x     = parenthesizeIfNeeds True x <> "(..)"

    isTopLevel :: SrcSpan -> Bool
    isTopLevel span = fmap (_character . _start) (srcSpanToRange span) == Just 0

    exportsAs :: HsDecl GhcPs -> Maybe (ExportsAs, Located (IdP GhcPs))
    exportsAs (ValD _ FunBind {fun_id})          = Just (ExportName, reLoc fun_id)
    exportsAs (ValD _ (PatSynBind _ PSB {psb_id})) = Just (ExportPattern, reLoc psb_id)
    exportsAs (TyClD _ SynDecl{tcdLName})      = Just (ExportName, reLoc tcdLName)
    exportsAs (TyClD _ DataDecl{tcdLName})     = Just (ExportAll, reLoc tcdLName)
    exportsAs (TyClD _ ClassDecl{tcdLName})    = Just (ExportAll, reLoc tcdLName)
    exportsAs (TyClD _ FamDecl{tcdFam})        = Just (ExportFamily, reLoc $ fdLName tcdFam)
    exportsAs _                                = Nothing

suggestAddTypeAnnotationToSatisfyConstraints :: Maybe T.Text -> Diagnostic -> [(T.Text, [TextEdit])]
suggestAddTypeAnnotationToSatisfyConstraints sourceOpt Diagnostic{_range=_range,..}
-- File.hs:52:41: warning:
--     * Defaulting the following constraint to type ‘Integer’
--        Num p0 arising from the literal ‘1’
--     * In the expression: 1
--       In an equation for ‘f’: f = 1
-- File.hs:52:41: warning:
--     * Defaulting the following constraints to type ‘[Char]’
--        (Show a0)
--          arising from a use of ‘traceShow’
--          at A.hs:228:7-25
--        (IsString a0)
--          arising from the literal ‘"debug"’
--          at A.hs:228:17-23
--     * In the expression: traceShow "debug" a
--       In an equation for ‘f’: f a = traceShow "debug" a
-- File.hs:52:41: warning:
--     * Defaulting the following constraints to type ‘[Char]’
--         (Show a0)
--          arising from a use of ‘traceShow’
--          at A.hs:255:28-43
--        (IsString a0)
--          arising from the literal ‘"test"’
--          at /Users/serhiip/workspace/ghcide/src/Development/IDE/Plugin/CodeAction.hs:255:38-43
--     * In the fourth argument of ‘seq’, namely ‘(traceShow "test")’
--       In the expression: seq "test" seq "test" (traceShow "test")
--       In an equation for ‘f’:
--          f = seq "test" seq "test" (traceShow "test")
    | Just [ty, lit] <- matchRegexUnifySpaces _message (pat False False True False)
                        <|> matchRegexUnifySpaces _message (pat False False False True)
                        <|> matchRegexUnifySpaces _message (pat False False False False)
            = codeEdit ty lit (makeAnnotatedLit ty lit)
    | Just source <- sourceOpt
    , Just [ty, lit] <- matchRegexUnifySpaces _message (pat True True False False)
            = let lit' = makeAnnotatedLit ty lit;
                  tir = textInRange _range source
              in codeEdit ty lit (T.replace lit lit' tir)
    | otherwise = []
    where
      makeAnnotatedLit ty lit = "(" <> lit <> " :: " <> ty <> ")"
      pat multiple at inArg inExpr = T.concat [ ".*Defaulting the following constraint"
                                       , if multiple then "s" else ""
                                       , " to type ‘([^ ]+)’ "
                                       , ".*arising from the literal ‘(.+)’"
                                       , if inArg then ".+In the.+argument" else ""
                                       , if at then ".+at" else ""
                                       , if inExpr then ".+In the expression" else ""
                                       , ".+In the expression"
                                       ]
      codeEdit ty lit replacement =
        let title = "Add type annotation ‘" <> ty <> "’ to ‘" <> lit <> "’"
            edits = [TextEdit _range replacement]
        in  [( title, edits )]


-- | GHC strips out backticks in case of infix functions as well as single quote
--   in case of quoted name when using TemplateHaskellQuotes. Which is not desired.
--
-- For example:
-- 1.
--
-- @
-- File.hs:52:41: error:
--     * Variable not in scope:
--         suggestAcion :: Maybe T.Text -> Range -> Range
--     * Perhaps you meant ‘suggestAction’ (line 83)
-- File.hs:94:37: error:
--     Not in scope: ‘T.isPrfixOf’
--     Perhaps you meant one of these:
--       ‘T.isPrefixOf’ (imported from Data.Text),
--       ‘T.isInfixOf’ (imported from Data.Text),
--       ‘T.isSuffixOf’ (imported from Data.Text)
--     Module ‘Data.Text’ does not export ‘isPrfixOf’.
-- @
--
-- * action: \`suggestAcion\` will be renamed to \`suggestAction\` keeping back ticks around the function
--
-- 2.
--
-- @
-- import Language.Haskell.TH (Name)
-- foo :: Name
-- foo = 'bread
--
-- File.hs:8:7: error:
--     Not in scope: ‘bread’
--       * Perhaps you meant one of these:
--         ‘break’ (imported from Prelude), ‘read’ (imported from Prelude)
--       * In the Template Haskell quotation 'bread
-- @
--
-- * action: 'bread will be renamed to 'break keeping single quote on beginning of name
suggestReplaceIdentifier :: Maybe T.Text -> Diagnostic -> [(T.Text, [TextEdit])]
suggestReplaceIdentifier contents Diagnostic{_range=_range,..}
    | renameSuggestions@(_:_) <- extractRenamableTerms _message
        = [ ("Replace with ‘" <> name <> "’", [mkRenameEdit contents _range name]) | name <- renameSuggestions ]
    | otherwise = []

matchVariableNotInScope :: T.Text -> Maybe (T.Text, Maybe T.Text)
matchVariableNotInScope message
  --     * Variable not in scope:
  --         suggestAcion :: Maybe T.Text -> Range -> Range
  --     * Variable not in scope:
  --         suggestAcion
  | Just (name, typ) <- matchVariableNotInScopeTyped message = Just (name, Just typ)
  | Just name <- matchVariableNotInScopeUntyped message = Just (name, Nothing)
  | otherwise = Nothing
  where
    matchVariableNotInScopeTyped message
      | Just [name, typ] <- matchRegexUnifySpaces message "Variable not in scope: ([^ ]+) :: ([^*•]+)" =
          Just (name, typ)
      | otherwise = Nothing
    matchVariableNotInScopeUntyped message
      | Just [name] <- matchRegexUnifySpaces message "Variable not in scope: ([^ ]+)" =
          Just name
      | otherwise = Nothing

matchFoundHole :: T.Text -> Maybe (T.Text, T.Text)
matchFoundHole message
  | Just [name, typ] <- matchRegexUnifySpaces message "Found hole: _([^ ]+) :: ([^*•]+) Or perhaps" =
      Just (name, typ)
  | otherwise = Nothing

matchFoundHoleIncludeUnderscore :: T.Text -> Maybe (T.Text, T.Text)
matchFoundHoleIncludeUnderscore message = first ("_" <>) <$> matchFoundHole message

suggestNewDefinition :: IdeOptions -> ParsedModule -> Maybe T.Text -> Diagnostic -> [(T.Text, [TextEdit])]
suggestNewDefinition ideOptions parsedModule contents Diagnostic {_message, _range}
  | Just (name, typ) <- matchVariableNotInScope message =
      newDefinitionAction ideOptions parsedModule _range name typ
  | Just (name, typ) <- matchFoundHole message,
    [(label, newDefinitionEdits)] <- newDefinitionAction ideOptions parsedModule _range name (Just typ) =
      [(label, mkRenameEdit contents _range name : newDefinitionEdits)]
  | otherwise = []
  where
    message = unifySpaces _message

newDefinitionAction :: IdeOptions -> ParsedModule -> Range -> T.Text -> Maybe T.Text -> [(T.Text, [TextEdit])]
newDefinitionAction IdeOptions {..} parsedModule Range {_start} name typ
  | Range _ lastLineP : _ <-
      [ realSrcSpanToRange sp
        | (L (locA -> l@(RealSrcSpan sp _)) _) <- hsmodDecls,
          _start `isInsideSrcSpan` l
      ],
    nextLineP <- Position {_line = _line lastLineP + 1, _character = 0} =
      [ ( "Define " <> sig,
          [TextEdit (Range nextLineP nextLineP) (T.unlines ["", sig, name <> " = _"])]
        )
      ]
  | otherwise = []
  where
    colon = if optNewColonConvention then " : " else " :: "
    sig = name <> colon <> T.dropWhileEnd isSpace (fromMaybe "_" typ)
    ParsedModule {pm_parsed_source = L _ HsModule {hsmodDecls}} = parsedModule

#if MIN_VERSION_ghc(9,2,1)
-- When GHC tells us that a variable is not bound, it will tell us either:
--  - there is an unbound variable with a given type
--  - there is an unbound variable (GHC provides no type suggestion)
--
-- When we receive either of these errors, we produce a text edit that will add a new argument (as a new pattern in the
-- last position of each LHS of the top-level bindings for this HsDecl).
--
-- NOTE When adding a new argument to a declaration, the corresponding argument's type in declaration's signature might
--      not be the last type in the signature, such as:
--         foo :: a -> b -> c -> d
--         foo a b = \c -> ...
--      In this case a new argument would have to add its type between b and c in the signature.
suggestAddArgument :: ParsedModule -> Diagnostic -> Either ResponseError [(T.Text, [TextEdit])]
suggestAddArgument parsedModule Diagnostic {_message, _range}
  | Just (name, typ) <- matchVariableNotInScope message = addArgumentAction parsedModule _range name typ
  | Just (name, typ) <- matchFoundHoleIncludeUnderscore message = addArgumentAction parsedModule _range name (Just typ)
  | otherwise = pure []
  where
    message = unifySpaces _message

-- Given a name for the new binding, add a new pattern to the match in the last position,
-- returning how many patterns there were in this match prior to the transformation:
--      addArgToMatch "foo" `bar arg1 arg2 = ...`
--   => (`bar arg1 arg2 foo = ...`, 2)
addArgToMatch :: T.Text -> GenLocated l (Match GhcPs body) -> (GenLocated l (Match GhcPs body), Int)
addArgToMatch name (L locMatch (Match xMatch ctxMatch pats rhs)) =
  let unqualName = mkRdrUnqual $ mkVarOcc $ T.unpack name
      newPat = L (noAnnSrcSpanDP1 generatedSrcSpan) $ VarPat NoExtField (noLocA unqualName)
  in (L locMatch (Match xMatch ctxMatch (pats <> [newPat]) rhs), length pats)

-- Attempt to insert a binding pattern into each match for the given LHsDecl; succeeds only if the function is a FunBind.
-- Also return:
--   - the declaration's name
--   - the number of bound patterns in the declaration's matches prior to the transformation
--
-- For example:
--    insertArg "new_pat" `foo bar baz = 1`
-- => (`foo bar baz new_pat = 1`, Just ("foo", 2))
appendFinalPatToMatches :: T.Text -> LHsDecl GhcPs -> TransformT (Either ResponseError) (LHsDecl GhcPs, Maybe (GenLocated SrcSpanAnnN RdrName, Int))
appendFinalPatToMatches name = \case
  (L locDecl (ValD xVal (FunBind xFunBind idFunBind mg coreFunBind))) -> do
    (mg', numPatsMay) <- modifyMgMatchesT' mg (pure . second Just . addArgToMatch name) Nothing combineMatchNumPats
    numPats <- lift $ maybeToEither (responseError "Unexpected empty match group in HsDecl") numPatsMay
    let decl' = L locDecl (ValD xVal (FunBind xFunBind idFunBind mg' coreFunBind))
    pure (decl', Just (idFunBind, numPats))
  decl -> pure (decl, Nothing)
  where
    combineMatchNumPats  Nothing other = pure other
    combineMatchNumPats  other Nothing = pure other
    combineMatchNumPats  (Just l) (Just r)
      | l == r = pure (Just l)
      | otherwise = Left $ responseError "Unexpected different numbers of patterns in HsDecl MatchGroup"

-- The add argument works as follows:
--  1. Attempt to add the given name as the last pattern of the declaration that contains `range`.
--  2. If such a declaration exists, use that declaration's name to modify the signature of said declaration, if it
--     has a type signature.
--
-- NOTE For the following situation, the type signature is not updated (it's unclear what should happen):
--   type FunctionTySyn = () -> Int
--   foo :: FunctionTySyn
--   foo () = new_def
--
-- TODO instead of inserting a typed hole; use GHC's suggested type from the error
addArgumentAction :: ParsedModule -> Range -> T.Text -> Maybe T.Text -> Either ResponseError [(T.Text, [TextEdit])]
addArgumentAction (ParsedModule _ moduleSrc _ _) range name _typ = do
    (newSource, _, _) <- runTransformT $ do
      (moduleSrc', join -> matchedDeclNameMay) <- addNameAsLastArgOfMatchingDecl (makeDeltaAst moduleSrc)
      case matchedDeclNameMay of
          Just (matchedDeclName, numPats) -> modifySigWithM (unLoc matchedDeclName) (addTyHoleToTySigArg numPats) moduleSrc'
          Nothing -> pure moduleSrc'
    let diff = makeDiffTextEdit (T.pack $ exactPrint moduleSrc) (T.pack $ exactPrint newSource)
    pure [("Add argument ‘" <> name <> "’ to function", fromLspList diff)]
  where
    addNameAsLastArgOfMatchingDecl = modifySmallestDeclWithM spanContainsRangeOrErr addNameAsLastArg
    addNameAsLastArg = fmap (first (:[])) . appendFinalPatToMatches name

    spanContainsRangeOrErr = maybeToEither (responseError "SrcSpan was not valid range") . (`spanContainsRange` range)

-- Transform an LHsType into a list of arguments and return type, to make transformations easier.
hsTypeToFunTypeAsList :: LHsType GhcPs -> ([(SrcSpanAnnA, XFunTy GhcPs, HsArrow GhcPs, LHsType GhcPs)], LHsType GhcPs)
hsTypeToFunTypeAsList = \case
  L spanAnnA (HsFunTy xFunTy arrow lhs rhs) ->
    let (rhsArgs, rhsRes) = hsTypeToFunTypeAsList rhs
    in ((spanAnnA, xFunTy, arrow, lhs):rhsArgs, rhsRes)
  ty -> ([], ty)

-- The inverse of `hsTypeToFunTypeAsList`
hsTypeFromFunTypeAsList :: ([(SrcSpanAnnA, XFunTy GhcPs, HsArrow GhcPs, LHsType GhcPs)], LHsType GhcPs) -> LHsType GhcPs
hsTypeFromFunTypeAsList (args, res) =
  foldr (\(spanAnnA, xFunTy, arrow, argTy) res -> L spanAnnA $ HsFunTy xFunTy arrow argTy res) res args

-- Add a typed hole to a type signature in the given argument position:
--   0 `foo :: ()` => foo :: _ -> ()
--   2 `foo :: FunctionTySyn` => foo :: FunctionTySyn
--   1 `foo :: () -> () -> Int` => foo :: () -> _ -> () -> Int
addTyHoleToTySigArg :: Int -> LHsSigType GhcPs -> (LHsSigType GhcPs)
addTyHoleToTySigArg loc (L annHsSig (HsSig xHsSig tyVarBndrs lsigTy)) =
    let (args, res) = hsTypeToFunTypeAsList lsigTy
        wildCardAnn = SrcSpanAnn (EpAnn genAnchor1 (AnnListItem [AddRarrowAnn d1]) emptyComments) generatedSrcSpan
        newArg = (SrcSpanAnn mempty generatedSrcSpan, noAnn, HsUnrestrictedArrow NormalSyntax, L wildCardAnn $ HsWildCardTy noExtField)
        -- NOTE if the location that the argument wants to be placed at is not one more than the number of arguments
        --      in the signature, then we return the original type signature.
        --      This situation most likely occurs due to a function type synonym in the signature
        insertArg n _ | n < 0 = error "Not possible"
        insertArg 0 as = newArg:as
        insertArg _ [] = []
        insertArg n (a:as) = a : insertArg (n - 1) as
        lsigTy' = hsTypeFromFunTypeAsList (insertArg loc args, res)
    in L annHsSig (HsSig xHsSig tyVarBndrs lsigTy')

fromLspList :: List a -> [a]
fromLspList (List a) = a
#endif

suggestFillTypeWildcard :: Diagnostic -> [(T.Text, TextEdit)]
suggestFillTypeWildcard Diagnostic{_range=_range,..}
-- Foo.hs:3:8: error:
--     * Found type wildcard `_' standing for `p -> p1 -> p'

    | "Found type wildcard" `T.isInfixOf` _message
    , " standing for " `T.isInfixOf` _message
    , typeSignature <- extractWildCardTypeSignature _message
        =  [("Use type signature: ‘" <> typeSignature <> "’", TextEdit _range typeSignature)]
    | otherwise = []

{- Handles two variants with different formatting

1. Could not find module ‘Data.Cha’
   Perhaps you meant Data.Char (from base-4.12.0.0)

2. Could not find module ‘Data.I’
   Perhaps you meant
      Data.Ix (from base-4.14.3.0)
      Data.Eq (from base-4.14.3.0)
      Data.Int (from base-4.14.3.0)
-}
suggestModuleTypo :: Diagnostic -> [(T.Text, TextEdit)]
suggestModuleTypo Diagnostic{_range=_range,..}
    | "Could not find module" `T.isInfixOf` _message =
      case T.splitOn "Perhaps you meant" _message of
          [_, stuff] ->
              [ ("replace with " <> modul, TextEdit _range modul)
              | modul <- mapMaybe extractModule (T.lines stuff)
              ]
          _ -> []
    | otherwise = []
  where
    extractModule line = case T.words line of
        [modul, "(from", _] -> Just modul
        _                   -> Nothing


suggestFillHole :: Diagnostic -> [(T.Text, TextEdit)]
suggestFillHole Diagnostic{_range=_range,..}
    | Just holeName <- extractHoleName _message
    , (holeFits, refFits) <- processHoleSuggestions (T.lines _message) =
      let isInfixHole = _message =~ addBackticks holeName :: Bool in
        map (proposeHoleFit holeName False isInfixHole) holeFits
        ++ map (proposeHoleFit holeName True isInfixHole) refFits
    | otherwise = []
    where
      extractHoleName = fmap head . flip matchRegexUnifySpaces "Found hole: ([^ ]*)"
      addBackticks text = "`" <> text <> "`"
      addParens text = "(" <> text <> ")"
      proposeHoleFit holeName parenthise isInfixHole name =
        let isInfixOperator = T.head name == '('
            name' = getOperatorNotation isInfixHole isInfixOperator name in
          ( "replace " <> holeName <> " with " <> name
          , TextEdit _range (if parenthise then addParens name' else name')
          )
      getOperatorNotation True False name                    = addBackticks name
      getOperatorNotation True True name                     = T.drop 1 (T.dropEnd 1 name)
      getOperatorNotation _isInfixHole _isInfixOperator name = name

processHoleSuggestions :: [T.Text] -> ([T.Text], [T.Text])
processHoleSuggestions mm = (holeSuggestions, refSuggestions)
{-
    • Found hole: _ :: LSP.Handlers

      Valid hole fits include def
      Valid refinement hole fits include
        fromMaybe (_ :: LSP.Handlers) (_ :: Maybe LSP.Handlers)
        fromJust (_ :: Maybe LSP.Handlers)
        haskell-lsp-types-0.22.0.0:Language.LSP.Types.Window.$sel:_value:ProgressParams (_ :: ProgressParams
                                                                                                        LSP.Handlers)
        T.foldl (_ :: LSP.Handlers -> Char -> LSP.Handlers)
                (_ :: LSP.Handlers)
                (_ :: T.Text)
        T.foldl' (_ :: LSP.Handlers -> Char -> LSP.Handlers)
                 (_ :: LSP.Handlers)
                 (_ :: T.Text)
-}
  where
    t = id @T.Text
    holeSuggestions = do
      -- get the text indented under Valid hole fits
      validHolesSection <-
        getIndentedGroupsBy (=~ t " *Valid (hole fits|substitutions) include") mm
      -- the Valid hole fits line can contain a hole fit
      holeFitLine <-
        mapHead
            (mrAfter . (=~ t " *Valid (hole fits|substitutions) include"))
            validHolesSection
      let holeFit = T.strip $ T.takeWhile (/= ':') holeFitLine
      guard (not $ T.null holeFit)
      return holeFit
    refSuggestions = do -- @[]
      -- get the text indented under Valid refinement hole fits
      refinementSection <-
        getIndentedGroupsBy (=~ t " *Valid refinement hole fits include") mm
      -- get the text for each hole fit
      holeFitLines <- getIndentedGroups (tail refinementSection)
      let holeFit = T.strip $ T.unwords holeFitLines
      guard $ not $ holeFit =~ t "Some refinement hole fits suppressed"
      return holeFit

    mapHead f (a:aa) = f a : aa
    mapHead _ []     = []

-- > getIndentedGroups [" H1", "  l1", "  l2", " H2", "  l3"] = [[" H1,", "  l1", "  l2"], [" H2", "  l3"]]
getIndentedGroups :: [T.Text] -> [[T.Text]]
getIndentedGroups [] = []
getIndentedGroups ll@(l:_) = getIndentedGroupsBy ((== indentation l) . indentation) ll
-- |
-- > getIndentedGroupsBy (" H" `isPrefixOf`) [" H1", "  l1", "  l2", " H2", "  l3"] = [[" H1", "  l1", "  l2"], [" H2", "  l3"]]
getIndentedGroupsBy :: (T.Text -> Bool) -> [T.Text] -> [[T.Text]]
getIndentedGroupsBy pred inp = case dropWhile (not.pred) inp of
    (l:ll) -> case span (\l' -> indentation l < indentation l') ll of
        (indented, rest) -> (l:indented) : getIndentedGroupsBy pred rest
    _ -> []

indentation :: T.Text -> Int
indentation = T.length . T.takeWhile isSpace

#if !MIN_VERSION_ghc(9,3,0)
suggestExtendImport :: ExportsMap -> ParsedSource -> Diagnostic -> [(T.Text, CodeActionKind, Rewrite)]
suggestExtendImport exportsMap (L _ HsModule {hsmodImports}) Diagnostic{_range=_range,..}
    | Just [binding, mod, srcspan] <-
      matchRegexUnifySpaces _message
      "Perhaps you want to add ‘([^’]*)’ to the import list in the import of ‘([^’]*)’ *\\((.*)\\).$"
    = suggestions hsmodImports binding mod srcspan
    | Just (binding, mod_srcspan) <-
      matchRegExMultipleImports _message
    = mod_srcspan >>= uncurry (suggestions hsmodImports binding)
    | otherwise = []
    where
        canUseDatacon = case extractNotInScopeName _message of
                            Just NotInScopeTypeConstructorOrClass{} -> False
                            _                                       -> True

        suggestions decls binding mod srcspan
          | range <- case [ x | (x,"") <- readSrcSpan (T.unpack srcspan)] of
                [s] -> let x = realSrcSpanToRange s
                   in x{_end = (_end x){_character = succ (_character (_end x))}}
                _ -> error "bug in srcspan parser",
            Just decl <- findImportDeclByRange decls range,
            Just ident <- lookupExportMap binding mod
          = [ ( "Add " <> renderImportStyle importStyle <> " to the import list of " <> mod
              , quickFixImportKind' "extend" importStyle
              , uncurry extendImport (unImportStyle importStyle) decl
              )
            | importStyle <- NE.toList $ importStyles ident
            ]
          | otherwise = []
        lookupExportMap binding mod
          | Just match <- Map.lookup binding (getExportsMap exportsMap)
          -- Only for the situation that data constructor name is same as type constructor name,
          -- let ident with parent be in front of the one without.
          , sortedMatch <- sortBy (\ident1 ident2 -> parent ident2 `compare` parent ident1) (Set.toList match)
          , idents <- filter (\ident -> moduleNameText ident == mod && (canUseDatacon || not (isDatacon ident))) sortedMatch
          , (not . null) idents -- Ensure fallback while `idents` is empty
          , ident <- head idents
          = Just ident

            -- fallback to using GHC suggestion even though it is not always correct
          | otherwise
          = Just IdentInfo
                { name = mkVarOcc $ T.unpack binding
                , rendered = binding
                , parent = Nothing
                , isDatacon = False
                , moduleNameText = mod}
#endif

data HidingMode
    = HideOthers [ModuleTarget]
    | ToQualified
        Bool
        -- ^ Parenthesised?
        ModuleName

data ModuleTarget
    = ExistingImp (NonEmpty (LImportDecl GhcPs))
    | ImplicitPrelude [LImportDecl GhcPs]

targetImports :: ModuleTarget -> [LImportDecl GhcPs]
targetImports (ExistingImp ne)     = NE.toList ne
targetImports (ImplicitPrelude xs) = xs

oneAndOthers :: [a] -> [(a, [a])]
oneAndOthers = go
    where
        go []       = []
        go (x : xs) = (x, xs) : map (second (x :)) (go xs)

isPreludeImplicit :: DynFlags -> Bool
isPreludeImplicit = xopt Lang.ImplicitPrelude

#if !MIN_VERSION_ghc(9,3,0)
-- | Suggests disambiguation for ambiguous symbols.
suggestImportDisambiguation ::
    DynFlags ->
    Maybe T.Text ->
    Annotated ParsedSource ->
    T.Text ->
    Diagnostic ->
    [(T.Text, [Either TextEdit Rewrite])]
suggestImportDisambiguation df (Just txt) ps fileContents diag@Diagnostic {..}
    | Just [ambiguous] <-
        matchRegexUnifySpaces
            _message
            "Ambiguous occurrence ‘([^’]+)’"
      , Just modules <-
            map last
                <$> allMatchRegexUnifySpaces _message "imported from ‘([^’]+)’"
      , local <- matchRegexUnifySpaces _message "defined at .+:[0-9]+:[0-9]+" =
        suggestions ambiguous modules (isJust local)
    | otherwise = []
    where
        L _ HsModule {hsmodImports} = astA ps

        locDic =
            fmap (NE.fromList . DL.toList) $
            Map.fromListWith (<>) $
                map
                    ( \i@(L _ idecl) ->
                        ( T.pack $ moduleNameString $ unLoc $ ideclName idecl
                        , DL.singleton i
                        )
                    )
                    hsmodImports
        toModuleTarget "Prelude"
            | isPreludeImplicit df
             = Just $ ImplicitPrelude $
                maybe [] NE.toList (Map.lookup "Prelude" locDic)
        toModuleTarget mName = ExistingImp <$> Map.lookup mName locDic
        parensed =
            "(" `T.isPrefixOf` T.strip (textInRange _range txt)
        -- > removeAllDuplicates [1, 1, 2, 3, 2] = [3]
        removeAllDuplicates = map head . filter ((==1) <$> length) . group . sort
        hasDuplicate xs = length xs /= length (S.fromList xs)
        suggestions symbol mods local
          | hasDuplicate mods = case mapM toModuleTarget (removeAllDuplicates mods) of
                                  Just targets -> suggestionsImpl symbol (map (, []) targets) local
                                  Nothing      -> []
          | otherwise         = case mapM toModuleTarget mods of
                                  Just targets -> suggestionsImpl symbol (oneAndOthers targets) local
                                  Nothing      -> []
        suggestionsImpl symbol targetsWithRestImports local =
            sortOn fst
            [ ( renderUniquify mode modNameText symbol False
              , disambiguateSymbol ps fileContents diag symbol mode
              )
            | (modTarget, restImports) <- targetsWithRestImports
            , let modName = targetModuleName modTarget
                  modNameText = T.pack $ moduleNameString modName
            , mode <-
                [ ToQualified parensed qual
                | ExistingImp imps <- [modTarget]
#if MIN_VERSION_ghc(9,0,0)
                {- HLINT ignore suggestImportDisambiguation "Use nubOrd" -}
                -- TODO: The use of nub here is slow and maybe wrong for UnhelpfulLocation
                -- nubOrd can't be used since SrcSpan is intentionally no Ord
                , L _ qual <- nub $ mapMaybe (ideclAs . unLoc)
#else
                , L _ qual <- nubOrd $ mapMaybe (ideclAs . unLoc)
#endif
                    $ NE.toList imps
                ]
                ++ [ToQualified parensed modName
                    | any (occursUnqualified symbol . unLoc)
                        (targetImports modTarget)
                    || case modTarget of
                        ImplicitPrelude{} -> True
                        _                 -> False
                    ]
                ++ [HideOthers restImports | not (null restImports)]
            ] ++ [ ( renderUniquify mode T.empty symbol True
              , disambiguateSymbol ps fileContents diag symbol mode
              ) | local, not (null targetsWithRestImports)
                , let mode = HideOthers (uncurry (:) (head targetsWithRestImports))
            ]
        renderUniquify HideOthers {} modName symbol local =
            "Use " <> (if local then "local definition" else modName) <> " for " <> symbol <> ", hiding other imports"
        renderUniquify (ToQualified _ qual) _ symbol _ =
            "Replace with qualified: "
                <> T.pack (moduleNameString qual)
                <> "."
                <> symbol
suggestImportDisambiguation _ _ _ _ _ = []
#endif

occursUnqualified :: T.Text -> ImportDecl GhcPs -> Bool
occursUnqualified symbol ImportDecl{..}
    | isNothing ideclAs = Just False /=
            -- I don't find this particularly comprehensible,
            -- but HLint suggested me to do so...
        (ideclHiding <&> \(isHiding, L _ ents) ->
            let occurs = any ((symbol `symbolOccursIn`) . unLoc) ents
            in isHiding && not occurs || not isHiding && occurs
        )
occursUnqualified _ _ = False

symbolOccursIn :: T.Text -> IE GhcPs -> Bool
symbolOccursIn symb = any ((== symb). printOutputable) . ieNames

targetModuleName :: ModuleTarget -> ModuleName
targetModuleName ImplicitPrelude{} = mkModuleName "Prelude"
targetModuleName (ExistingImp (L _ ImportDecl{..} :| _)) =
    unLoc ideclName
targetModuleName (ExistingImp _) =
    error "Cannot happen!"

#if !MIN_VERSION_ghc(9,3,0)
disambiguateSymbol ::
    Annotated ParsedSource ->
    T.Text ->
    Diagnostic ->
    T.Text ->
    HidingMode ->
    [Either TextEdit Rewrite]
disambiguateSymbol ps fileContents Diagnostic {..} (T.unpack -> symbol) = \case
    (HideOthers hiddens0) ->
        [ Right $ hideSymbol symbol idecl
        | ExistingImp idecls <- hiddens0
        , idecl <- NE.toList idecls
        ]
            ++ mconcat
                [ if null imps
                    then maybeToList $ Left . snd <$> newImportToEdit (hideImplicitPreludeSymbol $ T.pack symbol) ps fileContents
                    else Right . hideSymbol symbol <$> imps
                | ImplicitPrelude imps <- hiddens0
                ]
    (ToQualified parensed qualMod) ->
        let occSym = mkVarOcc symbol
            rdr = Qual qualMod occSym
         in Right <$> [ if parensed
                then Rewrite (rangeToSrcSpan "<dummy>" _range) $ \df ->
                    liftParseAST @(HsExpr GhcPs) df $
                    T.unpack $ printOutputable $
                        HsVar @GhcPs noExtField $
                            reLocA $ L (mkGeneralSrcSpan  "") rdr
                else Rewrite (rangeToSrcSpan "<dummy>" _range) $ \df ->
                    liftParseAST @RdrName df $
                    T.unpack $ printOutputable $ L (mkGeneralSrcSpan  "") rdr
            ]
#endif

findImportDeclByRange :: [LImportDecl GhcPs] -> Range -> Maybe (LImportDecl GhcPs)
findImportDeclByRange xs range = find (\(L (locA -> l) _)-> srcSpanToRange l == Just range) xs

suggestFixConstructorImport :: Diagnostic -> [(T.Text, TextEdit)]
suggestFixConstructorImport Diagnostic{_range=_range,..}
    -- ‘Success’ is a data constructor of ‘Result’
    -- To import it use
    -- import Data.Aeson.Types( Result( Success ) )
    -- or
    -- import Data.Aeson.Types( Result(..) ) (lsp-ui)
  | Just [constructor, typ] <-
    matchRegexUnifySpaces _message
    "‘([^’]*)’ is a data constructor of ‘([^’]*)’ To import it use"
  = let fixedImport = typ <> "(" <> constructor <> ")"
    in [("Fix import of " <> fixedImport, TextEdit _range fixedImport)]
  | otherwise = []

#if !MIN_VERSION_ghc(9,3,0)
-- | Suggests a constraint for a declaration for which a constraint is missing.
suggestConstraint :: DynFlags -> ParsedSource -> Diagnostic -> [(T.Text, Rewrite)]
suggestConstraint df (makeDeltaAst -> parsedModule) diag@Diagnostic {..}
  | Just missingConstraint <- findMissingConstraint _message
  = let codeAction = if _message =~ ("the type signature for:" :: String)
                        then suggestFunctionConstraint df parsedModule
                        else suggestInstanceConstraint df parsedModule
     in codeAction diag missingConstraint
  | otherwise = []
    where
      findMissingConstraint :: T.Text -> Maybe T.Text
      findMissingConstraint t =
        let regex = "(No instance for|Could not deduce) \\((.+)\\) arising from" -- a use of / a do statement
            regexImplicitParams = "Could not deduce: (\\?.+) arising from a use of"
            match = matchRegexUnifySpaces t regex
            matchImplicitParams = matchRegexUnifySpaces t regexImplicitParams
        in match <|> matchImplicitParams <&> last

-- | Suggests a constraint for an instance declaration for which a constraint is missing.
suggestInstanceConstraint :: DynFlags -> ParsedSource -> Diagnostic -> T.Text -> [(T.Text, Rewrite)]

suggestInstanceConstraint df (L _ HsModule {hsmodDecls}) Diagnostic {..} missingConstraint
  | Just instHead <- instanceHead
  = [(actionTitle missingConstraint , appendConstraint (T.unpack missingConstraint) instHead)]
  | otherwise = []
    where
      instanceHead
        -- Suggests a constraint for an instance declaration with no existing constraints.
        -- • No instance for (Eq a) arising from a use of ‘==’
        --   Possible fix: add (Eq a) to the context of the instance declaration
        -- • In the expression: x == y
        --   In an equation for ‘==’: (Wrap x) == (Wrap y) = x == y
        --   In the instance declaration for ‘Eq (Wrap a)’
        | Just [instanceDeclaration] <- matchRegexUnifySpaces _message "In the instance declaration for ‘([^`]*)’"
        , Just instHead <- findInstanceHead df (T.unpack instanceDeclaration) hsmodDecls
        = Just instHead
        -- Suggests a constraint for an instance declaration with one or more existing constraints.
        -- • Could not deduce (Eq b) arising from a use of ‘==’
        --   from the context: Eq a
        --     bound by the instance declaration at /path/to/Main.hs:7:10-32
        --   Possible fix: add (Eq b) to the context of the instance declaration
        -- • In the second argument of ‘(&&)’, namely ‘x' == y'’
        --   In the expression: x == y && x' == y'
        --   In an equation for ‘==’:
        --       (Pair x x') == (Pair y y') = x == y && x' == y'
        | Just [instanceLineStr, constraintFirstCharStr]
            <- matchRegexUnifySpaces _message "bound by the instance declaration at .+:([0-9]+):([0-9]+)"
#if !MIN_VERSION_ghc(9,2,0)
        , Just (L _ (InstD _ (ClsInstD _ ClsInstDecl {cid_poly_ty = HsIB{hsib_body}})))
#else
        , Just (L _ (InstD _ (ClsInstD _ ClsInstDecl {cid_poly_ty = (unLoc -> HsSig{sig_body = hsib_body})})))
#endif
            <- findDeclContainingLoc (Position (readPositionNumber instanceLineStr) (readPositionNumber constraintFirstCharStr)) hsmodDecls
        = Just hsib_body
        | otherwise
        = Nothing

      readPositionNumber :: T.Text -> UInt
      readPositionNumber = T.unpack >>> read @Integer >>> fromIntegral

      actionTitle :: T.Text -> T.Text
      actionTitle constraint = "Add `" <> constraint
        <> "` to the context of the instance declaration"

suggestImplicitParameter ::
  ParsedSource ->
  Diagnostic ->
  [(T.Text, Rewrite)]
suggestImplicitParameter (L _ HsModule {hsmodDecls}) Diagnostic {_message, _range}
  | Just [implicitT] <- matchRegexUnifySpaces _message "Unbound implicit parameter \\(([^:]+::.+)\\) arising",
    Just (L _ (ValD _ FunBind {fun_id = L _ funId})) <- findDeclContainingLoc (_start _range) hsmodDecls,
#if !MIN_VERSION_ghc(9,2,0)
    Just (TypeSig _ _ HsWC {hswc_body = HsIB {hsib_body}})
#else
    Just (TypeSig _ _ HsWC {hswc_body = (unLoc -> HsSig {sig_body = hsib_body})})
#endif
      <- findSigOfDecl (== funId) hsmodDecls
    =
      [( "Add " <> implicitT <> " to the context of " <> T.pack (printRdrName funId)
        , appendConstraint (T.unpack implicitT) hsib_body)]
  | otherwise = []
#endif

findTypeSignatureName :: T.Text -> Maybe T.Text
findTypeSignatureName t = matchRegexUnifySpaces t "([^ ]+) :: " <&> head

#if !MIN_VERSION_ghc(9,3,0)
-- | Suggests a constraint for a type signature with any number of existing constraints.
suggestFunctionConstraint :: DynFlags -> ParsedSource -> Diagnostic -> T.Text -> [(T.Text, Rewrite)]

suggestFunctionConstraint df (L _ HsModule {hsmodDecls}) Diagnostic {..} missingConstraint
-- • No instance for (Eq a) arising from a use of ‘==’
--   Possible fix:
--     add (Eq a) to the context of
--       the type signature for:
--         eq :: forall a. a -> a -> Bool
-- • In the expression: x == y
--   In an equation for ‘eq’: eq x y = x == y

-- • Could not deduce (Eq b) arising from a use of ‘==’
--   from the context: Eq a
--     bound by the type signature for:
--                eq :: forall a b. Eq a => Pair a b -> Pair a b -> Bool
--     at Main.hs:5:1-42
--   Possible fix:
--     add (Eq b) to the context of
--       the type signature for:
--         eq :: forall a b. Eq a => Pair a b -> Pair a b -> Bool
-- • In the second argument of ‘(&&)’, namely ‘y == y'’
--   In the expression: x == x' && y == y'
--   In an equation for ‘eq’:
--       eq (Pair x y) (Pair x' y') = x == x' && y == y'
  | Just typeSignatureName <- findTypeSignatureName _message
#if !MIN_VERSION_ghc(9,2,0)
  , Just (TypeSig _ _ HsWC{hswc_body = HsIB {hsib_body = sig}})
#else
  , Just (TypeSig _ _ HsWC{hswc_body = (unLoc -> HsSig {sig_body = sig})})
#endif
    <- findSigOfDecl ((T.unpack typeSignatureName ==) . showSDoc df . ppr) hsmodDecls
  , title <- actionTitle missingConstraint typeSignatureName
  = [(title, appendConstraint (T.unpack missingConstraint) sig)]
  | otherwise
  = []
    where
      actionTitle :: T.Text -> T.Text -> T.Text
      actionTitle constraint typeSignatureName = "Add `" <> constraint
        <> "` to the context of the type signature for `" <> typeSignatureName <> "`"

-- | Suggests the removal of a redundant constraint for a type signature.
removeRedundantConstraints :: DynFlags -> ParsedSource -> Diagnostic -> [(T.Text, Rewrite)]
removeRedundantConstraints df (L _ HsModule {hsmodDecls}) Diagnostic{..}
-- • Redundant constraint: Eq a
-- • In the type signature for:
--      foo :: forall a. Eq a => a -> a
-- • Redundant constraints: (Monoid a, Show a)
-- • In the type signature for:
--      foo :: forall a. (Num a, Monoid a, Eq a, Show a) => a -> Bool
  -- Account for both "Redundant constraint" and "Redundant constraints".
  | "Redundant constraint" `T.isInfixOf` _message
  , Just typeSignatureName <- findTypeSignatureName _message
#if !MIN_VERSION_ghc(9,2,0)
  , Just (TypeSig _ _ HsWC{hswc_body = HsIB {hsib_body = sig}})
#else
  , Just (TypeSig _ _ HsWC{hswc_body = (unLoc -> HsSig {sig_body = sig})})
#endif
    <- fmap(traceAst "redundantConstraint") $ findSigOfDeclRanged _range hsmodDecls
  , Just redundantConstraintList <- findRedundantConstraints _message
  , rewrite <- removeConstraint (toRemove df redundantConstraintList) sig
      = [(actionTitle redundantConstraintList typeSignatureName, rewrite)]
  | otherwise = []
    where
      toRemove df list a = T.pack (showSDoc df (ppr a)) `elem` list

      parseConstraints :: T.Text -> [T.Text]
      parseConstraints t = t
        & (T.strip >>> stripConstraintsParens >>> T.splitOn ",")
        <&> T.strip

      stripConstraintsParens :: T.Text -> T.Text
      stripConstraintsParens constraints =
        if "(" `T.isPrefixOf` constraints
           then constraints & T.drop 1 & T.dropEnd 1 & T.strip
           else constraints

{-
9.2: "message": "/private/var/folders/4m/d38fhm3936x_gy_9883zbq8h0000gn/T/extra-dir-53173393699/Testing.hs:4:1: warning:
    â¢ Redundant constraints: (Eq a, Show a)
    â¢ In the type signature for:
               foo :: forall a. (Eq a, Show a) => a -> Bool",

9.0: "message": "â¢ Redundant constraints: (Eq a, Show a)
    â¢ In the type signature for:
           foo :: forall a. (Eq a, Show a) => a -> Bool",
-}
      findRedundantConstraints :: T.Text -> Maybe [T.Text]
      findRedundantConstraints t = t
        & T.lines
        -- In <9.2 it's the first line, in 9.2 it' the second line
        & take 2
        & mapMaybe ((`matchRegexUnifySpaces` "Redundant constraints?: (.+)") . T.strip)
        & listToMaybe
        <&> (head >>> parseConstraints)

      formatConstraints :: [T.Text] -> T.Text
      formatConstraints [] = ""
      formatConstraints [constraint] = constraint
      formatConstraints constraintList = constraintList
        & T.intercalate ", "
        & \cs -> "(" <> cs <> ")"

      actionTitle :: [T.Text] -> T.Text -> T.Text
      actionTitle constraintList typeSignatureName =
        "Remove redundant constraint" <> (if length constraintList == 1 then "" else "s") <> " `"
        <> formatConstraints constraintList
        <> "` from the context of the type signature for `" <> typeSignatureName <> "`"

-------------------------------------------------------------------------------------------------

suggestNewOrExtendImportForClassMethod :: ExportsMap -> Annotated ParsedSource -> T.Text -> Diagnostic -> [(T.Text, CodeActionKind, [Either TextEdit Rewrite])]
suggestNewOrExtendImportForClassMethod packageExportsMap ps fileContents Diagnostic {_message}
  | Just [methodName, className] <-
      matchRegexUnifySpaces
        _message
        "‘([^’]*)’ is not a \\(visible\\) method of class ‘([^’]*)’",
    idents <-
      maybe [] (Set.toList . Set.filter (\x -> parent x == Just className)) $
        Map.lookup methodName $ getExportsMap packageExportsMap =
    mconcat $ suggest <$> idents
  | otherwise = []
  where
    suggest identInfo@IdentInfo {moduleNameText}
      | importStyle <- NE.toList $ importStyles identInfo,
        mImportDecl <- findImportDeclByModuleName (hsmodImports . unLoc . astA $ ps) (T.unpack moduleNameText) =
        case mImportDecl of
          -- extend
          Just decl ->
            [ ( "Add " <> renderImportStyle style <> " to the import list of " <> moduleNameText,
                quickFixImportKind' "extend" style,
                [Right $ uncurry extendImport (unImportStyle style) decl]
              )
              | style <- importStyle
            ]
          -- new
          _
            | Just (range, indent) <- newImportInsertRange ps fileContents
            ->
             (\(kind, unNewImport -> x) -> (x, kind, [Left $ TextEdit range (x <> "\n" <> T.replicate indent " ")])) <$>
            [ (quickFixImportKind' "new" style, newUnqualImport moduleNameText rendered False)
              | style <- importStyle,
                let rendered = renderImportStyle style
            ]
              <> [(quickFixImportKind "new.all", newImportAll moduleNameText)]
            | otherwise -> []
#endif

suggestNewImport :: ExportsMap -> Annotated ParsedSource -> T.Text -> Diagnostic -> [(T.Text, CodeActionKind, TextEdit)]
suggestNewImport packageExportsMap ps fileContents Diagnostic{_message}
  | msg <- unifySpaces _message
  , Just thingMissing <- extractNotInScopeName msg
  , qual <- extractQualifiedModuleName msg
  , qual' <-
      extractDoesNotExportModuleName msg
        >>= (findImportDeclByModuleName hsmodImports . T.unpack)
        >>= ideclAs . unLoc
        <&> T.pack . moduleNameString . unLoc
  , Just (range, indent) <- newImportInsertRange ps fileContents
  , extendImportSuggestions <- matchRegexUnifySpaces msg
    "Perhaps you want to add ‘[^’]*’ to the import list in the import of ‘([^’]*)’"
  = let suggestions = nubSortBy simpleCompareImportSuggestion
          (constructNewImportSuggestions packageExportsMap (qual <|> qual', thingMissing) extendImportSuggestions) in
    map (\(ImportSuggestion _ kind (unNewImport -> imp)) -> (imp, kind, TextEdit range (imp <> "\n" <> T.replicate indent " "))) suggestions
  where
    L _ HsModule {..} = astA ps
suggestNewImport _ _ _ _ = []

constructNewImportSuggestions
  :: ExportsMap -> (Maybe T.Text, NotInScope) -> Maybe [T.Text] -> [ImportSuggestion]
constructNewImportSuggestions exportsMap (qual, thingMissing) notTheseModules = nubOrdBy simpleCompareImportSuggestion
  [ suggestion
  | Just name <- [T.stripPrefix (maybe "" (<> ".") qual) $ notInScope thingMissing] -- strip away qualified module names from the unknown name
  , identInfo <- maybe [] Set.toList $ Map.lookup name (getExportsMap exportsMap)   -- look up the modified unknown name in the export map
  , canUseIdent thingMissing identInfo                                              -- check if the identifier information retrieved can be used
  , moduleNameText identInfo `notElem` fromMaybe [] notTheseModules                 -- check if the module of the identifier is allowed
  , suggestion <- renderNewImport identInfo                                         -- creates a list of import suggestions for the retrieved identifier information
  ]
 where
  renderNewImport :: IdentInfo -> [ImportSuggestion]
  renderNewImport identInfo
    | Just q <- qual
    = [ImportSuggestion importanceScore (quickFixImportKind "new.qualified") (newQualImport m q)]
    | otherwise
    = [ImportSuggestion importanceScore (quickFixImportKind' "new" importStyle) (newUnqualImport m (renderImportStyle importStyle) False)
      | importStyle <- NE.toList $ importStyles identInfo] ++
      [ImportSuggestion importanceScore (quickFixImportKind "new.all") (newImportAll m)]
    where
        -- The importance score takes 2 metrics into account. The first being the similarity using
        -- the Text.Fuzzy.Parallel.match function. The second is a factor of the relation between
        -- the modules prefix import suggestion and the unknown identifier names.
        importanceScore
          | Just q <- qual
          = let
              similarityScore = fromIntegral $ unpackMatchScore (TFP.match (T.toLower q) (T.toLower m)) :: Double
              (maxLength, minLength) = case (T.length q, T.length m) of
                 (la, lb)
                   | la >= lb -> (fromIntegral la, fromIntegral lb)
                   | otherwise -> (fromIntegral lb, fromIntegral la)
              lengthPenaltyFactor = 100 * minLength / maxLength
            in max 0 (floor (similarityScore * lengthPenaltyFactor))
          | otherwise
          = 0
          where
            unpackMatchScore pScore
              | Just score <- pScore = score
              | otherwise = 0
        m = moduleNameText identInfo

data ImportSuggestion = ImportSuggestion !Int !CodeActionKind !NewImport
  deriving ( Eq )

-- | Implements a lexicographic order for import suggestions that ignores the code action.
-- First it compares the importance score in DESCENDING order.
-- If the scores are equal it compares the import names alphabetical order.
--
-- TODO: this should be a correct Ord instance but CodeActionKind does not implement a Ord
-- which would lead to an unlawful Ord instance.
simpleCompareImportSuggestion :: ImportSuggestion -> ImportSuggestion -> Ordering
simpleCompareImportSuggestion (ImportSuggestion s1 _ i1) (ImportSuggestion s2 _ i2)
  = flip compare s1 s2 <> compare i1 i2

newtype NewImport = NewImport {unNewImport :: T.Text}
  deriving (Show, Eq, Ord)

newImportToEdit :: NewImport -> Annotated ParsedSource -> T.Text -> Maybe (T.Text, TextEdit)
newImportToEdit (unNewImport -> imp) ps fileContents
  | Just (range, indent) <- newImportInsertRange ps fileContents
  = Just (imp, TextEdit range (imp <> "\n" <> T.replicate indent " "))
  | otherwise = Nothing

-- | Finds the next valid position for inserting a new import declaration
-- * If the file already has existing imports it will be inserted under the last of these,
-- it is assumed that the existing last import declaration is in a valid position
-- * If the file does not have existing imports, but has a (module ... where) declaration,
-- the new import will be inserted directly under this declaration (accounting for explicit exports)
-- * If the file has neither existing imports nor a module declaration,
-- the import will be inserted at line zero if there are no pragmas,
-- * otherwise inserted one line after the last file-header pragma
newImportInsertRange :: Annotated ParsedSource -> T.Text -> Maybe (Range, Int)
newImportInsertRange ps fileContents
  |  Just ((l, c), col) <- case hsmodImports of
      -- When there is no existing imports, we only cares about the line number, setting column and indent to zero.
      [] -> (\line -> ((line, 0), 0)) <$> findPositionNoImports ps fileContents
      _  -> findPositionFromImports (map reLoc hsmodImports) last
  , let insertPos = Position (fromIntegral l) (fromIntegral c)
    = Just (Range insertPos insertPos, col)
  | otherwise = Nothing
  where
    L _ HsModule {..} = astA ps

-- | Find the position for a new import when there isn't an existing one.
-- * If there is a module declaration, a new import should be inserted under the module declaration (including exports list)
-- * Otherwise, a new import should be inserted after any file-header pragma.
findPositionNoImports :: Annotated ParsedSource -> T.Text -> Maybe Int
findPositionNoImports ps fileContents =
    maybe (Just (findNextPragmaPosition fileContents)) (findPositionAfterModuleName ps) hsmodName
  where
    L _ HsModule {..} = astA ps

-- | find line number right after module ... where
findPositionAfterModuleName :: Annotated ParsedSource
#if MIN_VERSION_ghc(9,2,0)
                            -> LocatedA ModuleName
#else
                            -> Located ModuleName
#endif
                            -> Maybe Int
findPositionAfterModuleName ps hsmodName' = do
    -- Note that 'where' keyword and comments are not part of the AST. They belongs to
    -- the exact-print information. To locate it, we need to find the previous AST node,
    -- calculate the gap between it and 'where', then add them up to produce the absolute
    -- position of 'where'.

    lineOffset <- whereKeywordLineOffset -- Calculate the gap before 'where' keyword.
    case prevSrcSpan of
        UnhelpfulSpan _ -> Nothing
        (RealSrcSpan prevSrcSpan' _) ->
            -- add them up produce the absolute location of 'where' keyword
            Just $ srcLocLine (realSrcSpanEnd prevSrcSpan') + lineOffset
  where
    L _ HsModule {..} = astA ps

    -- The last AST node before 'where' keyword. Might be module name or export list.
    prevSrcSpan = maybe (getLoc hsmodName') getLoc hsmodExports

    -- The relative position of 'where' keyword (in lines, relative to the previous AST node).
    -- The exact-print API changed a lot in ghc-9.2, so we need to handle it separately for different compiler versions.
    whereKeywordLineOffset :: Maybe Int
#if MIN_VERSION_ghc(9,2,0)
    whereKeywordLineOffset = case hsmodAnn of
        EpAnn _ annsModule _ -> do
            -- Find the first 'where'
            whereLocation <- fmap NE.head .  NE.nonEmpty . mapMaybe filterWhere .  am_main $ annsModule
            epaLocationToLine whereLocation
        EpAnnNotUsed -> Nothing
    filterWhere (AddEpAnn AnnWhere loc) = Just loc
    filterWhere _                       = Nothing

    epaLocationToLine :: EpaLocation -> Maybe Int
    epaLocationToLine (EpaSpan sp) = Just . srcLocLine . realSrcSpanEnd $ sp
    epaLocationToLine (EpaDelta (SameLine _) priorComments) = Just $ sumCommentsOffset priorComments
    -- 'priorComments' contains the comments right before the current EpaLocation
    -- Summing line offset of priorComments is necessary, as 'line' is the gap between the last comment and
    -- the current AST node
    epaLocationToLine (EpaDelta (DifferentLine line _) priorComments) = Just (line + sumCommentsOffset priorComments)

    sumCommentsOffset :: [LEpaComment] -> Int
    sumCommentsOffset = sum . fmap (\(L anchor _) -> anchorOpLine (anchor_op anchor))

    anchorOpLine :: AnchorOperation -> Int
    anchorOpLine UnchangedAnchor                      = 0
    anchorOpLine (MovedAnchor (SameLine _))           = 0
    anchorOpLine (MovedAnchor (DifferentLine line _)) = line
#else
    whereKeywordLineOffset = do
        ann <- annsA ps M.!? mkAnnKey (astA ps)
        deltaPos <- fmap NE.head . NE.nonEmpty .mapMaybe filterWhere $ annsDP ann
        pure $ deltaRow deltaPos

    -- Before ghc 9.2, DeltaPos doesn't take comment into account, so we don't need to sum line offset of comments.
    filterWhere :: (KeywordId, DeltaPos) -> Maybe DeltaPos
    filterWhere (keywordId, deltaPos) =
        if keywordId == G AnnWhere then Just deltaPos else Nothing
#endif

findPositionFromImports :: HasSrcSpan a => t -> (t -> a) -> Maybe ((Int, Int), Int)
findPositionFromImports hsField f = case getLoc (f hsField) of
  RealSrcSpan s _ ->
    let col = calcCol s
     in Just ((srcLocLine (realSrcSpanEnd s), col), col)
  _ -> Nothing
  where calcCol s = srcLocCol (realSrcSpanStart s) - 1

-- | Find the position one after the last file-header pragma
-- Defaults to zero if there are no pragmas in file
findNextPragmaPosition :: T.Text -> Int
findNextPragmaPosition contents = lineNumber
  where
    lineNumber = afterLangPragma . afterOptsGhc $ afterShebang
    afterLangPragma = afterPragma "LANGUAGE" contents'
    afterOptsGhc = afterPragma "OPTIONS_GHC" contents'
    afterShebang = lastLineWithPrefix (T.isPrefixOf "#!") contents' 0
    contents' = T.lines contents

afterPragma :: T.Text -> [T.Text] -> Int -> Int
afterPragma name contents lineNum = lastLineWithPrefix (checkPragma name) contents lineNum

lastLineWithPrefix :: (T.Text -> Bool) -> [T.Text] -> Int -> Int
lastLineWithPrefix p contents lineNum = max lineNum next
  where
    next = maybe lineNum succ $ listToMaybe . reverse $ findIndices p contents

checkPragma :: T.Text -> T.Text -> Bool
checkPragma name = check
  where
    check l = isPragma l && getName l == name
    getName l = T.take (T.length name) $ T.dropWhile isSpace $ T.drop 3 l
    isPragma = T.isPrefixOf "{-#"

-- | Construct an import declaration with at most one symbol
newImport
  :: T.Text -- ^ module name
  -> Maybe T.Text -- ^  the symbol
  -> Maybe T.Text -- ^ qualified name
  -> Bool -- ^ the symbol is to be imported or hidden
  -> NewImport
newImport modName mSymbol mQual hiding = NewImport impStmt
  where
     symImp
            | Just symbol <- mSymbol
              , symOcc <- mkVarOcc $ T.unpack symbol =
              " (" <> printOutputable (parenSymOcc symOcc $ ppr symOcc) <> ")"
            | otherwise = ""
     impStmt =
       "import "
         <> maybe "" (const "qualified ") mQual
         <> modName
         <> (if hiding then " hiding" else "")
         <> symImp
         <> maybe "" (\qual -> if modName == qual then "" else " as " <> qual) mQual

newQualImport :: T.Text -> T.Text -> NewImport
newQualImport modName qual = newImport modName Nothing (Just qual) False

newUnqualImport :: T.Text -> T.Text -> Bool -> NewImport
newUnqualImport modName symbol = newImport modName (Just symbol) Nothing

newImportAll :: T.Text -> NewImport
newImportAll modName = newImport modName Nothing Nothing False

hideImplicitPreludeSymbol :: T.Text -> NewImport
hideImplicitPreludeSymbol symbol = newUnqualImport "Prelude" symbol True

canUseIdent :: NotInScope -> IdentInfo -> Bool
canUseIdent NotInScopeDataConstructor{}        = isDatacon
canUseIdent NotInScopeTypeConstructorOrClass{} = not . isDatacon
canUseIdent _                                  = const True

data NotInScope
    = NotInScopeDataConstructor T.Text
    | NotInScopeTypeConstructorOrClass T.Text
    | NotInScopeThing T.Text
    deriving Show

notInScope :: NotInScope -> T.Text
notInScope (NotInScopeDataConstructor t)        = t
notInScope (NotInScopeTypeConstructorOrClass t) = t
notInScope (NotInScopeThing t)                  = t

extractNotInScopeName :: T.Text -> Maybe NotInScope
extractNotInScopeName x
  | Just [name] <- matchRegexUnifySpaces x "Data constructor not in scope: ([^ ]+)"
  = Just $ NotInScopeDataConstructor name
  | Just [name] <- matchRegexUnifySpaces x "Not in scope: data constructor [^‘]*‘([^’]*)’"
  = Just $ NotInScopeDataConstructor name
  | Just [name] <- matchRegexUnifySpaces x "ot in scope: type constructor or class [^‘]*‘([^’]*)’"
  = Just $ NotInScopeTypeConstructorOrClass name
  | Just [name] <- matchRegexUnifySpaces x "ot in scope: \\(([^‘ ]+)\\)"
  = Just $ NotInScopeThing name
  | Just [name] <- matchRegexUnifySpaces x "ot in scope: ([^‘ ]+)"
  = Just $ NotInScopeThing name
  | Just [name] <- matchRegexUnifySpaces x "ot in scope:[^‘]*‘([^’]*)’"
  = Just $ NotInScopeThing name
  | otherwise
  = Nothing

extractQualifiedModuleName :: T.Text -> Maybe T.Text
extractQualifiedModuleName x
  | Just [m] <- matchRegexUnifySpaces x "module named [^‘]*‘([^’]*)’"
  = Just m
  | otherwise
  = Nothing

-- | If a module has been imported qualified, and we want to ues the same qualifier for other modules
-- which haven't been imported, 'extractQualifiedModuleName' won't work. Thus we need extract the qualifier
-- from the imported one.
--
-- For example, we write f = T.putStrLn, where putStrLn comes from Data.Text.IO, with the following import(s):
-- 1.
-- import qualified Data.Text as T
--
-- Module ‘Data.Text’ does not export ‘putStrLn’.
--
-- 2.
-- import qualified Data.Text as T
-- import qualified Data.Functor as T
--
-- Neither ‘Data.Functor’ nor ‘Data.Text’ exports ‘putStrLn’.
--
-- 3.
-- import qualified Data.Text as T
-- import qualified Data.Functor as T
-- import qualified Data.Function as T
--
-- Neither ‘Data.Function’,
--         ‘Data.Functor’ nor ‘Data.Text’ exports ‘putStrLn’.
extractDoesNotExportModuleName :: T.Text -> Maybe T.Text
extractDoesNotExportModuleName x
  | Just [m] <-
    matchRegexUnifySpaces x "Module ‘([^’]*)’ does not export"
      <|> matchRegexUnifySpaces x "nor ‘([^’]*)’ exports"
  = Just m
  | otherwise
  = Nothing
-------------------------------------------------------------------------------------------------


mkRenameEdit :: Maybe T.Text -> Range -> T.Text -> TextEdit
mkRenameEdit contents range name
    | maybeIsInfixFunction == Just True = TextEdit range ("`" <> name <> "`")
    | maybeIsTemplateFunction == Just True = TextEdit range ("'" <> name)
    | otherwise = TextEdit range name
  where
    maybeIsInfixFunction = do
      curr <- textInRange range <$> contents
      pure $ "`" `T.isPrefixOf` curr && "`" `T.isSuffixOf` curr
    maybeIsTemplateFunction = do
      curr <- textInRange range <$> contents
      pure $ "'" `T.isPrefixOf` curr

-- | Extract the type and surround it in parentheses except in obviously safe cases.
--
-- Inferring when parentheses are actually needed around the type signature would
-- require understanding both the precedence of the context of the hole and of
-- the signature itself. Inserting them (almost) unconditionally is ugly but safe.
extractWildCardTypeSignature :: T.Text -> T.Text
extractWildCardTypeSignature msg
  | enclosed || not isApp || isToplevelSig = sig
  | otherwise                              = "(" <> sig <> ")"
  where
    msgSigPart      = snd $ T.breakOnEnd "standing for " msg
    (sig, rest)     = T.span (/='’') . T.dropWhile (=='‘') . T.dropWhile (/='‘') $ msgSigPart
    -- If we're completing something like ‘foo :: _’ parens can be safely omitted.
    isToplevelSig   = errorMessageRefersToToplevelHole rest
    -- Parenthesize type applications, e.g. (Maybe Char).
    isApp           = T.any isSpace sig
    -- Do not add extra parentheses to lists, tuples and already parenthesized types.
    enclosed        = not (T.null sig) && (T.head sig, T.last sig) `elem` [('(', ')'), ('[', ']')]

-- | Detect whether user wrote something like @foo :: _@ or @foo :: (_, Int)@.
-- The former is considered toplevel case for which the function returns 'True',
-- the latter is not toplevel and the returned value is 'False'.
--
-- When type hole is at toplevel then there’s a line starting with
-- "• In the type signature" which ends with " :: _" like in the
-- following snippet:
--
-- source/library/Language/Haskell/Brittany/Internal.hs:131:13: error:
--     • Found type wildcard ‘_’ standing for ‘HsDecl GhcPs’
--       To use the inferred type, enable PartialTypeSignatures
--     • In the type signature: decl :: _
--       In an equation for ‘splitAnnots’:
--           splitAnnots m@HsModule {hsmodAnn, hsmodDecls}
--             = undefined
--             where
--                 ann :: SrcSpanAnnA
--                 decl :: _
--                 L ann decl = head hsmodDecls
--     • Relevant bindings include
--       [REDACTED]
--
-- When type hole is not at toplevel there’s a stack of where
-- the hole was located ending with "In the type signature":
--
-- source/library/Language/Haskell/Brittany/Internal.hs:130:20: error:
--     • Found type wildcard ‘_’ standing for ‘GhcPs’
--       To use the inferred type, enable PartialTypeSignatures
--     • In the first argument of ‘HsDecl’, namely ‘_’
--       In the type ‘HsDecl _’
--       In the type signature: decl :: HsDecl _
--     • Relevant bindings include
--       [REDACTED]
errorMessageRefersToToplevelHole :: T.Text -> Bool
errorMessageRefersToToplevelHole msg =
  not (T.null prefix) && " :: _" `T.isSuffixOf` T.takeWhile (/= '\n') rest
  where
    (prefix, rest) = T.breakOn "• In the type signature:" msg

extractRenamableTerms :: T.Text -> [T.Text]
extractRenamableTerms msg
  -- Account for both "Variable not in scope" and "Not in scope"
  | "ot in scope:" `T.isInfixOf` msg = extractSuggestions msg
  | otherwise = []
  where
    extractSuggestions = map getEnclosed
                       . concatMap singleSuggestions
                       . filter isKnownSymbol
                       . T.lines
    singleSuggestions = T.splitOn "), " -- Each suggestion is comma delimited
    isKnownSymbol t = " (imported from" `T.isInfixOf` t || " (line " `T.isInfixOf` t
    getEnclosed = T.dropWhile (== '‘')
                . T.dropWhileEnd (== '’')
                . T.dropAround (\c -> c /= '‘' && c /= '’')

-- | If a range takes up a whole line (it begins at the start of the line and there's only whitespace
-- between the end of the range and the next newline), extend the range to take up the whole line.
extendToWholeLineIfPossible :: Maybe T.Text -> Range -> Range
extendToWholeLineIfPossible contents range@Range{..} =
    let newlineAfter = maybe False (T.isPrefixOf "\n" . T.dropWhile (\x -> isSpace x && x /= '\n') . snd . splitTextAtPosition _end) contents
        extend = newlineAfter && _character _start == 0 -- takes up an entire line, so remove the whole line
    in if extend then Range _start (Position (_line _end + 1) 0) else range

splitTextAtPosition :: Position -> T.Text -> (T.Text, T.Text)
splitTextAtPosition (Position (fromIntegral -> row) (fromIntegral -> col)) x
    | (preRow, mid:postRow) <- splitAt row $ T.splitOn "\n" x
    , (preCol, postCol) <- T.splitAt col mid
        = (T.intercalate "\n" $ preRow ++ [preCol], T.intercalate "\n" $ postCol : postRow)
    | otherwise = (x, T.empty)

-- | Returns [start .. end[
textInRange :: Range -> T.Text -> T.Text
textInRange (Range (Position (fromIntegral -> startRow) (fromIntegral -> startCol)) (Position (fromIntegral -> endRow) (fromIntegral -> endCol))) text =
    case compare startRow endRow of
      LT ->
        let (linesInRangeBeforeEndLine, endLineAndFurtherLines) = splitAt (endRow - startRow) linesBeginningWithStartLine
            (textInRangeInFirstLine, linesBetween) = case linesInRangeBeforeEndLine of
              [] -> ("", [])
              firstLine:linesInBetween -> (T.drop startCol firstLine, linesInBetween)
            maybeTextInRangeInEndLine = T.take endCol <$> listToMaybe endLineAndFurtherLines
        in T.intercalate "\n" (textInRangeInFirstLine : linesBetween ++ maybeToList maybeTextInRangeInEndLine)
      EQ ->
        let line = fromMaybe "" (listToMaybe linesBeginningWithStartLine)
        in T.take (endCol - startCol) (T.drop startCol line)
      GT -> ""
    where
      linesBeginningWithStartLine = drop startRow (T.splitOn "\n" text)

-- | Returns the ranges for a binding in an import declaration
rangesForBindingImport :: ImportDecl GhcPs -> String -> [Range]
rangesForBindingImport ImportDecl{ideclHiding = Just (False, L _ lies)} b =
    concatMap (mapMaybe srcSpanToRange . rangesForBinding' b') lies
  where
    b' = wrapOperatorInParens b
rangesForBindingImport _ _ = []

wrapOperatorInParens :: String -> String
wrapOperatorInParens x =
  case uncons x of
    -- see #2483 and #2859
    -- common lens functions use the _ prefix, and should not be wrapped in parens
    Just ('_', _t) -> x
    Just (h, _t)   -> if isAlpha h then x else "(" <> x <> ")"
    Nothing        -> mempty

smallerRangesForBindingExport :: [LIE GhcPs] -> String -> [Range]
smallerRangesForBindingExport lies b =
    concatMap (mapMaybe srcSpanToRange . ranges') lies
  where
    unqualify = snd . breakOnEnd "."
    b' = wrapOperatorInParens . unqualify $ b
#if !MIN_VERSION_ghc(9,2,0)
    ranges' (L _ (IEThingWith _ thing _  inners labels))
      | T.unpack (printOutputable thing) == b' = []
      | otherwise =
          [ locA l' | L l' x <- inners, T.unpack (printOutputable x) == b']
          ++ [ l' | L l' x <- labels, T.unpack (printOutputable x) == b']
#else
    ranges' (L _ (IEThingWith _ thing _  inners))
      | T.unpack (printOutputable thing) == b' = []
      | otherwise =
          [ locA l' | L l' x <- inners, T.unpack (printOutputable x) == b']
#endif
    ranges' _ = []

rangesForBinding' :: String -> LIE GhcPs -> [SrcSpan]
rangesForBinding' b (L (locA -> l) x@IEVar{}) | T.unpack (printOutputable x) == b = [l]
rangesForBinding' b (L (locA -> l) x@IEThingAbs{}) | T.unpack (printOutputable x) == b = [l]
rangesForBinding' b (L (locA -> l) (IEThingAll _ x)) | T.unpack (printOutputable x) == b = [l]
#if !MIN_VERSION_ghc(9,2,0)
rangesForBinding' b (L l (IEThingWith _ thing _  inners labels))
#else
rangesForBinding' b (L (locA -> l) (IEThingWith _ thing _  inners))
#endif
    | T.unpack (printOutputable thing) == b = [l]
    | otherwise =
        [ locA l' | L l' x <- inners, T.unpack (printOutputable x) == b]
#if !MIN_VERSION_ghc(9,2,0)
        ++ [ l' | L l' x <- labels, T.unpack (printOutputable x) == b]
#endif
rangesForBinding' _ _ = []

-- | 'matchRegex' combined with 'unifySpaces'
matchRegexUnifySpaces :: T.Text -> T.Text -> Maybe [T.Text]
matchRegexUnifySpaces message = matchRegex (unifySpaces message)

-- | 'allMatchRegex' combined with 'unifySpaces'
allMatchRegexUnifySpaces :: T.Text -> T.Text -> Maybe [[T.Text]]
allMatchRegexUnifySpaces message =
    allMatchRegex (unifySpaces message)

-- | Returns Just (the submatches) for the first capture, or Nothing.
matchRegex :: T.Text -> T.Text -> Maybe [T.Text]
matchRegex message regex = case message =~~ regex of
    Just (_ :: T.Text, _ :: T.Text, _ :: T.Text, bindings) -> Just bindings
    Nothing                                                -> Nothing

-- | Returns Just (all matches) for the first capture, or Nothing.
allMatchRegex :: T.Text -> T.Text -> Maybe [[T.Text]]
allMatchRegex message regex = message =~~ regex


unifySpaces :: T.Text -> T.Text
unifySpaces    = T.unwords . T.words

-- functions to help parse multiple import suggestions

-- | Returns the first match if found
regexSingleMatch :: T.Text -> T.Text -> Maybe T.Text
regexSingleMatch msg regex = case matchRegexUnifySpaces msg regex of
    Just (h:_) -> Just h
    _          -> Nothing

-- | Parses tuples like (‘Data.Map’, (app/ModuleB.hs:2:1-18)) and
-- | return (Data.Map, app/ModuleB.hs:2:1-18)
regExPair :: (T.Text, T.Text) -> Maybe (T.Text, T.Text)
regExPair (modname, srcpair) = do
  x <- regexSingleMatch modname "‘([^’]*)’"
  y <- regexSingleMatch srcpair "\\((.*)\\)"
  return (x, y)

-- | Process a list of (module_name, filename:src_span) values
-- | Eg. [(Data.Map, app/ModuleB.hs:2:1-18), (Data.HashMap.Strict, app/ModuleB.hs:3:1-29)]
regExImports :: T.Text -> Maybe [(T.Text, T.Text)]
regExImports msg = result
  where
    parts = T.words msg
    isPrefix = not . T.isPrefixOf "("
    (mod, srcspan) = partition isPrefix  parts
    -- check we have matching pairs like (Data.Map, (app/src.hs:1:2-18))
    result = if length mod == length srcspan then
               regExPair `traverse` zip mod srcspan
             else Nothing

matchRegExMultipleImports :: T.Text -> Maybe (T.Text, [(T.Text, T.Text)])
matchRegExMultipleImports message = do
  let pat = T.pack "Perhaps you want to add ‘([^’]*)’ to one of these import lists: *(‘.*\\))$"
  (binding, imports) <- case matchRegexUnifySpaces message pat of
                            Just [x, xs] -> Just (x, xs)
                            _            -> Nothing
  imps <- regExImports imports
  return (binding, imps)

-- | Possible import styles for an 'IdentInfo'.
--
-- The first 'Text' parameter corresponds to the 'rendered' field of the
-- 'IdentInfo'.
data ImportStyle
    = ImportTopLevel T.Text
      -- ^ Import a top-level export from a module, e.g., a function, a type, a
      -- class.
      --
      -- > import M (?)
      --
      -- Some exports that have a parent, like a type-class method or an
      -- associated type/data family, can still be imported as a top-level
      -- import.
      --
      -- Note that this is not the case for constructors, they must always be
      -- imported as part of their parent data type.

    | ImportViaParent T.Text T.Text
      -- ^ Import an export (first parameter) through its parent (second
      -- parameter).
      --
      -- import M (P(?))
      --
      -- @P@ and @?@ can be a data type and a constructor, a class and a method,
      -- a class and an associated type/data family, etc.

    | ImportAllConstructors T.Text
      -- ^ Import all constructors for a specific data type.
      --
      -- import M (P(..))
      --
      -- @P@ can be a data type or a class.
  deriving Show

importStyles :: IdentInfo -> NonEmpty ImportStyle
importStyles IdentInfo {parent, rendered, isDatacon}
  | Just p <- parent
    -- Constructors always have to be imported via their parent data type, but
    -- methods and associated type/data families can also be imported as
    -- top-level exports.
  = ImportViaParent rendered p
      :| [ImportTopLevel rendered | not isDatacon]
      <> [ImportAllConstructors p]
  | otherwise
  = ImportTopLevel rendered :| []

-- | Used for adding new imports
renderImportStyle :: ImportStyle -> T.Text
renderImportStyle (ImportTopLevel x)   = x
renderImportStyle (ImportViaParent x p@(T.uncons -> Just ('(', _))) = "type " <> p <> "(" <> x <> ")"
renderImportStyle (ImportViaParent x p) = p <> "(" <> x <> ")"
renderImportStyle (ImportAllConstructors p) = p <> "(..)"

-- | Used for extending import lists
unImportStyle :: ImportStyle -> (Maybe String, String)
unImportStyle (ImportTopLevel x)        = (Nothing, T.unpack x)
unImportStyle (ImportViaParent x y)     = (Just $ T.unpack y, T.unpack x)
unImportStyle (ImportAllConstructors x) = (Just $ T.unpack x, wildCardSymbol)


quickFixImportKind' :: T.Text -> ImportStyle -> CodeActionKind
quickFixImportKind' x (ImportTopLevel _) = CodeActionUnknown $ "quickfix.import." <> x <> ".list.topLevel"
quickFixImportKind' x (ImportViaParent _ _) = CodeActionUnknown $ "quickfix.import." <> x <> ".list.withParent"
quickFixImportKind' x (ImportAllConstructors _) = CodeActionUnknown $ "quickfix.import." <> x <> ".list.allConstructors"

quickFixImportKind :: T.Text -> CodeActionKind
quickFixImportKind x = CodeActionUnknown $ "quickfix.import." <> x
