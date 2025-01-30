-- Copyright (c) 2019 The DAML Authors. All rights reserved.
-- SPDX-License-Identifier: Apache-2.0
{-# LANGUAGE CPP   #-}
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
import           Control.Applicative.Combinators.NonEmpty          (sepBy1)
import           Control.Arrow                                     (second,
                                                                    (&&&),
                                                                    (>>>))
import           Control.Concurrent.STM.Stats                      (atomically)
import           Control.Monad.Extra
import           Control.Monad.IO.Class
import           Control.Monad.Trans.Except                        (ExceptT (ExceptT))
import           Control.Monad.Trans.Maybe
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
import qualified Data.Text.Encoding                                as T
import qualified Data.Text.Utf16.Rope.Mixed                        as Rope
import           Development.IDE.Core.FileStore                    (getUriContents)
import           Development.IDE.Core.Rules
import           Development.IDE.Core.RuleTypes
import           Development.IDE.Core.Service
import           Development.IDE.Core.Shake                        hiding (Log)
import           Development.IDE.GHC.Compat                        hiding
                                                                   (ImplicitPrelude)
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
import qualified Development.IDE.Plugin.Plugins.AddArgument
import           Development.IDE.Plugin.Plugins.Diagnostic
import           Development.IDE.Plugin.Plugins.FillHole           (suggestFillHole)
import           Development.IDE.Plugin.Plugins.FillTypeWildcard   (suggestFillTypeWildcard)
import           Development.IDE.Plugin.Plugins.ImportUtils
import           Development.IDE.Plugin.TypeLenses                 (suggestSignature)
import           Development.IDE.Types.Diagnostics
import           Development.IDE.Types.Exports
import           Development.IDE.Types.Location
import           Development.IDE.Types.Options
import           GHC                                               (AddEpAnn (AddEpAnn),
                                                                    AnnsModule (am_main),
                                                                    DeltaPos (..),
                                                                    EpAnn (..),
                                                                    LEpaComment)
import qualified GHC.LanguageExtensions                            as Lang
import           Ide.Logger                                        hiding
                                                                   (group)
import           Ide.PluginUtils                                   (extendToFullLines,
                                                                    extractTextInRange,
                                                                    subRange)
import           Ide.Types
import           Language.LSP.Protocol.Message                     (Method (..),
                                                                    SMethod (..))
import           Language.LSP.Protocol.Types                       (ApplyWorkspaceEditParams (..),
                                                                    CodeAction (..),
                                                                    CodeActionKind (CodeActionKind_QuickFix),
                                                                    CodeActionParams (CodeActionParams),
                                                                    Command,
                                                                    MessageType (..),
                                                                    Null (Null),
                                                                    ShowMessageParams (..),
                                                                    TextDocumentIdentifier (TextDocumentIdentifier),
                                                                    TextEdit (TextEdit, _range),
                                                                    UInt,
                                                                    WorkspaceEdit (WorkspaceEdit, _changeAnnotations, _changes, _documentChanges),
                                                                    type (|?) (InL, InR),
                                                                    uriToFilePath)
import qualified Text.Fuzzy.Parallel                               as TFP
import qualified Text.Regex.Applicative                            as RE
import           Text.Regex.TDFA                                   ((=~), (=~~))

-- See Note [Guidelines For Using CPP In GHCIDE Import Statements]

#if !MIN_VERSION_ghc(9,9,0)
import           Development.IDE.GHC.Compat.ExactPrint             (makeDeltaAst)
import           GHC                                               (Anchor (anchor_op),
                                                                    AnchorOperation (..),
                                                                    EpaLocation (..))
#endif

#if MIN_VERSION_ghc(9,9,0)
import           GHC                                               (EpaLocation,
                                                                    EpaLocation' (..),
                                                                    HasLoc (..))
import           GHC.Types.SrcLoc                                  (srcSpanToRealSrcSpan)
#endif

-------------------------------------------------------------------------------------------------

-- | Generate code actions.
codeAction :: PluginMethodHandler IdeState 'Method_TextDocumentCodeAction
codeAction state _ (CodeActionParams _ _ (TextDocumentIdentifier uri) range _) = do
  contents <- liftIO $ runAction "hls-refactor-plugin.codeAction.getUriContents" state $ getUriContents $ toNormalizedUri uri
  liftIO $ do
    let mbFile = toNormalizedFilePath' <$> uriToFilePath uri
    allDiags <- atomically $ fmap fdLspDiagnostic . filter (\d -> mbFile == Just (fdFilePath d)) <$> getDiagnostics state
    (join -> parsedModule) <- runAction "GhcideCodeActions.getParsedModule" state $ getParsedModule `traverse` mbFile
    let
      textContents = fmap Rope.toText contents
      actions = caRemoveRedundantImports parsedModule textContents allDiags range uri
               <> caRemoveInvalidExports parsedModule textContents allDiags range uri
    pure $ InL actions

-------------------------------------------------------------------------------------------------

iePluginDescriptor :: Recorder (WithPriority E.Log) -> PluginId -> PluginDescriptor IdeState
iePluginDescriptor recorder plId =
  let old =
        mkGhcideCAsPlugin [
            wrap suggestExportUnusedTopBinding
          , wrap suggestModuleTypo
          , wrap suggestFixConstructorImport
          , wrap suggestExtendImport
          , wrap suggestImportDisambiguation
          , wrap suggestNewOrExtendImportForClassMethod
          , wrap suggestHideShadow
          , wrap suggestNewImport
          , wrap suggestAddRecordFieldImport
          ]
          plId
          "Provides various quick fixes"
   in mkExactprintPluginDescriptor recorder $ old {pluginHandlers = pluginHandlers old <> mkPluginHandler SMethod_TextDocumentCodeAction codeAction }

typeSigsPluginDescriptor :: Recorder (WithPriority E.Log) -> PluginId -> PluginDescriptor IdeState
typeSigsPluginDescriptor recorder plId = mkExactprintPluginDescriptor recorder $
  mkGhcideCAsPlugin [
      wrap $ suggestSignature True
    , wrap suggestFillTypeWildcard
    , wrap suggestAddTypeAnnotationToSatisfyConstraints
    , wrap removeRedundantConstraints
    , wrap suggestConstraint
    ]
    plId
    "Provides various quick fixes for type signatures"

bindingsPluginDescriptor :: Recorder (WithPriority E.Log) ->  PluginId -> PluginDescriptor IdeState
bindingsPluginDescriptor recorder plId = mkExactprintPluginDescriptor recorder $
  mkGhcideCAsPlugin [
      wrap suggestReplaceIdentifier
    , wrap suggestImplicitParameter
    , wrap suggestNewDefinition
    , wrap Development.IDE.Plugin.Plugins.AddArgument.plugin
    , wrap suggestDeleteUnusedBinding
    ]
    plId
    "Provides various quick fixes for bindings"

fillHolePluginDescriptor :: Recorder (WithPriority E.Log) -> PluginId -> PluginDescriptor IdeState
fillHolePluginDescriptor recorder plId = mkExactprintPluginDescriptor recorder (mkGhcideCAPlugin (wrap suggestFillHole) plId "Provides a code action to fill a hole")

extendImportPluginDescriptor :: Recorder (WithPriority E.Log) -> PluginId -> PluginDescriptor IdeState
extendImportPluginDescriptor recorder plId = mkExactprintPluginDescriptor recorder $ (defaultPluginDescriptor plId "Provides a command to extend the import list")
  { pluginCommands = [extendImportCommand] }


-- | Add the ability for a plugin to call GetAnnotatedParsedSource
mkExactprintPluginDescriptor :: Recorder (WithPriority E.Log) -> PluginDescriptor a -> PluginDescriptor a
mkExactprintPluginDescriptor recorder desc = desc { pluginRules = pluginRules desc >> getAnnotatedParsedSourceRule recorder }

-------------------------------------------------------------------------------------------------


extendImportCommand :: PluginCommand IdeState
extendImportCommand =
  PluginCommand (CommandId extendImportCommandId) "additional edits for a completion" extendImportHandler

extendImportHandler :: CommandFunction IdeState ExtendImport
extendImportHandler ideState _ edit@ExtendImport {..} = ExceptT $ do
  res <- liftIO $ runMaybeT $ extendImportHandler' ideState edit
  whenJust res $ \(nfp, wedit@WorkspaceEdit {_changes}) -> do
    whenJust (listToMaybe =<< listToMaybe . M.elems =<< _changes) $ \TextEdit {_range} -> do
      let srcSpan = rangeToSrcSpan nfp _range
      pluginSendNotification SMethod_WindowShowMessage $
        ShowMessageParams MessageType_Info $
          "Import "
            <> maybe ("‘" <> newThing) (\x -> "‘" <> x <> " (" <> newThing <> ")") thingParent
            <> "’ from "
            <> importName
            <> " (at "
            <> printOutputable srcSpan
            <> ")"
      void $ pluginSendRequest SMethod_WorkspaceApplyEdit (ApplyWorkspaceEditParams Nothing wedit) (\_ -> pure ())
  return $ Right $ InR Null

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
              rewriteToWEdit df doc $
                  extendImport (T.unpack <$> thingParent) (T.unpack newThing)
#if MIN_VERSION_ghc(9,9,0)
                    imp
#else
                    (makeDeltaAst imp)
#endif

        Nothing -> do
            let qns = (,) <$> importQual <*> Just (qualifiedImportStyle df)
                n = newImport importName sym qns False
                sym = if isNothing importQual then Just it else Nothing
                it = case thingParent of
                  Nothing -> newThing
                  Just p  -> p <> "(" <> newThing <> ")"
            t <- liftMaybe $ snd <$> newImportToEdit n ps (Rope.toText (fromMaybe mempty contents))
            return (nfp, WorkspaceEdit {_changes=Just (M.singleton doc [t]), _documentChanges=Nothing, _changeAnnotations=Nothing})
  | otherwise =
    mzero

isWantedModule :: ModuleName -> Maybe ModuleName -> GenLocated l (ImportDecl GhcPs) -> Bool
isWantedModule wantedModule Nothing (L _ it@ImportDecl{ ideclName
#if MIN_VERSION_ghc(9,5,0)
                                                      , ideclImportList = Just (Exactly, _)
#else
                                                      , ideclHiding = Just (False, _)
#endif
                                                      }) =
    not (isQualifiedImport it) && unLoc ideclName == wantedModule
isWantedModule wantedModule (Just qual) (L _ ImportDecl{ ideclAs, ideclName
#if MIN_VERSION_ghc(9,5,0)
                                                       , ideclImportList = Just (Exactly, _)
#else
                                                       , ideclHiding = Just (False, _)
#endif
                                                       }) =
    unLoc ideclName == wantedModule && (wantedModule == qual || (unLoc <$> ideclAs) == Just qual)
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
      msum
        [findSigOfBinds range (grhssLocalBinds grhs) -- where clause
        , do
          grhs <- findDeclContainingLoc (_start range) (grhssGRHSs grhs)
          case unLoc grhs of
            GRHS _ _ bd -> findSigOfExpr (unLoc bd)
        ]

    findSigOfExpr :: HsExpr p -> Maybe (Sig p)
    findSigOfExpr = go
      where
#if !MIN_VERSION_ghc(9,9,0)
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
    [ hsib_body
      | L _ (InstD _ (ClsInstD _ ClsInstDecl {cid_poly_ty = (unLoc -> HsSig {sig_body = hsib_body})})) <- decls,
        showSDoc df (ppr hsib_body) == instanceHead
    ]

#if MIN_VERSION_ghc(9,9,0)
findDeclContainingLoc :: (Foldable t, HasLoc l) => Position -> t (GenLocated l e) -> Maybe (GenLocated l e)
#else
findDeclContainingLoc :: Foldable t => Position -> t (GenLocated (SrcSpanAnn' a) e) -> Maybe (GenLocated (SrcSpanAnn' a) e)
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
suggestHideShadow :: ParsedSource -> T.Text -> Maybe TcModuleResult -> Maybe HieAstResult -> Diagnostic -> [(T.Text, [Either TextEdit Rewrite])]
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
    L _ HsModule {hsmodImports} = ps

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

findImportDeclByModuleName :: [LImportDecl GhcPs] -> String -> Maybe (LImportDecl GhcPs)
findImportDeclByModuleName decls modName = flip find decls $ \case
  (L _ ImportDecl {..}) -> modName == moduleNameString (unLoc ideclName)

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
    , ranges <- map (rangesForBindingImport impDecl . T.unpack) (T.splitOn ", " bindings >>= trySplitIntoOriginalAndRecordField)
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
    where
      -- In case of an unused record field import, the binding from the message will not match any import directly
      -- In this case, we try if we can additionally extract a record field name
      -- Example: The import of ‘B(b2)’ from module ‘ModuleB’ is redundant
      trySplitIntoOriginalAndRecordField :: T.Text -> [T.Text]
      trySplitIntoOriginalAndRecordField binding =
        case matchRegexUnifySpaces binding "([^ ]+)\\(([^)]+)\\)" of
          Just [_, fields] -> [binding, fields]
          _                -> [binding]

diagInRange :: Diagnostic -> Range -> Bool
diagInRange Diagnostic {_range = dr} r = dr `subRange` extendedRange
  where
    -- Ensures the range captures full lines. Makes it easier to trigger the correct
    -- "remove redundant" code actions from anywhere on the offending line.
    extendedRange = extendToFullLines r

-- Note [Removing imports is preferred]
-- ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
-- It's good to prefer the remove imports code action because an unused import
-- is likely to be removed and less likely the warning will be disabled.
-- Therefore actions to remove a single or all redundant imports should be
-- preferred, so that the client can prioritize them higher.
caRemoveRedundantImports :: Maybe ParsedModule -> Maybe T.Text -> [Diagnostic] -> Range -> Uri -> [Command |? CodeAction]
caRemoveRedundantImports m contents allDiags contextRange uri
  | Just pm <- m,
    r <- join $ map (\d -> repeat d `zip` suggestRemoveRedundantImport pm contents d) allDiags,
    allEdits <- [ e | (_, (_, edits)) <- r, e <- edits],
    caRemoveAll <- removeAll allEdits,
    ctxEdits <- [ x | x@(d, _) <- r, d `diagInRange` contextRange],
    not $ null ctxEdits,
    caRemoveCtx <- map (\(d, (title, tedit)) -> removeSingle title tedit d) ctxEdits
      = caRemoveCtx ++ [caRemoveAll]
  | otherwise = []
  where
    removeSingle title tedit diagnostic = mkCA title (Just CodeActionKind_QuickFix) Nothing [diagnostic] WorkspaceEdit{..} where
        _changes = Just $ M.singleton uri tedit
        _documentChanges = Nothing
        _changeAnnotations = Nothing
    removeAll tedit = InR $ CodeAction{..} where
        _changes = Just $ M.singleton uri tedit
        _title = "Remove all redundant imports"
        _kind = Just CodeActionKind_QuickFix
        _diagnostics = Nothing
        _documentChanges = Nothing
        _edit = Just WorkspaceEdit{..}
        -- See Note [Removing imports is preferred]
        _isPreferred = Just True
        _command = Nothing
        _disabled = Nothing
        _data_ = Nothing
        _changeAnnotations = Nothing

caRemoveInvalidExports :: Maybe ParsedModule -> Maybe T.Text -> [Diagnostic] -> Range -> Uri -> [Command |? CodeAction]
caRemoveInvalidExports m contents allDiags contextRange uri
  | Just pm <- m,
    Just txt <- contents,
    txt' <- indexedByPosition $ T.unpack txt,
    r <- mapMaybe (groupDiag pm) allDiags,
    r' <- map (\(t,d,rs) -> (t,d,extend txt' rs)) r,
    caRemoveCtx <- mapMaybe removeSingle r',
    allRanges <- nubOrd $ [ range | (_,_,ranges) <- r, range <- ranges],
    allRanges' <- extend txt' allRanges,
    Just caRemoveAll <- removeAll allRanges',
    ctxEdits <- [ x | x@(_, d, _) <- r, d `diagInRange` contextRange],
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
        _changes = Just $ M.singleton uri tedit
        _title = title
        _kind = Just CodeActionKind_QuickFix
        _diagnostics = Just [diagnostic]
        _documentChanges = Nothing
        _edit = Just WorkspaceEdit{..}
        _command = Nothing
        -- See Note [Removing imports is preferred]
        _isPreferred = Just True
        _disabled = Nothing
        _data_ = Nothing
        _changeAnnotations = Nothing
    removeAll [] = Nothing
    removeAll ranges = Just $ InR $ CodeAction{..} where
        tedit = concatMap (\r -> [TextEdit r ""]) ranges
        _changes = Just $ M.singleton uri tedit
        _title = "Remove all redundant exports"
        _kind = Just CodeActionKind_QuickFix
        _diagnostics = Nothing
        _documentChanges = Nothing
        _edit = Just WorkspaceEdit{..}
        _command = Nothing
        -- See Note [Removing imports is preferred]
        _isPreferred = Just True
        _disabled = Nothing
        _data_ = Nothing
        _changeAnnotations = Nothing

suggestRemoveRedundantExport :: ParsedModule -> Diagnostic -> Maybe (T.Text, [Range])
suggestRemoveRedundantExport ParsedModule{pm_parsed_source = L _ HsModule{..}} Diagnostic{..}
  | msg <- unifySpaces _message
  , Just export <- hsmodExports
  , Just exportRange <- getLocatedRange export
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
            Just _ | [lname] <- lnames -> Just (getLoc lname, True)
            Just idx ->
              let targetLname = getLoc $ lnames !! idx
                  startLoc = srcSpanStart targetLname
                  endLoc = srcSpanEnd targetLname
                  startLoc' = if idx == 0
                              then startLoc
                              else srcSpanEnd . getLoc $ lnames !! (idx - 1)
                  endLoc' = if idx == 0 && idx < length lnames - 1
                            then srcSpanStart . getLoc $ lnames !! (idx + 1)
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
        case grhssLocalBinds of
          (HsValBinds _ (ValBinds _ bag lsigs)) -> go bag lsigs
          _                                     -> []

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
    printExport ExportPattern x = "pattern " <> parenthesizeIfNeeds False x
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
--
    | Just [ty, lit] <- matchRegexUnifySpaces _message (pat False False True False)
                    <|> matchRegexUnifySpaces _message (pat False False False True)
                    <|> matchRegexUnifySpaces _message (pat False False False False)

            = codeEdit _range ty lit (makeAnnotatedLit ty lit)
    | Just source <- sourceOpt
    , Just [ty, lit, srcspan] <- matchRegexUnifySpaces _message (pat True True False False)
    , range <- case [ x | (x,"") <- readSrcSpan (T.unpack srcspan)] of
                 [s] -> let x = realSrcSpanToRange s
                   in x{_end = (_end x){_character = succ (_character (_end x))}}
                 _ -> error "bug in srcspan parser"
    = let lit' = makeAnnotatedLit ty lit;
          tir = textInRange range source
      in codeEdit range ty lit (T.replace lit lit' tir)
    | otherwise = []
    where
      makeAnnotatedLit ty lit = "(" <> lit <> " :: " <> ty <> ")"
#if MIN_VERSION_ghc(9,4,0)
      pat multiple at inArg inExpr = T.concat [ ".*Defaulting the type variable "
                                       , ".*to type ‘([^ ]+)’ "
                                       , "in the following constraint"
                                       , if multiple then "s" else " "
                                       , ".*arising from the literal ‘(.+)’"
                                       , if inArg then ".+In the.+argument" else ""
                                       , if at then ".+at ([^ ]*)" else ""
                                       , if inExpr then ".+In the expression" else ""
                                       , ".+In the expression"
                                       ]
#else
      pat multiple at inArg inExpr = T.concat [ ".*Defaulting the following constraint"
                                       , if multiple then "s" else ""
                                       , " to type ‘([^ ]+)’ "
                                       , ".*arising from the literal ‘(.+)’"
                                       , if inArg then ".+In the.+argument" else ""
                                       , if at then ".+at ([^ ]*)" else ""
                                       , if inExpr then ".+In the expression" else ""
                                       , ".+In the expression"
                                       ]
#endif
      codeEdit range ty lit replacement =
        let title = "Add type annotation ‘" <> ty <> "’ to ‘" <> lit <> "’"
            edits = [TextEdit range replacement]
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

suggestExtendImport :: ExportsMap -> ParsedSource -> Diagnostic -> [(T.Text, CodeActionKind, Rewrite)]
suggestExtendImport exportsMap (L _ HsModule {hsmodImports}) Diagnostic{_range=_range,..}
    | Just [binding, mod, srcspan] <-
      matchRegexUnifySpaces _message
#if MIN_VERSION_ghc(9,7,0)
      "Add ‘([^’]*)’ to the import list in the import of ‘([^’]*)’ *\\(at (.*)\\)\\."
#else
      "Perhaps you want to add ‘([^’]*)’ to the import list in the import of ‘([^’]*)’ *\\((.*)\\)\\."
#endif
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
          | let em = getExportsMap exportsMap
#if MIN_VERSION_ghc(9,7,0)
                match = mconcat $ lookupOccEnv_AllNameSpaces em (mkVarOrDataOcc binding)
#else
                match1 = lookupOccEnv em (mkVarOrDataOcc binding)
                match2 = lookupOccEnv em (mkTypeOcc binding)
          , Just match <- match1 <> match2
#endif
          -- Only for the situation that data constructor name is same as type constructor name,
          -- let ident with parent be in front of the one without.
          , sortedMatch <- sortBy (\ident1 ident2 -> parent ident2 `compare` parent ident1) (Set.toList match)
          , idents <- filter (\ident -> moduleNameText ident == mod && (canUseDatacon || not (isDatacon ident))) sortedMatch
          , (ident:_) <- idents -- Ensure fallback while `idents` is empty
          = Just ident

            -- fallback to using GHC suggestion even though it is not always correct
          | otherwise
          = Just IdentInfo
                { name = mkVarOrDataOcc binding
                , parent = Nothing
                , identModuleName  = mkModuleNameFS $ mkFastStringByteString $ T.encodeUtf8 mod}

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

-- | Suggests disambiguation for ambiguous symbols.
suggestImportDisambiguation ::
    DynFlags ->
    Maybe T.Text ->
    ParsedSource ->
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
        L _ HsModule {hsmodImports} = ps

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
        removeAllDuplicates = map NE.head . filter ((==1) . length) . NE.group . sort
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
                {- HLINT ignore suggestImportDisambiguation "Use nubOrd" -}
                -- TODO: The use of nub here is slow and maybe wrong for UnhelpfulLocation
                -- nubOrd can't be used since SrcSpan is intentionally no Ord
                , L _ qual <- nub $ mapMaybe (ideclAs . unLoc)
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
            ] ++ case targetsWithRestImports of
                    (m,ms):_ | local ->
                        let mode = HideOthers (m:ms)
                        in [( renderUniquify mode T.empty symbol True
                            , disambiguateSymbol ps fileContents diag symbol mode
                            )]
                    _ -> []

        renderUniquify HideOthers {} modName symbol local =
            "Use " <> (if local then "local definition" else modName) <> " for " <> symbol <> ", hiding other imports"
        renderUniquify (ToQualified _ qual) _ symbol _ =
            "Replace with qualified: "
                <> T.pack (moduleNameString qual)
                <> "."
                <> symbol
suggestImportDisambiguation _ _ _ _ _ = []

occursUnqualified :: T.Text -> ImportDecl GhcPs -> Bool
occursUnqualified symbol ImportDecl{..}
    | isNothing ideclAs = Just False /=
            -- I don't find this particularly comprehensible,
            -- but HLint suggested me to do so...
#if MIN_VERSION_ghc(9,5,0)
        (ideclImportList <&> \(isHiding, L _ ents) ->
            let occurs = any ((symbol `symbolOccursIn`) . unLoc) ents
            in (isHiding == EverythingBut) && not occurs || (isHiding == Exactly) && occurs
        )
#else
        (ideclHiding <&> \(isHiding, L _ ents) ->
            let occurs = any ((symbol `symbolOccursIn`) . unLoc) ents
            in isHiding && not occurs || not isHiding && occurs
        )
#endif
occursUnqualified _ _ = False

symbolOccursIn :: T.Text -> IE GhcPs -> Bool
symbolOccursIn symb = any ((== symb). printOutputable) . ieNames

targetModuleName :: ModuleTarget -> ModuleName
targetModuleName ImplicitPrelude{} = mkModuleName "Prelude"
targetModuleName (ExistingImp (L _ ImportDecl{..} :| _)) =
    unLoc ideclName

disambiguateSymbol ::
    ParsedSource ->
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

findImportDeclByRange :: [LImportDecl GhcPs] -> Range -> Maybe (LImportDecl GhcPs)
findImportDeclByRange xs range = find (\(L (locA -> l) _)-> srcSpanToRange l == Just range) xs

suggestFixConstructorImport :: Diagnostic -> [(T.Text, TextEdit)]
suggestFixConstructorImport Diagnostic{_range=_range,..}
    -- ‘Success’ is a data constructor of ‘Result’
    -- To import it use
    -- import Data.Aeson.Types( Result( Success ) )
    -- or
    -- import Data.Aeson.Types( Result(..) ) (lsp-ui)
    --
    -- On 9.8+
    --
    -- In the import of ‘ModuleA’:
    -- an item called ‘Constructor’
    -- is exported, but it is a data constructor of
    -- ‘A’.
  | Just [constructor, typ] <-
    matchRegexUnifySpaces _message
#if MIN_VERSION_ghc(9,7,0)
    "an item called ‘([^’]*)’ is exported, but it is a data constructor of ‘([^’]*)’"
#else
    "‘([^’]*)’ is a data constructor of ‘([^’]*)’ To import it use"
#endif
  = let fixedImport = typ <> "(" <> constructor <> ")"
    in [("Fix import of " <> fixedImport, TextEdit _range fixedImport)]
  | otherwise = []

suggestAddRecordFieldImport :: ExportsMap -> DynFlags -> ParsedSource -> T.Text -> Diagnostic -> [(T.Text, CodeActionKind, TextEdit)]
suggestAddRecordFieldImport exportsMap df ps fileContents Diagnostic {..}
  | Just fieldName <- findMissingField _message
  , Just (range, indent) <- newImportInsertRange ps fileContents
    = let qis = qualifiedImportStyle df
          suggestions = nubSortBy simpleCompareImportSuggestion (constructNewImportSuggestions exportsMap (Nothing, NotInScopeThing fieldName) Nothing qis)
      in map (\(ImportSuggestion _ kind (unNewImport -> imp)) -> (imp, kind, TextEdit range (imp <> "\n" <> T.replicate indent " "))) suggestions
  | otherwise = []
    where
      findMissingField :: T.Text -> Maybe T.Text
      findMissingField t =
        let
            hasfieldRegex = "((.+\\.)?HasField) \"(.+)\" ([^ ]+) ([^ ]+)"
            regex = "(No instance for|Could not deduce):? (\\(" <> hasfieldRegex <> "\\)|‘" <> hasfieldRegex <> "’|" <> hasfieldRegex <> ")"
            match = filter (/="") <$> matchRegexUnifySpaces t regex
        in case match of
               Just [_, _, _, _, fieldName, _, _] -> Just fieldName
               _                                  -> Nothing

-- | Suggests a constraint for a declaration for which a constraint is missing.
suggestConstraint :: DynFlags -> ParsedSource -> Diagnostic -> [(T.Text, Rewrite)]
suggestConstraint df ps diag@Diagnostic {..}
  | Just missingConstraint <- findMissingConstraint _message
  = let
#if MIN_VERSION_ghc(9,9,0)
        parsedSource = ps
#else
        parsedSource = makeDeltaAst ps
#endif
        codeAction = if _message =~ ("the type signature for:" :: String)
                        then suggestFunctionConstraint df parsedSource
                        else suggestInstanceConstraint df parsedSource
     in codeAction diag missingConstraint
  | otherwise = []
    where
      findMissingConstraint :: T.Text -> Maybe T.Text
      findMissingConstraint t =
        let -- The regex below can be tested at:
            --   https://regex101.com/r/dfSivJ/1
            regex = "(No instance for|Could not deduce):? (\\((.+)\\)|‘(.+)’|.+) arising from" -- a use of / a do statement

            match = matchRegexUnifySpaces t regex

            -- For a string like:
            --   "Could not deduce: ?a::() arising from"
            -- The `matchRegexUnifySpaces` function returns two empty match
            -- groups at the end of the list. It's not clear why this is the
            -- case, so we select the last non-empty match group.
            getCorrectGroup = last . filter (/="")

        in getCorrectGroup <$> match

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
        , Just (L _ (InstD _ (ClsInstD _ ClsInstDecl {cid_poly_ty = (unLoc -> HsSig{sig_body = hsib_body})})))
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
    Just (TypeSig _ _ HsWC {hswc_body = (unLoc -> HsSig {sig_body = hsib_body})})
      <- findSigOfDecl (== funId) hsmodDecls
    =
      [( "Add " <> implicitT <> " to the context of " <> T.pack (printRdrName funId)
        , appendConstraint (T.unpack implicitT) hsib_body)]
  | otherwise = []

findTypeSignatureName :: T.Text -> Maybe T.Text
findTypeSignatureName t = matchRegexUnifySpaces t "([^ ]+) :: " >>= listToMaybe

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
  , Just (TypeSig _ _ HsWC{hswc_body = (unLoc -> HsSig {sig_body = sig})})
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
#if MIN_VERSION_ghc(9,9,0)
removeRedundantConstraints df (L _ HsModule {hsmodDecls}) Diagnostic{..}
#else
removeRedundantConstraints df (makeDeltaAst -> L _ HsModule {hsmodDecls}) Diagnostic{..}
#endif
-- • Redundant constraint: Eq a
-- • In the type signature for:
--      foo :: forall a. Eq a => a -> a
-- • Redundant constraints: (Monoid a, Show a)
-- • In the type signature for:
--      foo :: forall a. (Num a, Monoid a, Eq a, Show a) => a -> Bool
  -- Account for both "Redundant constraint" and "Redundant constraints".
  | "Redundant constraint" `T.isInfixOf` _message
  , Just typeSignatureName <- findTypeSignatureName _message
  , Just (TypeSig _ _ HsWC{hswc_body = (unLoc -> HsSig {sig_body = sig})})
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
        >>= listToMaybe
        <&> parseConstraints

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

suggestNewOrExtendImportForClassMethod :: ExportsMap -> ParsedSource -> T.Text -> Diagnostic -> [(T.Text, CodeActionKind, [Either TextEdit Rewrite])]
suggestNewOrExtendImportForClassMethod packageExportsMap ps fileContents Diagnostic {_message}
  | Just [methodName, className] <-
      matchRegexUnifySpaces
        _message
        "‘([^’]*)’ is not a \\(visible\\) method of class ‘([^’]*)’",
    idents <-
      maybe [] (Set.toList . Set.filter (\x -> fmap occNameText (parent x) == Just className)) $
        lookupOccEnv (getExportsMap packageExportsMap) (mkVarOrDataOcc methodName) =
    mconcat $ suggest <$> idents
  | otherwise = []
  where
    suggest identInfo
      | importStyle <- NE.toList $ importStyles identInfo,
        mImportDecl <- findImportDeclByModuleName (hsmodImports . unLoc $ ps) (T.unpack moduleText) =
        case mImportDecl of
          -- extend
          Just decl ->
            [ ( "Add " <> renderImportStyle style <> " to the import list of " <> moduleText,
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
            [ (quickFixImportKind' "new" style, newUnqualImport moduleText rendered False)
              | style <- importStyle,
                let rendered = renderImportStyle style
            ]
              <> [(quickFixImportKind "new.all", newImportAll moduleText)]
            | otherwise -> []
        where moduleText = moduleNameText identInfo

suggestNewImport :: DynFlags -> ExportsMap -> ParsedSource -> T.Text -> Diagnostic -> [(T.Text, CodeActionKind, TextEdit)]
suggestNewImport df packageExportsMap ps fileContents Diagnostic{..}
  | msg <- unifySpaces _message
  , Just thingMissing <- extractNotInScopeName msg
  , qual <- extractQualifiedModuleName msg
  , qual' <-
      extractDoesNotExportModuleName msg
        >>= (findImportDeclByModuleName hsmodImports . T.unpack)
        >>= ideclAs . unLoc
        <&> T.pack . moduleNameString . unLoc
  , -- tentative workaround for detecting qualification in GHC 9.4
    -- FIXME: We can delete this after dropping the support for GHC 9.4
    qualGHC94 <-
        guard (ghcVersion == GHC94)
            *> extractQualifiedModuleNameFromMissingName (extractTextInRange _range fileContents)
  , Just (range, indent) <- newImportInsertRange ps fileContents
  , extendImportSuggestions <- matchRegexUnifySpaces msg
#if MIN_VERSION_ghc(9,7,0)
    "Add ‘[^’]*’ to the import list in the import of ‘([^’]*)’"
#else
    "Perhaps you want to add ‘[^’]*’ to the import list in the import of ‘([^’]*)’"
#endif
  = let qis = qualifiedImportStyle df
        -- FIXME: we can use thingMissing once the support for GHC 9.4 is dropped.
        -- In what fllows, @missing@ is assumed to be qualified name.
        -- @thingMissing@ is already as desired with GHC != 9.4.
        -- In GHC 9.4, however, GHC drops a module qualifier from a qualified symbol.
        -- Thus we need to explicitly concatenate qualifier explicity in GHC 9.4.
        missing
            | GHC94 <- ghcVersion
            , isNothing (qual <|> qual')
            , Just q <- qualGHC94 =
                qualify q thingMissing
            | otherwise = thingMissing
        suggestions = nubSortBy simpleCompareImportSuggestion
          (constructNewImportSuggestions packageExportsMap (qual <|> qual' <|> qualGHC94, missing) extendImportSuggestions qis) in
    map (\(ImportSuggestion _ kind (unNewImport -> imp)) -> (imp, kind, TextEdit range (imp <> "\n" <> T.replicate indent " "))) suggestions
  where
    qualify q (NotInScopeDataConstructor d) = NotInScopeDataConstructor (q <> "." <> d)
    qualify q (NotInScopeTypeConstructorOrClass d) = NotInScopeTypeConstructorOrClass (q <> "." <> d)
    qualify q (NotInScopeThing d) = NotInScopeThing (q <> "." <> d)

    L _ HsModule {..} = ps
suggestNewImport _ _ _ _ _ = []

{- |
Extracts qualifier of the symbol from the missing symbol.
Input must be either a plain qualified variable or possibly-parenthesized qualified binary operator (though no strict checking is done for symbol part).
This is only needed to alleviate the issue #3473.

FIXME: We can delete this after dropping the support for GHC 9.4

>>> extractQualifiedModuleNameFromMissingName "P.lookup"
Just "P"

>>> extractQualifiedModuleNameFromMissingName "ΣP3_'.σlookup"
Just "\931P3_'"

>>> extractQualifiedModuleNameFromMissingName "ModuleA.Gre_ekσ.goodδ"
Just "ModuleA.Gre_ek\963"

>>> extractQualifiedModuleNameFromMissingName "(ModuleA.Gre_ekσ.+)"
Just "ModuleA.Gre_ek\963"

>>> extractQualifiedModuleNameFromMissingName "(ModuleA.Gre_ekσ..|.)"
Just "ModuleA.Gre_ek\963"

>>> extractQualifiedModuleNameFromMissingName "A.B.|."
Just "A.B"
-}
extractQualifiedModuleNameFromMissingName :: T.Text -> Maybe T.Text
extractQualifiedModuleNameFromMissingName (T.strip -> missing)
    = T.pack <$> (T.unpack missing RE.=~ qualIdentP)
    where
        {-
        NOTE: Haskell 2010 allows /unicode/ upper & lower letters
        as a module name component; otoh, regex-tdfa only allows
        /ASCII/ letters to be matched with @[[:upper:]]@ and/or @[[:lower:]]@.
        Hence we use regex-applicative(-text) for finer-grained predicates.

        RULES (from [Section 10 of Haskell 2010 Report](https://www.haskell.org/onlinereport/haskell2010/haskellch10.html)):
            modid	→	{conid .} conid
            conid	→	large {small | large | digit | ' }
            small	→	ascSmall | uniSmall | _
            ascSmall	→	a | b | … | z
            uniSmall	→	any Unicode lowercase letter
            large	→	ascLarge | uniLarge
            ascLarge	→	A | B | … | Z
            uniLarge	→	any uppercase or titlecase Unicode letter
        -}

        qualIdentP = parensQualOpP <|> qualVarP
        parensQualOpP = RE.sym '(' *> modNameP <* RE.sym '.' <* RE.anySym <* RE.few RE.anySym <* RE.sym ')'
        qualVarP = modNameP <* RE.sym '.' <* RE.some RE.anySym
        conIDP = RE.withMatched $
            RE.psym isUpper
            *> RE.many
                (RE.psym $ \c -> c == '\'' || c == '_' || isUpper c || isLower c || isDigit c)
        modNameP = fmap snd $ RE.withMatched $ conIDP `sepBy1` RE.sym '.'


constructNewImportSuggestions
  :: ExportsMap -> (Maybe T.Text, NotInScope) -> Maybe [T.Text] -> QualifiedImportStyle -> [ImportSuggestion]
constructNewImportSuggestions exportsMap (qual, thingMissing) notTheseModules qis = nubOrdBy simpleCompareImportSuggestion
  [ suggestion
  | Just name <- [T.stripPrefix (maybe "" (<> ".") qual) $ notInScope thingMissing] -- strip away qualified module names from the unknown name
  , identInfo <- maybe [] Set.toList $ lookupOccEnv (getExportsMap exportsMap) (mkVarOrDataOcc name)
                                    <> lookupOccEnv (getExportsMap exportsMap) (mkTypeOcc name) -- look up the modified unknown name in the export map
  , canUseIdent thingMissing identInfo                                              -- check if the identifier information retrieved can be used
  , moduleNameText identInfo `notElem` fromMaybe [] notTheseModules                 -- check if the module of the identifier is allowed
  , suggestion <- renderNewImport identInfo                                         -- creates a list of import suggestions for the retrieved identifier information
  ]
 where
  renderNewImport :: IdentInfo -> [ImportSuggestion]
  renderNewImport identInfo
    | Just q <- qual
    = [ImportSuggestion importanceScore (quickFixImportKind "new.qualified") (newQualImport m q qis)]
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
  = compare s2 s1 <> compare i1 i2

newtype NewImport = NewImport {unNewImport :: T.Text}
  deriving (Show, Eq, Ord)

newImportToEdit :: NewImport -> ParsedSource -> T.Text -> Maybe (T.Text, TextEdit)
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
newImportInsertRange :: ParsedSource -> T.Text -> Maybe (Range, Int)
newImportInsertRange ps fileContents
  |  Just ((l, c), col) <- case hsmodImports of
      -- When there is no existing imports, we only cares about the line number, setting column and indent to zero.
      [] -> (\line -> ((line, 0), 0)) <$> findPositionNoImports ps fileContents
      _  -> findPositionFromImports hsmodImports last
  , let insertPos = Position (fromIntegral l) (fromIntegral c)
    = Just (Range insertPos insertPos, col)
  | otherwise = Nothing
  where
    L _ HsModule {..} = ps

-- | Find the position for a new import when there isn't an existing one.
-- * If there is a module declaration, a new import should be inserted under the module declaration (including exports list)
-- * Otherwise, a new import should be inserted after any file-header pragma.
findPositionNoImports :: ParsedSource -> T.Text -> Maybe Int
findPositionNoImports ps fileContents =
    maybe (Just (findNextPragmaPosition fileContents)) (findPositionAfterModuleName ps) hsmodName
  where
    L _ HsModule {..} = ps

-- | find line number right after module ... where
findPositionAfterModuleName :: ParsedSource
                            -> LocatedA ModuleName
                            -> Maybe Int
findPositionAfterModuleName ps _hsmodName' = do
    -- Note that 'where' keyword and comments are not part of the AST. They belongs to
    -- the exact-print information. To locate it, we need to find the previous AST node,
    -- calculate the gap between it and 'where', then add them up to produce the absolute
    -- position of 'where'.

    lineOffset <- whereKeywordLineOffset -- Calculate the gap before 'where' keyword.
#if MIN_VERSION_ghc(9,9,0)
    pure lineOffset
#else
    -- The last AST node before 'where' keyword. Might be module name or export list.
    let prevSrcSpan = maybe (getLoc _hsmodName') getLoc hsmodExports
    case prevSrcSpan of
        UnhelpfulSpan _ -> Nothing
        (RealSrcSpan prevSrcSpan' _) ->
            -- add them up produce the absolute location of 'where' keyword
            Just $ srcLocLine (realSrcSpanEnd prevSrcSpan') + lineOffset
#endif
  where
    L _ HsModule {..} = ps

    -- The relative position of 'where' keyword (in lines, relative to the previous AST node).
    -- The exact-print API changed a lot in ghc-9.2, so we need to handle it separately for different compiler versions.
    whereKeywordLineOffset :: Maybe Int
#if MIN_VERSION_ghc(9,5,0)
    whereKeywordLineOffset = case hsmodAnn hsmodExt of
#else
    whereKeywordLineOffset = case hsmodAnn of
#endif
        EpAnn _ annsModule _ -> do
            -- Find the first 'where'
            whereLocation <- listToMaybe . mapMaybe filterWhere $ am_main annsModule
            epaLocationToLine whereLocation
#if !MIN_VERSION_ghc(9,9,0)
        EpAnnNotUsed -> Nothing
#endif
    filterWhere (AddEpAnn AnnWhere loc) = Just loc
    filterWhere _                       = Nothing

    epaLocationToLine :: EpaLocation -> Maybe Int
#if MIN_VERSION_ghc(9,9,0)
    epaLocationToLine (EpaSpan sp)
      = fmap (srcLocLine . realSrcSpanEnd) $ srcSpanToRealSrcSpan sp
#elif MIN_VERSION_ghc(9,5,0)
    epaLocationToLine (EpaSpan sp _)
      = Just . srcLocLine . realSrcSpanEnd $ sp
#else
    epaLocationToLine (EpaSpan sp)
      = Just . srcLocLine . realSrcSpanEnd $ sp
#endif
    epaLocationToLine (EpaDelta (SameLine _) priorComments) = Just $ sumCommentsOffset priorComments
    -- 'priorComments' contains the comments right before the current EpaLocation
    -- Summing line offset of priorComments is necessary, as 'line' is the gap between the last comment and
    -- the current AST node
    epaLocationToLine (EpaDelta (DifferentLine line _) priorComments) = Just (line + sumCommentsOffset priorComments)

    sumCommentsOffset :: [LEpaComment] -> Int
#if MIN_VERSION_ghc(9,9,0)
    sumCommentsOffset = sum . fmap (\(L anchor _) -> anchorOpLine anchor)
#else
    sumCommentsOffset = sum . fmap (\(L anchor _) -> anchorOpLine (anchor_op anchor))
#endif

#if MIN_VERSION_ghc(9,9,0)
    anchorOpLine :: EpaLocation' a -> Int
    anchorOpLine EpaSpan{}                           = 0
    anchorOpLine (EpaDelta (SameLine _) _)           = 0
    anchorOpLine (EpaDelta (DifferentLine line _) _) = line
#else
    anchorOpLine :: AnchorOperation -> Int
    anchorOpLine UnchangedAnchor                      = 0
    anchorOpLine (MovedAnchor (SameLine _))           = 0
    anchorOpLine (MovedAnchor (DifferentLine line _)) = line
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
  -> Maybe (T.Text, QualifiedImportStyle) -- ^ qualified name and style
  -> Bool -- ^ the symbol is to be imported or hidden
  -> NewImport
newImport modName mSymbol mQualNameStyle hiding = NewImport impStmt
  where
     symImp
            | Just symbol <- mSymbol
              , symOcc <- mkVarOcc $ T.unpack symbol =
              " (" <> printOutputable (parenSymOcc symOcc $ ppr symOcc) <> ")"
            | otherwise = ""
     impStmt =
       "import "
         <> qualifiedModName (snd <$> mQualNameStyle)
         <> (if hiding then " hiding" else "")
         <> symImp
         <> maybe "" (\qual -> if modName == qual then "" else " as " <> qual) mQual
     mQual = fst <$> mQualNameStyle
     qualifiedModName Nothing                       = modName
     qualifiedModName (Just QualifiedImportPrefix)  = "qualified " <> modName
     qualifiedModName (Just QualifiedImportPostfix) = modName <> " qualified"


newQualImport :: T.Text -> T.Text -> QualifiedImportStyle -> NewImport
newQualImport modName qual qis = newImport modName Nothing (Just (qual, qis)) False

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
#if MIN_VERSION_ghc(9,4,0)
    matchRegexUnifySpaces x "the module ‘([^’]*)’ does not export"
      <|> matchRegexUnifySpaces x "nor ‘([^’]*)’ export"
#else
    matchRegexUnifySpaces x "Module ‘([^’]*)’ does not export"
      <|> matchRegexUnifySpaces x "nor ‘([^’]*)’ exports"
#endif
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
#if MIN_VERSION_ghc(9,5,0)
rangesForBindingImport ImportDecl{
  ideclImportList = Just (Exactly, L _ lies)
  } b =
    concatMap (mapMaybe srcSpanToRange . rangesForBinding' b') lies
  where
    b' = wrapOperatorInParens b
#else
rangesForBindingImport ImportDecl{
  ideclHiding = Just (False, L _ lies)
  } b =
    concatMap (mapMaybe srcSpanToRange . rangesForBinding' b') lies
  where
    b' = wrapOperatorInParens b
#endif
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
    b' = wrapOperatorInParens $ unqualify b
    ranges'
        ( L
            _
            ( IEThingWith
                _
                thing
                _
                inners
#if MIN_VERSION_ghc(9,9,0)
                _
#endif
            )
        )
      | T.unpack (printOutputable thing) == b' = []
      | otherwise =
          [ locA l' | L l' x <- inners, T.unpack (printOutputable x) == b']
    ranges' _ = []

rangesForBinding' :: String -> LIE GhcPs -> [SrcSpan]
#if MIN_VERSION_ghc(9,9,0)
rangesForBinding' b (L (locA -> l) (IEVar _ nm _))
#else
rangesForBinding' b (L (locA -> l) (IEVar _ nm))
#endif
  | L _ (IEPattern _ (L _ b')) <- nm
  , T.unpack (printOutputable b') == b
  = [l]
rangesForBinding' b (L (locA -> l) x@IEVar{})
  | T.unpack (printOutputable x) == b = [l]
rangesForBinding' b (L (locA -> l) x@IEThingAbs{}) | T.unpack (printOutputable x) == b = [l]
#if MIN_VERSION_ghc(9,9,0)
rangesForBinding' b (L (locA -> l) (IEThingAll _ x _))
#else
rangesForBinding' b (L (locA -> l) (IEThingAll _ x))
#endif
  | T.unpack (printOutputable x) == b = [l]
#if MIN_VERSION_ghc(9,9,0)
rangesForBinding' b (L (locA -> l) (IEThingWith _ thing _  inners _))
#else
rangesForBinding' b (L (locA -> l) (IEThingWith _ thing _  inners))
#endif
    | T.unpack (printOutputable thing) == b = [l]
    | otherwise =
        [ locA l' | L l' x <- inners, T.unpack (printOutputable x) == b]
rangesForBinding' _ _ = []

-- | 'allMatchRegex' combined with 'unifySpaces'
allMatchRegexUnifySpaces :: T.Text -> T.Text -> Maybe [[T.Text]]
allMatchRegexUnifySpaces message =
    allMatchRegex (unifySpaces message)

-- | Returns Just (all matches) for the first capture, or Nothing.
allMatchRegex :: T.Text -> T.Text -> Maybe [[T.Text]]
allMatchRegex message regex = message =~~ regex


-- functions to help parse multiple import suggestions

-- | Returns the first match if found
regexSingleMatch :: T.Text -> T.Text -> Maybe T.Text
regexSingleMatch msg regex = case matchRegexUnifySpaces msg regex of
    Just (h:_) -> Just h
    _          -> Nothing

-- | Process a list of (module_name, filename:src_span) values
--
-- Eg. [(Data.Map, app/ModuleB.hs:2:1-18), (Data.HashMap.Strict, app/ModuleB.hs:3:1-29)]
regExImports :: T.Text -> Maybe [(T.Text, T.Text)]
regExImports msg
    | Just mods' <- allMatchRegex msg "‘([^’]*)’"
    , Just srcspans' <- allMatchRegex msg
    -- This regex has to be able to deal both with single-line srcpans like "(/path/to/File.hs:2:1-18)"
    -- as well as multi-line srcspans like "(/path/to/File.hs:(3,1)-(5,2))"
#if MIN_VERSION_ghc(9,7,0)
                          "\\(at ([^:]+:[^ ]+)\\)"
#else
                          "\\(([^:]+:[^ ]+)\\)"
#endif
    , mods <- [mod | [_,mod] <- mods']
    , srcspans <- [srcspan | [_,srcspan] <- srcspans']
      -- check we have matching pairs like (Data.Map, (app/src.hs:1:2-18))
    , let result = if length mods == length srcspans then
                   Just (zip mods srcspans) else Nothing
    = result
    | otherwise = Nothing

matchRegExMultipleImports :: T.Text -> Maybe (T.Text, [(T.Text, T.Text)])
matchRegExMultipleImports message = do
#if MIN_VERSION_ghc(9,7,0)
  let pat = T.pack "Add ‘([^’]*)’ to one of these import lists: *(‘.*\\))$"
#else
  let pat = T.pack "Perhaps you want to add ‘([^’]*)’ to one of these import lists: *(‘.*\\))$"
#endif
  (binding, imports) <- case matchRegexUnifySpaces message pat of
                            Just [x, xs] -> Just (x, xs)
                            _            -> Nothing
  imps <- regExImports imports
  return (binding, imps)
