{-# LANGUAGE CPP                   #-}
{-# LANGUAGE DeriveAnyClass        #-}
{-# LANGUAGE DerivingStrategies    #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE PatternSynonyms       #-}
{-# LANGUAGE RecordWildCards       #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE ViewPatterns          #-}

{-# OPTIONS -Wno-orphans #-}

module Ide.Plugin.Retrie (descriptor, Log) where

import           Control.Concurrent.STM                (readTVarIO)
import           Control.Exception.Safe                (Exception (..),
                                                        SomeException, assert,
                                                        catch, throwIO, try)
import           Control.Lens.Operators
import           Control.Monad                         (filterM, forM, unless,
                                                        when)
import           Control.Monad.Error.Class             (MonadError (throwError))
import           Control.Monad.IO.Class                (MonadIO (liftIO))
import           Control.Monad.Trans.Class             (MonadTrans (lift))
import           Control.Monad.Trans.Except            (ExceptT (..),
                                                        runExceptT)
import           Data.Maybe                            (fromMaybe, isNothing)

import           Control.Monad.Trans.Maybe             (MaybeT, hoistMaybe,
                                                        runMaybeT)
import           Data.Aeson                            (FromJSON (..),
                                                        ToJSON (..))
import           Data.Bifunctor                        (second)
import qualified Data.ByteString                       as BS
import           Data.Data
import           Data.Either                           (partitionEithers)
import           Data.Foldable                         (toList)
import           Data.Hashable                         (unhashed)
import qualified Data.HashSet                          as Set
import           Data.List.Extra                       (sortOn)
import qualified Data.Map                              as Map
import           Data.Monoid                           (First (First), getFirst)
import qualified Data.Text                             as T
import qualified Data.Text.Encoding                    as T
import qualified Data.Text.Utf16.Rope.Mixed            as Rope
import           Development.IDE                       hiding (pluginHandlers)
import           Development.IDE.Core.Actions          (lookupMod)
import           Development.IDE.Core.PluginUtils
import           Development.IDE.Core.PositionMapping
import           Development.IDE.Core.Shake            (ShakeExtras (ShakeExtras, knownTargetsVar),
                                                        getShakeExtras,
                                                        hiedbWriter,
                                                        toKnownFiles, withHieDb)
import           Development.IDE.GHC.Compat            (GRHSs (GRHSs),
                                                        GenLocated (L), GhcPs,
                                                        GhcRn,
                                                        HsBindLR (FunBind),
                                                        HsExpr, HsGroup (..),
                                                        HsValBindsLR (..),
                                                        HscEnv, ImportDecl (..),
                                                        LHsExpr, LRuleDecls,
                                                        ModSummary (ModSummary, ms_hspp_buf, ms_mod),
                                                        Outputable,
                                                        ParsedModule,
                                                        RuleDecl (HsRule),
                                                        RuleDecls (HsRules),
                                                        TyClDecl (SynDecl),
                                                        TyClGroup (..), fun_id,
                                                        locA, ms_hspp_opts,
                                                        nameOccName, occNameFS,
                                                        pattern RealSrcSpan,
                                                        pm_parsed_source,
                                                        rds_rules, topDir,
                                                        unLoc, unLocA)
import qualified Development.IDE.GHC.Compat            as GHC
import           Development.IDE.GHC.Compat.Util       hiding (catch, toList,
                                                        try)
import           Development.IDE.GHC.ExactPrint        (GetAnnotatedParsedSource (GetAnnotatedParsedSource),
                                                        TransformT)
import           Development.IDE.Plugin.CodeAction     (newImportInsertRange)
import           Development.IDE.Spans.AtPoint         (LookupModule,
                                                        nameToLocation)
import           Development.IDE.Types.Shake           (WithHieDb)
import qualified GHC                                   as GHCGHC
import qualified GHC.Data.EnumSet                      as EnumSet
import           GHC.Generics                          (Generic)
import           GHC.Iface.Ext.Types                   (BindType (RegularBind),
                                                        ContextInfo (ValBind),
                                                        identInfo)
import qualified GHC.LanguageExtensions.Type           as LangExt (Extension (..))
import           Ide.Plugin.Error                      (PluginError (..),
                                                        getNormalizedFilePathE)
import           Ide.Plugin.Resolve                    (mkCodeActionWithResolveAndCommand)
import           Ide.PluginUtils
import           Ide.Types
import qualified Language.LSP.Protocol.Lens            as L
import           Language.LSP.Protocol.Message         as LSP
import           Language.LSP.Protocol.Types           as LSP
import           Language.LSP.Server                   (ProgressCancellable (Cancellable))
import           Retrie                                (Annotated (astA),
                                                        AnnotatedModule,
                                                        Options, Options_ (..),
                                                        Verbosity (Loud), apply,
                                                        applyWithUpdate,
                                                        applyWithUpdateAndStrategy,
                                                        topDown)
import           Retrie.Context
import           Retrie.CPP                            (CPP (NoCPP), parseCPP,
                                                        printCPP)
import           Retrie.ExactPrint                     (AnnotatedImports, fix,
                                                        makeDeltaAst,
                                                        transformA, unsafeMkA)
import           Retrie.Expr                           (mkLocatedHsVar)
import           Retrie.Fixity                         (FixityEnv, mkFixityEnv)
import           Retrie.Monad                          (Retrie, getGroundTerms,
                                                        runRetrie)
import           Retrie.Options                        (defaultOptions,
                                                        getTargetFiles)
import           Retrie.Replace                        (Change (..),
                                                        Replacement (..))
import           Retrie.Rewrites
import           Retrie.Rewrites.Function              (matchToRewrites)
import           Retrie.Substitution                   (HoleVal (HoleExpr),
                                                        foldSubst)
import           Retrie.SYB                            (GenericQ, everything,
                                                        extQ, mkQ)
import           Retrie.Types
import           Retrie.Universe                       (Universe)

import           Development.IDE.GHC.Compat.ExactPrint (exactPrint)

import           Ide.Plugin.Retrie.Imports

data Log
  = LogParsingModule FilePath
  | forall a. Pretty a => LogResolve a

instance Pretty Log where
  pretty = \case
    LogParsingModule fp -> "Parsing module:" <+> pretty fp
    LogResolve l        -> pretty l

-- | Parameters for the runRetrie rewrite.
data RunRetrieParams = RunRetrieParams
  { description     :: T.Text,
    spec            :: RewriteSpec,
    originatingFile :: Uri
  }
  deriving (Eq, Show, Generic, FromJSON, ToJSON)

data RunRetrieInlineThisParams = RunRetrieInlineThisParams
  { inlineIntoThisLocation :: !Location,
    inlineFromThisLocation :: !Location,
    inlineThisDefinition   :: !T.Text
  }
  deriving (Eq, Show, Generic, FromJSON, ToJSON)

-- | Data passed back from the client as part of the resolve stage.
data RetrieResolveData
  = ResolveRunRetrie RunRetrieParams
  | ResolveInlineThis RunRetrieInlineThisParams
  deriving (Eq, Show, Generic, FromJSON, ToJSON)

descriptor :: Recorder (WithPriority Log) -> PluginId -> PluginDescriptor IdeState
descriptor recorder plId =
  (defaultPluginDescriptor plId "Provides code actions to inline Haskell definitions")
    { pluginHandlers = handlers
    , pluginCommands = commands
    }
  where
    (commands, handlers) =
      mkCodeActionWithResolveAndCommand
        (cmapWithPrio LogResolve recorder)
        plId
        provider
        (resolveProvider recorder)

-- | We receive a resolve request when the user has selected a code action in
-- the UI. The client passes back the 'RetrieResolveData' we attached to the
-- code action so we can figure out what transformation to apply.
resolveProvider :: Recorder (WithPriority Log) -> ResolveFunction IdeState RetrieResolveData Method_CodeActionResolve
resolveProvider recorder state _plId ca _uri = \case
  ResolveRunRetrie params  -> resolveRunRetrie recorder state ca params
  ResolveInlineThis params -> resolveInlineThis recorder state ca params

resolveRunRetrie
  :: Recorder (WithPriority Log)
  -> IdeState
  -> CodeAction
  -> RunRetrieParams
  -> ExceptT PluginError (HandlerM Config) CodeAction
resolveRunRetrie recorder state ca RunRetrieParams{originatingFile = uri, ..} = ExceptT $
  pluginWithIndefiniteProgress description Nothing Cancellable $ \_updater -> runExceptT $ do
    nfp <- getNormalizedFilePathE uri
    (session, _) <-
        runActionE "Retrie.GhcSessionDeps" state $
            useWithStaleE GhcSessionDeps
            nfp
    (_, binds, _, _, _) <- runActionE "Retrie.getBinds" state $ getBinds nfp
    (errors, edits) <- liftIO $
        callRetrie
            recorder
            state
            (hscEnv session)
            spec
            nfp
            binds
    unless (null errors) $
        lift $ pluginSendNotification SMethod_WindowShowMessage $
                ShowMessageParams MessageType_Warning $
                T.unlines $
                    "## Found errors during rewrite:" :
                    ["-" <> T.pack (show e) | e <- errors]
    return $ ca & L.edit ?~ edits

resolveInlineThis
  :: Recorder (WithPriority Log)
  -> IdeState
  -> CodeAction
  -> RunRetrieInlineThisParams
  -> ExceptT PluginError (HandlerM Config) CodeAction
resolveInlineThis recorder state ca RunRetrieInlineThisParams{..} = ExceptT $ do
  outcome <- runExceptT $ runExceptT $ do
    nfp <- getNormalizedFilePathE $ getLocationUri inlineIntoThisLocation
    nfpSource <- getNormalizedFilePathE $ getLocationUri inlineFromThisLocation
    -- What we do here:
    --   Find the identifier in the given position
    --   Construct an inline rewrite for it
    --   Run retrie on the identifier in the given position only
    --   Attach the edit to the code action
    astSrc <- runActionE "retrie" state $
        useE GetAnnotatedParsedSource nfpSource
    let fromRange = rangeToRealSrcSpan nfpSource $ getLocationRange inlineFromThisLocation
        intoRange = rangeToRealSrcSpan nfp $ getLocationRange inlineIntoThisLocation
        -- on the binder itself, inline every use
        site = if intoRange == fromRange then Nothing else Just intoRange
    check <- runActionE "retrie" state $ useE TypeCheck nfpSource
    checktgt <- runActionE "retrie" state $ useE TypeCheck nfp
    sourceFixities <- fixityEnvFor <$> runActionE "retrie" state (useE GetFixity nfpSource)
    targetFixities <- fixityEnvFor <$> runActionE "retrie" state (useE GetFixity nfp)
    (fun_id, fun_matches) <-
      maybe (throwError PluginStaleResolve) pure $
        findBinder fromRange (tmrRenamed check)
    when (isNothing site && referencesName (unLoc fun_id) fun_matches) $
      refuse $ "Not inlined: " <> inlineThisDefinition <> " is recursive"
    imports <- either (refuse . T.pack) pure $
      extractInlineImports check checktgt fun_matches
    let
        annotatedImports = renderImports imports
        uri = filePathToUri $ fromNormalizedFilePath nfp
    fixedSrc <- liftIO $ transformA (unsafeMkA (makeDeltaAst astSrc) 0) (fix sourceFixities)
    inlineRewrite <- liftIO $
      constructInlineFromIdentifer annotatedImports fixedSrc fromRange
    when (null inlineRewrite) $ throwError $ PluginInternalError "Empty rewrite"
    (session, _) <- runActionE "retrie" state $
      useWithStaleE GhcSessionDeps nfp
    (cpp, ps, contents) <- liftIO $ getCPPmodule recorder state (hscEnv session) targetFixities $ fromNormalizedFilePath nfp
    let rewrites = maybe inlineRewrite (\s -> map (headAt s) inlineRewrite) site
        inline
          | GHCGHC.isExternalName (unLoc fun_id) =
              applyAt site updateContext rewrites
          | otherwise = applyLocal site fromRange rewrites
    result <- liftIO $ try @_ @SomeException $
        runRetrie (sourceFixities <> targetFixities) inline cpp
    case result of
        Left err -> throwError $ PluginInternalError $ "Retrie - crashed with: " <> T.pack (show err)
        -- a refused site is not an error: leave the action without an edit
        Right (_,_,NoChange) ->
            refuse "Not inlined: the call site did not match the definition, or a binding there would capture one of its variables"
        Right (_,cpp',change) -> do
            let edits = asEditMap $ asTextEdits uri ps contents cpp cpp' change
                wedit = WorkspaceEdit (Just edits) Nothing Nothing
            return $ ca & L.edit ?~ wedit
  case outcome of
    Left msg -> do
      -- warn and don't make any edits
      pluginSendNotification SMethod_WindowShowMessage $
        ShowMessageParams MessageType_Warning msg
      return (Right ca)
    Right resolved -> return resolved
  where
    refuse :: T.Text -> ExceptT PluginError (ExceptT T.Text (HandlerM Config)) a
    refuse = lift . throwError


-- | Finds the binder at the provided span.
findBinder
  :: GHCGHC.RealSrcSpan
  -> GHC.RenamedSource
  -> Maybe ( GHC.LIdP GhcRn, GHCGHC.MatchGroup GhcRn (LHsExpr GhcRn) )
findBinder originSpan =
    getFirst <$> everything (<>) (First Nothing `mkQ` matcher)
  where
    matcher
      :: HsBindLR GhcRn GhcRn
      -> First ( GHC.LIdP GhcRn, GHCGHC.MatchGroup GhcRn (LHsExpr GhcRn) )
    matcher FunBind{fun_id, fun_matches}
      | RealSrcSpan sp _ <- GHC.getLocA fun_id
      , sp == originSpan =
        First $ Just (fun_id, fun_matches)
    matcher _ = First Nothing

-- | Whether a definition's body uses the name it defines.
referencesName :: GHC.Name -> GHCGHC.MatchGroup GhcRn (LHsExpr GhcRn) -> Bool
referencesName name = everything (||) (False `mkQ` uses)
  where
    uses :: GRHSs GhcRn (LHsExpr GhcRn) -> Bool
    uses = everything (||) (False `mkQ` (== name))

-- | Only install rewrites when the target subtree has been reached.
applyLocal :: Maybe GHCGHC.RealSrcSpan -> GHCGHC.RealSrcSpan -> [Rewrite Universe] -> Retrie ()
applyLocal site defSpan rewrites = applyAt site updater moduleRewrites
  where
    -- retrie will exit early if there are no rewrites, so just stub them out
    -- at the top level.
    moduleRewrites = map (setRewriteTransformer $ \_ _ -> pure NoMatch) rewrites

    updater :: ContextUpdater
    updater c i x = do
        c' <- updateContext c i x
        pure $ if bindsDefinition i x then install c' else c'

    -- attach the rewrites at this subtree
    install c = c
      { ctxtRewriter =
          foldMap (mkLocalRewriter (ctxtInScope c)) rewrites <> ctxtRewriter c }

    -- does this node bind the target definition for it's ith child?
    bindsDefinition :: Int -> GenericQ Bool
    bindsDefinition i = ownBinds `extQ` stmts `extQ` guards
      where
        stmts :: [GHC.ExprLStmt GhcPs] -> Bool
        stmts (L _ s : _) = ownBinds s
        stmts []          = False

        guards :: GHCGHC.GRHS GhcPs (LHsExpr GhcPs) -> Bool
        guards (GHCGHC.GRHS _ gs _) = i == body && any (ownBinds . unLoc) gs
          where body = 2 -- after the extension field and the guard list

    ownBinds :: GenericQ Bool
    ownBinds = or . gmapQ (False `mkQ` definesIt)

    definesIt :: GHC.HsLocalBinds GhcPs -> Bool
    definesIt (GHC.HsValBinds _ (ValBinds _ binds _)) =
        or [ sp == defSpan
           | L _ FunBind{fun_id} <- toList binds
           , RealSrcSpan sp _ <- [GHC.getLocA fun_id] ]
    definesIt _ = False

applyAt :: Maybe GHCGHC.RealSrcSpan -> ContextUpdater -> [Rewrite Universe] -> Retrie ()
applyAt (Just site) update = applyWithUpdate (atSite site update)
applyAt Nothing update     = applyWithUpdateAndStrategy update topDown

-- | In the case of nested matches, such as @f (f x)@, the cursor on the inner
-- @f@ matches both the inner and outer applications. If a match occurs within
-- an argument hole then map it to 'NoMatch' so the traversal decends into the
-- inner match.
headAt :: GHCGHC.RealSrcSpan -> Rewrite Universe -> Rewrite Universe
headAt site q = q { qResult = second (\transform c -> fmap refuse . transform c) (qResult q) }
  where
    refuse (MatchResult sub _)
      | foldSubst (\(_, v) found -> found || holds v) False sub = NoMatch
    refuse m = m

    holds (HoleExpr e) = RealSrcSpan site Nothing `GHC.isSubspanOf` GHC.getLocA (astA e)
    holds _            = False

-- | Drop rewriters outside of the target span.
atSite :: GHCGHC.RealSrcSpan -> ContextUpdater -> ContextUpdater
atSite site update c i x = narrow <$> update c i x
  where
    narrow c'
      | gmapQi i holdsSite x = c'
      | otherwise = c' { ctxtRewriter = mempty }

    -- anything but an expression can still lead to the site
    holdsSite :: GenericQ Bool
    holdsSite = True `mkQ` inExpr

    inExpr :: LHsExpr GhcPs -> Bool
    inExpr e = RealSrcSpan site Nothing `GHC.isSubspanOf` GHC.getLocA e

-------------------------------------------------------------------------------

provider :: PluginMethodHandler IdeState Method_TextDocumentCodeAction
provider state _plId (CodeActionParams _ _ (TextDocumentIdentifier uri) range ca) = do
  let (LSP.CodeActionContext _diags _monly _) = ca
  nfp <- getNormalizedFilePathE uri

  (ModSummary{ms_mod}, topLevelBinds, posMapping, hs_ruleds, hs_tyclds)
    <- runActionE "retrie" state $
        getBinds nfp

  extras@ShakeExtras{ withHieDb, hiedbWriter } <- liftIO $ runAction "" state getShakeExtras

  range <- fromCurrentRangeE posMapping range
  let pos = range ^. L.start
  let rewrites =
        concatMap (suggestBindRewrites uri pos ms_mod) topLevelBinds
          ++ concatMap (suggestRuleRewrites uri pos ms_mod) hs_ruleds
          ++ [ r
               | TyClGroup {group_tyclds} <- hs_tyclds,
                 L (locA -> l) g <- group_tyclds,
                 pos `isInsideSrcSpan` l,
                 r <- suggestTypeRewrites uri ms_mod g
             ]

  let retrieActions =
        [ mkCodeAction title kind (ResolveRunRetrie params)
        | (title, kind, params) <- rewrites
        ]

  inlineSuggestions <- liftIO $ runIdeAction "" extras $
    suggestBindInlines uri topLevelBinds range withHieDb (lookupMod hiedbWriter)
  let inlineActions =
        [ mkCodeAction title CodeActionKind_RefactorInline (ResolveInlineThis params)
        | (title, params) <- inlineSuggestions
        ]
  return $ InL [InR c | c <- retrieActions ++ inlineActions]

-- | Creates a 'CodeAction' for the client to display.
mkCodeAction :: T.Text -> CodeActionKind -> RetrieResolveData -> CodeAction
mkCodeAction title kind resolveData =
  CodeAction
    { _title       = title
    , _kind        = Just kind
    , _diagnostics = Nothing
    , _isPreferred = Nothing
    , _disabled    = Nothing
    , _edit        = Nothing
    , _command     = Nothing
    , _data_       = Just (toJSON resolveData)
    }

getLocationUri :: Location -> Uri
getLocationUri Location{_uri} = _uri

getLocationRange :: Location -> Range
getLocationRange Location{_range} = _range

getBinds :: NormalizedFilePath -> ExceptT PluginError Action
  ( ModSummary
  , [HsBindLR GhcRn GhcRn]
  , PositionMapping
  , [LRuleDecls GhcRn]
  , [TyClGroup GhcRn]
  )
getBinds nfp = do
  (tm, posMapping) <- useWithStaleE TypeCheck nfp
  -- we use the typechecked source instead of the parsed source
  -- to be able to extract module names from the Ids,
  -- so that we can include adding the required imports in the retrie command
  let rn = tmrRenamed tm
  case rn of
#if MIN_VERSION_ghc(9,9,0)
    (HsGroup{hs_valds, hs_ruleds, hs_tyclds}, _, _, _, _) -> do
#else
    (HsGroup{hs_valds, hs_ruleds, hs_tyclds}, _, _, _) -> do
#endif
      topLevelBinds <- case hs_valds of
        ValBinds{} -> throwError $ PluginInternalError "getBinds: ValBinds not supported"
        XValBindsLR (GHC.NValBinds binds _sigs :: GHC.NHsValBindsLR GhcRn) ->
          pure
            [ decl
#if MIN_VERSION_ghc(9,11,0)
            | (_, listBinds) <- binds
            , L _ decl <- listBinds
#else
            | (_, bagBinds) <- binds
            , L _ decl <- bagToList bagBinds
#endif
            ]
      return (tmrModSummary tm, topLevelBinds, posMapping, hs_ruleds, hs_tyclds)

suggestBindRewrites ::
  Uri ->
  Position ->
  GHC.Module ->
  HsBindLR GhcRn GhcRn ->
  [(T.Text, CodeActionKind, RunRetrieParams)]
suggestBindRewrites originatingFile pos ms_mod FunBind {fun_id = L (locA -> l') rdrName}
  | pos `isInsideSrcSpan` l' =
    let pprNameText = printOutputable rdrName
        pprName = T.unpack pprNameText
        unfoldRewrite =
            let spec = Unfold (qualify ms_mod pprName)
                description = "Unfold " <> pprNameText
            in (description, CodeActionKind_RefactorInline, RunRetrieParams {..})
        foldRewrite =
          let spec = Fold (qualify ms_mod pprName)
              description = "Fold " <> pprNameText
           in (description, CodeActionKind_RefactorExtract, RunRetrieParams {..})
     in [unfoldRewrite, foldRewrite]
suggestBindRewrites _ _ _ _ = []

  -- find all the identifiers in the AST for which have source definitions
suggestBindInlines ::
    Uri
    -> [HsBindLR GhcRn GhcRn]
    -> Range
    -> WithHieDb
    -> (FilePath -> GHCGHC.ModuleName -> GHCGHC.Unit -> Bool -> MaybeT IdeAction Uri)
    -> IdeAction [(T.Text, RunRetrieInlineThisParams)]
suggestBindInlines _uri binds range hie lookupMod = do
    identifiers <- definedIdentifiers
    return $ map (\(name, siteLoc, srcLoc) ->
        let
            title = "Inline " <> printedName
            printedName = printOutputable name
            params = RunRetrieInlineThisParams
                { inlineIntoThisLocation = siteLoc
                , inlineFromThisLocation = srcLoc
                , inlineThisDefinition= printedName
                }
        in (title, params)
        )
        (Set.toList identifiers)
    where
      definedIdentifiers =
        -- we search for candidates to inline in RHSs only, skipping LHSs
        everything (<>) (pure mempty `mkQ` getGRHSIdentifierDetails hie lookupMod) binds

      getGRHSIdentifierDetails ::
        WithHieDb
        -> (FilePath -> GHCGHC.ModuleName -> GHCGHC.Unit -> Bool -> MaybeT IdeAction Uri)
        -> GRHSs GhcRn (LHsExpr GhcRn)
        -> IdeAction (Set.HashSet (GHC.OccName, Location, Location))
      getGRHSIdentifierDetails a b it@GRHSs{} =
        -- we only select candidates for which we have source code
        everything (<>) (pure mempty `mkQ` getDefinedIdentifierDetailsViaHieDb a b `extQ` getLocalBinderDetails) it

      -- a local binder is its own definition: inline it into every use
      getLocalBinderDetails :: HsBindLR GhcRn GhcRn -> IdeAction (Set.HashSet (GHC.OccName, Location, Location))
      getLocalBinderDetails FunBind{fun_id}
        | Just defLoc <- srcSpanToLocation (GHC.getLocA fun_id)
        , range `isSubrangeOf` getLocationRange defLoc =
            pure $ Set.singleton (nameOccName (unLoc fun_id), defLoc, defLoc)
      getLocalBinderDetails _ = pure mempty

#if MIN_VERSION_ghc(9,13,0)
      getDefinedIdentifierDetailsViaHieDb :: WithHieDb -> LookupModule IdeAction -> GHCGHC.LIdOccP GhcRn -> IdeAction (Set.HashSet (GHC.OccName, Location, Location))
#else
      getDefinedIdentifierDetailsViaHieDb :: WithHieDb -> LookupModule IdeAction -> GHC.LIdP GhcRn -> IdeAction (Set.HashSet (GHC.OccName, Location, Location))
#endif
      getDefinedIdentifierDetailsViaHieDb withHieDb lookupModule lname
        | name <- GHCGHC.getName (unLoc lname) =
        case srcSpanToLocation (GHC.getLocA lname) of
            Just siteLoc
              | siteRange <- getLocationRange siteLoc
              , range `isSubrangeOf` siteRange -> do
                    srcLocations <- nameToLocation withHieDb lookupModule name
                    equations <- filterM (equationBound name) (fromMaybe [] srcLocations)
                    return $ Set.fromList (map (nameOccName name, siteLoc,) equations)
            _ -> pure mempty

-- | Whether name is defined by a regular ValBind (i.e. it's not a class
-- method, field, constructor, pattern synonym, etc).
equationBound :: GHC.Name -> Location -> IdeAction Bool
equationBound name loc = fmap (fromMaybe False) $ runMaybeT $ do
    defFile <- hoistMaybe $
      uriToNormalizedFilePath $ toNormalizedUri $ getLocationUri loc
    (HAR{refMap}, _) <- useWithStaleFastMT GetHieAst defFile
    -- occurrences of the name in the defining module
    let occs = Map.findWithDefault [] (Right name) refMap
    pure $ any (any isEquation . identInfo . snd) occs
  where
    isEquation (ValBind RegularBind _ _) = True
    isEquation _                         = False

suggestTypeRewrites ::
  Uri ->
  GHC.Module ->
  TyClDecl GhcRn ->
  [(T.Text, CodeActionKind, RunRetrieParams)]
suggestTypeRewrites originatingFile ms_mod SynDecl {tcdLName} =
    let pprNameText = printOutputable (unLocA tcdLName)
        pprName = T.unpack pprNameText
        unfoldRewrite =
            let spec = TypeForward (qualify ms_mod pprName)
                description = "Unfold " <> pprNameText
           in (description, CodeActionKind_RefactorInline, RunRetrieParams {..})
        foldRewrite =
          let spec = TypeBackward (qualify ms_mod pprName)
              description = "Fold " <> pprNameText
           in (description, CodeActionKind_RefactorExtract, RunRetrieParams {..})
     in [unfoldRewrite, foldRewrite]
suggestTypeRewrites _ _ _ = []

suggestRuleRewrites ::
  Uri ->
  Position ->
  GHC.Module ->
  LRuleDecls GhcRn ->
  [(T.Text, CodeActionKind, RunRetrieParams)]
suggestRuleRewrites originatingFile pos ms_mod (L _ HsRules {rds_rules}) =
    concat
        [ [ forwardRewrite   ruleName
          , backwardsRewrite ruleName
          ]
        | L (locA -> l) r  <- rds_rules,
          pos `isInsideSrcSpan` l,
          let HsRule {rd_name = L _ rn} = r,
          let ruleName = unpackFS rn
      ]
  where
    forwardRewrite ruleName =
        let spec = RuleForward (qualify ms_mod ruleName)
            description = "Apply rule " <> T.pack ruleName <> " forward"

        in ( description,
            CodeActionKind_Refactor,
            RunRetrieParams {..}
            )
    backwardsRewrite ruleName =
          let spec = RuleBackward (qualify ms_mod ruleName)
              description = "Apply rule " <> T.pack ruleName <> " backwards"
           in ( description,
                CodeActionKind_Refactor,
                RunRetrieParams {..}
              )

qualify :: Outputable mod => mod -> String -> String
qualify ms_mod x = T.unpack (printOutputable ms_mod) <> "." <> x

-------------------------------------------------------------------------------
-- Retrie driving code

data CallRetrieError
  = CallRetrieInternalError String NormalizedFilePath
  | NoParse NormalizedFilePath
  | GHCParseError NormalizedFilePath String
  | NoTypeCheck NormalizedFilePath
  | CannotImport NormalizedFilePath String
  deriving (Eq)

instance Show CallRetrieError where
  show (CallRetrieInternalError msg f) = msg <> " - " <> fromNormalizedFilePath f
  show (NoParse f) = "Cannot parse: " <> fromNormalizedFilePath f
  show (GHCParseError f m) = "Cannot parse " <> fromNormalizedFilePath f <> " : " <> m
  show (NoTypeCheck f) = "File does not typecheck: " <> fromNormalizedFilePath f
  show (CannotImport f m) = "Cannot add import to " <> fromNormalizedFilePath f <> " : " <> m

instance Exception CallRetrieError

-- TODO(simon):
-- * Load TypeCheck for each impacted file
-- * We need accurate typecheck results for name resolution etc
-- * extractImports needs to know the GlobalRdrEnv to reject adding clashing imports
-- * it also looks up the definition binder to figure out what names need importing

callRetrie ::
  Recorder (WithPriority Log) ->
  IdeState ->
  HscEnv ->
  RewriteSpec ->
  NormalizedFilePath ->
  [HsBindLR GhcRn GhcRn] ->
  IO ([CallRetrieError], WorkspaceEdit)
callRetrie recorder state session spec origin binds = do
  knownFiles <- toKnownFiles . unhashed <$> liftIO (readTVarIO (knownTargetsVar $ shakeExtras state))
  let
      -- TODO cover all workspaceFolders
      target = "."

      retrieOptions :: Retrie.Options
      retrieOptions = (defaultOptions target)
        {Retrie.verbosity = Loud
        ,Retrie.targetFiles = map fromNormalizedFilePath $
            Set.toList knownFiles
        }

      -- (theImports, theRewrites) = partitionEithers rewrites

      -- annotatedImports =
      --   unsafeMkA (map (noLocA . toImportDecl) theImports) 0

  tcd <- useOrFail state "retrie.check" NoTypeCheck TypeCheck origin
  originFixities <- fixityEnvForFile state origin
  originPm <- useOrFail state "Retrie.GetParsedModuleWithComments" NoParse GetParsedModuleWithComments origin
  originParsedModule <- transformA (fixAnns originPm) (fix originFixities)
  -- TODO(simon): It would be better to compute imports per-clause.
  rewrites <- parseSpecs state origin originParsedModule originFixities [spec]
  let retrie = apply rewrites

  targets <- getTargetFiles retrieOptions (getGroundTerms retrie)

  results <- forM targets $ \t -> runExceptT $ do
    let nt = toNormalizedFilePath' (toAbsolute (rootDir state) t)
        uri = filePathToUri $ fromNormalizedFilePath nt
    tct <- ExceptT $ try $ useOrFail state "retrie.check" NoTypeCheck TypeCheck nt
    targetFixities <- ExceptT $ try $ fixityEnvForFile state nt
    (cpp, pm, contents) <- ExceptT $ try $
      getCPPmodule recorder state session targetFixities t
    imps <- either (throwError . CannotImport nt) pure $
      extractImports tcd tct binds spec
    (_, cpp', change) <- lift $ runRetrie (originFixities <> targetFixities) retrie cpp
    return $ asTextEdits uri pm contents cpp cpp' $ case change of
      NoChange              -> NoChange
      Change replacements _ -> Change replacements [imps]

  let (errors :: [CallRetrieError], replacements) = partitionEithers results
      editParams :: WorkspaceEdit
      editParams =
        WorkspaceEdit (Just $ asEditMap $ concat replacements) Nothing Nothing

  return (errors, editParams)

useOrFail ::
  IdeRule r v =>
  IdeState ->
  String ->
  (NormalizedFilePath -> CallRetrieError) ->
  r ->
  NormalizedFilePath ->
  IO (RuleResult r)
useOrFail state lbl mkException rule f =
  useRule lbl state rule f >>= maybe (liftIO $ throwIO $ mkException f) return

fixityEnvFor :: FixityMap -> FixityEnv
fixityEnvFor (FixityMap fixities) =
  -- retrie keys fixities by the operator's unqualified name
  mkFixityEnv
    [ (fs, (fs, fixity))
    | (n, fixity) <- Map.toList fixities
    , let fs = occNameFS (nameOccName n)
    ]

fixityEnvForFile :: IdeState -> NormalizedFilePath -> IO FixityEnv
fixityEnvForFile state f =
  useRuleBlocking "Retrie.GetFixity" state GetFixity f
    >>= maybe (throwIO $ NoTypeCheck f) (return . fixityEnvFor)

fixAnns :: ParsedModule -> Annotated GHC.ParsedSource
fixAnns GHC.ParsedModule{pm_parsed_source} = unsafeMkA (makeDeltaAst pm_parsed_source) 0

parseSpecs
  :: IdeState
  -> NormalizedFilePath
  -> AnnotatedModule
  -> FixityEnv
  -> [RewriteSpec]
  -> IO [Rewrite Universe]
parseSpecs state origin originParsedModule originFixities specs = do
  -- retrie needs the libdir for `parseRewriteSpecs`
  libdir <- topDir . ms_hspp_opts . msrModSummary <$> useOrFail state "Retrie.GetModSummary" (CallRetrieInternalError "file not found") GetModSummary origin
  parseRewriteSpecs
    libdir
    (\_f -> return $ NoCPP originParsedModule)
    originFixities
    specs

constructfromFunMatches ::
  Annotated [GHCGHC.LocatedA (ImportDecl GhcPs)]
  -> GHCGHC.LocatedN GHCGHC.RdrName
  -> GHCGHC.MatchGroup GhcPs (GHCGHC.LocatedA (HsExpr GhcPs))
  -> TransformT IO [Rewrite Universe]
constructfromFunMatches imps fun_id fun_matches = do
    fe <- mkLocatedHsVar fun_id
    rewrites <- concat <$>
        forM (unLoc $ GHC.mg_alts fun_matches) (matchToRewrites fe imps LeftToRight)
    let urewrites = toURewrite <$> rewrites
    -- traceShowM $ map showQuery urewrites
    assert (not $ null urewrites) $
        return urewrites

-- showQuery :: Rewrite Universe -> String
-- showQuery = ppRewrite
--
-- showQuery :: Rewrite (LHsExpr GhcPs) -> String
-- showQuery q = unlines
--     [ "template: " <> show (hash (printOutputable . showAstData NoBlankSrcSpan . astA . tTemplate . fst . qResult $ q))
--     , "quantifiers: " <> show (hash (T.pack (show(Ext.toList $ qQuantifiers q))))
--     , "matcher: " <> show (hash (printOutputable . showAstData NoBlankSrcSpan . astA . qPattern $ q))
--     ]
--
-- s :: Data a => a -> String
-- s = T.unpack . printOutputable . showAstData NoBlankSrcSpan
--         NoBlankEpAnnotations

constructInlineFromIdentifer :: Data a => AnnotatedImports -> Annotated (GenLocated l a) -> GHCGHC.RealSrcSpan -> IO [Rewrite Universe]
constructInlineFromIdentifer imports originParsedModule originSpan = do
    -- traceM $ s $ astA originParsedModule
    fmap astA $ transformA originParsedModule $ \(L _ m) -> do
        let ast = everything (<>) (First Nothing `mkQ` matcher) m
            matcher :: HsBindLR GhcPs GhcPs
                -> First ( GHCGHC.LocatedN GHCGHC.RdrName
                         , GHCGHC.MatchGroup GhcPs (GHCGHC.LocatedA (HsExpr GhcPs))
                         )
            matcher FunBind{fun_id, fun_matches}
                -- trace (show (GHC.getLocA fun_id) <> ": " <> s fun_id) False = undefined
                | RealSrcSpan sp _ <- GHC.getLocA fun_id
                , sp == originSpan =
                First $ Just (fun_id, fun_matches)
            matcher _ = First Nothing
        case ast of
            First (Just (fun_id, fun_matches))
              -> constructfromFunMatches imports fun_id fun_matches
            _ -> return $ error "could not find source code to inline"

-- | Group 'TextEdit's by file.
asEditMap :: [(Uri, TextEdit)] -> Map.Map Uri [TextEdit]
asEditMap =
  Map.map (sortOn (^. L.range . L.start))
    . Map.fromListWith (++)
    . map (second pure)

asTextEdits
  :: Uri
  -> GHCGHC.ParsedSource
  -> T.Text
  -> CPP AnnotatedModule
  -> CPP AnnotatedModule
  -> Change
  -> [(Uri, TextEdit)]
asTextEdits _ _ _ _ _ NoChange = []
asTextEdits _ _ _ _ _ (Change [] _) = []
asTextEdits target ps contents before after (Change reps imports) =
  importTextEdits target ps contents imports <>
  map (target,) (rewriteTextEdits before after reps)

rewriteTextEdits
  :: CPP AnnotatedModule
  -> CPP AnnotatedModule
  -> [Replacement]
  -> [TextEdit]
rewriteTextEdits before after reps =
  makeDiffTextEdit
    (T.pack $ printCPP [] before)
    (T.pack $ printCPP reps after)

importTextEdits
  :: Uri -> GHCGHC.ParsedSource -> T.Text -> [AnnotatedImports] -> [(Uri, TextEdit)]
importTextEdits uri ps contents imp
  | not (null imp)
  , Just (range, indent) <- newImportInsertRange ps contents
  = let print = [(T.pack . exactPrint) i | i <- concatMap astA imp]
        sep   = "\n" <> T.replicate indent " "
        text  = T.concat [ p <> sep | p <- print ]
        edit  = TextEdit range text
    in [(uri, edit)]
importTextEdits _ _ _ _ = []

-------------------------------------------------------------------------------
-- Rule wrappers

useRuleBlocking,
  _useRuleStale,
  useRule ::
    (IdeRule k v) =>
    String ->
    IdeState ->
    k ->
    NormalizedFilePath ->
    IO (Maybe (RuleResult k))
useRuleBlocking label state rule f = runAction label state (use rule f)
_useRuleStale label state rule f =
  fmap fst
    <$> runIdeAction label (shakeExtras state) (useWithStaleFast rule f)

-- | Chosen approach for calling ghcide Shake rules
useRule label = _useRuleStale ("Retrie." <> label)

-------------------------------------------------------------------------------
-- Serialization wrappers and instances

deriving instance Eq RewriteSpec

deriving instance Show RewriteSpec

deriving instance Generic RewriteSpec

deriving instance FromJSON RewriteSpec

deriving instance ToJSON RewriteSpec

-- | Parsed source of a file, reassociated with the given fixities.
getCPPmodule :: Recorder (WithPriority Log) -> IdeState -> HscEnv -> FixityEnv -> FilePath -> IO (CPP AnnotatedModule, GHCGHC.ParsedSource, T.Text)
getCPPmodule recorder state session fixities t = do
    -- TODO: is it safe to drop this makeAbsolute?
    let nt = toNormalizedFilePath' $ (toAbsolute $ rootDir state) t
    let parseWith contents = do
          modSummary <- msrModSummary <$>
            useOrFail state "Retrie.GetModSummary" (CallRetrieInternalError "file not found") GetModSummary nt
          let ms' =
                modSummary
                  { ms_hspp_buf =
                      Just (stringToStringBuffer contents)
                    -- keep comments, as GetParsedModuleWithComments does
                  , ms_hspp_opts =
                      ms_hspp_opts modSummary
                        `GHC.gopt_set` GHC.Opt_KeepRawTokenStream
                        `GHC.gopt_unset` GHC.Opt_Haddock
                  }
          logWith recorder Info $ LogParsingModule t
          parsed <- evalGhcEnv session (GHCGHC.parseModule ms')
              `catch` \e -> throwIO (GHCParseError nt (show @SomeException e))
          transformA (fixAnns parsed) (fix fixities)

    contents <- do
      mbContentsVFS <-
        runAction "Retrie.GetFileContents" state $ getFileContents nt
      case mbContentsVFS of
        Just contents -> return $ Rope.toText contents
        Nothing       -> T.decodeUtf8 <$> BS.readFile (fromNormalizedFilePath nt)
    pm <- useOrFail state "Retrie.GetParsedModuleWithComments" NoParse GetParsedModuleWithComments nt
    let
      ps = GHC.pm_parsed_source pm
      extensions = GHC.extensionFlags (GHCGHC.ms_hspp_opts (GHCGHC.pm_mod_summary pm))
    cpp <- if LangExt.Cpp `EnumSet.member` extensions
      then parseCPP parseWith contents
      else NoCPP <$> transformA (fixAnns pm) (fix fixities)
    return (cpp, ps, contents)
