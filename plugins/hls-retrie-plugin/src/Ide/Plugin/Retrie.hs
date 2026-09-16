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
import           Control.Monad                         (forM, unless, when)
import           Control.Monad.Error.Class             (MonadError (throwError))
import           Control.Monad.IO.Class                (MonadIO (liftIO))
import           Control.Monad.Trans.Class             (MonadTrans (lift))
import           Control.Monad.Trans.Except            (ExceptT (..),
                                                        runExceptT)

import           Control.Monad.Trans.Maybe             (MaybeT)
import           Data.Aeson                            (FromJSON (..),
                                                        ToJSON (..))
import           Data.Bifunctor                        (second)
import qualified Data.ByteString                       as BS
import           Data.Data
import           Data.Either                           (fromRight,
                                                        partitionEithers)
import           Data.Hashable                         (unhashed)
import qualified Data.HashSet                          as Set
import           Data.IORef.Extra                      (atomicModifyIORef'_,
                                                        newIORef, readIORef)
import           Data.List.Extra                       (nubOrdOn)
import qualified Data.Map                              as Map
import           Data.Monoid                           (First (First), getFirst)
import           Data.String                           (IsString)
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
                                                        HsExpr (HsApp, OpApp),
                                                        HsGroup (..),
                                                        HsValBindsLR (..),
                                                        HscEnv, ImportDecl (..),
                                                        LHsExpr, LRuleDecls,
                                                        Match, ModIface,
                                                        ModSummary (ModSummary, ms_hspp_buf, ms_mod),
                                                        Outputable,
                                                        ParsedModule,
                                                        RuleDecl (HsRule),
                                                        RuleDecls (HsRules),
                                                        TyClDecl (SynDecl),
                                                        TyClGroup (..), fun_id,
                                                        locA, mi_fixities,
                                                        ms_hspp_opts,
                                                        nameOccName, occNameFS,
                                                        pattern RealSrcSpan,
                                                        pm_parsed_source,
                                                        rds_rules, srcSpanFile,
                                                        topDir, unLoc, unLocA)
import qualified Development.IDE.GHC.Compat            as GHC
import           Development.IDE.GHC.Compat.Util       hiding (catch, try)
import           Development.IDE.GHC.ExactPrint        (GetAnnotatedParsedSource (GetAnnotatedParsedSource),
                                                        TransformT)
import           Development.IDE.Plugin.CodeAction     (newImportInsertRange)
import           Development.IDE.Spans.AtPoint         (LookupModule,
                                                        nameToLocation)
import           Development.IDE.Types.Shake           (WithHieDb)
import qualified GHC                                   as GHCGHC
import           GHC.Generics                          (Generic)
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
                                                        Fixity (Fixity),
                                                        FixityDirection (InfixL),
                                                        Options, Options_ (..),
                                                        Verbosity (Loud), apply,
                                                        applyWithUpdate)
import           Retrie.Context
import           Retrie.CPP                            (CPP (NoCPP), parseCPP)
import           Retrie.ExactPrint                     (AnnotatedImports, fix,
                                                        makeDeltaAst,
                                                        transformA, unsafeMkA)
import           Retrie.Expr                           (mkLocatedHsVar)
import           Retrie.Fixity                         (FixityEnv, lookupOp,
                                                        mkFixityEnv)
import           Retrie.Monad                          (getGroundTerms,
                                                        runRetrie)
import           Retrie.Options                        (defaultOptions,
                                                        getTargetFiles)
import           Retrie.Replace                        (Change (..),
                                                        Replacement (..))
import           Retrie.Rewrites
import           Retrie.Rewrites.Function              (matchToRewrites)
import           Retrie.SYB                            (everything, extQ, mkQ)
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
  { description               :: T.Text,
    spec                      :: RewriteSpec,
    originatingFile           :: Uri
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
resolveInlineThis recorder state ca RunRetrieInlineThisParams{..} = do
    nfp <- getNormalizedFilePathE $ getLocationUri inlineIntoThisLocation
    nfpSource <- getNormalizedFilePathE $ getLocationUri inlineFromThisLocation
    -- What we do here:
    --   Find the identifier in the given position
    --   Construct an inline rewrite for it
    --   Run retrie to get a list of changes
    --   Select the change that inlines the identifier in the given position
    --   Attach the edit to the code action
    astSrc <- runActionE "retrie" state $
        useE GetAnnotatedParsedSource nfpSource
    let fromRange = rangeToRealSrcSpan nfpSource $ getLocationRange inlineFromThisLocation
        intoRange = rangeToRealSrcSpan nfp $ getLocationRange inlineIntoThisLocation
    check <- runActionE "retrie" state $ useE TypeCheck nfpSource
    checktgt <- runActionE "retrie" state $ useE TypeCheck nfp
    (_, fun_matches) <-
      maybe (throwError PluginStaleResolve) pure $
        findBinder fromRange (tmrRenamed check)
    imports <- either (throwError . PluginInternalError . T.pack) pure $
      extractInlineImports check checktgt fun_matches
    let
        annotatedImports = renderImports imports
    inlineRewrite <- liftIO $
      constructInlineFromIdentifer annotatedImports (unsafeMkA astSrc 0) fromRange
    when (null inlineRewrite) $ throwError $ PluginInternalError "Empty rewrite"
    (session, _) <- runActionE "retrie" state $
      useWithStaleE GhcSessionDeps nfp
    (fixityEnv, cpp, ps, contents) <- liftIO $ getCPPmodule recorder state (hscEnv session) $ fromNormalizedFilePath nfp
    result <- liftIO $ try @_ @SomeException $
        runRetrie fixityEnv (applyWithUpdate myContextUpdater inlineRewrite) cpp
    case result of
        Left err -> throwError $ PluginInternalError $ "Retrie - crashed with: " <> T.pack (show err)
        Right (_,_,NoChange) -> throwError $ PluginInternalError "Retrie - inline produced no changes"
        Right (_,_,Change replacements imports) -> do
            let edits = asEditMap $ asTextEdits nfp ps contents $ Change ourReplacement imports
                wedit = WorkspaceEdit (Just edits) Nothing Nothing
                ourReplacement = [ r
                    | r@Replacement{..} <- replacements
                    , RealSrcSpan intoRange Nothing `GHC.isSubspanOf` replLocation]
            return $ ca & L.edit ?~ wedit

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

-- Override to skip adding binders to the context, which prevents inlining
-- nested defined functions
myContextUpdater :: ContextUpdater
myContextUpdater c i =
    updateContext c i
    `extQ` (return . updExp)
    `extQ` (skipUpdate @(GRHSs GhcPs (LHsExpr GhcPs)))
    `extQ` (skipUpdate @(Match GhcPs (LHsExpr GhcPs)))
  where
    skipUpdate :: forall a m . Monad m => a -> TransformT m Context
    skipUpdate _ = pure c

    -- override to skip the HsLet case
    updExp :: HsExpr GhcPs -> Context
    updExp HsApp{} =
#if MIN_VERSION_ghc(9,11,0)
      c { ctxtParentPrec = HasPrec $ Retrie.Fixity (10 + i - firstChild) InfixL }
#else
      c { ctxtParentPrec = HasPrec $ Retrie.Fixity (SourceText "HsApp") (10 + i - firstChild) InfixL }
#endif
    -- Reason for 10 + i: (i is index of child, 0 = left, 1 = right)
    -- In left child, prec is 10, so HsApp child will NOT get paren'd
    -- In right child, prec is 11, so every child gets paren'd (unless atomic)
    updExp (OpApp _ _ op _) = c { ctxtParentPrec = HasPrec $ lookupOp op (ctxtFixityEnv c) }
    updExp _ = c { ctxtParentPrec = NeverParen }
    -- Deal with Trees-That-Grow adding extension points
    -- as the first child everywhere.
    firstChild :: Int
    firstChild = 1

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
        everything (<>) (pure mempty `mkQ` getDefinedIdentifierDetailsViaHieDb a b) it

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
                    mbSrcLocation <- nameToLocation withHieDb lookupModule name
                    return $ maybe mempty (Set.fromList . map (nameOccName name, siteLoc,)) mbSrcLocation
            _ -> pure mempty

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
  deriving (Eq)

instance Show CallRetrieError where
  show (CallRetrieInternalError msg f) = msg <> " - " <> fromNormalizedFilePath f
  show (NoParse f) = "Cannot parse: " <> fromNormalizedFilePath f
  show (GHCParseError f m) = "Cannot parse " <> fromNormalizedFilePath f <> " : " <> m
  show (NoTypeCheck f) = "File does not typecheck: " <> fromNormalizedFilePath f

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
  (originFixities, originParsedModule) <- reuseParsedModule state origin
  -- TODO(simon): It would be better to compute imports per-clause.
  rewrites <- parseSpecs state origin originParsedModule originFixities [spec]
  let retrie = apply rewrites

  targets <- getTargetFiles retrieOptions (getGroundTerms retrie)

  results <- forM targets $ \t -> runExceptT $ do
    (fixityEnv, cpp, pm, contents) <- ExceptT $ try $
      getCPPmodule recorder state session t
    let nt = toNormalizedFilePath' (toAbsolute (rootDir state) t)
    tct <- ExceptT $ try $ useOrFail state "retrie.check" NoTypeCheck TypeCheck nt
    -- FIXME(simon): raise the error properly
    let imps = fromRight mempty $ extractImports tcd tct binds spec
    (_user, _ast, change) <- lift $ runRetrie fixityEnv retrie cpp
    return $ asTextEdits nt pm contents $ case change of
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

fixityEnvFromModIface :: ModIface -> FixityEnv
fixityEnvFromModIface modIface =
  mkFixityEnv
    [ (fs, (fs, fixity))
      | (n, fixity) <- mi_fixities modIface,
        let fs = occNameFS n
    ]

fixFixities :: Data ast =>
  IdeState
  -> NormalizedFilePath
  -> Annotated ast
  -> IO (FixityEnv, Annotated ast)
fixFixities state f pm = do
      ModIfaceResult{hirModIface} <-
        useOrFail state "GetModIface" NoTypeCheck GetModIface f
      let fixities = fixityEnvFromModIface hirModIface
      res <- transformA pm (fix fixities)
      return (fixities, res)

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

asEditMap :: [(Uri, TextEdit)] -> Map.Map Uri [TextEdit]
asEditMap = Map.fromListWith (++) . map (second pure)

asTextEdits :: NormalizedFilePath -> GHCGHC.ParsedSource -> T.Text -> Change -> [(Uri, TextEdit)]
asTextEdits _ _ _ NoChange = []
asTextEdits _ _ _ (Change [] _) = []
asTextEdits target ps contents (Change reps imports) =
  replacementTextEdits reps <>
  importTextEdits target ps contents imports

replacementTextEdits :: [Replacement] -> [(Uri, TextEdit)]
replacementTextEdits reps =
  [ (filePathToUri spanLoc, edit)
  | Replacement{..} <- nubOrdOn (realSpan . replLocation) reps
  , (RealSrcSpan rspan _) <- [replLocation]
  , let spanLoc = unpackFS $ srcSpanFile rspan
  , let edit = TextEdit (realSrcSpanToRange rspan) (T.pack replReplacement)
  ]

importTextEdits
  :: NormalizedFilePath -> GHCGHC.ParsedSource -> T.Text -> [AnnotatedImports] -> [(Uri, TextEdit)]
importTextEdits target ps contents imp
  | not (null imp)
  , Just (range, indent) <- newImportInsertRange ps contents
  = let print = [(T.pack . exactPrint) i | i <- concatMap astA imp]
        sep   = "\n" <> T.replicate indent " "
        text  = T.concat [ p <> sep | p <- print ]
        edit  = TextEdit range text
        uri   = filePathToUri $ fromNormalizedFilePath target
    in [(uri, edit)]
importTextEdits _ _ _ _ = []

-------------------------------------------------------------------------------
-- Rule wrappers

_useRuleBlocking,
  _useRuleStale,
  useRule ::
    (IdeRule k v) =>
    String ->
    IdeState ->
    k ->
    NormalizedFilePath ->
    IO (Maybe (RuleResult k))
_useRuleBlocking label state rule f = runAction label state (use rule f)
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

reuseParsedModule :: IdeState -> NormalizedFilePath -> IO (FixityEnv, Annotated GHCGHC.ParsedSource)
reuseParsedModule state f = do
        pm <- useOrFail state "Retrie.GetParsedModule" NoParse GetParsedModule f
        (fixities, pm') <- fixFixities state f (fixAnns pm)
        return (fixities, pm')

getCPPmodule :: Recorder (WithPriority Log) -> IdeState -> HscEnv -> FilePath -> IO (FixityEnv, CPP AnnotatedModule, GHCGHC.ParsedSource, T.Text)
getCPPmodule recorder state session t = do
    -- TODO: is it safe to drop this makeAbsolute?
    let nt = toNormalizedFilePath' $ (toAbsolute $ rootDir state) t
    let getParsedModule f contents = do
          modSummary <- msrModSummary <$>
            useOrFail state "Retrie.GetModSummary" (CallRetrieInternalError "file not found") GetModSummary nt
          let ms' =
                modSummary
                  { ms_hspp_buf =
                      Just (stringToStringBuffer contents)
                  }
          logWith recorder Info $ LogParsingModule t
          parsed <- evalGhcEnv session (GHCGHC.parseModule ms')
              `catch` \e -> throwIO (GHCParseError nt (show @SomeException e))
          (fixities, parsed) <- fixFixities state f (fixAnns parsed)
          return (fixities, parsed)

    contents <- do
      mbContentsVFS <-
        runAction "Retrie.GetFileContents" state $ getFileContents nt
      case mbContentsVFS of
        Just contents -> return $ Rope.toText contents
        Nothing       -> T.decodeUtf8 <$> BS.readFile (fromNormalizedFilePath nt)
    ps <- useOrFail state "GetAnnotatedParsedSource" NoParse GetAnnotatedParsedSource nt
    if any (T.isPrefixOf "#if" . T.toLower) (T.lines contents)
      then do
        fixitiesRef <- newIORef mempty
        let parseModule x = do
              (fix, res) <- getParsedModule nt x
              atomicModifyIORef'_ fixitiesRef (fix <>)
              return res
        res <- parseCPP parseModule contents
        fixities <- readIORef fixitiesRef
        return (fixities, res, ps, contents)
      else do
        (fixities, pm) <- reuseParsedModule state nt
        return (fixities, NoCPP pm, ps, contents)
