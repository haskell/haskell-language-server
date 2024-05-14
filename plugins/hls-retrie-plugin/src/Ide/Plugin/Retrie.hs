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

import           Control.Concurrent.STM               (readTVarIO)
import           Control.Exception.Safe               (Exception (..),
                                                       SomeException, assert,
                                                       catch, throwIO, try)
import           Control.Lens.Operators
import           Control.Monad                        (forM, unless, when)
import           Control.Monad.Error.Class            (MonadError (throwError))
import           Control.Monad.IO.Class               (MonadIO (liftIO))
import           Control.Monad.Trans.Class            (MonadTrans (lift))
import           Control.Monad.Trans.Except           (ExceptT (..), runExceptT)

import           Control.Monad.Trans.Maybe            (MaybeT)
import           Data.Aeson                           (FromJSON (..),
                                                       ToJSON (..))
import           Data.Bifunctor                       (second)
import qualified Data.ByteString                      as BS
import           Data.Data
import           Data.Either                          (partitionEithers)
import           Data.Hashable                        (unhashed)
import qualified Data.HashSet                         as Set
import           Data.IORef.Extra                     (atomicModifyIORef'_,
                                                       newIORef, readIORef)
import           Data.List.Extra                      (find, nubOrdOn)
import qualified Data.Map                             as Map
import           Data.Maybe                           (catMaybes)
import           Data.Monoid                          (First (First))
import           Data.String                          (IsString)
import qualified Data.Text                            as T
import qualified Data.Text.Encoding                   as T
import           Development.IDE                      hiding (pluginHandlers)
import           Development.IDE.Core.Actions         (lookupMod)
import           Development.IDE.Core.PluginUtils
import           Development.IDE.Core.PositionMapping
import           Development.IDE.Core.Shake           (ShakeExtras (ShakeExtras, knownTargetsVar),
                                                       getShakeExtras,
                                                       hiedbWriter,
                                                       toKnownFiles, withHieDb)
import           Development.IDE.GHC.Compat           (GRHSs (GRHSs),
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
                                                       Outputable, ParsedModule,
                                                       RuleDecl (HsRule),
                                                       RuleDecls (HsRules),
                                                       SourceText (..),
                                                       TyClDecl (SynDecl),
                                                       TyClGroup (..), fun_id,
                                                       isQual, isQual_maybe,
                                                       locA, mi_fixities,
                                                       moduleNameString,
                                                       ms_hspp_opts,
                                                       nameModule_maybe,
                                                       nameOccName, nameRdrName,
                                                       noLocA, occNameFS,
                                                       occNameString,
                                                       pattern IsBoot,
                                                       pattern NotBoot,
                                                       pattern RealSrcSpan,
                                                       pm_parsed_source,
                                                       rdrNameOcc, rds_rules,
                                                       srcSpanFile, topDir,
                                                       unLoc, unLocA)
import qualified Development.IDE.GHC.Compat           as GHC
import           Development.IDE.GHC.Compat.Util      hiding (catch, try)
import           Development.IDE.GHC.ExactPrint       (GetAnnotatedParsedSource (GetAnnotatedParsedSource),
                                                       TransformT)
import           Development.IDE.Spans.AtPoint        (LookupModule,
                                                       nameToLocation)
import           Development.IDE.Types.Shake          (WithHieDb)
import qualified GHC                                  as GHCGHC
import           GHC.Generics                         (Generic)
import           Ide.Plugin.Error                     (PluginError (PluginInternalError),
                                                       getNormalizedFilePathE)
import           Ide.PluginUtils
import           Ide.Types
import qualified Language.LSP.Protocol.Lens           as L
import           Language.LSP.Protocol.Message        as LSP
import           Language.LSP.Protocol.Types          as LSP
import           Language.LSP.Server                  (ProgressCancellable (Cancellable),
                                                       sendNotification,
                                                       sendRequest,
                                                       withIndefiniteProgress)
import           Retrie                               (Annotated (astA),
                                                       AnnotatedModule,
                                                       Fixity (Fixity),
                                                       FixityDirection (InfixL),
                                                       Options, Options_ (..),
                                                       Verbosity (Loud),
                                                       addImports, apply,
                                                       applyWithUpdate)
import           Retrie.Context
import           Retrie.CPP                           (CPP (NoCPP), parseCPP)
import           Retrie.ExactPrint                    (fix, makeDeltaAst,
                                                       transformA, unsafeMkA)
import           Retrie.Expr                          (mkLocatedHsVar)
import           Retrie.Fixity                        (FixityEnv, lookupOp,
                                                       mkFixityEnv)
import           Retrie.Monad                         (getGroundTerms,
                                                       runRetrie)
import           Retrie.Options                       (defaultOptions,
                                                       getTargetFiles)
import           Retrie.Replace                       (Change (..),
                                                       Replacement (..))
import           Retrie.Rewrites
import           Retrie.Rewrites.Function             (matchToRewrites)
import           Retrie.SYB                           (everything, extQ,
                                                       listify, mkQ)
import           Retrie.Types
import           Retrie.Universe                      (Universe)
import           System.FilePath                      (isAbsolute, (</>))

#if MIN_VERSION_ghc(9,3,0)
import           GHC.Types.PkgQual
#endif

data Log
  = LogParsingModule FilePath

instance Pretty Log where
  pretty = \case
    LogParsingModule fp -> "Parsing module:" <+> pretty fp

descriptor :: Recorder (WithPriority Log) -> PluginId -> PluginDescriptor IdeState
descriptor recorder plId =
  (defaultPluginDescriptor plId "Provides code actions to inline Haskell definitions")
    { pluginHandlers = mkPluginHandler SMethod_TextDocumentCodeAction provider,
      pluginCommands = [retrieCommand recorder, retrieInlineThisCommand recorder]
    }

retrieCommandId :: CommandId
retrieCommandId = "retrieCommand"

retrieInlineThisCommandId :: CommandId
retrieInlineThisCommandId = "retrieInlineThisCommand"

retrieCommand :: Recorder (WithPriority Log) -> PluginCommand IdeState
retrieCommand recorder =
  PluginCommand retrieCommandId "run the refactoring" (runRetrieCmd recorder)

retrieInlineThisCommand :: Recorder (WithPriority Log) -> PluginCommand IdeState
retrieInlineThisCommand recorder =
  PluginCommand retrieInlineThisCommandId "inline function call"
    (runRetrieInlineThisCmd recorder)

-- | Parameters for the runRetrie PluginCommand.
data RunRetrieParams = RunRetrieParams
  { description               :: T.Text,
    rewrites                  :: [RewriteSpec],
    originatingFile           :: Uri,
    restrictToOriginatingFile :: Bool
  }
  deriving (Eq, Show, Generic, FromJSON, ToJSON)

runRetrieCmd :: Recorder (WithPriority Log) ->  CommandFunction IdeState RunRetrieParams
runRetrieCmd recorder state token RunRetrieParams{originatingFile = uri, ..} = ExceptT $
  withIndefiniteProgress description token Cancellable $ \_updater -> do
    _ <- runExceptT $ do
        nfp <- getNormalizedFilePathE uri
        (session, _) <-
            runActionE "Retrie.GhcSessionDeps" state $
                useWithStaleE GhcSessionDeps
                nfp
        (ms, binds, _, _, _) <- runActionE "Retrie.getBinds" state $ getBinds nfp
        let importRewrites = concatMap (extractImports ms binds) rewrites
        (errors, edits) <- liftIO $
            callRetrie
                recorder
                state
                (hscEnv session)
                (map Right rewrites <> map Left importRewrites)
                nfp
                restrictToOriginatingFile
        unless (null errors) $
            lift $ sendNotification SMethod_WindowShowMessage $
                    ShowMessageParams MessageType_Warning $
                    T.unlines $
                        "## Found errors during rewrite:" :
                        ["-" <> T.pack (show e) | e <- errors]
        _ <- lift $ sendRequest SMethod_WorkspaceApplyEdit (ApplyWorkspaceEditParams Nothing edits) (\_ -> pure ())
        return ()
    return $ Right $ InR Null

data RunRetrieInlineThisParams = RunRetrieInlineThisParams
  { inlineIntoThisLocation :: !Location,
    inlineFromThisLocation :: !Location,
    inlineThisDefinition   :: !T.Text
  }
  deriving (Eq, Show, Generic, FromJSON, ToJSON)

runRetrieInlineThisCmd :: Recorder (WithPriority Log) -> CommandFunction IdeState RunRetrieInlineThisParams
runRetrieInlineThisCmd recorder state _token RunRetrieInlineThisParams{..} = do
    nfp <- getNormalizedFilePathE $ getLocationUri inlineIntoThisLocation
    nfpSource <- getNormalizedFilePathE $ getLocationUri inlineFromThisLocation
    -- What we do here:
    --   Find the identifier in the given position
    --   Construct an inline rewrite for it
    --   Run retrie to get a list of changes
    --   Select the change that inlines the identifier in the given position
    --   Apply the edit
    astSrc <- runActionE "retrie" state $
        useE GetAnnotatedParsedSource nfpSource
    let fromRange = rangeToRealSrcSpan nfpSource $ getLocationRange inlineFromThisLocation
        intoRange = rangeToRealSrcSpan nfp $ getLocationRange inlineIntoThisLocation
    inlineRewrite <- liftIO $ constructInlineFromIdentifer astSrc fromRange
    when (null inlineRewrite) $ throwError $ PluginInternalError "Empty rewrite"
    (session, _) <- runActionE "retrie" state $
      useWithStaleE GhcSessionDeps nfp
    (fixityEnv, cpp) <- liftIO $ getCPPmodule recorder state (hscEnv session) $ fromNormalizedFilePath nfp
    result <- liftIO $ try @_ @SomeException $
        runRetrie fixityEnv (applyWithUpdate myContextUpdater inlineRewrite) cpp
    case result of
        Left err -> throwError $ PluginInternalError $ "Retrie - crashed with: " <> T.pack (show err)
        Right (_,_,NoChange) -> throwError $ PluginInternalError "Retrie - inline produced no changes"
        Right (_,_,Change replacements imports) -> do
            let edits = asEditMap $ asTextEdits $ Change ourReplacement imports
                wedit = WorkspaceEdit (Just edits) Nothing Nothing
                ourReplacement = [ r
                    | r@Replacement{..} <- replacements
                    , RealSrcSpan intoRange Nothing `GHC.isSubspanOf` replLocation]
            _ <- lift $ sendRequest SMethod_WorkspaceApplyEdit
                (ApplyWorkspaceEditParams Nothing wedit) (\_ -> pure ())
            return $ InR Null

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
        c { ctxtParentPrec = HasPrec $ Retrie.Fixity (SourceText "HsApp") (10 + i - firstChild) InfixL }
    -- Reason for 10 + i: (i is index of child, 0 = left, 1 = right)
    -- In left child, prec is 10, so HsApp child will NOT get paren'd
    -- In right child, prec is 11, so every child gets paren'd (unless atomic)
    updExp (OpApp _ _ op _) = c { ctxtParentPrec = HasPrec $ lookupOp op (ctxtFixityEnv c) }
    updExp _ = c { ctxtParentPrec = NeverParen }
    -- Deal with Trees-That-Grow adding extension points
    -- as the first child everywhere.
    firstChild :: Int
    firstChild = 1

extractImports :: ModSummary -> [HsBindLR GhcRn GhcRn] -> RewriteSpec -> [ImportSpec]
extractImports ModSummary{ms_mod} topLevelBinds (Unfold thing)
  | Just FunBind {fun_matches}
  <- find (\case FunBind{fun_id = L _ n} -> T.unpack (printOutputable n) == thing ; _ -> False) topLevelBinds
  , names <- listify p fun_matches
  =
    [ AddImport {..}
    | let ideclSource = False,
        name <- names,
        let r = nameRdrName name,
        let ideclQualifiedBool = isQual r,
        let ideclAsString = moduleNameString . fst <$> isQual_maybe r,
        let ideclThing = Just (IEVar $ occNameString $ rdrNameOcc r),
        Just ideclNameString <-
        [moduleNameString . GHC.moduleName <$> nameModule_maybe name]
    ]
    where
        p name = nameModule_maybe name /= Just ms_mod
-- TODO handle imports for all rewrites
extractImports _ _ _ = []

-------------------------------------------------------------------------------

provider :: PluginMethodHandler IdeState Method_TextDocumentCodeAction
provider state plId (CodeActionParams _ _ (TextDocumentIdentifier uri) range ca) = do
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

  retrieCommands <- lift $
    forM rewrites $ \(title, kind, params) -> liftIO $ do
      let c = mkLspCommand plId retrieCommandId title (Just [toJSON params])
      return $ CodeAction title (Just kind) Nothing Nothing Nothing Nothing (Just c) Nothing

  inlineSuggestions <- liftIO $ runIdeAction "" extras $
    suggestBindInlines plId uri topLevelBinds range withHieDb (lookupMod hiedbWriter)
  let inlineCommands =
        [ Just $
            CodeAction _title (Just CodeActionKind_RefactorInline) Nothing Nothing Nothing Nothing (Just c) Nothing
        | c@Command{..} <- inlineSuggestions
        ]
  return $ InL [InR c | c <- retrieCommands ++ catMaybes inlineCommands]

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
    (HsGroup{hs_valds, hs_ruleds, hs_tyclds}, _, _, _) -> do
      topLevelBinds <- case hs_valds of
        ValBinds{} -> throwError $ PluginInternalError "getBinds: ValBinds not supported"
        XValBindsLR (GHC.NValBinds binds _sigs :: GHC.NHsValBindsLR GhcRn) ->
          pure [ decl
               | (_, bagBinds) <- binds
               , L _ decl <- bagToList bagBinds
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
        unfoldRewrite restrictToOriginatingFile =
            let rewrites = [Unfold (qualify ms_mod pprName)]
                description = "Unfold " <> pprNameText <> describeRestriction restrictToOriginatingFile
            in (description, CodeActionKind_RefactorInline, RunRetrieParams {..})
        foldRewrite restrictToOriginatingFile =
          let rewrites = [Fold (qualify ms_mod pprName)]
              description = "Fold " <> pprNameText <> describeRestriction restrictToOriginatingFile
           in (description, CodeActionKind_RefactorExtract, RunRetrieParams {..})
     in [unfoldRewrite False, unfoldRewrite True, foldRewrite False, foldRewrite True]
suggestBindRewrites _ _ _ _ = []

  -- find all the identifiers in the AST for which have source definitions
suggestBindInlines ::
    PluginId
    -> Uri
    -> [HsBindLR GhcRn GhcRn]
    -> Range
    -> WithHieDb
    -> (FilePath -> GHCGHC.ModuleName -> GHCGHC.Unit -> Bool -> MaybeT IdeAction Uri)
    -> IdeAction [Command]
suggestBindInlines plId _uri binds range hie lookupMod = do
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
        in mkLspCommand plId retrieInlineThisCommandId title (Just [toJSON params])
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

      getDefinedIdentifierDetailsViaHieDb :: WithHieDb -> LookupModule IdeAction -> GHC.LIdP GhcRn -> IdeAction (Set.HashSet (GHC.OccName, Location, Location))
      getDefinedIdentifierDetailsViaHieDb withHieDb lookupModule lname | name <- unLoc lname =
        case srcSpanToLocation (GHC.getLocA lname) of
            Just siteLoc
              | siteRange <- getLocationRange siteLoc
              , range `isSubrangeOf` siteRange -> do
                    mbSrcLocation <- nameToLocation withHieDb lookupModule name
                    return $ maybe mempty (Set.fromList . map (nameOccName name, siteLoc,)) mbSrcLocation
            _ -> pure mempty


describeRestriction :: IsString p => Bool -> p
describeRestriction restrictToOriginatingFile =
        if restrictToOriginatingFile then " in current file" else ""

suggestTypeRewrites ::
  Uri ->
  GHC.Module ->
  TyClDecl GhcRn ->
  [(T.Text, CodeActionKind, RunRetrieParams)]
suggestTypeRewrites originatingFile ms_mod SynDecl {tcdLName} =
    let pprNameText = printOutputable (unLocA tcdLName)
        pprName = T.unpack pprNameText
        unfoldRewrite restrictToOriginatingFile =
            let rewrites = [TypeForward (qualify ms_mod pprName)]
                description = "Unfold " <> pprNameText <> describeRestriction restrictToOriginatingFile
           in (description, CodeActionKind_RefactorInline, RunRetrieParams {..})
        foldRewrite restrictToOriginatingFile =
          let rewrites = [TypeBackward (qualify ms_mod pprName)]
              description = "Fold " <> pprNameText <> describeRestriction restrictToOriginatingFile
           in (description, CodeActionKind_RefactorExtract, RunRetrieParams {..})
     in [unfoldRewrite False, unfoldRewrite True, foldRewrite False, foldRewrite True]
suggestTypeRewrites _ _ _ = []

suggestRuleRewrites ::
  Uri ->
  Position ->
  GHC.Module ->
  LRuleDecls GhcRn ->
  [(T.Text, CodeActionKind, RunRetrieParams)]
suggestRuleRewrites originatingFile pos ms_mod (L _ HsRules {rds_rules}) =
    concat
        [ [ forwardRewrite   ruleName True
          , forwardRewrite   ruleName False
          , backwardsRewrite ruleName True
          , backwardsRewrite ruleName False
          ]
        | L (locA -> l) r  <- rds_rules,
          pos `isInsideSrcSpan` l,
#if MIN_VERSION_ghc(9,5,0)
          let HsRule {rd_name = L _ rn} = r,
#else
          let HsRule {rd_name = L _ (_, rn)} = r,
#endif
          let ruleName = unpackFS rn
      ]
  where
    forwardRewrite ruleName restrictToOriginatingFile =
        let rewrites = [RuleForward (qualify ms_mod ruleName)]
            description = "Apply rule " <> T.pack ruleName <> " forward" <>
                            describeRestriction restrictToOriginatingFile

        in ( description,
            CodeActionKind_Refactor,
            RunRetrieParams {..}
            )
    backwardsRewrite ruleName restrictToOriginatingFile =
          let rewrites = [RuleBackward (qualify ms_mod ruleName)]
              description = "Apply rule " <> T.pack ruleName <> " backwards" <>
                              describeRestriction restrictToOriginatingFile
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
  deriving (Eq, Typeable)

instance Show CallRetrieError where
  show (CallRetrieInternalError msg f) = msg <> " - " <> fromNormalizedFilePath f
  show (NoParse f) = "Cannot parse: " <> fromNormalizedFilePath f
  show (GHCParseError f m) = "Cannot parse " <> fromNormalizedFilePath f <> " : " <> m
  show (NoTypeCheck f) = "File does not typecheck: " <> fromNormalizedFilePath f

instance Exception CallRetrieError

callRetrie ::
  Recorder (WithPriority Log) ->
  IdeState ->
  HscEnv ->
  [Either ImportSpec RewriteSpec] ->
  NormalizedFilePath ->
  Bool ->
  IO ([CallRetrieError], WorkspaceEdit)
callRetrie recorder state session rewrites origin restrictToOriginatingFile = do
  knownFiles <- toKnownFiles . unhashed <$> readTVarIO (knownTargetsVar $ shakeExtras state)
  let
      -- TODO cover all workspaceFolders
      target = "."

      retrieOptions :: Retrie.Options
      retrieOptions = (defaultOptions target)
        {Retrie.verbosity = Loud
        ,Retrie.targetFiles = map fromNormalizedFilePath $
            if restrictToOriginatingFile
                then [origin]
                else Set.toList knownFiles
        }

      (theImports, theRewrites) = partitionEithers rewrites

      annotatedImports =
        unsafeMkA (map (noLocA . toImportDecl) theImports) 0

  (originFixities, originParsedModule) <- reuseParsedModule state origin
  retrie <-
    (\specs -> apply specs >> addImports annotatedImports)
      <$> parseSpecs state origin originParsedModule originFixities theRewrites

  targets <- getTargetFiles retrieOptions (getGroundTerms retrie)

  results <- forM targets $ \t -> runExceptT $ do
    (fixityEnv, cpp) <- ExceptT $ try $ getCPPmodule recorder state session t
    -- TODO add the imports to the resulting edits
    (_user, _ast, change@(Change _replacements _imports)) <-
      lift $ runRetrie fixityEnv retrie cpp
    return $ asTextEdits change

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
      HiFileResult {hirModIface} <-
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

constructInlineFromIdentifer :: Data a => Annotated (GenLocated l a) -> GHCGHC.RealSrcSpan -> IO [Rewrite Universe]
constructInlineFromIdentifer originParsedModule originSpan = do
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
                ->
                let imports = mempty in
                constructfromFunMatches imports fun_id fun_matches
            _ -> return $ error "could not find source code to inline"

asEditMap :: [(Uri, TextEdit)] -> Map.Map Uri [TextEdit]
asEditMap = Map.fromListWith (++) . map (second pure)

asTextEdits :: Change -> [(Uri, TextEdit)]
asTextEdits NoChange = []
asTextEdits (Change reps _imports) =
  [ (filePathToUri spanLoc, edit)
    | Replacement {..} <- nubOrdOn (realSpan . replLocation) reps,
      (RealSrcSpan rspan _) <- [replLocation],
      let spanLoc = unpackFS $ srcSpanFile rspan,
      let edit = TextEdit (realSrcSpanToRange rspan) (T.pack replReplacement)
  ]

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

newtype IE name
  = IEVar name
  deriving stock (Eq, Show, Generic)
  deriving anyclass (FromJSON, ToJSON)


data ImportSpec = AddImport
  { ideclNameString    :: String,
    ideclSource        :: Bool,
    ideclQualifiedBool :: Bool,
    ideclAsString      :: Maybe String,
    ideclThing         :: Maybe (IE String)
  }
  deriving (Eq, Show, Generic, FromJSON, ToJSON)

toImportDecl :: ImportSpec -> GHC.ImportDecl GHC.GhcPs
toImportDecl AddImport {..} = GHC.ImportDecl {ideclSource = ideclSource', ..}
  where
    ideclSource' = if ideclSource then IsBoot else NotBoot
    toMod = noLocA . GHC.mkModuleName
    ideclName = toMod ideclNameString
    ideclSafe = False
    ideclImplicit = False
    ideclSourceSrc = NoSourceText
    ideclAs = toMod <$> ideclAsString
    ideclQualified = if ideclQualifiedBool then GHC.QualifiedPre else GHC.NotQualified

#if MIN_VERSION_ghc(9,3,0)
    ideclPkgQual = NoRawPkgQual
#else
    ideclPkgQual = Nothing
#endif

#if MIN_VERSION_ghc(9,5,0)
    ideclImportList = Nothing
    ideclExt = GHCGHC.XImportDeclPass
      { ideclAnn = GHCGHC.EpAnnNotUsed
      , ideclSourceText = ideclSourceSrc
      , ideclImplicit = ideclImplicit
      }
#else
    ideclExt = GHCGHC.EpAnnNotUsed
    ideclHiding = Nothing
#endif


reuseParsedModule :: IdeState -> NormalizedFilePath -> IO (FixityEnv, Annotated GHCGHC.ParsedSource)
reuseParsedModule state f = do
        pm <- useOrFail state "Retrie.GetParsedModule" NoParse GetParsedModule f
        (fixities, pm') <- fixFixities state f (fixAnns pm)
        return (fixities, pm')

toAbsolute :: FilePath -> FilePath -> FilePath
toAbsolute dir file
    | isAbsolute file = file
    | otherwise = dir </> file

getCPPmodule :: Recorder (WithPriority Log) -> IdeState -> HscEnv -> FilePath -> IO (FixityEnv, CPP AnnotatedModule)
getCPPmodule recorder state session t = do
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
      (_, mbContentsVFS) <-
        runAction "Retrie.GetFileContents" state $ getFileContents nt
      case mbContentsVFS of
        Just contents -> return contents
        Nothing       -> T.decodeUtf8 <$> BS.readFile (fromNormalizedFilePath nt)
    if any (T.isPrefixOf "#if" . T.toLower) (T.lines contents)
      then do
        fixitiesRef <- newIORef mempty
        let parseModule x = do
              (fix, res) <- getParsedModule nt x
              atomicModifyIORef'_ fixitiesRef (fix <>)
              return res
        res <- parseCPP parseModule contents
        fixities <- readIORef fixitiesRef
        return (fixities, res)
      else do
        (fixities, pm) <- reuseParsedModule state nt
        return (fixities, NoCPP pm)
