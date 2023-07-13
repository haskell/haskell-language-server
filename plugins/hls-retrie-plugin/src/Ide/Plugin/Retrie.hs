{-# LANGUAGE CPP                   #-}
{-# LANGUAGE DeriveAnyClass        #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE NamedFieldPuns        #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE PatternSynonyms       #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE RecordWildCards       #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE StandaloneDeriving    #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE ViewPatterns          #-}

{-# OPTIONS -Wno-orphans #-}
{-# LANGUAGE TupleSections         #-}

module Ide.Plugin.Retrie (descriptor) where

import           Control.Concurrent.STM               (readTVarIO)
import           Control.Exception.Safe               (Exception (..),
                                                       SomeException, assert,
                                                       catch, throwIO, try)
import           Control.Monad                        (forM, unless, when)
import           Control.Monad.IO.Class               (MonadIO (liftIO))
import           Control.Monad.Trans.Class            (MonadTrans (lift))
import           Control.Monad.Trans.Except           (ExceptT (ExceptT),
                                                       runExceptT, throwE)
import           Control.Monad.Trans.Maybe
import           Control.Monad.Trans.Writer.Strict
import           Data.Aeson                           (FromJSON (..),
                                                       ToJSON (..),
                                                       Value (Null))
import           Data.Bifunctor                       (second)
import qualified Data.ByteString                      as BS
import           Data.Coerce
import           Data.Data
import           Data.Either                          (partitionEithers)
import           Data.Hashable                        (Hashable (hash),
                                                       unhashed)
import qualified Data.HashMap.Strict                  as HM
import qualified Data.HashSet                         as Set
import           Data.IORef.Extra                     (atomicModifyIORef'_,
                                                       newIORef, readIORef)
import           Data.List.Extra                      (find, nubOrdOn)
import           Data.Maybe                           (catMaybes, fromJust,
                                                       listToMaybe)
import           Data.String                          (IsString)
import qualified Data.Text                            as T
import qualified Data.Text.Encoding                   as T
import           Data.Typeable                        (Typeable)
import           Debug.Trace
import           Development.IDE                      hiding (pluginHandlers)
import           Development.IDE.Core.PositionMapping
import           Development.IDE.Core.Shake           (ShakeExtras (ShakeExtras, knownTargetsVar),
                                                       clientCapabilities,
                                                       getShakeExtras,
                                                       hiedbWriter,
                                                       toKnownFiles, withHieDb)
import           Development.IDE.GHC.Compat           (GRHSs (GRHSs),
                                                       GenLocated (L), GhcPs,
                                                       GhcRn, GhcTc,
                                                       HsBindLR (FunBind),
                                                       HsExpr (HsApp, OpApp),
                                                       HsGroup (..),
                                                       HsValBindsLR (..),
                                                       HscEnv, IdP,
                                                       ImportDecl (..), LHsExpr,
                                                       LRuleDecls, Match,
                                                       ModIface,
                                                       ModSummary (ModSummary, ms_hspp_buf, ms_mod),
                                                       Name, Outputable,
                                                       ParsedModule (..),
                                                       RealSrcLoc,
                                                       RuleDecl (HsRule),
                                                       RuleDecls (HsRules),
                                                       SourceText (..),
                                                       TyClDecl (SynDecl),
                                                       TyClGroup (..), fun_id,
                                                       hm_iface, isQual,
                                                       isQual_maybe, isVarOcc,
                                                       locA, mi_fixities,
                                                       moduleName,
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
                                                       printWithoutUniques,
                                                       rdrNameOcc, rds_rules,
                                                       srcSpanFile, topDir,
                                                       unLoc, unLocA)
import qualified Development.IDE.GHC.Compat           as GHC
import           Development.IDE.GHC.Compat.Util      hiding (catch, try)
import           Development.IDE.GHC.Dump             (showAstDataHtml)
import           Development.IDE.GHC.ExactPrint       (ExceptStringT (ExceptStringT),
                                                       GetAnnotatedParsedSource (GetAnnotatedParsedSource),
                                                       TransformT,
                                                       graftExprWithM,
                                                       graftSmallestDeclsWithM,
                                                       hoistGraft, transformM)
import qualified GHC                                  (Module, ParsedSource,
                                                       moduleName, parseModule)
import qualified GHC                                  as GHCGHC
import           GHC.Generics                         (Generic)
import           GHC.Hs.Dump
import           Ide.PluginUtils
import           Ide.Types
import           Language.LSP.Server                  (LspM,
                                                       ProgressCancellable (Cancellable),
                                                       sendNotification,
                                                       sendRequest,
                                                       withIndefiniteProgress)
import           Language.LSP.Types                   as J hiding
                                                           (SemanticTokenAbsolute (length, line),
                                                            SemanticTokenRelative (length),
                                                            SemanticTokensEdit (_start))
import           Retrie                               (Annotated (astA),
                                                       AnnotatedModule,
                                                       Fixity (Fixity),
                                                       FixityDirection (InfixL),
                                                       Options, Options_ (..),
                                                       RewriteSpec,
                                                       Verbosity (Loud),
                                                       addImports, apply,
                                                       applyWithUpdate)
import           Retrie.Context
import           Retrie.CPP                           (CPP (NoCPP), parseCPP)
import           Retrie.ExactPrint                    (Annotated, fix,
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
import           System.Directory                     (makeAbsolute)

#if MIN_VERSION_ghc(9,3,0)
import           GHC.Types.PkgQual
#endif

#if MIN_VERSION_ghc(9,2,0)
import           Control.Exception                    (evaluate)
import           Data.Monoid                          (First (First))
import           Retrie.ExactPrint                    (makeDeltaAst)
import           Retrie.GHC                           (ann)
#else
import           Data.Monoid                          (First (..))
import qualified GHC.Exts                             as Ext
import           Retrie.AlphaEnv                      (extendAlphaEnv)
import           Retrie.ExactPrint                    (relativiseApiAnns)
#endif
import           Control.Arrow                        ((&&&))
import           Development.IDE.Core.Actions         (lookupMod)
import qualified Development.IDE.Core.PluginUtils     as PluginUtils
import           Development.IDE.Spans.AtPoint        (LookupModule,
                                                       getNamesAtPoint,
                                                       nameToLocation)
import           Development.IDE.Types.Shake          (WithHieDb)

descriptor :: PluginId -> PluginDescriptor IdeState
descriptor plId =
  (defaultPluginDescriptor plId)
    { pluginHandlers = mkPluginHandler STextDocumentCodeAction provider,
      pluginCommands = [retrieCommand, retrieInlineThisCommand]
    }

retrieCommandName :: T.Text
retrieCommandName = "retrieCommand"

retrieInlineThisCommandName :: T.Text
retrieInlineThisCommandName = "retrieInlineThisCommand"

retrieCommand :: PluginCommand IdeState
retrieCommand =
  PluginCommand (coerce retrieCommandName) "run the refactoring" runRetrieCmd

retrieInlineThisCommand :: PluginCommand IdeState
retrieInlineThisCommand =
  PluginCommand (coerce retrieInlineThisCommandName) "inline function call"
     runRetrieInlineThisCmd

-- | Parameters for the runRetrie PluginCommand.
data RunRetrieParams = RunRetrieParams
  { description               :: T.Text,
    rewrites                  :: [RewriteSpec],
    originatingFile           :: Uri,
    restrictToOriginatingFile :: Bool
  }
  deriving (Eq, Show, Generic, FromJSON, ToJSON)
runRetrieCmd ::
  IdeState ->
  RunRetrieParams ->
  LspM c (Either ResponseError Value)
runRetrieCmd state RunRetrieParams{originatingFile = uri, ..} =
  withIndefiniteProgress description Cancellable $ do
    PluginUtils.pluginResponse $ do
        nfp <- PluginUtils.withPluginError $ getNormalizedFilePath uri
        (session, _) <-
            PluginUtils.runAction "Retrie.GhcSessionDeps" state $
                PluginUtils.useWithStale GhcSessionDeps
                nfp
        (ms, binds, _, _, _) <- PluginUtils.runAction "Retrie.getBinds" state $ getBinds nfp
        let importRewrites = concatMap (extractImports ms binds) rewrites
        (errors, edits) <- liftIO $
            callRetrie
                state
                (hscEnv session)
                (map Right rewrites <> map Left importRewrites)
                nfp
                restrictToOriginatingFile
        unless (null errors) $
            lift $ sendNotification SWindowShowMessage $
                    ShowMessageParams MtWarning $
                    T.unlines $
                        "## Found errors during rewrite:" :
                        ["-" <> T.pack (show e) | e <- errors]
        lift $ sendRequest SWorkspaceApplyEdit (ApplyWorkspaceEditParams Nothing edits) (\_ -> pure ())
        return ()
    return $ Right Null

data RunRetrieInlineThisParams = RunRetrieInlineThisParams
  { inlineIntoThisLocation :: !Location,
    inlineFromThisLocation :: !Location,
    inlineThisDefinition   :: !T.Text
  }
  deriving (Eq, Show, Generic, FromJSON, ToJSON)

runRetrieInlineThisCmd :: IdeState
    -> RunRetrieInlineThisParams -> LspM c (Either ResponseError Value)
runRetrieInlineThisCmd state RunRetrieInlineThisParams{..} = PluginUtils.pluginResponse $ do
    nfp <- PluginUtils.withPluginError $
        getNormalizedFilePath $ getLocationUri inlineIntoThisLocation
    nfpSource <- PluginUtils.withPluginError $
        getNormalizedFilePath $ getLocationUri inlineFromThisLocation
    -- What we do here:
    --   Find the identifier in the given position
    --   Construct an inline rewrite for it
    --   Run retrie to get a list of changes
    --   Select the change that inlines the identifier in the given position
    --   Apply the edit
    ast <- PluginUtils.runAction "retrie" state $
        PluginUtils.use GetAnnotatedParsedSource nfp
    astSrc <- PluginUtils.runAction "retrie" state $
        PluginUtils.use GetAnnotatedParsedSource nfpSource
    msr <- PluginUtils.runAction "retrie" state $
        PluginUtils.use GetModSummaryWithoutTimestamps nfp
    hiFileRes <- PluginUtils.runAction "retrie" state $
        PluginUtils.use GetModIface nfpSource
    let fixityEnv = fixityEnvFromModIface (hirModIface hiFileRes)
        fromRange = rangeToRealSrcSpan nfpSource $ getLocationRange inlineFromThisLocation
        intoRange = rangeToRealSrcSpan nfp $ getLocationRange inlineIntoThisLocation
    inlineRewrite <- liftIO $ constructInlineFromIdentifer astSrc fromRange
    when (null inlineRewrite) $ throwE $ PluginUtils.mkPluginErrorMessage "Empty rewrite"
    let ShakeExtras{..} = shakeExtras state
    (session, _) <- PluginUtils.runAction "retrie" state $
      PluginUtils.useWithStale GhcSessionDeps nfp
    (fixityEnv, cpp) <- liftIO $ getCPPmodule state (hscEnv session) $ fromNormalizedFilePath nfp
    result <- liftIO $ try @_ @SomeException $
        runRetrie fixityEnv (applyWithUpdate myContextUpdater inlineRewrite) cpp
    case result of
        Left err -> throwE $ PluginUtils.mkPluginErrorMessage $ "Retrie - crashed with: " <> T.pack (show err)
        Right (_,_,NoChange) -> throwE $ PluginUtils.mkPluginErrorMessage "Retrie - inline produced no changes"
        Right (_,_,Change replacements imports) -> do
            let edits = asEditMap $ asTextEdits $ Change ourReplacement imports
                wedit = WorkspaceEdit (Just edits) Nothing Nothing
                ourReplacement = [ r
                    | r@Replacement{..} <- replacements
                    , RealSrcSpan intoRange Nothing `GHC.isSubspanOf` replLocation]
            lift $ sendRequest SWorkspaceApplyEdit
                (ApplyWorkspaceEditParams Nothing wedit) (\_ -> pure ())
            return Null

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

provider :: PluginMethodHandler IdeState TextDocumentCodeAction
provider state plId (CodeActionParams _ _ (TextDocumentIdentifier uri) range ca) = PluginUtils.pluginResponse $ do
  let (J.CodeActionContext _diags _monly) = ca
  nfp <- PluginUtils.withPluginError $ getNormalizedFilePath uri

  (ModSummary{ms_mod}, topLevelBinds, posMapping, hs_ruleds, hs_tyclds)
    <- PluginUtils.runAction "retrie" state $
        getBinds nfp

  extras@ShakeExtras{ withHieDb, hiedbWriter } <- liftIO $ runAction "" state getShakeExtras

  range <- handleMaybe (PluginUtils.mkPluginErrorMessage "range") $ fromCurrentRange posMapping range
  let pos = _start range
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
      let c = mkLspCommand plId (coerce retrieCommandName) title (Just [toJSON params])
      return $ CodeAction title (Just kind) Nothing Nothing Nothing Nothing (Just c) Nothing

  inlineSuggestions <- liftIO $ runIdeAction "" extras $
    suggestBindInlines plId uri topLevelBinds range withHieDb (lookupMod hiedbWriter)
  let inlineCommands =
        [ Just $
            CodeAction _title (Just CodeActionRefactorInline) Nothing Nothing Nothing Nothing (Just c) Nothing
        | c@Command{..} <- inlineSuggestions
        ]
  return $ J.List [InR c | c <- retrieCommands ++ catMaybes inlineCommands]

getLocationUri :: Location -> Uri
getLocationUri Location{_uri} = _uri

getLocationRange Location{_range} = _range

getBinds :: NormalizedFilePath -> ExceptT PluginUtils.GhcidePluginError Action (ModSummary, [HsBindLR GhcRn GhcRn], PositionMapping, [LRuleDecls GhcRn], [TyClGroup GhcRn])
getBinds nfp = do
  (tm, posMapping) <- PluginUtils.useWithStale TypeCheck nfp
  -- we use the typechecked source instead of the parsed source
  -- to be able to extract module names from the Ids,
  -- so that we can include adding the required imports in the retrie command
  let rn = tmrRenamed tm
      ( HsGroup
          { hs_valds =
              XValBindsLR
                (GHC.NValBinds binds _sigs :: GHC.NHsValBindsLR GhcRn),
            hs_ruleds,
            hs_tyclds
          },
        _,
        _,
        _
        ) = rn

      topLevelBinds =
        [ decl
          | (_, bagBinds) <- binds,
            L _ decl <- bagToList bagBinds
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
            in (description, CodeActionRefactorInline, RunRetrieParams {..})
        foldRewrite restrictToOriginatingFile =
          let rewrites = [Fold (qualify ms_mod pprName)]
              description = "Fold " <> pprNameText <> describeRestriction restrictToOriginatingFile
           in (description, CodeActionRefactorExtract, RunRetrieParams {..})
     in [unfoldRewrite False, unfoldRewrite True, foldRewrite False, foldRewrite True]
suggestBindRewrites _ _ _ _ = []

  -- find all the identifiers in the AST for which have source definitions
suggestBindInlines :: PluginId -> Uri -> [HsBindLR GhcRn GhcRn] -> Range -> WithHieDb -> _ -> IdeAction [Command]
suggestBindInlines plId uri binds range hie lookupMod = do
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
        in mkLspCommand plId (coerce retrieInlineThisCommandName) title (Just [toJSON params])
        )
        (Set.toList identifiers)
    where
      definedIdentifiers =
        -- we search for candidates to inline in RHSs only, skipping LHSs
        everything (<>) (pure mempty `mkQ` getGRHSIdentifierDetails hie lookupMod) binds

      getGRHSIdentifierDetails :: WithHieDb -> _ -> GRHSs GhcRn (LHsExpr GhcRn) -> IdeAction (Set.HashSet (GHC.OccName, Location, Location))
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
  (Outputable (IdP GhcRn)) =>
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
           in (description, CodeActionRefactorInline, RunRetrieParams {..})
        foldRewrite restrictToOriginatingFile =
          let rewrites = [TypeBackward (qualify ms_mod pprName)]
              description = "Fold " <> pprNameText <> describeRestriction restrictToOriginatingFile
           in (description, CodeActionRefactorExtract, RunRetrieParams {..})
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
            CodeActionRefactor,
            RunRetrieParams {..}
            )
    backwardsRewrite ruleName restrictToOriginatingFile =
          let rewrites = [RuleBackward (qualify ms_mod ruleName)]
              description = "Apply rule " <> T.pack ruleName <> " backwards" <>
                              describeRestriction restrictToOriginatingFile
           in ( description,
                CodeActionRefactor,
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
  IdeState ->
  HscEnv ->
  [Either ImportSpec RewriteSpec] ->
  NormalizedFilePath ->
  Bool ->
  IO ([CallRetrieError], WorkspaceEdit)
callRetrie state session rewrites origin restrictToOriginatingFile = do
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
#if MIN_VERSION_ghc(9,2,0)
        unsafeMkA (map (noLocA . toImportDecl) theImports) 0
#else
        unsafeMkA (map (noLocA . toImportDecl) theImports) mempty 0
#endif

  (originFixities, originParsedModule) <- reuseParsedModule state origin
  retrie <-
    (\specs -> apply specs >> addImports annotatedImports)
      <$> parseSpecs state origin originParsedModule originFixities theRewrites

  targets <- getTargetFiles retrieOptions (getGroundTerms retrie)

  results <- forM targets $ \t -> runExceptT $ do
    (fixityEnv, cpp) <- ExceptT $ try $ getCPPmodule state session t
    -- TODO add the imports to the resulting edits
    (_user, ast, change@(Change _replacements _imports)) <-
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
#if MIN_VERSION_ghc(9,2,0)
fixAnns GHC.ParsedModule{pm_parsed_source} = unsafeMkA (makeDeltaAst pm_parsed_source) 0
#else
fixAnns GHC.ParsedModule {..} =
      let ranns = relativiseApiAnns pm_parsed_source pm_annotations
       in unsafeMkA pm_parsed_source ranns 0
#endif

parseSpecs
  :: IdeState
  -> NormalizedFilePath
  -> AnnotatedModule
  -> FixityEnv
  -> [RewriteSpec]
  -> IO [Rewrite Universe]
parseSpecs state origin originParsedModule originFixities specs = do
#if MIN_VERSION_ghc(9,2,0)
  -- retrie needs the libdir for `parseRewriteSpecs`
  libdir <- topDir . ms_hspp_opts . msrModSummary <$> useOrFail state "Retrie.GetModSummary" (CallRetrieInternalError "file not found") GetModSummary origin
#endif
  parseRewriteSpecs
#if MIN_VERSION_ghc(9,2,0)
    libdir
#endif
    (\_f -> return $ NoCPP originParsedModule)
    originFixities
    specs

constructfromFunMatches imps fun_id fun_matches = do
    let fName = occNameFS (GHC.occName (unLoc fun_id))
    fe <- mkLocatedHsVar fun_id
    rewrites <- concat <$>
        forM (unLoc $ GHC.mg_alts fun_matches) (matchToRewrites fe imps LeftToRight)
    let urewrites = toURewrite <$> rewrites
    -- traceShowM $ map showQuery urewrites
    assert (not $ null urewrites) $
        return urewrites

showQuery = ppRewrite
-- showQuery :: Rewrite (LHsExpr GhcPs) -> String
-- showQuery q = unlines
--     [ "template: " <> show (hash (printOutputable . showAstData NoBlankSrcSpan . astA . tTemplate . fst . qResult $ q))
--     , "quantifiers: " <> show (hash (T.pack (show(Ext.toList $ qQuantifiers q))))
--     , "matcher: " <> show (hash (printOutputable . showAstData NoBlankSrcSpan . astA . qPattern $ q))
--     ]

s :: Data a => a -> String
s = T.unpack . printOutputable . showAstData NoBlankSrcSpan
#if MIN_VERSION_ghc(9,2,0)
        NoBlankEpAnnotations
#endif
constructInlineFromIdentifer originParsedModule originSpan = do
    -- traceM $ s $ astA originParsedModule
    fmap astA $ transformA originParsedModule $ \(L _ m) -> do
        let ast = everything (<>) (First Nothing `mkQ` matcher) m
            matcher :: HsBindLR GhcPs GhcPs -> First _
            matcher FunBind{fun_id, fun_matches}
                --  | trace (show (GHC.getLocA fun_id) <> ": " <> s fun_id) False = undefined
                | RealSrcSpan sp _ <- GHC.getLocA fun_id
                , sp == originSpan =
                First $ Just (fun_id, fun_matches)
            matcher _ = First Nothing
        case ast of
            First (Just (fun_id, fun_matches))
                ->
                let imports = mempty in
                constructfromFunMatches imports fun_id fun_matches
            _ -> return $ error "cound not find source code to inline"

asEditMap :: [(Uri, TextEdit)] -> WorkspaceEditMap
asEditMap = coerce . HM.fromListWith (++) . map (second pure)

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
  deriving (Eq, Show, Generic, FromJSON, ToJSON)

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
#if MIN_VERSION_ghc(9,3,0)
    ideclPkgQual = NoRawPkgQual
#else
    ideclPkgQual = Nothing
#endif
    ideclSafe = False
    ideclImplicit = False
    ideclHiding = Nothing
    ideclSourceSrc = NoSourceText
#if MIN_VERSION_ghc(9,5,0)
    ideclExt = GHCGHC.XImportDeclPass
      { ideclAnn = GHCGHC.EpAnnNotUsed
      , ideclSourceText = ideclSourceSrc
      , ideclImplicit = ideclImplicit
      }
#elif MIN_VERSION_ghc(9,2,0)
    ideclExt = GHCGHC.EpAnnNotUsed
#else
    ideclExt = GHC.noExtField
#endif
    ideclAs = toMod <$> ideclAsString
    ideclQualified = if ideclQualifiedBool then GHC.QualifiedPre else GHC.NotQualified

reuseParsedModule state f = do
        pm <- useOrFail state "Retrie.GetParsedModule" NoParse GetParsedModule f
        (fixities, pm') <- fixFixities state f (fixAnns pm)
        return (fixities, pm')
getCPPmodule state session t = do
    nt <- toNormalizedFilePath' <$> makeAbsolute t
    let getParsedModule f contents = do
          modSummary <- msrModSummary <$>
            useOrFail state "Retrie.GetModSummary" (CallRetrieInternalError "file not found") GetModSummary nt
          let ms' =
                modSummary
                  { ms_hspp_buf =
                      Just (stringToStringBuffer contents)
                  }
          logPriority (ideLogger state) Info $ T.pack $ "Parsing module: " <> t
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
