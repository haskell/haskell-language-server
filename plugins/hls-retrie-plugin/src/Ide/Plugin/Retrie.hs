{-# LANGUAGE CPP                 #-}
{-# LANGUAGE DeriveAnyClass      #-}
{-# LANGUAGE DeriveGeneric       #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE NamedFieldPuns      #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE PatternSynonyms     #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving  #-}
{-# LANGUAGE TypeApplications    #-}
{-# LANGUAGE TypeFamilies        #-}
{-# LANGUAGE ViewPatterns        #-}

{-# OPTIONS -Wno-orphans #-}

module Ide.Plugin.Retrie (descriptor) where

import           Control.Concurrent.STM               (readTVarIO)
import           Control.Exception.Safe               (Exception (..),
                                                       SomeException, catch,
                                                       throwIO, try)
import           Control.Monad                        (forM, unless)
import           Control.Monad.IO.Class               (MonadIO (liftIO))
import           Control.Monad.Trans.Class            (MonadTrans (lift))
import           Control.Monad.Trans.Except           (ExceptT (ExceptT),
                                                       runExceptT)
import           Control.Monad.Trans.Maybe
import           Data.Aeson                           (FromJSON (..),
                                                       ToJSON (..),
                                                       Value (Null))
import           Data.Bifunctor                       (second)
import qualified Data.ByteString                      as BS
import           Data.Coerce
import           Data.Either                          (partitionEithers)
import           Data.Hashable                        (unhashed)
import qualified Data.HashMap.Strict                  as HM
import qualified Data.HashSet                         as Set
import           Data.IORef.Extra                     (atomicModifyIORef'_,
                                                       newIORef, readIORef)
import           Data.List.Extra                      (find, nubOrdOn)
import           Data.String                          (IsString)
import qualified Data.Text                            as T
import qualified Data.Text.Encoding                   as T
import           Data.Typeable                        (Typeable)
import           Development.IDE                      hiding (pluginHandlers)
import           Development.IDE.Core.PositionMapping
import           Development.IDE.Core.Shake           (ShakeExtras (knownTargetsVar),
                                                       toKnownFiles)
import           Development.IDE.GHC.Compat           (GenLocated (L), GhcPs,
                                                       GhcRn, GhcTc,
                                                       HsBindLR (FunBind),
                                                       HsGroup (..),
                                                       HsValBindsLR (..),
                                                       HscEnv, IdP, LRuleDecls,
                                                       ModSummary (ModSummary, ms_hspp_buf, ms_mod),
                                                       Outputable,
                                                       ParsedModule (..),
                                                       RuleDecl (HsRule),
                                                       RuleDecls (HsRules),
                                                       SourceText (..),
                                                       TyClDecl (SynDecl),
                                                       TyClGroup (..), fun_id,
                                                       hm_iface, isQual,
                                                       isQual_maybe, locA,
                                                       mi_fixities,
                                                       moduleNameString,
                                                       ms_hspp_opts,
                                                       nameModule_maybe,
                                                       nameRdrName, noLocA,
                                                       occNameFS, occNameString,
                                                       pattern IsBoot,
                                                       pattern NotBoot,
                                                       pattern RealSrcSpan,
                                                       pm_parsed_source,
                                                       rdrNameOcc, rds_rules,
                                                       srcSpanFile, topDir,
                                                       unLocA)
import           Development.IDE.GHC.Compat.Util      hiding (catch, try)
import qualified GHC                                  (Module,
                                                       ParsedModule (..),
                                                       moduleName, parseModule)
import           GHC.Generics                         (Generic)
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
import           Retrie.CPP                           (CPP (NoCPP), parseCPP)
import           Retrie.ExactPrint                    (Annotated, fix,
                                                       transformA, unsafeMkA)

#if MIN_VERSION_ghc(9,3,0)
import GHC.Types.PkgQual
#endif

#if MIN_VERSION_ghc(9,2,0)
import           Retrie.ExactPrint                    (makeDeltaAst)
#else
import           Retrie.ExactPrint                    (relativiseApiAnns)
#endif
import           Retrie.Fixity                        (mkFixityEnv)
import qualified Retrie.GHC                           as GHC
import           Retrie.Monad                         (addImports, apply,
                                                       getGroundTerms,
                                                       runRetrie)
import qualified Retrie.Options                       as Retrie
import           Retrie.Options                       (defaultOptions,
                                                       getTargetFiles)
import           Retrie.Replace                       (Change (..),
                                                       Replacement (..))
import           Retrie.Rewrites
import           Retrie.SYB                           (listify)
import           Retrie.Util                          (Verbosity (Loud))
import           System.Directory                     (makeAbsolute)

descriptor :: PluginId -> PluginDescriptor IdeState
descriptor plId =
  (defaultPluginDescriptor plId)
    { pluginHandlers = mkPluginHandler STextDocumentCodeAction provider,
      pluginCommands = [retrieCommand]
    }

retrieCommandName :: T.Text
retrieCommandName = "retrieCommand"

retrieCommand :: PluginCommand IdeState
retrieCommand =
  PluginCommand (coerce retrieCommandName) "run the refactoring" runRetrieCmd

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
    runMaybeT $ do
        nfp <- MaybeT $ return $ uriToNormalizedFilePath $ toNormalizedUri uri
        (session, _) <- MaybeT $ liftIO $
            runAction "Retrie.GhcSessionDeps" state $
                useWithStale GhcSessionDeps
                nfp
        (ms, binds, _, _, _) <- MaybeT $ liftIO $ runAction "Retrie.getBinds" state $ getBinds nfp
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
provider state plId (CodeActionParams _ _ (TextDocumentIdentifier uri) range ca) = pluginResponse $ do
  let (J.CodeActionContext _diags _monly) = ca
      nuri = toNormalizedUri uri
  nfp <- handleMaybe "uri" $ uriToNormalizedFilePath nuri

  (ModSummary{ms_mod}, topLevelBinds, posMapping, hs_ruleds, hs_tyclds)
    <- handleMaybeM "typecheck" $ liftIO $ runAction "retrie" state $ getBinds nfp

  pos <- handleMaybe "pos" $ _start <$> fromCurrentRange posMapping range
  let rewrites =
        concatMap (suggestBindRewrites uri pos ms_mod) topLevelBinds
          ++ concatMap (suggestRuleRewrites uri pos ms_mod) hs_ruleds
          ++ [ r
               | TyClGroup {group_tyclds} <- hs_tyclds,
                 L (locA -> l) g <- group_tyclds,
                 pos `isInsideSrcSpan` l,
                 r <- suggestTypeRewrites uri ms_mod g

             ]

  commands <- lift $
    forM rewrites $ \(title, kind, params) -> liftIO $ do
      let c = mkLspCommand plId (coerce retrieCommandName) title (Just [toJSON params])
      return $ CodeAction title (Just kind) Nothing Nothing Nothing Nothing (Just c) Nothing

  return $ J.List [InR c | c <- commands]

getBinds :: NormalizedFilePath -> Action (Maybe (ModSummary, [HsBindLR GhcRn GhcRn], PositionMapping, [LRuleDecls GhcRn], [TyClGroup GhcRn]))
getBinds nfp = runMaybeT $ do
  (tm, posMapping) <- MaybeT $ useWithStale TypeCheck nfp
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
            L _ decl <- GHC.bagToList bagBinds
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
          let HsRule {rd_name = L _ (_, rn)} = r,
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
suggestRuleRewrites _ _ _ _ = []

qualify :: GHC.Module -> String -> String
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
#if MIN_VERSION_ghc(9,2,0)
  -- retrie needs the libdir for `parseRewriteSpecs`
  libdir <- topDir . ms_hspp_opts . msrModSummary <$> useOrFail "Retrie.GetModSummary" (CallRetrieInternalError "file not found") GetModSummary origin
#endif
  let reuseParsedModule f = do
        pm <- useOrFail "Retrie.GetParsedModule" NoParse GetParsedModule f
        (fixities, pm') <- fixFixities f (fixAnns pm)
        return (fixities, pm')
      getCPPmodule t = do
        nt <- toNormalizedFilePath' <$> makeAbsolute t
        let getParsedModule f contents = do
              modSummary <- msrModSummary <$>
                useOrFail "Retrie.GetModSummary" (CallRetrieInternalError "file not found") GetModSummary nt
              let ms' =
                    modSummary
                      { ms_hspp_buf =
                          Just (stringToStringBuffer contents)
                      }
              logPriority (ideLogger state) Info $ T.pack $ "Parsing module: " <> t
              parsed <- evalGhcEnv session (GHC.parseModule ms')
                  `catch` \e -> throwIO (GHCParseError nt (show @SomeException e))
              (fixities, parsed) <- fixFixities f (fixAnns parsed)
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
            (fixities, pm) <- reuseParsedModule nt
            return (fixities, NoCPP pm)

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

  (originFixities, originParsedModule) <- reuseParsedModule origin
  retrie <-
    (\specs -> apply specs >> addImports annotatedImports)
      <$> parseRewriteSpecs
#if MIN_VERSION_ghc(9,2,0)
        libdir
#endif
        (\_f -> return $ NoCPP originParsedModule)
        originFixities
        theRewrites

  targets <- getTargetFiles retrieOptions (getGroundTerms retrie)

  results <- forM targets $ \t -> runExceptT $ do
    (fixityEnv, cpp) <- ExceptT $ try $ getCPPmodule t
    -- TODO add the imports to the resulting edits
    (_user, ast, change@(Change _replacements _imports)) <-
      lift $ runRetrie fixityEnv retrie cpp
    return $ asTextEdits change

  let (errors :: [CallRetrieError], replacements) = partitionEithers results
      editParams :: WorkspaceEdit
      editParams =
        WorkspaceEdit (Just $ asEditMap replacements) Nothing Nothing

  return (errors, editParams)
  where
    useOrFail ::
      IdeRule r v =>
      String ->
      (NormalizedFilePath -> CallRetrieError) ->
      r ->
      NormalizedFilePath ->
      IO (RuleResult r)
    useOrFail lbl mkException rule f =
      useRule lbl state rule f >>= maybe (liftIO $ throwIO $ mkException f) return
    fixityEnvFromModIface modIface =
      mkFixityEnv
        [ (fs, (fs, fixity))
          | (n, fixity) <- mi_fixities modIface,
            let fs = occNameFS n
        ]
    fixFixities f pm = do
      HiFileResult {hirModIface} <-
        useOrFail "GetModIface" NoTypeCheck GetModIface f
      let fixities = fixityEnvFromModIface hirModIface
      res <- transformA pm (fix fixities)
      return (fixities, res)
#if MIN_VERSION_ghc(9,2,0)
    fixAnns GHC.ParsedModule{pm_parsed_source} = unsafeMkA (makeDeltaAst pm_parsed_source) 0
#else
    fixAnns GHC.ParsedModule {..} =
      let ranns = relativiseApiAnns pm_parsed_source pm_annotations
       in unsafeMkA pm_parsed_source ranns 0
#endif

asEditMap :: [[(Uri, TextEdit)]] -> WorkspaceEditMap
asEditMap = coerce . HM.fromListWith (++) . concatMap (map (second pure))

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

data QualName = QualName {qual, name :: String}
  deriving (Eq, Show, Generic, FromJSON, ToJSON)

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
#if MIN_VERSION_ghc(9,2,0)
    ideclExt = GHC.EpAnnNotUsed
#else
    ideclExt = GHC.noExtField
#endif
    ideclAs = toMod <$> ideclAsString
    ideclQualified = if ideclQualifiedBool then GHC.QualifiedPre else GHC.NotQualified
