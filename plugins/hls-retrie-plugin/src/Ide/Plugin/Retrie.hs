{-# LANGUAGE CPP                 #-}
{-# LANGUAGE DeriveAnyClass      #-}
{-# LANGUAGE DeriveGeneric       #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE NamedFieldPuns      #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving  #-}
{-# LANGUAGE TupleSections       #-}
{-# LANGUAGE TypeApplications    #-}
{-# LANGUAGE TypeFamilies        #-}

{-# OPTIONS -Wno-orphans #-}
#include "ghc-api-version.h"

module Ide.Plugin.Retrie (descriptor) where

import           Control.Concurrent.Extra       (readVar)
import           Control.Exception.Safe         (Exception (..), SomeException,
                                                 catch, throwIO, try)
import           Control.Monad                  (forM, unless)
import           Control.Monad.IO.Class         (MonadIO (liftIO))
import           Control.Monad.Trans.Class      (MonadTrans (lift))
import           Control.Monad.Trans.Except     (ExceptT (..), runExceptT,
                                                 throwE)
import           Data.Aeson                     (genericParseJSON, FromJSON(..), ToJSON (..), Value (Null))
import           Data.Bifunctor                 (Bifunctor (first), second)
import           Data.Coerce
import           Data.Either                    (partitionEithers)
import           Data.Hashable                  (unhashed)
import qualified Data.HashMap.Strict            as HM
import qualified Data.HashSet                   as Set
import           Data.IORef.Extra               (atomicModifyIORef'_, newIORef,
                                                 readIORef)
import           Data.List.Extra                (find, nubOrdOn)
import           Data.String                    (IsString (fromString))
import qualified Data.Text                      as T
import qualified Data.Text.IO                   as T
import           Data.Typeable                  (Typeable)
import           Development.IDE
import           Development.IDE.Core.Shake     (toKnownFiles, ShakeExtras(knownTargetsVar))
import           Development.IDE.GHC.Compat     (GenLocated (L), GhcRn,
                                                 HsBindLR (FunBind),
                                                 HsGroup (..),
                                                 HsValBindsLR (..), HscEnv, IdP,
                                                 LRuleDecls,
                                                 ModSummary (ModSummary, ms_hspp_buf, ms_mod),
                                                 NHsValBindsLR (..),
                                                 ParsedModule (..),
                                                 RuleDecl (HsRule),
                                                 RuleDecls (HsRules),
                                                 SrcSpan (..),
                                                 TyClDecl (SynDecl),
                                                 TyClGroup (..), fun_id,
                                                 mi_fixities, moduleNameString,
                                                 parseModule, rds_rules,
                                                 srcSpanFile)
import           GHC.Generics                   (Generic)
import           GhcPlugins                     (Outputable,
                                                 SourceText (NoSourceText),
                                                 hm_iface, isQual, isQual_maybe,
                                                 nameModule_maybe, nameRdrName,
                                                 occNameFS, occNameString,
                                                 rdrNameOcc, unpackFS)
import           Ide.PluginUtils
import           Ide.Types
import           Language.Haskell.LSP.Core      (LspFuncs (..), ProgressCancellable (Cancellable))
import           Language.Haskell.LSP.Messages  (FromServerMessage (NotShowMessage))
import           Language.Haskell.LSP.Types     as J
import           Retrie.CPP                     (CPP (NoCPP), parseCPP)
import           Retrie.ExactPrint              (fix, relativiseApiAnns,
                                                 transformA, unsafeMkA)
import           Retrie.Fixity                  (mkFixityEnv)
import qualified Retrie.GHC                     as GHC
import           Retrie.Monad                   (addImports, apply,
                                                 getGroundTerms, runRetrie)
import           Retrie.Options                 (defaultOptions, getTargetFiles)
import qualified Retrie.Options                 as Retrie
import           Retrie.Replace                 (Change (..), Replacement (..))
import           Retrie.Rewrites
import           Retrie.SYB                     (listify)
import           Retrie.Util                    (Verbosity (Loud))
import           StringBuffer                   (stringToStringBuffer)
import           System.Directory               (makeAbsolute)
import Control.Monad.Trans.Maybe
import Development.IDE.Core.PositionMapping
import qualified Data.Aeson as Aeson

descriptor :: PluginId -> PluginDescriptor IdeState
descriptor plId =
  (defaultPluginDescriptor plId)
    { pluginCodeActionProvider = Just provider,
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
    originatingFile           :: NormalizedUriJSON,
    restrictToOriginatingFile :: Bool
  }
  deriving (Eq, Show, Generic, FromJSON, ToJSON)

newtype NormalizedUriJSON = NormalizedUriJSON NormalizedUri
  deriving (Eq, Show)

instance FromJSON NormalizedUriJSON where
    parseJSON = fmap NormalizedUriJSON . genericParseJSON Aeson.defaultOptions

instance ToJSON NormalizedUriJSON where
    toJSON (NormalizedUriJSON x) = Aeson.genericToJSON Aeson.defaultOptions x

runRetrieCmd ::
  LspFuncs a ->
  IdeState ->
  RunRetrieParams ->
  IO (Either ResponseError Value, Maybe (ServerMethod, ApplyWorkspaceEditParams))
runRetrieCmd lsp state RunRetrieParams{originatingFile = NormalizedUriJSON nuri, ..} =
  withIndefiniteProgress lsp description Cancellable $ do
    res <- runMaybeT $ do
        nfp <- MaybeT $ return $ uriToNormalizedFilePath nuri
        (session, _) <- MaybeT $
            runAction "Retrie.GhcSessionDeps" state $
                useWithStale GhcSessionDeps $
                nfp
        (ms, binds, _, _, _) <- MaybeT $ runAction "Retrie.getBinds" state $ getBinds nfp
        let importRewrites = concatMap (extractImports ms binds) rewrites
        (errors, edits) <- lift $
            callRetrie
                state
                (hscEnv session)
                (map Right rewrites <> map Left importRewrites)
                nfp
                restrictToOriginatingFile
        unless (null errors) $
            lift $ sendFunc lsp $
                NotShowMessage $
                NotificationMessage "2.0" WindowShowMessage $
                    ShowMessageParams MtWarning $
                    T.unlines $
                        "## Found errors during rewrite:" :
                        ["-" <> T.pack (show e) | e <- errors]
        return (WorkspaceApplyEdit, ApplyWorkspaceEditParams edits)
    return
      (Right Null, res)

extractImports :: ModSummary -> [HsBindLR GhcRn GhcRn] -> RewriteSpec -> [ImportSpec]
extractImports ModSummary{ms_mod} topLevelBinds (Unfold thing)
  | Just FunBind {fun_matches}
  <- find (\case FunBind{fun_id = L _ n} -> prettyPrint n == thing ; _ -> False) topLevelBinds
  , names <- listify p fun_matches
  =
    [ AddImport {..}
    | name <- names,
        Just ideclNameString <-
        [moduleNameString . GHC.moduleName <$> nameModule_maybe name],
        let ideclSource = False,
        let r = nameRdrName name,
        let ideclQualifiedBool = isQual r,
        let ideclAsString = moduleNameString . fst <$> isQual_maybe r,
        let ideclThing = Just (IEVar $ occNameString $ rdrNameOcc r)
    ]
    where
        p name = nameModule_maybe name /= Just ms_mod
-- TODO handle imports for all rewrites
extractImports _ _ _ = []

-------------------------------------------------------------------------------

provider :: CodeActionProvider IdeState
provider _a state plId (TextDocumentIdentifier uri) range ca = response $ do
  let (J.CodeActionContext _diags _monly) = ca
      nuri = toNormalizedUri uri
      nuriJson = NormalizedUriJSON nuri
  nfp <- handleMaybe "uri" $ uriToNormalizedFilePath nuri

  (ModSummary{ms_mod}, topLevelBinds, posMapping, hs_ruleds, hs_tyclds)
    <- handleMaybeM "typecheck" $ runAction "retrie" state $ getBinds nfp

  pos <- handleMaybe "pos" $ _start <$> fromCurrentRange posMapping range
  let rewrites =
        concatMap (suggestBindRewrites nuriJson pos ms_mod) topLevelBinds
          ++ concatMap (suggestRuleRewrites nuriJson pos ms_mod) hs_ruleds
          ++ [ r
               | TyClGroup {group_tyclds} <- hs_tyclds,
                 L l g <- group_tyclds,
                 r <- suggestTypeRewrites nuriJson ms_mod g,
                 pos `isInsideSrcSpan` l

             ]

  commands <- lift $
    forM rewrites $ \(title, kind, params) -> do
      c <- mkLspCommand plId (coerce retrieCommandName) title (Just [toJSON params])
      return $ CodeAction title (Just kind) Nothing Nothing (Just c)

  return $ J.List [CACodeAction c | c <- commands]

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
                (NValBinds binds _sigs :: NHsValBindsLR GHC.GhcRn),
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
  NormalizedUriJSON ->
  Position ->
  GHC.Module ->
  HsBindLR GhcRn GhcRn ->
  [(T.Text, CodeActionKind, RunRetrieParams)]
suggestBindRewrites originatingFile pos ms_mod (FunBind {fun_id = L l' rdrName})
  | pos `isInsideSrcSpan` l' =
    let pprName = prettyPrint rdrName
        pprNameText = T.pack pprName
        unfoldRewrite restrictToOriginatingFile =
            let rewrites = [Unfold (qualify ms_mod pprName)]
                description = "Unfold " <> pprNameText <> describeRestriction restrictToOriginatingFile
            in (description, CodeActionRefactorInline, RunRetrieParams {..})
        foldRewrite restrictToOriginatingFile =
          let rewrites = [Fold (qualify ms_mod pprName)]
              description = "Fold " <> pprNameText <> describeRestriction restrictToOriginatingFile
           in (description, CodeActionRefactorExtract, RunRetrieParams {..})
     in [unfoldRewrite False, unfoldRewrite True, foldRewrite False, foldRewrite True]
  where
suggestBindRewrites _ _ _ _ = []

describeRestriction :: IsString p => Bool -> p
describeRestriction restrictToOriginatingFile =
        if restrictToOriginatingFile then " in current file" else ""

suggestTypeRewrites ::
  (Outputable (IdP pass)) =>
  NormalizedUriJSON ->
  GHC.Module ->
  TyClDecl pass ->
  [(T.Text, CodeActionKind, RunRetrieParams)]
suggestTypeRewrites originatingFile ms_mod (SynDecl {tcdLName = L _ rdrName}) =
    let pprName = prettyPrint rdrName
        pprNameText = T.pack pprName
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
  NormalizedUriJSON ->
  Position ->
  GHC.Module ->
  LRuleDecls pass ->
  [(T.Text, CodeActionKind, RunRetrieParams)]
suggestRuleRewrites originatingFile pos ms_mod (L _ (HsRules {rds_rules})) =
    concat
        [ [ forwardRewrite   ruleName True
          , forwardRewrite   ruleName False
          , backwardsRewrite ruleName True
          , backwardsRewrite ruleName False
          ]
        | L l r  <- rds_rules,
          pos `isInsideSrcSpan` l,
#if MIN_GHC_API_VERSION(8,8,0)
          let HsRule {rd_name = L _ (_, rn)} = r,
#else
          let HsRule _ (L _ (_,rn)) _ _ _ _ = r,
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

suggestRuleRewrites _ _ _ _ = []

qualify :: GHC.Module -> String -> String
qualify ms_mod x = prettyPrint ms_mod <> "." <> x

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
  knownFiles <- toKnownFiles . unhashed <$> readVar (knownTargetsVar $ shakeExtras state)
  print knownFiles
  let reuseParsedModule f = do
        pm <-
          useOrFail "GetParsedModule" NoParse GetParsedModule f
        (fixities, pm) <- fixFixities f (fixAnns pm)
        return (fixities, pm)
      getCPPmodule t = do
        nt <- toNormalizedFilePath' <$> makeAbsolute t
        let getParsedModule f contents = do
              (modSummary, _) <-
                useOrFail "GetModSummary" (CallRetrieInternalError "file not found") GetModSummary nt
              let ms' =
                    modSummary
                      { ms_hspp_buf =
                          Just (stringToStringBuffer contents)
                      }
              logPriority (ideLogger state) Info $ T.pack $ "Parsing module: " <> t
              parsed <-
                evalGhcEnv session (parseModule ms')
                  `catch` \e -> throwIO (GHCParseError nt (show @SomeException e))
              (fixities, parsed) <- fixFixities f (fixAnns parsed)
              return (fixities, parsed)

        contents <- do
          (_, mbContentsVFS) <-
            runAction "Retrie.GetFileContents" state $ getFileContents nt
          case mbContentsVFS of
            Just contents -> return contents
            Nothing       -> T.readFile (fromNormalizedFilePath nt)
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
        unsafeMkA (map (GHC.noLoc . toImportDecl) theImports) mempty 0

  (originFixities, originParsedModule) <- reuseParsedModule origin
  retrie <-
    (\specs -> apply specs >> addImports annotatedImports)
      <$> parseRewriteSpecs
        (\_f -> return $ NoCPP originParsedModule)
        originFixities
        theRewrites

  targets <- getTargetFiles retrieOptions (getGroundTerms retrie)

  results <- forM targets $ \t -> runExceptT $ do
    (fixityEnv, cpp) <- ExceptT $ try $ getCPPmodule t
    -- TODO add the imports to the resulting edits
    (_user, ast, change@(Change _replacements _imports)) <-
      lift $ runRetrie fixityEnv retrie cpp
    case ast of
      _ ->
        return $ asTextEdits change

  let (errors :: [CallRetrieError], replacements) = partitionEithers results
      editParams :: WorkspaceEdit
      editParams =
        WorkspaceEdit (Just $ asEditMap replacements) Nothing

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
      HiFileResult {hirHomeMod} <-
        useOrFail "GetModIface" NoTypeCheck GetModIface f
      let fixities = fixityEnvFromModIface $ hm_iface hirHomeMod
      res <- transformA pm (fix fixities)
      return (fixities, res)
    fixAnns ParsedModule {..} =
      let ranns = relativiseApiAnns pm_parsed_source pm_annotations
       in unsafeMkA pm_parsed_source ranns 0

asEditMap :: [[(Uri, TextEdit)]] -> WorkspaceEditMap
asEditMap = coerce . HM.fromListWith (++) . concatMap (map (second pure))

asTextEdits :: Change -> [(Uri, TextEdit)]
asTextEdits NoChange = []
asTextEdits (Change reps _imports) =
  [ (filePathToUri spanLoc, edit)
    | Replacement {..} <- nubOrdOn replLocation reps,
      (RealSrcSpan rspan) <- [replLocation],
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
-- Error handling combinators

handleMaybe :: Monad m => e -> Maybe b -> ExceptT e m b
handleMaybe msg = maybe (throwE msg) return

handleMaybeM :: Monad m => e -> m (Maybe b) -> ExceptT e m b
handleMaybeM msg act = maybe (throwE msg) return =<< lift act

response :: ExceptT String IO a -> IO (Either ResponseError a)
response =
  fmap (first (\msg -> ResponseError InternalError (fromString msg) Nothing))
    . runExceptT

-------------------------------------------------------------------------------
-- Serialization wrappers and instances

deriving instance Eq RewriteSpec

deriving instance Show RewriteSpec

deriving instance Generic RewriteSpec

deriving instance FromJSON RewriteSpec

deriving instance ToJSON RewriteSpec

data QualName = QualName {qual, name :: String}
  deriving (Eq, Show, Generic, FromJSON, ToJSON)

data IE name
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
toImportDecl AddImport {..} = GHC.ImportDecl {..}
  where
    toMod = GHC.noLoc . GHC.mkModuleName
    ideclName = toMod ideclNameString
    ideclPkgQual = Nothing
    ideclSafe = False
    ideclImplicit = False
    ideclHiding = Nothing
    ideclSourceSrc = NoSourceText
    ideclExt = GHC.noExtField
    ideclAs = toMod <$> ideclAsString
#if MIN_GHC_API_VERSION(8,10,0)
    ideclQualified = if ideclQualifiedBool then GHC.QualifiedPre else GHC.NotQualified
#else
    ideclQualified = ideclQualifiedBool
#endif
