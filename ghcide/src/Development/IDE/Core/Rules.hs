-- Copyright (c) 2019 The DAML Authors. All rights reserved.
-- SPDX-License-Identifier: Apache-2.0

{-# LANGUAGE CPP                   #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE TypeFamilies          #-}

-- | A Shake implementation of the compiler service, built
--   using the "Shaker" abstraction layer for in-memory use.
--
module Development.IDE.Core.Rules(
    -- * Types
    IdeState, GetParsedModule(..), TransitiveDependencies(..),
    GhcSessionIO(..), GetClientSettings(..),
    -- * Functions
    runAction,
    toIdeResult,
    defineNoFile,
    defineEarlyCutOffNoFile,
    mainRule,
    RulesConfig(..),
    getParsedModule,
    getParsedModuleWithComments,
    getClientConfigAction,
    usePropertyAction,
    usePropertyByPathAction,
    getHieFile,
    -- * Rules
    CompiledLinkables(..),
    getParsedModuleRule,
    getParsedModuleWithCommentsRule,
    getLocatedImportsRule,
    reportImportCyclesRule,
    typeCheckRule,
    getDocMapRule,
    loadGhcSession,
    getModIfaceFromDiskRule,
    getModIfaceRule,
    getModSummaryRule,
    getModuleGraphRule,
    knownFilesRule,
    getClientSettingsRule,
    getHieAstsRule,
    getBindingsRule,
    needsCompilationRule,
    generateCoreRule,
    getImportMapRule,
    regenerateHiFile,
    ghcSessionDepsDefinition,
    getParsedModuleDefinition,
    typeCheckRuleDefinition,
    getRebuildCount,
    getSourceFileSource,
    currentLinkables,
    GhcSessionDepsConfig(..),
    Log(..),
    DisplayTHWarning(..),
    ) where

import           Control.Applicative
import           Control.Concurrent.STM.Stats                 (atomically)
import           Control.Concurrent.STM.TVar
import           Control.Concurrent.Strict
import           Control.DeepSeq
import           Control.Exception                            (evaluate)
import           Control.Exception.Safe
import           Control.Lens                                 ((%~), (&), (.~))
import           Control.Monad.Extra
import           Control.Monad.IO.Unlift
import           Control.Monad.Reader
import           Control.Monad.State
import           Control.Monad.Trans.Except                   (ExceptT, except,
                                                               runExceptT)
import           Control.Monad.Trans.Maybe
import           Data.Aeson                                   (toJSON)
import qualified Data.Binary                                  as B
import qualified Data.ByteString                              as BS
import qualified Data.ByteString.Lazy                         as LBS
import           Data.Coerce
import           Data.Default                                 (Default, def)
import           Data.Foldable
import           Data.Hashable
import qualified Data.HashMap.Strict                          as HM
import qualified Data.HashSet                                 as HashSet
import           Data.IntMap.Strict                           (IntMap)
import qualified Data.IntMap.Strict                           as IntMap
import           Data.IORef
import           Data.List
import           Data.List.Extra                              (nubOrd, nubOrdOn)
import qualified Data.Map                                     as M
import           Data.Maybe
import           Data.Proxy
import qualified Data.Text                                    as T
import qualified Data.Text.Encoding                           as T
import qualified Data.Text.Utf16.Rope.Mixed                   as Rope
import           Data.Time                                    (UTCTime (..))
import           Data.Time.Clock.POSIX                        (posixSecondsToUTCTime)
import           Data.Tuple.Extra
import           Data.Typeable                                (cast)
import           Development.IDE.Core.Compile
import           Development.IDE.Core.FileExists              hiding (Log,
                                                               LogShake)
import           Development.IDE.Core.FileStore               (getFileContents,
                                                               getFileModTimeContents,
                                                               getModTime)
import           Development.IDE.Core.IdeConfiguration
import           Development.IDE.Core.OfInterest              hiding (Log,
                                                               LogShake)
import           Development.IDE.Core.PositionMapping
import           Development.IDE.Core.RuleTypes
import           Development.IDE.Core.Service                 hiding (Log,
                                                               LogShake)
import           Development.IDE.Core.Shake                   hiding (Log)
import qualified Development.IDE.Core.Shake                   as Shake
import           Development.IDE.GHC.Compat                   hiding
                                                              (TargetId (..),
                                                               Var,
                                                               loadInterface,
                                                               nest,
                                                               parseModule,
                                                               settings, vcat,
                                                               (<+>))
import qualified Development.IDE.GHC.Compat                   as Compat hiding
                                                                        (nest,
                                                                         vcat)
import qualified Development.IDE.GHC.Compat.Util              as Util
import           Development.IDE.GHC.Error
import           Development.IDE.GHC.Util                     hiding
                                                              (modifyDynFlags)
import           Development.IDE.Graph
import           Development.IDE.Import.DependencyInformation
import           Development.IDE.Import.FindImports
import qualified Development.IDE.Spans.AtPoint                as AtPoint
import           Development.IDE.Spans.Documentation
import           Development.IDE.Spans.LocalBindings
import           Development.IDE.Types.Diagnostics            as Diag
import           Development.IDE.Types.HscEnvEq
import           Development.IDE.Types.Location
import           Development.IDE.Types.Options
import qualified Development.IDE.Types.Shake                  as Shake
import           GHC.Iface.Ext.Types                          (HieASTs (..))
import           GHC.Iface.Ext.Utils                          (generateReferencesMap)
import qualified GHC.LanguageExtensions                       as LangExt
#if MIN_VERSION_ghc(9,13,0)
import           GHC.Types.PkgQual                            (PkgQual (NoPkgQual))
import           GHC.Types.Basic                              (ImportLevel (..))
import           GHC.Unit.Types                               (GenWithIsBoot(..))
import           GHC.Unit.Module.Graph                        (mkModuleEdge)
import           GHC.Unit.Module.ModNodeKey                   (mnkModuleName)
#endif
import           HIE.Bios.Ghc.Gap                             (hostIsDynamic)
import qualified HieDb
import           Ide.Logger                                   (Pretty (pretty),
                                                               Recorder,
                                                               WithPriority,
                                                               cmapWithPrio,
                                                               logWith, nest,
                                                               vcat, (<+>))
import qualified Ide.Logger                                   as Logger
import           Ide.Plugin.Config
import           Ide.Plugin.Properties                        (HasProperty,
                                                               HasPropertyByPath,
                                                               KeyNamePath,
                                                               KeyNameProxy,
                                                               Properties,
                                                               ToHsType,
                                                               useProperty,
                                                               usePropertyByPath)
import           Ide.Types                                    (DynFlagsModifications (dynFlagsModifyGlobal, dynFlagsModifyParser),
                                                               PluginId, getVirtualFileFromVFS)
import qualified Language.LSP.Protocol.Lens                   as JL
import           Language.LSP.Protocol.Message                (SMethod (SMethod_CustomMethod, SMethod_WindowShowMessage))
import           Language.LSP.Protocol.Types                  (MessageType (MessageType_Info),
                                                               ShowMessageParams (ShowMessageParams))
import           Language.LSP.Server                          (LspT)
import qualified Language.LSP.Server                          as LSP
import           Language.LSP.VFS
import           Prelude                                      hiding (mod)
import           System.Directory                             (doesFileExist)
import           System.Info.Extra                            (isWindows)


import qualified Data.IntMap                                  as IM
import           GHC.Fingerprint

data Log
  = LogShake Shake.Log
  | LogReindexingHieFile !NormalizedFilePath
  | LogLoadingHieFile !NormalizedFilePath
  | LogLoadingHieFileFail !FilePath !SomeException
  | LogLoadingHieFileSuccess !FilePath
  | LogTypecheckedFOI !NormalizedFilePath
  deriving Show

instance Pretty Log where
  pretty = \case
    LogShake msg -> pretty msg
    LogReindexingHieFile path ->
      "Re-indexing hie file for" <+> pretty (fromNormalizedFilePath path)
    LogLoadingHieFile path ->
      "LOADING HIE FILE FOR" <+> pretty (fromNormalizedFilePath path)
    LogLoadingHieFileFail path e ->
      nest 2 $
        vcat
          [ "FAILED LOADING HIE FILE FOR" <+> pretty path
          , pretty (displayException e) ]
    LogLoadingHieFileSuccess path ->
      "SUCCEEDED LOADING HIE FILE FOR" <+> pretty path
    LogTypecheckedFOI path -> vcat
      [ "Typechecked a file which is not currently open in the editor:" <+> pretty (fromNormalizedFilePath path)
      , "This can indicate a bug which results in excessive memory usage."
      , "This may be a spurious warning if you have recently closed the file."
      , "If you haven't opened this file recently, please file a report on the issue tracker mentioning"
        <+> "the HLS version being used, the plugins enabled, and if possible the codebase and file which"
        <+> "triggered this warning."
      ]

templateHaskellInstructions :: T.Text
templateHaskellInstructions = "https://haskell-language-server.readthedocs.io/en/latest/troubleshooting.html#static-binaries"

-- | This is useful for rules to convert rules that can only produce errors or
-- a result into the more general IdeResult type that supports producing
-- warnings while also producing a result.
toIdeResult :: Either [FileDiagnostic] v -> IdeResult v
toIdeResult = either (, Nothing) (([],) . Just)

------------------------------------------------------------
-- Exposed API
------------------------------------------------------------

-- TODO: rename
-- TODO: return text --> return rope
getSourceFileSource :: NormalizedFilePath -> Action BS.ByteString
getSourceFileSource nfp = do
    msource <- getFileContents nfp
    case msource of
        Nothing     -> liftIO $ BS.readFile (fromNormalizedFilePath nfp)
        Just source -> pure $ T.encodeUtf8 $ Rope.toText source

-- | Parse the contents of a haskell file.
getParsedModule :: NormalizedFilePath -> Action (Maybe ParsedModule)
getParsedModule = use GetParsedModule

-- | Parse the contents of a haskell file,
-- ensuring comments are preserved in annotations
getParsedModuleWithComments :: NormalizedFilePath -> Action (Maybe ParsedModule)
getParsedModuleWithComments = use GetParsedModuleWithComments

------------------------------------------------------------
-- Rules
-- These typically go from key to value and are oracles.

-- | WARNING:
-- We currently parse the module both with and without Opt_Haddock, and
-- return the one with Haddocks if it -- succeeds. However, this may not work
-- for hlint or any client code that might need the parsed source with all
-- annotations, including comments.
-- For that use case you might want to use `getParsedModuleWithCommentsRule`
-- See https://github.com/haskell/ghcide/pull/350#discussion_r370878197
-- and https://github.com/mpickering/ghcide/pull/22#issuecomment-625070490
-- GHC wiki about: https://gitlab.haskell.org/ghc/ghc/-/wikis/api-annotations
getParsedModuleRule :: Recorder (WithPriority Log) -> Rules ()
getParsedModuleRule recorder =
  -- this rule does not have early cutoff since all its dependencies already have it
  define (cmapWithPrio LogShake recorder) $ \GetParsedModule file -> do
    ModSummaryResult{msrModSummary = ms', msrHscEnv = hsc} <- use_ GetModSummary file
    opt <- getIdeOptions
    modify_dflags <- getModifyDynFlags dynFlagsModifyParser
    let ms = ms' { ms_hspp_opts = modify_dflags $ ms_hspp_opts ms' }
        reset_ms pm = pm { pm_mod_summary = ms' }

    liftIO $ (fmap.fmap.fmap) reset_ms $ getParsedModuleDefinition hsc opt file ms

withoutOptHaddock :: ModSummary -> ModSummary
withoutOptHaddock = withoutOption Opt_Haddock

withOption :: GeneralFlag -> ModSummary -> ModSummary
withOption opt ms = ms{ms_hspp_opts= gopt_set (ms_hspp_opts ms) opt}

withoutOption :: GeneralFlag -> ModSummary -> ModSummary
withoutOption opt ms = ms{ms_hspp_opts= gopt_unset (ms_hspp_opts ms) opt}

-- | This rule provides a ParsedModule preserving all annotations,
-- including keywords, punctuation and comments.
-- So it is suitable for use cases where you need a perfect edit.
getParsedModuleWithCommentsRule :: Recorder (WithPriority Log) -> Rules ()
getParsedModuleWithCommentsRule recorder =
  -- The parse diagnostics are owned by the GetParsedModule rule
  -- For this reason, this rule does not produce any diagnostics
  defineNoDiagnostics (cmapWithPrio LogShake recorder) $ \GetParsedModuleWithComments file -> do
    ModSummaryResult{msrModSummary = ms, msrHscEnv = hsc} <- use_ GetModSummary file
    opt <- getIdeOptions

    let ms' = withoutOptHaddock $ withOption Opt_KeepRawTokenStream ms
    modify_dflags <- getModifyDynFlags dynFlagsModifyParser
    let ms'' = ms' { ms_hspp_opts = modify_dflags $ ms_hspp_opts ms' }
        reset_ms pm = pm { pm_mod_summary = ms' }

    liftIO $ fmap (fmap reset_ms) $ snd <$> getParsedModuleDefinition hsc opt file ms''

getModifyDynFlags :: (DynFlagsModifications -> a) -> Action a
getModifyDynFlags f = do
  opts <- getIdeOptions
  cfg <- getClientConfigAction
  pure $ f $ optModifyDynFlags opts cfg


getParsedModuleDefinition
    :: HscEnv
    -> IdeOptions
    -> NormalizedFilePath
    -> ModSummary -> IO ([FileDiagnostic], Maybe ParsedModule)
getParsedModuleDefinition packageState opt file ms = do
    let fp = fromNormalizedFilePath file
    (diag, res) <- parseModule opt packageState fp ms
    case res of
        Nothing   -> pure (diag, Nothing)
        Just modu -> pure (diag, Just modu)

getLocatedImportsRule :: Recorder (WithPriority Log) -> Rules ()
getLocatedImportsRule recorder =
    define (cmapWithPrio LogShake recorder) $ \GetLocatedImports file -> do
        ModSummaryResult{msrModSummary = ms} <- use_ GetModSummaryWithoutTimestamps file
        (KnownTargets targets targetsMap) <- useNoFile_ GetKnownTargets
#if MIN_VERSION_ghc(9,13,0)
        let imports = [(False, lvl, mbPkgName, modName) | (lvl, mbPkgName, modName) <- ms_textual_imps ms]
                   ++ [(True, NormalLevel, NoPkgQual, noLoc modName) | L _ modName <- ms_srcimps ms]
#else
        let imports = [(False, imp) | imp <- ms_textual_imps ms] ++ [(True, imp) | imp <- ms_srcimps ms]
#endif
        env_eq <- use_ GhcSession file
        let env = hscEnv env_eq
        let import_dirs = map (second homeUnitEnv_dflags) $ hugElts $ hsc_HUG env
        let dflags = hsc_dflags env
        opt <- getIdeOptions
        let getTargetFor modName nfp
                | Just (TargetFile nfp') <- HM.lookup (TargetFile nfp) targetsMap = do
                    -- reuse the existing NormalizedFilePath in order to maximize sharing
                    itExists <- getFileExists nfp'
                    return $ if itExists then Just nfp' else Nothing
                | Just tt <- HM.lookup (TargetModule modName) targets = do
                    -- reuse the existing NormalizedFilePath in order to maximize sharing
                    let ttmap = HM.mapWithKey const (HashSet.toMap tt)
                        nfp' = HM.lookupDefault nfp nfp ttmap
                    itExists <- getFileExists nfp'
                    return $ if itExists then Just nfp' else Nothing
                | otherwise = do
                    itExists <- getFileExists nfp
                    return $ if itExists then Just nfp else Nothing
#if MIN_VERSION_ghc(9,13,0)
        (diags, imports') <- fmap unzip $ forM imports $ \(isSource, _lvl, mbPkgName, modName) -> do
#else
        (diags, imports') <- fmap unzip $ forM imports $ \(isSource, (mbPkgName, modName)) -> do
#endif
            diagOrImp <- locateModule (hscSetFlags dflags env) import_dirs (optExtensions opt) getTargetFor modName mbPkgName isSource
            case diagOrImp of
                Left diags              -> pure (diags, Just (modName, Nothing))
                Right (FileImport path) -> pure ([], Just (modName, Just path))
                Right PackageImport     -> pure ([], Nothing)

        {- IS THIS REALLY NEEDED? DOESNT SEEM SO

        -- does this module have an hs-boot file? If so add a direct dependency
        let bootPath = toNormalizedFilePath' $ fromNormalizedFilePath file <.> "hs-boot"
        boot <- use GetFileExists bootPath
        bootArtifact <- if boot == Just True
              then do
                let modName = ms_mod_name ms
                loc <- liftIO $ mkHomeModLocation dflags' modName (fromNormalizedFilePath bootPath)
                return $ Just (noLoc modName, Just (ArtifactsLocation bootPath (Just loc) True))
              else pure Nothing
        -}
        let bootArtifact = Nothing

        let moduleImports = catMaybes $ bootArtifact : imports'
        pure (concat diags, Just moduleImports)

type RawDepM a = StateT (RawDependencyInformation, IntMap ArtifactsLocation) Action a

execRawDepM :: Monad m => StateT (RawDependencyInformation, IntMap a1) m a2 -> m (RawDependencyInformation, IntMap a1)
execRawDepM act =
    execStateT act
        ( RawDependencyInformation IntMap.empty emptyPathIdMap IntMap.empty
        , IntMap.empty
        )

-- | Given a target file path, construct the raw dependency results by following
-- imports recursively.
rawDependencyInformation :: [NormalizedFilePath] -> Action (RawDependencyInformation, BootIdMap)
rawDependencyInformation fs = do
    (rdi, ss) <- execRawDepM (goPlural fs)
    let bm = IntMap.foldrWithKey (updateBootMap rdi) IntMap.empty ss
    return (rdi, bm)
  where
    goPlural ff = do
        mss <- lift $ (fmap.fmap) msrModSummary <$> uses GetModSummaryWithoutTimestamps ff
        zipWithM go ff mss

    go :: NormalizedFilePath -- ^ Current module being processed
       -> Maybe ModSummary   -- ^ ModSummary of the module
       -> RawDepM FilePathId
    go f mbModSum = do
      -- First check to see if we have already processed the FilePath
      -- If we have, just return its Id but don't update any of the state.
      -- Otherwise, we need to process its imports.
      checkAlreadyProcessed f $ do
          let al = modSummaryToArtifactsLocation f mbModSum
          -- Get a fresh FilePathId for the new file
          fId <- getFreshFid al
          -- Record this module and its location
          whenJust mbModSum $ \ms ->
            modifyRawDepInfo (\rd -> rd { rawModuleMap = IntMap.insert (getFilePathId fId)
                                                                           (ShowableModule $ ms_mod ms)
                                                                           (rawModuleMap rd)})
          -- Adding an edge to the bootmap so we can make sure to
          -- insert boot nodes before the real files.
          addBootMap al fId
          -- Try to parse the imports of the file
          importsOrErr <- lift $ use GetLocatedImports f
          case importsOrErr of
            Nothing -> do
            -- File doesn't parse so add the module as a failure into the
            -- dependency information, continue processing the other
            -- elements in the queue
              modifyRawDepInfo (insertImport fId (Left ModuleParseError))
              return fId
            Just modImports -> do
              -- Get NFPs of the imports which have corresponding files
              -- Imports either come locally from a file or from a package.
              let (no_file, with_file) = splitImports modImports
                  (mns, ls) = unzip with_file
              -- Recursively process all the imports we just learnt about
              -- and get back a list of their FilePathIds
              fids <- goPlural $ map artifactFilePath ls
              -- Associate together the ModuleName with the FilePathId
              let moduleImports' = map (,Nothing) no_file ++ zip mns (map Just fids)
              -- Insert into the map the information about this modules
              -- imports.
              modifyRawDepInfo $ insertImport fId (Right $ ModuleImports moduleImports')
              return fId


    checkAlreadyProcessed :: NormalizedFilePath -> RawDepM FilePathId -> RawDepM FilePathId
    checkAlreadyProcessed nfp k = do
      (rawDepInfo, _) <- get
      maybe k return (lookupPathToId (rawPathIdMap rawDepInfo) nfp)

    modifyRawDepInfo :: (RawDependencyInformation -> RawDependencyInformation) -> RawDepM ()
    modifyRawDepInfo f = modify (first f)

    addBootMap ::  ArtifactsLocation -> FilePathId -> RawDepM ()
    addBootMap al fId =
      modify (\(rd, ss) -> (rd, if isBootLocation al
                                  then IntMap.insert (getFilePathId fId) al ss
                                  else ss))

    getFreshFid :: ArtifactsLocation -> RawDepM FilePathId
    getFreshFid al = do
      (rawDepInfo, ss) <- get
      let (fId, path_map) = getPathId al (rawPathIdMap rawDepInfo)
      -- Insert the File into the bootmap if it's a boot module
      let rawDepInfo' = rawDepInfo { rawPathIdMap = path_map }
      put (rawDepInfo', ss)
      return fId

    -- Split in (package imports, local imports)
    splitImports :: [(Located ModuleName, Maybe ArtifactsLocation)]
                 -> ([Located ModuleName], [(Located ModuleName, ArtifactsLocation)])
    splitImports = foldr splitImportsLoop ([],[])

    splitImportsLoop (imp, Nothing) (ns, ls)       = (imp:ns, ls)
    splitImportsLoop (imp, Just artifact) (ns, ls) = (ns, (imp,artifact) : ls)

    updateBootMap pm boot_mod_id ArtifactsLocation{..} bm =
      if not artifactIsSource
        then
          let msource_mod_id = lookupPathToId (rawPathIdMap pm) (toNormalizedFilePath' $ dropBootSuffix $ fromNormalizedFilePath artifactFilePath)
          in case msource_mod_id of
               Just source_mod_id -> insertBootId source_mod_id (FilePathId boot_mod_id) bm
               Nothing -> bm
        else bm

    dropBootSuffix :: FilePath -> FilePath
    dropBootSuffix hs_src = reverse . drop (length @[] "-boot") . reverse $ hs_src

reportImportCyclesRule :: Recorder (WithPriority Log) -> Rules ()
reportImportCyclesRule recorder =
    defineEarlyCutoff (cmapWithPrio LogShake recorder) $ Rule $ \ReportImportCycles file -> fmap (\errs -> if null errs then (Just "1",([], Just ())) else (Nothing, (errs, Nothing))) $ do
        DependencyInformation{..} <- useWithSeparateFingerprintRule_ GetModuleGraphTransDepsFingerprints GetModuleGraph file
        case pathToId depPathIdMap file of
          -- The header of the file does not parse, so it can't be part of any import cycles.
          Nothing -> pure []
          Just fileId ->
            case IntMap.lookup (getFilePathId fileId) depErrorNodes of
              Nothing -> pure []
              Just errs -> do
                  let cycles = mapMaybe (cycleErrorInFile fileId) (toList errs)
                  -- Convert cycles of files into cycles of module names
                  forM cycles $ \(imp, files) -> do
                      modNames <- forM files $
                          getModuleName . idToPath depPathIdMap
                      pure $ toDiag imp $ sort modNames
    where cycleErrorInFile f (PartOfCycle imp fs)
            | f `elem` fs = Just (imp, fs)
          cycleErrorInFile _ _ = Nothing
          toDiag imp mods =
            ideErrorWithSource (Just "Import cycle detection") (Just DiagnosticSeverity_Error) fp ("Cyclic module dependency between " <> showCycle mods) Nothing
              & fdLspDiagnosticL %~ JL.range .~ rng
            where rng = fromMaybe noRange $ srcSpanToRange (getLoc imp)
                  fp = toNormalizedFilePath' $ fromMaybe noFilePath $ srcSpanToFilename (getLoc imp)
          getModuleName file = do
           ms <- msrModSummary <$> use_ GetModSummaryWithoutTimestamps file
           pure (moduleNameString . moduleName . ms_mod $ ms)
          showCycle mods  = T.intercalate ", " (map T.pack mods)

getHieAstsRule :: Recorder (WithPriority Log) -> Rules ()
getHieAstsRule recorder =
    define (cmapWithPrio LogShake recorder) $ \GetHieAst f -> do
      tmr <- use_ TypeCheck f
      hsc <- hscEnv <$> use_ GhcSessionDeps f
      getHieAstRuleDefinition f hsc tmr

persistentHieFileRule :: Recorder (WithPriority Log) -> Rules ()
persistentHieFileRule recorder = addPersistentRule GetHieAst $ \file -> runMaybeT $ do
  res <- readHieFileForSrcFromDisk recorder file
  vfsRef <- asks vfsVar
  vfsData <- liftIO $ _vfsMap <$> readTVarIO vfsRef
  (currentSource, ver) <- liftIO $ case getVirtualFileFromVFS (VFS vfsData) (filePathToUri' file) of
    Nothing -> (,Nothing) . T.decodeUtf8 <$> BS.readFile (fromNormalizedFilePath file)
    Just vf -> pure (virtualFileText vf, Just $ virtualFileVersion vf)
  let refmap = generateReferencesMap . getAsts . Compat.hie_asts $ res
      del = deltaFromDiff (T.decodeUtf8 $ Compat.hie_hs_src res) currentSource
  pure (HAR (Compat.hie_module res) (Compat.hie_asts res) refmap mempty (HieFromDisk res),del,ver)

getHieAstRuleDefinition :: NormalizedFilePath -> HscEnv -> TcModuleResult -> Action (IdeResult HieAstResult)
getHieAstRuleDefinition f hsc tmr = do
  (diags, masts') <- liftIO $ generateHieAsts hsc tmr
#if MIN_VERSION_ghc(9,11,0)
  let masts = fst <$> masts'
#else
  let masts = masts'
#endif
  se <- getShakeExtras

  isFoi <- use_ IsFileOfInterest f
  diagsWrite <- case isFoi of
    IsFOI Modified{firstOpen = False} -> do
      when (coerce $ ideTesting se) $ liftIO $ mRunLspT (lspEnv se) $
        LSP.sendNotification (SMethod_CustomMethod (Proxy @"ghcide/reference/ready")) $
          toJSON $ fromNormalizedFilePath f
      pure []
    _ | Just asts <- masts' -> do
          source <- getSourceFileSource f
          let exports = tcg_exports $ tmrTypechecked tmr
              modSummary = tmrModSummary tmr
          liftIO $ writeAndIndexHieFile hsc se modSummary f exports asts source
    _ -> pure []

  let refmap = generateReferencesMap . getAsts <$> masts
      typemap = AtPoint.computeTypeReferences . getAsts <$> masts
  pure (diags <> diagsWrite, HAR (ms_mod $ tmrModSummary tmr) <$> masts <*> refmap <*> typemap <*> pure HieFresh)

getImportMapRule :: Recorder (WithPriority Log) -> Rules ()
getImportMapRule recorder = define (cmapWithPrio LogShake recorder) $ \GetImportMap f -> do
  im <- use GetLocatedImports f
  let mkImports fileImports = M.fromList $ mapMaybe (\(m, mfp) -> (unLoc m,) . artifactFilePath <$> mfp) fileImports
  pure ([], ImportMap . mkImports <$> im)

-- | Ensure that go to definition doesn't block on startup
persistentImportMapRule :: Rules ()
persistentImportMapRule = addPersistentRule GetImportMap $ \_ -> pure $ Just (ImportMap mempty, idDelta, Nothing)

getBindingsRule :: Recorder (WithPriority Log) -> Rules ()
getBindingsRule recorder =
  define (cmapWithPrio LogShake recorder) $ \GetBindings f -> do
    HAR{hieKind=kind, refMap=rm} <- use_ GetHieAst f
    case kind of
      HieFresh      -> pure ([], Just $ bindings rm)
      HieFromDisk _ -> pure ([], Nothing)

getDocMapRule :: Recorder (WithPriority Log) -> Rules ()
getDocMapRule recorder =
    define (cmapWithPrio LogShake recorder) $ \GetDocMap file -> do
      -- Stale data for the scenario where a broken module has previously typechecked
      -- but we never generated a DocMap for it
      (tmrTypechecked -> tc, _) <- useWithStale_ TypeCheck file
      (hscEnv -> hsc, _)        <- useWithStale_ GhcSessionDeps file
      (HAR{refMap=rf}, _)       <- useWithStale_ GetHieAst file

      dkMap <- liftIO $ mkDocMap hsc rf tc
      return ([],Just dkMap)

-- | Persistent rule to ensure that hover doesn't block on startup
persistentDocMapRule :: Rules ()
persistentDocMapRule = addPersistentRule GetDocMap $ \_ -> pure $ Just (DKMap mempty mempty mempty, idDelta, Nothing)

readHieFileForSrcFromDisk :: Recorder (WithPriority Log) -> NormalizedFilePath -> MaybeT IdeAction Compat.HieFile
readHieFileForSrcFromDisk recorder file = do
  ShakeExtras{withHieDb} <- ask
  row <- MaybeT $ liftIO $ withHieDb (\hieDb -> HieDb.lookupHieFileFromSource hieDb $ fromNormalizedFilePath file)
  let hie_loc = HieDb.hieModuleHieFile row
  liftIO $ logWith recorder Logger.Debug $ LogLoadingHieFile file
  exceptToMaybeT $ readHieFileFromDisk recorder hie_loc

readHieFileFromDisk :: Recorder (WithPriority Log) -> FilePath -> ExceptT SomeException IdeAction Compat.HieFile
readHieFileFromDisk recorder hie_loc = do
  nc <- asks ideNc
  res <- liftIO $ tryAny $ loadHieFile (mkUpdater nc) hie_loc
  case res of
    Left e -> liftIO $ logWith recorder Logger.Debug $ LogLoadingHieFileFail hie_loc e
    Right _ -> liftIO $ logWith recorder Logger.Debug $ LogLoadingHieFileSuccess hie_loc
  except res

-- | Typechecks a module.
typeCheckRule :: Recorder (WithPriority Log) -> Rules ()
typeCheckRule recorder = define (cmapWithPrio LogShake recorder) $ \TypeCheck file -> do
    pm <- use_ GetParsedModule file
    hsc  <- hscEnv <$> use_ GhcSessionDeps file
    foi <- use_ IsFileOfInterest file
    -- We should only call the typecheck rule for files of interest.
    -- Keeping typechecked modules in memory for other files is
    -- very expensive.
    when (foi == NotFOI) $
      logWith recorder Logger.Warning $ LogTypecheckedFOI file
    typeCheckRuleDefinition hsc pm file

knownFilesRule :: Recorder (WithPriority Log) -> Rules ()
knownFilesRule recorder = defineEarlyCutOffNoFile (cmapWithPrio LogShake recorder) $ \GetKnownTargets -> do
  alwaysRerun
  fs <- knownTargets
  pure (LBS.toStrict $ B.encode $ hash fs, unhashed fs)

getFileHashRule :: Recorder (WithPriority Log) -> Rules ()
getFileHashRule recorder =
    defineEarlyCutoff (cmapWithPrio LogShake recorder) $ Rule $ \GetFileHash file -> do
        void $ use_ GetModificationTime file
        fileHash <- liftIO $ Util.getFileHash (fromNormalizedFilePath file)
        return (Just (fingerprintToBS fileHash), ([], Just fileHash))

getModuleGraphRule :: Recorder (WithPriority Log) -> Rules ()
getModuleGraphRule recorder = defineEarlyCutOffNoFile (cmapWithPrio LogShake recorder) $ \GetModuleGraph -> do
  fs <- toKnownFiles <$> useNoFile_ GetKnownTargets
  dependencyInfoForFiles (HashSet.toList fs)

#if MIN_VERSION_ghc(9,13,0)
-- | Build level-aware module graph edges from a ModSummary and a list of dependency NodeKeys.
-- A module can be imported at multiple levels (e.g. @import splice M@ + @import M@),
-- so we collect ALL levels per module and produce one edge per (module, level) pair.
-- This is required for GHC 9.14's level-aware module graph (@mg_zero_graph@).
mkLevelEdges :: ModSummary -> [NodeKey] -> [ModuleNodeEdge]
mkLevelEdges ms dep_node_keys = concatMap (\nk -> map (\lvl -> mkModuleEdge lvl nk) (lookupLevels nk)) dep_node_keys
  where
    importLevelsMap = M.map nubOrd $ M.fromListWith (++)
      [(unLoc mn, [lvl]) | (lvl, _pkg, mn) <- ms_textual_imps ms]
    lookupLevels nk = case nk of
      NodeKey_Module mnk ->
        M.findWithDefault [NormalLevel] (gwib_mod $ mnkModuleName mnk) importLevelsMap
      _ -> [NormalLevel]
#endif

dependencyInfoForFiles :: [NormalizedFilePath] -> Action (BS.ByteString, DependencyInformation)
dependencyInfoForFiles fs = do
  (rawDepInfo, bm) <- rawDependencyInformation fs
  let (all_fs, _all_ids) = unzip $ HM.toList $ pathToIdMap $ rawPathIdMap rawDepInfo
  msrs <- uses GetModSummaryWithoutTimestamps all_fs
  let mss = map (fmap msrModSummary) msrs
  let deps = map (\i -> IM.lookup (getFilePathId i) (rawImports rawDepInfo)) _all_ids
      nodeKeys = IM.fromList $ catMaybes $ zipWith (\fi mms -> (getFilePathId fi,) . NodeKey_Module . msKey <$> mms) _all_ids mss
      mns = catMaybes $ zipWith go mss deps
#if MIN_VERSION_ghc(9,13,0)
      go (Just ms) (Just (Right (ModuleImports xs))) = Just $ ModuleNode this_dep_edges (ModuleNodeCompile ms)
        where this_dep_ids = mapMaybe snd xs
              this_dep_node_keys = mapMaybe (\fi -> IM.lookup (getFilePathId fi) nodeKeys) this_dep_ids
              this_dep_edges = mkLevelEdges ms this_dep_node_keys
      go (Just ms) _ = Just $ ModuleNode [] (ModuleNodeCompile ms)
#else
      go (Just ms) (Just (Right (ModuleImports xs))) = Just $ ModuleNode this_dep_keys ms
        where this_dep_ids = mapMaybe snd xs
              this_dep_keys = mapMaybe (\fi -> IM.lookup (getFilePathId fi) nodeKeys) this_dep_ids
      go (Just ms) _ = Just $ ModuleNode [] ms
#endif
      go _ _ = Nothing
      mg = mkModuleGraph mns
  let shallowFingers = IntMap.fromList $ foldr' (\(i, m) acc -> case m of
                                        Just x -> (getFilePathId i,msrFingerprint x):acc
                                        Nothing -> acc) [] $ zip _all_ids msrs
  pure (fingerprintToBS $ Util.fingerprintFingerprints $ map (maybe fingerprint0 msrFingerprint) msrs, processDependencyInformation rawDepInfo bm mg shallowFingers)

-- This is factored out so it can be directly called from the GetModIface
-- rule. Directly calling this rule means that on the initial load we can
-- garbage collect all the intermediate typechecked modules rather than
-- retain the information forever in the shake graph.
typeCheckRuleDefinition
    :: HscEnv
    -> ParsedModule
    -> NormalizedFilePath
    -> Action (IdeResult TcModuleResult)
typeCheckRuleDefinition hsc pm fp = do
  IdeOptions { optDefer = defer } <- getIdeOptions

  unlift <- askUnliftIO
  let dets = TypecheckHelpers
           { getLinkables = unliftIO unlift . uses_ GetLinkable
           , getModuleGraph = unliftIO unlift $ useWithSeparateFingerprintRule_ GetModuleGraphTransDepsFingerprints GetModuleGraph fp
           }
  addUsageDependencies $ liftIO $
    typecheckModule defer hsc dets pm
  where
    addUsageDependencies :: Action (a, Maybe TcModuleResult) -> Action (a, Maybe TcModuleResult)
    addUsageDependencies a = do
      r@(_, mtc) <- a
      forM_ mtc $ \tc -> do
        used_files <- liftIO $ readIORef $ tcg_dependent_files $ tmrTypechecked tc
        void $ uses_ GetModificationTime (map toNormalizedFilePath' used_files)
      return r

-- | Get all the linkables stored in the graph, i.e. the ones we *do not* need to unload.
-- Doesn't actually contain the code, since we don't need it to unload
currentLinkables :: Action (ModuleEnv UTCTime)
currentLinkables = do
    compiledLinkables <- getCompiledLinkables <$> getIdeGlobalAction
    liftIO $ readVar compiledLinkables

loadGhcSession :: Recorder (WithPriority Log) -> GhcSessionDepsConfig -> Rules ()
loadGhcSession recorder ghcSessionDepsConfig = do
    -- This function should always be rerun because it tracks changes
    -- to the version of the collection of HscEnv's.
    defineEarlyCutOffNoFile (cmapWithPrio LogShake recorder) $ \GhcSessionIO -> do
        alwaysRerun
        opts <- getIdeOptions
        config <- getClientConfigAction
        res <- optGhcSession opts

        let fingerprint = LBS.toStrict $ LBS.concat
                [ B.encode (hash (sessionVersion res))
                -- When the session version changes, reload all session
                -- hsc env sessions
                , B.encode (show (sessionLoading config))
                -- The loading config affects session loading.
                -- Invalidate all build nodes.
                -- Changing the session loading config will increment
                -- the 'sessionVersion', thus we don't generate the same fingerprint
                -- twice by accident.
                ]
        return (fingerprint, res)

    defineEarlyCutoff (cmapWithPrio LogShake recorder) $ Rule $ \GhcSession file -> do
        IdeGhcSession{loadSessionFun} <- useNoFile_ GhcSessionIO
        -- loading is always returning a absolute path now
        (val,deps) <- liftIO $ loadSessionFun $ fromNormalizedFilePath file

        -- add the deps to the Shake graph
        let addDependency fp = do
                -- VSCode uses absolute paths in its filewatch notifications
                let nfp = toNormalizedFilePath' fp
                itExists <- getFileExists nfp
                when itExists $ void $ do
                  use_ GetPhysicalModificationTime nfp

        mapM_ addDependency deps

        let cutoffHash = LBS.toStrict $ B.encode (hash (snd val))
        return (Just cutoffHash, val)

    defineNoDiagnostics (cmapWithPrio LogShake recorder) $ \(GhcSessionDeps_ fullModSummary) file -> do
        env <- use_ GhcSession file
        ghcSessionDepsDefinition fullModSummary ghcSessionDepsConfig env file

newtype GhcSessionDepsConfig = GhcSessionDepsConfig
    { fullModuleGraph :: Bool
    }
instance Default GhcSessionDepsConfig where
  def = GhcSessionDepsConfig
    { fullModuleGraph = True
    }

-- | Note [GhcSessionDeps]
--   ~~~~~~~~~~~~~~~~~~~~~
-- For a file 'Foo', GhcSessionDeps "Foo.hs" results in an HscEnv which includes
-- 1. HomeModInfo's (in the HUG/HPT) for all modules in the transitive closure of "Foo", **NOT** including "Foo" itself.
-- 2. ModSummary's (in the ModuleGraph) for all modules in the transitive closure of "Foo", including "Foo" itself.
-- 3. ModLocation's (in the FinderCache) all modules in the transitive closure of "Foo", including "Foo" itself.
ghcSessionDepsDefinition
    :: -- | full mod summary
        Bool ->
        GhcSessionDepsConfig -> HscEnvEq -> NormalizedFilePath -> Action (Maybe HscEnvEq)
ghcSessionDepsDefinition fullModSummary GhcSessionDepsConfig{..} hscEnvEq file = do
    mbdeps <- mapM(fmap artifactFilePath . snd) <$> use_ GetLocatedImports file
    case mbdeps of
        Nothing -> return Nothing
        Just deps -> do
            when fullModuleGraph $ void $ use_ ReportImportCycles file
            msr <- if fullModSummary
                then use_ GetModSummary file
                else use_ GetModSummaryWithoutTimestamps file
            let
                ms = msrModSummary msr
                -- This `HscEnv` has its plugins initialized in `parsePragmasIntoHscEnv`
                -- Fixes the bug in #4631
                env = msrHscEnv msr
            depSessions <- map hscEnv <$> uses_ (GhcSessionDeps_ fullModSummary) deps
            ifaces <- uses_ GetModIface deps
            let inLoadOrder = map (\HiFileResult{..} -> HomeModInfo hirModIface hirModDetails emptyHomeModInfoLinkable) ifaces
            de <- useWithSeparateFingerprintRule_ GetModuleGraphTransDepsFingerprints GetModuleGraph file
            mg <- do
              if fullModuleGraph
              then return $ depModuleGraph de
              else do
                let mgs = map hsc_mod_graph depSessions
                -- On GHC 9.4+, the module graph contains not only ModSummary's but each `ModuleNode` in the graph
                -- also points to all the direct descendants of the current module. To get the keys for the descendants
                -- we must get their `ModSummary`s
                !final_deps <- do
                  dep_mss <- map msrModSummary <$> uses_ GetModSummaryWithoutTimestamps deps
                  return $!! map (NodeKey_Module . msKey) dep_mss
#if MIN_VERSION_ghc(9,13,0)
                let final_dep_edges = mkLevelEdges ms final_deps
                let module_graph_nodes =
                      nubOrdOn mkNodeKey (ModuleNode final_dep_edges (ModuleNodeCompile ms) : concatMap mgModSummaries' mgs)
#else
                let module_graph_nodes =
                      nubOrdOn mkNodeKey (ModuleNode final_deps ms : concatMap mgModSummaries' mgs)
#endif
                liftIO $ evaluate $ liftRnf rwhnf module_graph_nodes
                return $ mkModuleGraph module_graph_nodes
            session' <- liftIO $ mergeEnvs env mg de ms inLoadOrder depSessions

            -- Here we avoid a call to to `newHscEnvEqWithImportPaths`, which creates a new
            -- ExportsMap when it is called. We only need to create the ExportsMap once per
            -- session, while `ghcSessionDepsDefinition` will be called for each file we need
            -- to compile. `updateHscEnvEq` will refresh the HscEnv (session') and also
            -- generate a new Unique.
            Just <$> liftIO (updateHscEnvEq hscEnvEq session')

-- | Load a iface from disk, or generate it if there isn't one or it is out of date
-- This rule also ensures that the `.hie` and `.o` (if needed) files are written out.
getModIfaceFromDiskRule :: Recorder (WithPriority Log) -> Rules ()
getModIfaceFromDiskRule recorder = defineEarlyCutoff (cmapWithPrio LogShake recorder) $ RuleWithOldValue $ \GetModIfaceFromDisk f old -> do
  ms <- msrModSummary <$> use_ GetModSummary f
  mb_session <- use GhcSessionDeps f
  case mb_session of
    Nothing -> return (Nothing, ([], Nothing))
    Just session -> do
      linkableType <- getLinkableType f
      ver <- use_ GetModificationTime f
      let m_old = case old of
            Shake.Succeeded (Just old_version) v -> Just (v, old_version)
            Shake.Stale _   (Just old_version) v -> Just (v, old_version)
            _                                    -> Nothing
          recompInfo = RecompilationInfo
            { source_version = ver
            , old_value = m_old
            , get_file_version = use GetModificationTime_{missingFileDiagnostics = False}
            , get_linkable_hashes = \fs -> map (snd . fromJust . hirCoreFp) <$> uses_ GetModIface fs
            , get_module_graph = useWithSeparateFingerprintRule_ GetModuleGraphTransDepsFingerprints GetModuleGraph f
            , regenerate = regenerateHiFile session f ms
            }
      hsc_env' <- setFileCacheHook (hscEnv session)
      r <- loadInterface hsc_env' ms linkableType recompInfo
      case r of
        (diags, Nothing) -> return (Nothing, (diags, Nothing))
        (diags, Just x) -> do
          let !fp = Just $! hiFileFingerPrint x
          return (fp, (diags, Just x))

-- | Check state of hiedb after loading an iface from disk - have we indexed the corresponding `.hie` file?
-- This function is responsible for ensuring database consistency
-- Whenever we read a `.hi` file, we must check to ensure we have also
-- indexed the corresponding `.hie` file. If this is not the case (for example,
-- `ghcide` could be killed before indexing finishes), we must re-index the
-- `.hie` file. There should be an up2date `.hie` file on
-- disk since we are careful to write out the `.hie` file before writing the
-- `.hi` file
getModIfaceFromDiskAndIndexRule :: Recorder (WithPriority Log) -> Rules ()
getModIfaceFromDiskAndIndexRule recorder =
  -- doesn't need early cutoff since all its dependencies already have it
  defineNoDiagnostics (cmapWithPrio LogShake recorder) $ \GetModIfaceFromDiskAndIndex f -> do
  x <- use_ GetModIfaceFromDisk f
  se@ShakeExtras{withHieDb} <- getShakeExtras

  -- GetModIfaceFromDisk should have written a `.hie` file, must check if it matches version in db
  let ms = hirModSummary x
      hie_loc = Compat.ml_hie_file $ ms_location ms
  fileHash <- liftIO $ Util.getFileHash hie_loc
  mrow <- liftIO $ withHieDb (\hieDb -> HieDb.lookupHieFileFromSource hieDb (fromNormalizedFilePath f))
  let hie_loc' = HieDb.hieModuleHieFile <$> mrow
  case mrow of
    Just row
      | fileHash == HieDb.modInfoHash (HieDb.hieModInfo row)
      && Just hie_loc == hie_loc'
      -> do
      -- All good, the db has indexed the file
      when (coerce $ ideTesting se) $ liftIO $ mRunLspT (lspEnv se) $
        LSP.sendNotification (SMethod_CustomMethod (Proxy @"ghcide/reference/ready")) $
          toJSON $ fromNormalizedFilePath f
    -- Not in db, must re-index
    _ -> do
      ehf <- liftIO $ runIdeAction "GetModIfaceFromDiskAndIndex" se $ runExceptT $
        readHieFileFromDisk recorder hie_loc
      case ehf of
        -- Uh oh, we failed to read the file for some reason, need to regenerate it
        Left err -> fail $ "failed to read .hie file " ++ show hie_loc ++ ": " ++ displayException err
        -- can just re-index the file we read from disk
        Right hf -> liftIO $ do
          logWith recorder Logger.Debug $ LogReindexingHieFile f
          indexHieFile se ms f fileHash hf

  return (Just x)

newtype DisplayTHWarning = DisplayTHWarning (IO())
instance IsIdeGlobal DisplayTHWarning

getModSummaryRule :: LspT Config IO () -> Recorder (WithPriority Log) -> Rules ()
getModSummaryRule displayTHWarning recorder = do
    menv <- lspEnv <$> getShakeExtrasRules
    case menv of
      Just env -> do
        displayItOnce <- liftIO $ once $ LSP.runLspT env displayTHWarning
        addIdeGlobal (DisplayTHWarning displayItOnce)
      Nothing -> do
        logItOnce <- liftIO $ once $ putStrLn ""
        addIdeGlobal (DisplayTHWarning logItOnce)

    defineEarlyCutoff (cmapWithPrio LogShake recorder) $ Rule $ \GetModSummary f -> do
        session' <- hscEnv <$> use_ GhcSession f
        modify_dflags <- getModifyDynFlags dynFlagsModifyGlobal
        let session = setNonHomeFCHook $ hscSetFlags (modify_dflags $ hsc_dflags session') session' -- TODO wz1000
        (modTime, mFileContent) <- getFileModTimeContents f
        let fp = fromNormalizedFilePath f
        modS <- liftIO $ runExceptT $
                getModSummaryFromImports session fp modTime (textToStringBuffer . Rope.toText <$> mFileContent)
        case modS of
            Right res -> do
                -- Check for Template Haskell
                when (uses_th_qq $ msrModSummary res) $ do
                    DisplayTHWarning act <- getIdeGlobalAction
                    liftIO act
                let bufFingerPrint = ms_hs_hash (msrModSummary res)
                let fingerPrint = Util.fingerprintFingerprints
                        [ msrFingerprint res, bufFingerPrint ]
                return ( Just (fingerprintToBS fingerPrint) , ([], Just res))
            Left diags -> return (Nothing, (diags, Nothing))

    defineEarlyCutoff (cmapWithPrio LogShake recorder) $ RuleNoDiagnostics $ \GetModSummaryWithoutTimestamps f -> do
        mbMs <- use GetModSummary f
        case mbMs of
            Just res@ModSummaryResult{..} -> do
                let ms = msrModSummary {
                    ms_hspp_buf = error "use GetModSummary instead of GetModSummaryWithoutTimestamps"
                    }
                    fp = fingerprintToBS msrFingerprint
                return (Just fp, Just res{msrModSummary = ms})
            Nothing -> return (Nothing, Nothing)

generateCore :: RunSimplifier -> NormalizedFilePath -> Action (IdeResult ModGuts)
generateCore runSimplifier file = do
    packageState <- hscEnv <$> use_ GhcSessionDeps file
    hsc' <- setFileCacheHook packageState
    tm <- use_ TypeCheck file
    liftIO $ compileModule runSimplifier hsc' (tmrModSummary tm) (tmrTypechecked tm)

generateCoreRule :: Recorder (WithPriority Log) -> Rules ()
generateCoreRule recorder =
    define (cmapWithPrio LogShake recorder) $ \GenerateCore -> generateCore (RunSimplifier True)

getModIfaceRule :: Recorder (WithPriority Log) -> Rules ()
getModIfaceRule recorder = defineEarlyCutoff (cmapWithPrio LogShake recorder) $ Rule $ \GetModIface f -> do
  fileOfInterest <- use_ IsFileOfInterest f
  res <- case fileOfInterest of
    IsFOI status -> do
      -- Never load from disk for files of interest
      tmr <- use_ TypeCheck f
      linkableType <- getLinkableType f
      hsc <- hscEnv <$> use_ GhcSessionDeps f
      hsc' <- setFileCacheHook hsc
      let compile = fmap ([],) $ use GenerateCore f
      se <- getShakeExtras
      (diags, !mbHiFile) <- writeCoreFileIfNeeded se hsc' linkableType compile tmr
      let fp = hiFileFingerPrint <$> mbHiFile
      hiDiags <- case mbHiFile of
        Just hiFile
          | OnDisk <- status
          , not (tmrDeferredError tmr) -> liftIO $ writeHiFile se hsc' hiFile
        _ -> pure []
      return (fp, (diags++hiDiags, mbHiFile))
    NotFOI -> do
      hiFile <- use GetModIfaceFromDiskAndIndex f
      let fp = hiFileFingerPrint <$> hiFile
      return (fp, ([], hiFile))

  pure res

-- | Count of total times we asked GHC to recompile
newtype RebuildCounter = RebuildCounter { getRebuildCountVar :: TVar Int }
instance IsIdeGlobal RebuildCounter

getRebuildCount :: Action Int
getRebuildCount = do
  count <- getRebuildCountVar <$> getIdeGlobalAction
  liftIO $ readTVarIO count

incrementRebuildCount :: Action ()
incrementRebuildCount = do
  count <- getRebuildCountVar <$> getIdeGlobalAction
  liftIO $ atomically $ modifyTVar' count (+1)

setFileCacheHook :: HscEnv -> Action HscEnv
setFileCacheHook old_hsc_env = do
#if MIN_VERSION_ghc(9,11,0)
  unlift <- askUnliftIO
  return $ old_hsc_env { hsc_FC = (hsc_FC old_hsc_env) { lookupFileCache = unliftIO unlift . use_ GetFileHash . toNormalizedFilePath'  } }
#else
  return old_hsc_env
#endif

-- | Also generates and indexes the `.hie` file, along with the `.o` file if needed
-- Invariant maintained is that if the `.hi` file was successfully written, then the
-- `.hie` and `.o` file (if needed) were also successfully written
regenerateHiFile :: HscEnvEq -> NormalizedFilePath -> ModSummary -> Maybe LinkableType -> Action ([FileDiagnostic], Maybe HiFileResult)
regenerateHiFile sess f ms compNeeded = do
    hsc <- setFileCacheHook (hscEnv sess)
    opt <- getIdeOptions

    -- By default, we parse with `-haddock` unless 'OptHaddockParse' is overwritten.
    (diags, mb_pm) <- liftIO $ getParsedModuleDefinition hsc opt f ms
    case mb_pm of
        Nothing -> return (diags, Nothing)
        Just pm -> do
            -- Invoke typechecking directly to update it without incurring a dependency
            -- on the parsed module and the typecheck rules
            (diags', mtmr) <- typeCheckRuleDefinition hsc pm f
            case mtmr of
              Nothing -> pure (diags', Nothing)
              Just tmr -> do

                let compile = liftIO $ compileModule (RunSimplifier True) hsc (pm_mod_summary pm) $ tmrTypechecked tmr

                se <- getShakeExtras

                -- Bang pattern is important to avoid leaking 'tmr'
                (diags'', !res) <- writeCoreFileIfNeeded se hsc compNeeded compile tmr

                -- Write hi file
                hiDiags <- case res of
                  Just !hiFile -> do

                    -- Write hie file. Do this before writing the .hi file to
                    -- ensure that we always have a up2date .hie file if we have
                    -- a .hi file
                    se' <- getShakeExtras
                    (gDiags, masts) <- liftIO $ generateHieAsts hsc tmr
                    source <- getSourceFileSource f
                    wDiags <- forM masts $ \asts ->
                      liftIO $ writeAndIndexHieFile hsc se' (tmrModSummary tmr) f (tcg_exports $ tmrTypechecked tmr) asts source

                    -- We don't write the `.hi` file if there are deferred errors, since we won't get
                    -- accurate diagnostics next time if we do
                    hiDiags <- if not $ tmrDeferredError tmr
                               then liftIO $ writeHiFile se' hsc hiFile
                               else pure []

                    pure (hiDiags <> gDiags <> concat wDiags)
                  Nothing -> pure []

                return (diags <> diags' <> diags'' <> hiDiags, res)


-- | HscEnv should have deps included already
-- This writes the core file if a linkable is required
-- The actual linkable will be generated on demand when required by `GetLinkable`
writeCoreFileIfNeeded :: ShakeExtras -> HscEnv -> Maybe LinkableType -> Action (IdeResult ModGuts) -> TcModuleResult -> Action (IdeResult HiFileResult)
writeCoreFileIfNeeded _ hsc Nothing _ tmr = do
  incrementRebuildCount
  res <- liftIO $ mkHiFileResultNoCompile hsc tmr
  pure ([], Just $! res)
writeCoreFileIfNeeded se hsc (Just _) getGuts tmr = do
  incrementRebuildCount
  (diags, mguts) <- getGuts
  case mguts of
    Nothing -> pure (diags, Nothing)
    Just guts -> do
      (diags', !res) <- liftIO $ mkHiFileResultCompile se hsc tmr guts
      pure (diags++diags', res)

-- See Note [Client configuration in Rules]
getClientSettingsRule :: Recorder (WithPriority Log) -> Rules ()
getClientSettingsRule recorder = defineEarlyCutOffNoFile (cmapWithPrio LogShake recorder) $ \GetClientSettings -> do
  alwaysRerun
  settings <- clientSettings <$> getIdeConfiguration
  return (LBS.toStrict $ B.encode $ hash settings, settings)

usePropertyAction ::
  (HasProperty s k t r) =>
  KeyNameProxy s ->
  PluginId ->
  Properties r ->
  Action (ToHsType t)
usePropertyAction kn plId p = do
  pluginConfig <- getPluginConfigAction plId
  pure $ useProperty kn p $ plcConfig pluginConfig

usePropertyByPathAction ::
  (HasPropertyByPath props path t) =>
  KeyNamePath path ->
  PluginId ->
  Properties props ->
  Action (ToHsType t)
usePropertyByPathAction path plId p = do
  pluginConfig <- getPluginConfigAction plId
  pure $ usePropertyByPath path p $ plcConfig pluginConfig

-- ---------------------------------------------------------------------

getLinkableRule :: Recorder (WithPriority Log) -> Rules ()
getLinkableRule recorder =
  defineEarlyCutoff (cmapWithPrio LogShake recorder) $ Rule $ \GetLinkable f -> do
    HiFileResult{hirModSummary, hirModIface, hirModDetails, hirCoreFp} <- use_ GetModIface f
    let obj_file  = ml_obj_file (ms_location hirModSummary)
        core_file = ml_core_file (ms_location hirModSummary)
#if MIN_VERSION_ghc(9,11,0)
        mkLinkable t mod l = Linkable t mod (pure l)
        dotO o = DotO o ModuleObject
#else
        mkLinkable t mod l = LM t mod [l]
        dotO = DotO
#endif
    case hirCoreFp of
      Nothing -> error $ "called GetLinkable for a file without a linkable: " ++ show f
      Just (bin_core, fileHash) -> do
        session <- use_ GhcSessionDeps f
        linkableType <- getLinkableType f >>= \case
          Nothing -> error $ "called GetLinkable for a file which doesn't need compilation: " ++ show f
          Just t -> pure t
        -- Can't use `GetModificationTime` rule because the core file was possibly written in this
        -- very session, so the results aren't reliable
        core_t <- liftIO $ getModTime core_file
        (warns, hmi) <- case linkableType of
          -- Bytecode needs to be regenerated from the core file
          BCOLinkable -> liftIO $ coreFileToLinkable linkableType (hscEnv session) hirModSummary hirModIface hirModDetails bin_core (posixSecondsToUTCTime core_t)
          -- Object code can be read from the disk
          ObjectLinkable -> do
            -- object file is up to date if it is newer than the core file
            -- Can't use a rule like 'GetModificationTime' or 'GetFileExists' because 'coreFileToLinkable' will write the object file, and
            -- thus bump its modification time, forcing this rule to be rerun every time.
            exists <- liftIO $ doesFileExist obj_file
            mobj_time <- liftIO $
              if exists
              then Just <$> getModTime obj_file
              else pure Nothing
            case mobj_time of
              Just obj_t
                | obj_t >= core_t -> pure ([], Just $ HomeModInfo hirModIface hirModDetails (justObjects $ mkLinkable (posixSecondsToUTCTime obj_t) (ms_mod hirModSummary) (dotO obj_file)))
              _ -> liftIO $ coreFileToLinkable linkableType (hscEnv session) hirModSummary hirModIface hirModDetails bin_core (error "object doesn't have time")
        -- Record the linkable so we know not to unload it, and unload old versions
        whenJust ((homeModInfoByteCode =<< hmi) <|> (homeModInfoObject =<< hmi))
#if MIN_VERSION_ghc(9,11,0)
          $ \(Linkable time mod _) -> do
#else
          $ \(LM time mod _) -> do
#endif
            compiledLinkables <- getCompiledLinkables <$> getIdeGlobalAction
            liftIO $ modifyVar compiledLinkables $ \old -> do
              let !to_keep = extendModuleEnv old mod time
              --We need to unload old linkables before we can load in new linkables. However,
              --the unload function in the GHC API takes a list of linkables to keep (i.e.
              --not unload). Earlier we unloaded right before loading in new linkables, which
              --is effectively once per splice. This can be slow as unload needs to walk over
              --the list of all loaded linkables, for each splice.
              --
              --Solution: now we unload old linkables right after we generate a new linkable and
              --just before returning it to be loaded. This has a substantial effect on recompile
              --times as the number of loaded modules and splices increases.
              --
              --We use a dummy DotA linkable part to fake a NativeCode linkable.
              --The unload function doesn't care about the exact linkable parts.
              unload (hscEnv session) (map (\(mod', time') -> mkLinkable time' mod' (DotA "dummy")) $ moduleEnvToList to_keep)
              return (to_keep, ())
        return (fileHash <$ hmi, (warns, LinkableResult <$> hmi <*> pure fileHash))

-- | For now we always use bytecode unless something uses unboxed sums and tuples along with TH
getLinkableType :: NormalizedFilePath -> Action (Maybe LinkableType)
getLinkableType f = use_ NeedsCompilation f

needsCompilationRule :: NormalizedFilePath  -> Action (IdeResultNoDiagnosticsEarlyCutoff (Maybe LinkableType))
needsCompilationRule file
  | "boot" `isSuffixOf` fromNormalizedFilePath file =
    pure (Just $ encodeLinkableType Nothing, Just Nothing)
needsCompilationRule file = do
  graph <- useWithSeparateFingerprintRule GetModuleGraphImmediateReverseDepsFingerprints GetModuleGraph file
  res <- case graph of
    -- Treat as False if some reverse dependency header fails to parse
    Nothing -> pure Nothing
    Just depinfo -> case immediateReverseDependencies file depinfo of
      -- If we fail to get immediate reverse dependencies, fail with an error message
      Nothing -> fail $ "Failed to get the immediate reverse dependencies of " ++ show file
      Just revdeps -> do
        -- It's important to use stale data here to avoid wasted work.
        -- if NeedsCompilation fails for a module M its result will be  under-approximated
        -- to False in its dependencies. However, if M actually used TH, this will
        -- cause a re-evaluation of GetModIface for all dependencies
        -- (since we don't need to generate object code anymore).
        -- Once M is fixed we will discover that we actually needed all the object code
        -- that we just threw away, and thus have to recompile all dependencies once
        -- again, this time keeping the object code.
        -- A file needs to be compiled if any file that depends on it uses TemplateHaskell or needs to be compiled
        (modsums,needsComps) <- liftA2
            (,) (map (fmap (msrModSummary . fst)) <$> usesWithStale GetModSummaryWithoutTimestamps revdeps)
                (uses NeedsCompilation revdeps)
        pure $ computeLinkableType modsums (map join needsComps)
  pure (Just $ encodeLinkableType res, Just res)
  where
    computeLinkableType :: [Maybe ModSummary] -> [Maybe LinkableType] -> Maybe LinkableType
    computeLinkableType deps xs
      | Just ObjectLinkable `elem` xs     = Just ObjectLinkable -- If any dependent needs object code, so do we
      | Just BCOLinkable    `elem` xs     = Just BCOLinkable    -- If any dependent needs bytecode, then we need to be compiled
      | any (maybe False uses_th_qq) deps = Just BCOLinkable    -- If any dependent needs TH, then we need to be compiled
      | otherwise                         = Nothing             -- If none of these conditions are satisfied, we don't need to compile

uses_th_qq :: ModSummary -> Bool
uses_th_qq (ms_hspp_opts -> dflags) =
      xopt LangExt.TemplateHaskell dflags || xopt LangExt.QuasiQuotes dflags

-- | Tracks which linkables are current, so we don't need to unload them
newtype CompiledLinkables = CompiledLinkables { getCompiledLinkables :: Var (ModuleEnv UTCTime) }
instance IsIdeGlobal CompiledLinkables

data RulesConfig = RulesConfig
    { -- | Share the computation for the entire module graph
      -- We usually compute the full module graph for the project
      -- and share it for all files.
      -- However, in large projects it might not be desirable to wait
      -- for computing the entire module graph before starting to
      -- typecheck a particular file.
      -- Disabling this drastically decreases sharing and is likely to
      -- increase memory usage if you have multiple files open
      -- Disabling this also disables checking for import cycles
      fullModuleGraph        :: Bool
    -- | Disable TH for improved performance in large codebases
    , enableTemplateHaskell  :: Bool
    -- | Warning to show when TH is not supported by the current HLS binary
    , templateHaskellWarning :: LspT Config IO ()
    }

instance Default RulesConfig where
    def = RulesConfig True True displayTHWarning
      where
        displayTHWarning :: LspT c IO ()
        displayTHWarning
            | not isWindows && not hostIsDynamic = do
                LSP.sendNotification SMethod_WindowShowMessage $
                    ShowMessageParams MessageType_Info thWarningMessage
            | otherwise = return ()

thWarningMessage :: T.Text
thWarningMessage = T.unwords
  [ "This HLS binary does not support Template Haskell."
  , "Follow the [instructions](" <> templateHaskellInstructions <> ")"
  , "to build an HLS binary with support for Template Haskell."
  ]

-- | A rule that wires per-file rules together
mainRule :: Recorder (WithPriority Log) -> RulesConfig -> Rules ()
mainRule recorder RulesConfig{..} = do
    linkables <- liftIO $ newVar emptyModuleEnv
    addIdeGlobal $ CompiledLinkables linkables
    rebuildCountVar <- liftIO $ newTVarIO 0
    addIdeGlobal $ RebuildCounter rebuildCountVar
    getParsedModuleRule recorder
    getParsedModuleWithCommentsRule recorder
    getLocatedImportsRule recorder
    reportImportCyclesRule recorder
    typeCheckRule recorder
    getDocMapRule recorder
    loadGhcSession recorder def{fullModuleGraph}
    getModIfaceFromDiskRule recorder
    getModIfaceFromDiskAndIndexRule recorder
    getModIfaceRule recorder
    getModSummaryRule templateHaskellWarning recorder
    getModuleGraphRule recorder
    getFileHashRule recorder
    knownFilesRule recorder
    getClientSettingsRule recorder
    getHieAstsRule recorder
    getBindingsRule recorder
    -- This rule uses a custom newness check that relies on the encoding
    --  produced by 'encodeLinkable'. This works as follows:
    --   * <previous> -> <new>
    --   * ObjectLinkable -> BCOLinkable : the prev linkable can be reused,  signal "no change"
    --   * Object/BCO -> NoLinkable      : the prev linkable can be ignored, signal "no change"
    --   * otherwise                     : the prev linkable cannot be reused, signal "value has changed"
    if enableTemplateHaskell
      then defineEarlyCutoff (cmapWithPrio LogShake recorder) $ RuleWithCustomNewnessCheck (<=) $ \NeedsCompilation file ->
                needsCompilationRule file
      else defineNoDiagnostics (cmapWithPrio LogShake recorder) $ \NeedsCompilation _ -> return $ Just Nothing
    generateCoreRule recorder
    getImportMapRule recorder
    persistentHieFileRule recorder
    persistentDocMapRule
    persistentImportMapRule
    getLinkableRule recorder
    defineEarlyCutoff (cmapWithPrio LogShake recorder) $ Rule $ \GetModuleGraphTransDepsFingerprints file -> do
        di <- useNoFile_ GetModuleGraph
        let finger = lookupFingerprint file di (depTransDepsFingerprints di)
        return (fingerprintToBS <$> finger, ([], finger))
    defineEarlyCutoff (cmapWithPrio LogShake recorder) $ Rule $ \GetModuleGraphTransReverseDepsFingerprints file -> do
        di <- useNoFile_ GetModuleGraph
        let finger = lookupFingerprint file di (depTransReverseDepsFingerprints di)
        return (fingerprintToBS <$> finger, ([], finger))
    defineEarlyCutoff (cmapWithPrio LogShake recorder) $ Rule $ \GetModuleGraphImmediateReverseDepsFingerprints file -> do
        di <- useNoFile_ GetModuleGraph
        let finger = lookupFingerprint file di (depImmediateReverseDepsFingerprints di)
        return (fingerprintToBS <$> finger, ([], finger))


-- | Get HieFile for haskell file on NormalizedFilePath
getHieFile :: NormalizedFilePath -> Action (Maybe HieFile)
getHieFile nfp = runMaybeT $ do
  HAR {hieAst} <- MaybeT $ use GetHieAst nfp
  tmr <- MaybeT $ use TypeCheck nfp
  ghc <- MaybeT $ use GhcSession nfp
  msr <- MaybeT $ use GetModSummaryWithoutTimestamps nfp
  source <- lift $ getSourceFileSource nfp
  let exports = tcg_exports $ tmrTypechecked tmr
  typedAst <- MaybeT $ pure $ cast hieAst
  liftIO $ runHsc (hscEnv ghc) $ mkHieFile' (msrModSummary msr) exports typedAst source
