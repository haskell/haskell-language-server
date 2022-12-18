-- Copyright (c) 2019 The DAML Authors. All rights reserved.
-- SPDX-License-Identifier: Apache-2.0

{-# LANGUAGE CPP                   #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE TypeFamilies          #-}

-- | A Shake implementation of the compiler service, built
--   using the "Shaker" abstraction layer for in-memory use.
--
module Development.IDE.Core.Rules(
    -- * Types
    IdeState, GetParsedModule(..), TransitiveDependencies(..),
    Priority(..), GhcSessionIO(..), GetClientSettings(..),
    -- * Functions
    priorityTypeCheck,
    priorityGenerateCore,
    priorityFilesOfInterest,
    runAction,
    toIdeResult,
    defineNoFile,
    defineEarlyCutOffNoFile,
    mainRule,
    RulesConfig(..),
    getDependencies,
    getParsedModule,
    getParsedModuleWithComments,
    getClientConfigAction,
    usePropertyAction,
    getHieFile,
    -- * Rules
    CompiledLinkables(..),
    getParsedModuleRule,
    getParsedModuleWithCommentsRule,
    getLocatedImportsRule,
    getDependencyInformationRule,
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
    computeLinkableTypeForDynFlags,
    generateCoreRule,
    getImportMapRule,
    regenerateHiFile,
    ghcSessionDepsDefinition,
    getParsedModuleDefinition,
    typeCheckRuleDefinition,
    getRebuildCount,
    getSourceFileSource,
    GhcSessionDepsConfig(..),
    Log(..),
    DisplayTHWarning(..),
    ) where

import           Control.Concurrent.Async                     (concurrently)
import           Control.Concurrent.Strict
import           Control.DeepSeq
import           Control.Exception.Safe
import           Control.Monad.Extra
import           Control.Monad.Reader
import           Control.Monad.State
import           Control.Monad.Trans.Except                   (ExceptT, except,
                                                               runExceptT)
import           Control.Monad.Trans.Maybe
import           Data.Aeson                                   (Result (Success),
                                                               toJSON)
import qualified Data.Aeson.Types                             as A
import qualified Data.Binary                                  as B
import qualified Data.ByteString                              as BS
import qualified Data.ByteString.Lazy                         as LBS
import           Data.Coerce
import           Data.Foldable
import qualified Data.HashMap.Strict                          as HM
import qualified Data.HashSet                                 as HashSet
import           Data.Hashable
import           Data.IORef
import           Control.Concurrent.STM.TVar
import           Data.IntMap.Strict                           (IntMap)
import qualified Data.IntMap.Strict                           as IntMap
import           Data.List
import qualified Data.Map                                     as M
import           Data.Maybe
import qualified Data.Text.Utf16.Rope                         as Rope
import qualified Data.Set                                     as Set
import qualified Data.Text                                    as T
import qualified Data.Text.Encoding                           as T
import           Data.Time                                    (UTCTime (..))
import           Data.Tuple.Extra
import           Data.Typeable                                (cast)
import           Development.IDE.Core.Compile
import           Development.IDE.Core.FileExists hiding (LogShake, Log)
import           Development.IDE.Core.FileStore               (getFileContents,
                                                               getModTime)
import           Development.IDE.Core.IdeConfiguration
import           Development.IDE.Core.OfInterest hiding (LogShake, Log)
import           Development.IDE.Core.PositionMapping
import           Development.IDE.Core.RuleTypes
import           Development.IDE.Core.Service hiding (LogShake, Log)
import           Development.IDE.Core.Shake hiding (Log)
import           Development.IDE.GHC.Compat.Env
import           Development.IDE.GHC.Compat                   hiding
                                                              (vcat, nest, parseModule,
                                                               TargetId(..),
                                                               loadInterface,
                                                               Var,
                                                               (<+>))
import qualified Development.IDE.GHC.Compat                   as Compat hiding (vcat, nest)
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
import qualified GHC.LanguageExtensions                       as LangExt
import qualified HieDb
import           Ide.Plugin.Config
import qualified Language.LSP.Server                          as LSP
import           Language.LSP.Types                           (SMethod (SCustomMethod, SWindowShowMessage), ShowMessageParams (ShowMessageParams), MessageType (MtInfo))
import           Language.LSP.VFS
import           System.Directory                             (makeAbsolute, doesFileExist)
import           Data.Default                                 (def, Default)
import           Ide.Plugin.Properties                        (HasProperty,
                                                               KeyNameProxy,
                                                               Properties,
                                                               ToHsType,
                                                               useProperty)
import           Ide.PluginUtils                              (configForPlugin)
import           Ide.Types                                    (DynFlagsModifications (dynFlagsModifyGlobal, dynFlagsModifyParser),
                                                               PluginId, PluginDescriptor (pluginId), IdePlugins (IdePlugins))
import Control.Concurrent.STM.Stats (atomically)
import Language.LSP.Server (LspT)
import System.Info.Extra (isWindows)
import HIE.Bios.Ghc.Gap (hostIsDynamic)
import Development.IDE.Types.Logger (Recorder, logWith, cmapWithPrio, WithPriority, Pretty (pretty), (<+>), nest, vcat)
import qualified Development.IDE.Core.Shake as Shake
import qualified Development.IDE.Types.Logger as Logger
import qualified Development.IDE.Types.Shake as Shake
import           Development.IDE.GHC.CoreFile
import           Data.Time.Clock.POSIX             (posixSecondsToUTCTime)
import Control.Monad.IO.Unlift
#if MIN_VERSION_ghc(9,3,0)
import GHC.Unit.Module.Graph
import GHC.Unit.Env
#endif

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
    LogShake log -> pretty log
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
-- | Get all transitive file dependencies of a given module.
-- Does not include the file itself.
getDependencies :: NormalizedFilePath -> Action (Maybe [NormalizedFilePath])
getDependencies file =
    fmap transitiveModuleDeps . (`transitiveDeps` file) <$> use_ GetDependencyInformation file

getSourceFileSource :: NormalizedFilePath -> Action BS.ByteString
getSourceFileSource nfp = do
    (_, msource) <- getFileContents nfp
    case msource of
        Nothing     -> liftIO $ BS.readFile (fromNormalizedFilePath nfp)
        Just source -> pure $ T.encodeUtf8 source

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

priorityTypeCheck :: Priority
priorityTypeCheck = Priority 0

priorityGenerateCore :: Priority
priorityGenerateCore = Priority (-1)

priorityFilesOfInterest :: Priority
priorityFilesOfInterest = Priority (-2)

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
    ModSummaryResult{msrModSummary = ms'} <- use_ GetModSummary file
    sess <- use_ GhcSession file
    let hsc = hscEnv sess
    opt <- getIdeOptions
    modify_dflags <- getModifyDynFlags dynFlagsModifyParser
    let ms = ms' { ms_hspp_opts = modify_dflags $ ms_hspp_opts ms' }
        reset_ms pm = pm { pm_mod_summary = ms' }

    -- We still parse with Haddocks whether Opt_Haddock is True or False to collect information
    -- but we no longer need to parse with and without Haddocks separately for above GHC90.
    res@(_,pmod) <- if Compat.ghcVersion >= Compat.GHC90 then
      liftIO $ (fmap.fmap.fmap) reset_ms $ getParsedModuleDefinition hsc opt file (withOptHaddock ms)
    else do
        let dflags    = ms_hspp_opts ms
            mainParse = getParsedModuleDefinition hsc opt file ms

        -- Parse again (if necessary) to capture Haddock parse errors
        if gopt Opt_Haddock dflags
            then
                liftIO $ (fmap.fmap.fmap) reset_ms mainParse
            else do
                let haddockParse = getParsedModuleDefinition hsc opt file (withOptHaddock ms)

                -- parse twice, with and without Haddocks, concurrently
                -- we cannot ignore Haddock parse errors because files of
                -- non-interest are always parsed with Haddocks
                -- If we can parse Haddocks, might as well use them
                ((diags,res),(diagsh,resh)) <- liftIO $ (fmap.fmap.fmap.fmap) reset_ms $ concurrently mainParse haddockParse

                -- Merge haddock and regular diagnostics so we can always report haddock
                -- parse errors
                let diagsM = mergeParseErrorsHaddock diags diagsh
                case resh of
                  Just _
                    | HaddockParse <- optHaddockParse opt
                    -> pure (diagsM, resh)
                  -- If we fail to parse haddocks, report the haddock diagnostics as well and
                  -- return the non-haddock parse.
                  -- This seems to be the correct behaviour because the Haddock flag is added
                  -- by us and not the user, so our IDE shouldn't stop working because of it.
                  _ -> pure (diagsM, res)
    -- Add dependencies on included files
    _ <- uses GetModificationTime $ map toNormalizedFilePath' (maybe [] pm_extra_src_files pmod)
    pure res

withOptHaddock :: ModSummary -> ModSummary
withOptHaddock = withOption Opt_Haddock

withOption :: GeneralFlag -> ModSummary -> ModSummary
withOption opt ms = ms{ms_hspp_opts= gopt_set (ms_hspp_opts ms) opt}

withoutOption :: GeneralFlag -> ModSummary -> ModSummary
withoutOption opt ms = ms{ms_hspp_opts= gopt_unset (ms_hspp_opts ms) opt}

-- | Given some normal parse errors (first) and some from Haddock (second), merge them.
--   Ignore Haddock errors that are in both. Demote Haddock-only errors to warnings.
mergeParseErrorsHaddock :: [FileDiagnostic] -> [FileDiagnostic] -> [FileDiagnostic]
mergeParseErrorsHaddock normal haddock = normal ++
    [ (a,b,c{_severity = Just DsWarning, _message = fixMessage $ _message c})
    | (a,b,c) <- haddock, Diag._range c `Set.notMember` locations]
  where
    locations = Set.fromList $ map (Diag._range . thd3) normal

    fixMessage x | "parse error " `T.isPrefixOf` x = "Haddock " <> x
                 | otherwise = "Haddock: " <> x

-- | This rule provides a ParsedModule preserving all annotations,
-- including keywords, punctuation and comments.
-- So it is suitable for use cases where you need a perfect edit.
getParsedModuleWithCommentsRule :: Recorder (WithPriority Log) -> Rules ()
getParsedModuleWithCommentsRule recorder =
  -- The parse diagnostics are owned by the GetParsedModule rule
  -- For this reason, this rule does not produce any diagnostics
  defineNoDiagnostics (cmapWithPrio LogShake recorder) $ \GetParsedModuleWithComments file -> do
    ModSummaryResult{msrModSummary = ms} <- use_ GetModSummary file
    sess <- use_ GhcSession file
    opt <- getIdeOptions

    let ms' = withoutOption Opt_Haddock $ withOption Opt_KeepRawTokenStream ms
    modify_dflags <- getModifyDynFlags dynFlagsModifyParser
    let ms = ms' { ms_hspp_opts = modify_dflags $ ms_hspp_opts ms' }
        reset_ms pm = pm { pm_mod_summary = ms' }

    liftIO $ fmap (fmap reset_ms) $ snd <$> getParsedModuleDefinition (hscEnv sess) opt file ms

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
        targets <- useNoFile_ GetKnownTargets
        let targetsMap = HM.mapWithKey const targets
        let imports = [(False, imp) | imp <- ms_textual_imps ms] ++ [(True, imp) | imp <- ms_srcimps ms]
        env_eq <- use_ GhcSession file
        let env = hscEnvWithImportPaths env_eq
        let import_dirs = deps env_eq
        let dflags = hsc_dflags env
            isImplicitCradle = isNothing $ envImportPaths env_eq
        dflags <- return $ if isImplicitCradle
                    then addRelativeImport file (moduleName $ ms_mod ms) dflags
                    else dflags
        opt <- getIdeOptions
        let getTargetFor modName nfp
                | isImplicitCradle = do
                    itExists <- getFileExists nfp
                    return $ if itExists then Just nfp else Nothing
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
                | otherwise
                = return Nothing
        (diags, imports') <- fmap unzip $ forM imports $ \(isSource, (mbPkgName, modName)) -> do
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
                loc <- liftIO $ mkHomeModLocation dflags modName (fromNormalizedFilePath bootPath)
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
        ( RawDependencyInformation IntMap.empty emptyPathIdMap IntMap.empty IntMap.empty
        , IntMap.empty
        )

-- | Given a target file path, construct the raw dependency results by following
-- imports recursively.
rawDependencyInformation :: [NormalizedFilePath] -> Action RawDependencyInformation
rawDependencyInformation fs = do
    (rdi, ss) <- execRawDepM (goPlural fs)
    let bm = IntMap.foldrWithKey (updateBootMap rdi) IntMap.empty ss
    return (rdi { rawBootMap = bm })
  where
    goPlural ff = do
        mss <- lift $ (fmap.fmap) msrModSummary <$> uses GetModSummaryWithoutTimestamps ff
        zipWithM go ff mss

    go :: NormalizedFilePath -- ^ Current module being processed
       -> Maybe ModSummary   -- ^ ModSummary of the module
       -> RawDepM FilePathId
    go f msum = do
      -- First check to see if we have already processed the FilePath
      -- If we have, just return its Id but don't update any of the state.
      -- Otherwise, we need to process its imports.
      checkAlreadyProcessed f $ do
          let al = modSummaryToArtifactsLocation f msum
          -- Get a fresh FilePathId for the new file
          fId <- getFreshFid al
          -- Record this module and its location
          whenJust msum $ \ms ->
            modifyRawDepInfo (\rd -> rd { rawModuleNameMap = IntMap.insert (getFilePathId fId)
                                                                           (ShowableModuleName (moduleName $ ms_mod ms))
                                                                           (rawModuleNameMap rd)})
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

getDependencyInformationRule :: Recorder (WithPriority Log) -> Rules ()
getDependencyInformationRule recorder =
    define (cmapWithPrio LogShake recorder) $ \GetDependencyInformation file -> do
       rawDepInfo <- rawDependencyInformation [file]
       pure ([], Just $ processDependencyInformation rawDepInfo)

reportImportCyclesRule :: Recorder (WithPriority Log) -> Rules ()
reportImportCyclesRule recorder =
    define (cmapWithPrio LogShake recorder) $ \ReportImportCycles file -> fmap (\errs -> if null errs then ([], Just ()) else (errs, Nothing)) $ do
        DependencyInformation{..} <- use_ GetDependencyInformation file
        let fileId = pathToId depPathIdMap file
        case IntMap.lookup (getFilePathId fileId) depErrorNodes of
            Nothing -> pure []
            Just errs -> do
                let cycles = mapMaybe (cycleErrorInFile fileId) (toList errs)
                -- Convert cycles of files into cycles of module names
                forM cycles $ \(imp, files) -> do
                    modNames <- forM files $ \fileId -> do
                        let file = idToPath depPathIdMap fileId
                        getModuleName file
                    pure $ toDiag imp $ sort modNames
    where cycleErrorInFile f (PartOfCycle imp fs)
            | f `elem` fs = Just (imp, fs)
          cycleErrorInFile _ _ = Nothing
          toDiag imp mods = (fp , ShowDiag , ) $ Diagnostic
            { _range = rng
            , _severity = Just DsError
            , _source = Just "Import cycle detection"
            , _message = "Cyclic module dependency between " <> showCycle mods
            , _code = Nothing
            , _relatedInformation = Nothing
            , _tags = Nothing
            }
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
  (currentSource, ver) <- liftIO $ case M.lookup (filePathToUri' file) vfsData of
    Nothing -> (,Nothing) . T.decodeUtf8 <$> BS.readFile (fromNormalizedFilePath file)
    Just vf -> pure (Rope.toText $ _file_text vf, Just $ _lsp_version vf)
  let refmap = Compat.generateReferencesMap . Compat.getAsts . Compat.hie_asts $ res
      del = deltaFromDiff (T.decodeUtf8 $ Compat.hie_hs_src res) currentSource
  pure (HAR (Compat.hie_module res) (Compat.hie_asts res) refmap mempty (HieFromDisk res),del,ver)

getHieAstRuleDefinition :: NormalizedFilePath -> HscEnv -> TcModuleResult -> Action (IdeResult HieAstResult)
getHieAstRuleDefinition f hsc tmr = do
  (diags, masts) <- liftIO $ generateHieAsts hsc tmr
  se <- getShakeExtras

  isFoi <- use_ IsFileOfInterest f
  diagsWrite <- case isFoi of
    IsFOI Modified{firstOpen = False} -> do
      when (coerce $ ideTesting se) $ liftIO $ mRunLspT (lspEnv se) $
        LSP.sendNotification (SCustomMethod "ghcide/reference/ready") $
          toJSON $ fromNormalizedFilePath f
      pure []
    _ | Just asts <- masts -> do
          source <- getSourceFileSource f
          let exports = tcg_exports $ tmrTypechecked tmr
              msum = tmrModSummary tmr
          liftIO $ writeAndIndexHieFile hsc se msum f exports asts source
    _ -> pure []

  let refmap = Compat.generateReferencesMap . Compat.getAsts <$> masts
      typemap = AtPoint.computeTypeReferences . Compat.getAsts <$> masts
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
persistentDocMapRule = addPersistentRule GetDocMap $ \_ -> pure $ Just (DKMap mempty mempty, idDelta, Nothing)

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
  let log = (liftIO .) . logWith recorder
  case res of
    Left e -> log Logger.Debug $ LogLoadingHieFileFail hie_loc e
    Right _ -> log Logger.Debug $ LogLoadingHieFileSuccess hie_loc
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
    typeCheckRuleDefinition hsc pm

knownFilesRule :: Recorder (WithPriority Log) -> Rules ()
knownFilesRule recorder = defineEarlyCutOffNoFile (cmapWithPrio LogShake recorder) $ \GetKnownTargets -> do
  alwaysRerun
  fs <- knownTargets
  pure (LBS.toStrict $ B.encode $ hash fs, unhashed fs)

getModuleGraphRule :: Recorder (WithPriority Log) -> Rules ()
getModuleGraphRule recorder = defineNoFile (cmapWithPrio LogShake recorder) $ \GetModuleGraph -> do
  fs <- toKnownFiles <$> useNoFile_ GetKnownTargets
  rawDepInfo <- rawDependencyInformation (HashSet.toList fs)
  pure $ processDependencyInformation rawDepInfo

-- This is factored out so it can be directly called from the GetModIface
-- rule. Directly calling this rule means that on the initial load we can
-- garbage collect all the intermediate typechecked modules rather than
-- retain the information forever in the shake graph.
typeCheckRuleDefinition
    :: HscEnv
    -> ParsedModule
    -> Action (IdeResult TcModuleResult)
typeCheckRuleDefinition hsc pm = do
  setPriority priorityTypeCheck
  IdeOptions { optDefer = defer } <- getIdeOptions

  unlift <- askUnliftIO
  let dets = TypecheckHelpers
           { getLinkables = unliftIO unlift . uses_ GetLinkable
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
        res <- optGhcSession opts

        let fingerprint = LBS.toStrict $ B.encode $ hash (sessionVersion res)
        return (fingerprint, res)

    defineEarlyCutoff (cmapWithPrio LogShake recorder) $ Rule $ \GhcSession file -> do
        IdeGhcSession{loadSessionFun} <- useNoFile_ GhcSessionIO
        (val,deps) <- liftIO $ loadSessionFun $ fromNormalizedFilePath file

        -- add the deps to the Shake graph
        let addDependency fp = do
                -- VSCode uses absolute paths in its filewatch notifications
                afp <- liftIO $ makeAbsolute fp
                let nfp = toNormalizedFilePath' afp
                itExists <- getFileExists nfp
                when itExists $ void $ do
                  use_ GetModificationTime nfp
        mapM_ addDependency deps

        let cutoffHash = LBS.toStrict $ B.encode (hash (snd val))
        return (Just cutoffHash, val)

    defineNoDiagnostics (cmapWithPrio LogShake recorder) $ \(GhcSessionDeps_ fullModSummary) file -> do
        env <- use_ GhcSession file
        ghcSessionDepsDefinition fullModSummary ghcSessionDepsConfig env file

newtype GhcSessionDepsConfig = GhcSessionDepsConfig
    { checkForImportCycles :: Bool
    }
instance Default GhcSessionDepsConfig where
  def = GhcSessionDepsConfig
    { checkForImportCycles = True
    }

-- | Note [GhcSessionDeps]
-- For a file 'Foo', GhcSessionDeps "Foo.hs" results in an HscEnv which includes
-- 1. HomeModInfo's (in the HUG/HPT) for all modules in the transitive closure of "Foo", **NOT** including "Foo" itself.
-- 2. ModSummary's (in the ModuleGraph) for all modules in the transitive closure of "Foo", including "Foo" itself.
-- 3. ModLocation's (in the FinderCache) all modules in the transitive closure of "Foo", including "Foo" itself.
ghcSessionDepsDefinition
    :: -- | full mod summary
        Bool ->
        GhcSessionDepsConfig -> HscEnvEq -> NormalizedFilePath -> Action (Maybe HscEnvEq)
ghcSessionDepsDefinition fullModSummary GhcSessionDepsConfig{..} env file = do
    let hsc = hscEnv env

    mbdeps <- mapM(fmap artifactFilePath . snd) <$> use_ GetLocatedImports file
    case mbdeps of
        Nothing -> return Nothing
        Just deps -> do
            when checkForImportCycles $ void $ uses_ ReportImportCycles deps
            ms <- msrModSummary <$> if fullModSummary
                then use_ GetModSummary file
                else use_ GetModSummaryWithoutTimestamps file

            depSessions <- map hscEnv <$> uses_ (GhcSessionDeps_ fullModSummary) deps
            ifaces <- uses_ GetModIface deps
            let inLoadOrder = map (\HiFileResult{..} -> HomeModInfo hirModIface hirModDetails Nothing) ifaces
#if MIN_VERSION_ghc(9,3,0)
            -- On GHC 9.4+, the module graph contains not only ModSummary's but each `ModuleNode` in the graph
            -- also points to all the direct descendants of the current module. To get the keys for the descendants
            -- we must get their `ModSummary`s
            !final_deps <- do
              dep_mss <- map msrModSummary <$> uses_ GetModSummaryWithoutTimestamps deps
             -- Don't want to retain references to the entire ModSummary when just the key will do
              return $!! map (NodeKey_Module . msKey) dep_mss
            let moduleNode = (ms, final_deps)
#else
            let moduleNode = ms
#endif
            session' <- liftIO $ mergeEnvs hsc moduleNode inLoadOrder depSessions

            Just <$> liftIO (newHscEnvEqWithImportPaths (envImportPaths env) session' [])

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
      ShakeExtras{ideNc} <- getShakeExtras
      let m_old = case old of
            Shake.Succeeded (Just old_version) v -> Just (v, old_version)
            Shake.Stale _   (Just old_version) v -> Just (v, old_version)
            _ -> Nothing
          recompInfo = RecompilationInfo
            { source_version = ver
            , old_value = m_old
            , get_file_version = use GetModificationTime_{missingFileDiagnostics = False}
            , get_linkable_hashes = \fs -> map (snd . fromJust . hirCoreFp) <$> uses_ GetModIface fs
            , regenerate = regenerateHiFile session f ms
            }
      r <- loadInterface (hscEnv session) ms linkableType recompInfo
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
  hash <- liftIO $ Util.getFileHash hie_loc
  mrow <- liftIO $ withHieDb (\hieDb -> HieDb.lookupHieFileFromSource hieDb (fromNormalizedFilePath f))
  hie_loc' <- liftIO $ traverse (makeAbsolute . HieDb.hieModuleHieFile) mrow
  case mrow of
    Just row
      | hash == HieDb.modInfoHash (HieDb.hieModInfo row)
      && Just hie_loc == hie_loc'
      -> do
      -- All good, the db has indexed the file
      when (coerce $ ideTesting se) $ liftIO $ mRunLspT (lspEnv se) $
        LSP.sendNotification (SCustomMethod "ghcide/reference/ready") $
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
          indexHieFile se ms f hash hf

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
        let session = hscSetFlags (modify_dflags $ hsc_dflags session') session'
        (modTime, mFileContent) <- getFileContents f
        let fp = fromNormalizedFilePath f
        modS <- liftIO $ runExceptT $
                getModSummaryFromImports session fp modTime (textToStringBuffer <$> mFileContent)
        case modS of
            Right res -> do
                -- Check for Template Haskell
                when (uses_th_qq $ msrModSummary res) $ do
                    DisplayTHWarning act <- getIdeGlobalAction
                    liftIO act
#if MIN_VERSION_ghc(9,3,0)
                let bufFingerPrint = ms_hs_hash (msrModSummary res)
#else
                bufFingerPrint <- liftIO $
                    fingerprintFromStringBuffer $ fromJust $ ms_hspp_buf $ msrModSummary res
#endif
                let fingerPrint = Util.fingerprintFingerprints
                        [ msrFingerprint res, bufFingerPrint ]
                return ( Just (fingerprintToBS fingerPrint) , ([], Just res))
            Left diags -> return (Nothing, (diags, Nothing))

    defineEarlyCutoff (cmapWithPrio LogShake recorder) $ RuleNoDiagnostics $ \GetModSummaryWithoutTimestamps f -> do
        ms <- use GetModSummary f
        case ms of
            Just res@ModSummaryResult{..} -> do
                let ms = msrModSummary {
#if !MIN_VERSION_ghc(9,3,0)
                    ms_hs_date = error "use GetModSummary instead of GetModSummaryWithoutTimestamps",
#endif
                    ms_hspp_buf = error "use GetModSummary instead of GetModSummaryWithoutTimestamps"
                    }
                    fp = fingerprintToBS msrFingerprint
                return (Just fp, Just res{msrModSummary = ms})
            Nothing -> return (Nothing, Nothing)

generateCore :: RunSimplifier -> NormalizedFilePath -> Action (IdeResult ModGuts)
generateCore runSimplifier file = do
    packageState <- hscEnv <$> use_ GhcSessionDeps file
    tm <- use_ TypeCheck file
    setPriority priorityGenerateCore
    liftIO $ compileModule runSimplifier packageState (tmrModSummary tm) (tmrTypechecked tm)

generateCoreRule :: Recorder (WithPriority Log) -> Rules ()
generateCoreRule recorder =
    define (cmapWithPrio LogShake recorder) $ \GenerateCore -> generateCore (RunSimplifier True)

getModIfaceRule :: Recorder (WithPriority Log) -> Rules ()
getModIfaceRule recorder = defineEarlyCutoff (cmapWithPrio LogShake recorder) $ Rule $ \GetModIface f -> do
  fileOfInterest <- use_ IsFileOfInterest f
  res@(_,(_,mhmi)) <- case fileOfInterest of
    IsFOI status -> do
      -- Never load from disk for files of interest
      tmr <- use_ TypeCheck f
      linkableType <- getLinkableType f
      hsc <- hscEnv <$> use_ GhcSessionDeps f
      let compile = fmap ([],) $ use GenerateCore f
      se <- getShakeExtras
      (diags, !hiFile) <- writeCoreFileIfNeeded se hsc linkableType compile tmr
      let fp = hiFileFingerPrint <$> hiFile
      hiDiags <- case hiFile of
        Just hiFile
          | OnDisk <- status
          , not (tmrDeferredError tmr) -> liftIO $ writeHiFile se hsc hiFile
        _ -> pure []
      return (fp, (diags++hiDiags, hiFile))
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

-- | Also generates and indexes the `.hie` file, along with the `.o` file if needed
-- Invariant maintained is that if the `.hi` file was successfully written, then the
-- `.hie` and `.o` file (if needed) were also successfully written
regenerateHiFile :: HscEnvEq -> NormalizedFilePath -> ModSummary -> Maybe LinkableType -> Action ([FileDiagnostic], Maybe HiFileResult)
regenerateHiFile sess f ms compNeeded = do
    let hsc = hscEnv sess
    opt <- getIdeOptions

    -- Embed haddocks in the interface file
    (diags, mb_pm) <- liftIO $ getParsedModuleDefinition hsc opt f (withOptHaddock ms)
    (diags, mb_pm) <-
        -- We no longer need to parse again if GHC version is above 9.0. https://github.com/haskell/haskell-language-server/issues/1892
        if Compat.ghcVersion >= Compat.GHC90 || isJust mb_pm then do
            return (diags, mb_pm)
        else do
            -- if parsing fails, try parsing again with Haddock turned off
            (diagsNoHaddock, mb_pm) <- liftIO $ getParsedModuleDefinition hsc opt f ms
            return (mergeParseErrorsHaddock diagsNoHaddock diags, mb_pm)
    case mb_pm of
        Nothing -> return (diags, Nothing)
        Just pm -> do
            -- Invoke typechecking directly to update it without incurring a dependency
            -- on the parsed module and the typecheck rules
            (diags', mtmr) <- typeCheckRuleDefinition hsc pm
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
                    se <- getShakeExtras
                    (gDiags, masts) <- liftIO $ generateHieAsts hsc tmr
                    source <- getSourceFileSource f
                    wDiags <- forM masts $ \asts ->
                      liftIO $ writeAndIndexHieFile hsc se (tmrModSummary tmr) f (tcg_exports $ tmrTypechecked tmr) asts source

                    -- We don't write the `.hi` file if there are deferred errors, since we won't get
                    -- accurate diagnostics next time if we do
                    hiDiags <- if not $ tmrDeferredError tmr
                               then liftIO $ writeHiFile se hsc hiFile
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

-- ---------------------------------------------------------------------

getLinkableRule :: Recorder (WithPriority Log) -> Rules ()
getLinkableRule recorder =
  defineEarlyCutoff (cmapWithPrio LogShake recorder) $ Rule $ \GetLinkable f -> do
    ModSummaryResult{msrModSummary = ms} <- use_ GetModSummary f
    HiFileResult{hirModIface, hirModDetails, hirCoreFp} <- use_ GetModIface f
    let obj_file  = ml_obj_file (ms_location ms)
        core_file = ml_core_file (ms_location ms)
    -- Can't use `GetModificationTime` rule because the core file was possibly written in this
    -- very session, so the results aren't reliable
    core_t <- liftIO $ getModTime core_file
    case hirCoreFp of
      Nothing -> error "called GetLinkable for a file without a linkable"
      Just (bin_core, hash) -> do
        session <- use_ GhcSessionDeps f
        ShakeExtras{ideNc} <- getShakeExtras
        let namecache_updater = mkUpdater ideNc
        linkableType <- getLinkableType f >>= \case
          Nothing -> error "called GetLinkable for a file which doesn't need compilation"
          Just t -> pure t
        (warns, hmi) <- case linkableType of
          -- Bytecode needs to be regenerated from the core file
          BCOLinkable -> liftIO $ coreFileToLinkable linkableType (hscEnv session) ms hirModIface hirModDetails bin_core (posixSecondsToUTCTime core_t)
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
                | obj_t >= core_t -> pure ([], Just $ HomeModInfo hirModIface hirModDetails (Just $ LM (posixSecondsToUTCTime obj_t) (ms_mod ms) [DotO obj_file]))
              _ -> liftIO $ coreFileToLinkable linkableType (hscEnv session) ms hirModIface hirModDetails bin_core (error "object doesn't have time")
        -- Record the linkable so we know not to unload it, and unload old versions
        whenJust (hm_linkable =<< hmi) $ \(LM time mod _) -> do
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
              unload (hscEnv session) (map (\(mod, time) -> LM time mod []) $ moduleEnvToList to_keep)
              return (to_keep, ())
        return (hash <$ hmi, (warns, LinkableResult <$> hmi <*> pure hash))

-- | For now we always use bytecode unless something uses unboxed sums and tuples along with TH
getLinkableType :: NormalizedFilePath -> Action (Maybe LinkableType)
getLinkableType f = use_ NeedsCompilation f

-- needsCompilationRule :: Rules ()
needsCompilationRule :: NormalizedFilePath  -> Action (IdeResultNoDiagnosticsEarlyCutoff (Maybe LinkableType))
needsCompilationRule file
  | "boot" `isSuffixOf` (fromNormalizedFilePath file) =
    pure (Just $ encodeLinkableType Nothing, Just Nothing)
needsCompilationRule file = do
  graph <- useNoFile GetModuleGraph
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
        ms <- msrModSummary . fst <$> useWithStale_ GetModSummaryWithoutTimestamps file
        (modsums,needsComps) <- liftA2
            (,) (map (fmap (msrModSummary . fst)) <$> usesWithStale GetModSummaryWithoutTimestamps revdeps)
                (uses NeedsCompilation revdeps)
        pure $ computeLinkableType ms modsums (map join needsComps)
  pure (Just $ encodeLinkableType res, Just res)
  where
    computeLinkableType :: ModSummary -> [Maybe ModSummary] -> [Maybe LinkableType] -> Maybe LinkableType
    computeLinkableType this deps xs
      | Just ObjectLinkable `elem` xs     = Just ObjectLinkable -- If any dependent needs object code, so do we
      | Just BCOLinkable    `elem` xs     = Just this_type      -- If any dependent needs bytecode, then we need to be compiled
      | any (maybe False uses_th_qq) deps = Just this_type      -- If any dependent needs TH, then we need to be compiled
      | otherwise                         = Nothing             -- If none of these conditions are satisfied, we don't need to compile
      where
        this_type = computeLinkableTypeForDynFlags (ms_hspp_opts this)

uses_th_qq :: ModSummary -> Bool
uses_th_qq (ms_hspp_opts -> dflags) =
      xopt LangExt.TemplateHaskell dflags || xopt LangExt.QuasiQuotes dflags

-- | How should we compile this module?
-- (assuming we do in fact need to compile it).
-- Depends on whether it uses unboxed tuples or sums
computeLinkableTypeForDynFlags :: DynFlags -> LinkableType
computeLinkableTypeForDynFlags d
#if defined(GHC_PATCHED_UNBOXED_BYTECODE) || MIN_VERSION_ghc(9,2,0)
          = BCOLinkable
#else
          | unboxed_tuples_or_sums = ObjectLinkable
          | otherwise              = BCOLinkable
#endif
  where
        unboxed_tuples_or_sums =
            xopt LangExt.UnboxedTuples d || xopt LangExt.UnboxedSums d

-- | Tracks which linkables are current, so we don't need to unload them
newtype CompiledLinkables = CompiledLinkables { getCompiledLinkables :: Var (ModuleEnv UTCTime) }
instance IsIdeGlobal CompiledLinkables

data RulesConfig = RulesConfig
    { -- | Disable import cycle checking for improved performance in large codebases
      checkForImportCycles :: Bool
    -- | Disable TH for improved performance in large codebases
    , enableTemplateHaskell :: Bool
    -- | Warning to show when TH is not supported by the current HLS binary
    , templateHaskellWarning :: LspT Config IO ()
    }

instance Default RulesConfig where
    def = RulesConfig True True displayTHWarning
      where
        displayTHWarning :: LspT c IO ()
        displayTHWarning
            | not isWindows && not hostIsDynamic = do
                LSP.sendNotification SWindowShowMessage $
                    ShowMessageParams MtInfo thWarningMessage
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
    getDependencyInformationRule recorder
    reportImportCyclesRule recorder
    typeCheckRule recorder
    getDocMapRule recorder
    loadGhcSession recorder def{checkForImportCycles}
    getModIfaceFromDiskRule recorder
    getModIfaceFromDiskAndIndexRule recorder
    getModIfaceRule recorder
    getModSummaryRule templateHaskellWarning recorder
    getModuleGraphRule recorder
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
