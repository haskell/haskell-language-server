{-# LANGUAGE CPP              #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE RankNTypes       #-}
{-# LANGUAGE TypeFamilies     #-}

module Development.IDE.Plugin.Completions
    ( descriptor
    , Log(..)
    , ghcideCompletionsPluginPriority
    ) where

import           Control.Concurrent.Async                 (concurrently)
import           Control.Concurrent.STM.Stats             (readTVarIO)
import           Control.Monad.IO.Class
import           Control.Lens                            ((&), (.~))
import qualified Data.HashMap.Strict                      as Map
import qualified Data.HashSet                             as Set
import           Data.Aeson
import           Data.Maybe
import qualified Data.Text                                as T
import           Development.IDE.Core.PositionMapping
import           Development.IDE.Core.Compile
import           Development.IDE.Core.RuleTypes
import           Development.IDE.Core.Service             hiding (Log, LogShake)
import           Development.IDE.Core.Shake               hiding (Log)
import qualified Development.IDE.Core.Shake               as Shake
import           Development.IDE.GHC.Compat
import           Development.IDE.GHC.Util
import           Development.IDE.Graph
import           Development.IDE.Spans.Common
import           Development.IDE.Spans.Documentation
import           Development.IDE.Plugin.Completions.Logic
import           Development.IDE.Plugin.Completions.Types
import           Development.IDE.Types.Exports
import           Development.IDE.Types.HscEnvEq           (HscEnvEq (envPackageExports, envVisibleModuleNames),
                                                           hscEnv)
import qualified Development.IDE.Types.KnownTargets       as KT
import           Development.IDE.Types.Location
import           Development.IDE.Types.Logger             (Pretty (pretty),
                                                           Recorder,
                                                           WithPriority,
                                                           cmapWithPrio)
import           Ide.Types
import qualified Language.LSP.Server                      as LSP
import           Language.LSP.Types
import qualified Language.LSP.Types.Lens         as J
import qualified Language.LSP.VFS                         as VFS
import           Numeric.Natural
import           Text.Fuzzy.Parallel                      (Scored (..))

import           Development.IDE.Core.Rules               (usePropertyAction)
import qualified GHC.LanguageExtensions                   as LangExt
import qualified Ide.Plugin.Config                        as Config

data Log = LogShake Shake.Log deriving Show

instance Pretty Log where
  pretty = \case
    LogShake log -> pretty log

ghcideCompletionsPluginPriority :: Natural
ghcideCompletionsPluginPriority = defaultPluginPriority

descriptor :: Recorder (WithPriority Log) -> PluginId -> PluginDescriptor IdeState
descriptor recorder plId = (defaultPluginDescriptor plId)
  { pluginRules = produceCompletions recorder
  , pluginHandlers = mkPluginHandler STextDocumentCompletion getCompletionsLSP
                  <> mkPluginHandler SCompletionItemResolve resolveCompletion
  , pluginConfigDescriptor = defaultConfigDescriptor {configCustomConfig = mkCustomConfig properties}
  , pluginPriority = ghcideCompletionsPluginPriority
  }


produceCompletions :: Recorder (WithPriority Log) -> Rules ()
produceCompletions recorder = do
    define (cmapWithPrio LogShake recorder) $ \LocalCompletions file -> do
        let uri = fromNormalizedUri $ normalizedFilePathToUri file
        pm <- useWithStale GetParsedModule file
        case pm of
            Just (pm, _) -> do
                let cdata = localCompletionsForParsedModule uri pm
                return ([], Just cdata)
            _ -> return ([], Nothing)
    define (cmapWithPrio LogShake recorder) $ \NonLocalCompletions file -> do
        -- For non local completions we avoid depending on the parsed module,
        -- synthesizing a fake module with an empty body from the buffer
        -- in the ModSummary, which preserves all the imports
        ms <- fmap fst <$> useWithStale GetModSummaryWithoutTimestamps file
        sess <- fmap fst <$> useWithStale GhcSessionDeps file

        case (ms, sess) of
            (Just ModSummaryResult{..}, Just sess) -> do
              let env = hscEnv sess
              -- We do this to be able to provide completions of items that are not restricted to the explicit list
              (global, inScope) <- liftIO $ tcRnImportDecls env (dropListFromImportDecl <$> msrImports) `concurrently` tcRnImportDecls env msrImports
              case (global, inScope) of
                  ((_, Just globalEnv), (_, Just inScopeEnv)) -> do
                      visibleMods <- liftIO $ fmap (fromMaybe []) $ envVisibleModuleNames sess
                      let uri = fromNormalizedUri $ normalizedFilePathToUri file
                      let cdata = cacheDataProducer uri visibleMods (ms_mod msrModSummary) globalEnv inScopeEnv msrImports
                      return ([], Just cdata)
                  (_diag, _) ->
                      return ([], Nothing)
            _ -> return ([], Nothing)

-- Drop any explicit imports in ImportDecl if not hidden
dropListFromImportDecl :: LImportDecl GhcPs -> LImportDecl GhcPs
dropListFromImportDecl iDecl = let
    f d@ImportDecl {ideclHiding} = case ideclHiding of
        Just (False, _) -> d {ideclHiding=Nothing}
        -- if hiding or Nothing just return d
        _               -> d
    f x = x
    in f <$> iDecl

resolveCompletion :: IdeState -> PluginId -> CompletionItem -> LSP.LspM Config (Either ResponseError CompletionItem)
resolveCompletion ide _ comp@CompletionItem{_detail,_documentation,_xdata}
  | Just resolveData <- _xdata
  , Success (CompletionResolveData uri needType (NameDetails mod occ)) <- fromJSON resolveData
  , Just file <- uriToNormalizedFilePath $ toNormalizedUri uri
  = liftIO $ runIdeAction "Completion resolve" (shakeExtras ide) $ do
    msess <- useWithStaleFast GhcSessionDeps file
    case msess of
      Nothing -> pure (Right comp) -- File doesn't compile, return original completion item
      Just (sess,_) -> do
        let nc = ideNc $ shakeExtras ide
#if MIN_VERSION_ghc(9,3,0)
        name <- liftIO $ lookupNameCache nc mod occ
#else
        name <- liftIO $ upNameCache nc (lookupNameCache mod occ)
#endif
        mdkm <- useWithStaleFast GetDocMap file
        let (dm,km) = case mdkm of
              Just (DKMap dm km, _) -> (dm,km)
              Nothing -> (mempty, mempty)
        doc <- case lookupNameEnv dm name of
          Just doc -> pure $ spanDocToMarkdown doc
          Nothing -> liftIO $ spanDocToMarkdown <$> getDocumentationTryGhc (hscEnv sess) name
        typ <- case lookupNameEnv km name of
          _ | not needType -> pure Nothing
          Just ty -> pure (safeTyThingType ty)
          Nothing -> do
            (safeTyThingType =<<) <$> liftIO (lookupName (hscEnv sess) name)
        let det1 = case typ of
              Just ty -> Just (":: " <> printOutputable (stripForall ty) <> "\n")
              Nothing -> Nothing
            doc1 = case _documentation of
              Just (CompletionDocMarkup (MarkupContent MkMarkdown old)) ->
                CompletionDocMarkup $ MarkupContent MkMarkdown $ T.intercalate sectionSeparator (old:doc)
              _ -> CompletionDocMarkup $ MarkupContent MkMarkdown $ T.intercalate sectionSeparator doc
        pure (Right $ comp & J.detail .~ (det1 <> _detail)
                           & J.documentation .~ Just doc1
                           )
  where
    stripForall ty = case splitForAllTyCoVars ty of
      (_,res) -> res
resolveCompletion _ _ comp = pure (Right comp)

-- | Generate code actions.
getCompletionsLSP
    :: IdeState
    -> PluginId
    -> CompletionParams
    -> LSP.LspM Config (Either ResponseError (ResponseResult TextDocumentCompletion))
getCompletionsLSP ide plId
  CompletionParams{_textDocument=TextDocumentIdentifier uri
                  ,_position=position
                  ,_context=completionContext} = do
    contents <- LSP.getVirtualFile $ toNormalizedUri uri
    fmap Right $ case (contents, uriToFilePath' uri) of
      (Just cnts, Just path) -> do
        let npath = toNormalizedFilePath' path
        (ideOpts, compls, moduleExports, astres) <- liftIO $ runIdeAction "Completion" (shakeExtras ide) $ do
            opts <- liftIO $ getIdeOptionsIO $ shakeExtras ide
            localCompls <- useWithStaleFast LocalCompletions npath
            nonLocalCompls <- useWithStaleFast NonLocalCompletions npath
            pm <- useWithStaleFast GetParsedModule npath
            binds <- fromMaybe (mempty, zeroMapping) <$> useWithStaleFast GetBindings npath
            knownTargets <- liftIO $ runAction  "Completion" ide $ useNoFile GetKnownTargets
            let localModules = maybe [] Map.keys knownTargets
            let lModules = mempty{importableModules = map toModueNameText localModules}
            -- set up the exports map including both package and project-level identifiers
            packageExportsMapIO <- fmap(envPackageExports . fst) <$> useWithStaleFast GhcSession npath
            packageExportsMap <- mapM liftIO packageExportsMapIO
            projectExportsMap <- liftIO $ readTVarIO (exportsMap $ shakeExtras ide)
            let exportsMap = fromMaybe mempty packageExportsMap <> projectExportsMap

            let moduleExports = getModuleExportsMap exportsMap
                exportsCompItems = foldMap (map (fromIdentInfo uri) . Set.toList) . nonDetOccEnvElts . getExportsMap $ exportsMap
                exportsCompls = mempty{anyQualCompls = exportsCompItems}
            let compls = (fst <$> localCompls) <> (fst <$> nonLocalCompls) <> Just exportsCompls <> Just lModules

            -- get HieAst if OverloadedRecordDot is enabled
#if MIN_VERSION_ghc(9,2,0)
            let uses_overloaded_record_dot (ms_hspp_opts . msrModSummary -> dflags) = xopt LangExt.OverloadedRecordDot dflags
#else
            let uses_overloaded_record_dot _ = False
#endif
            ms <- fmap fst <$> useWithStaleFast GetModSummaryWithoutTimestamps npath
            astres <- case ms of
              Just ms' | uses_overloaded_record_dot ms'
                ->  useWithStaleFast GetHieAst npath
              _ -> return Nothing

            pure (opts, fmap (,pm,binds) compls, moduleExports, astres)
        case compls of
          Just (cci', parsedMod, bindMap) -> do
            let pfix = getCompletionPrefix position cnts
            case (pfix, completionContext) of
              ((PosPrefixInfo _ "" _ _), Just CompletionContext { _triggerCharacter = Just "."})
                -> return (InL $ List [])
              (_, _) -> do
                let clientCaps = clientCapabilities $ shakeExtras ide
                    plugins = idePlugins $ shakeExtras ide
                config <- liftIO $ runAction "" ide $ getCompletionsConfig plId

                allCompletions <- liftIO $ getCompletions plugins ideOpts cci' parsedMod astres bindMap pfix clientCaps config moduleExports uri
                pure $ InL (List $ orderedCompletions allCompletions)
              _ -> return (InL $ List [])
          _ -> return (InL $ List [])
      _ -> return (InL $ List [])

getCompletionsConfig :: PluginId -> Action CompletionsConfig
getCompletionsConfig pId =
  CompletionsConfig
    <$> usePropertyAction #snippetsOn pId properties
    <*> usePropertyAction #autoExtendOn pId properties
    <*> (Config.maxCompletions <$> getClientConfigAction)

{- COMPLETION SORTING
   We return an ordered set of completions (local -> nonlocal -> global).
   Ordering is important because local/nonlocal are import aware, whereas
   global are not and will always insert import statements, potentially redundant.

   Moreover, the order prioritizes qualifiers, for instance, given:

   import qualified MyModule
   foo = MyModule.<complete>

   The identifiers defined in MyModule will be listed first, followed by other
   identifiers in importable modules.

   According to the LSP specification, if no sortText is provided, the label is used
   to sort alphabetically. Alphabetical ordering is almost never what we want,
   so we force the LSP client to respect our ordering by using a numbered sequence.
-}

orderedCompletions :: [Scored CompletionItem] -> [CompletionItem]
orderedCompletions [] = []
orderedCompletions xx = zipWith addOrder [0..] xx
    where
    lxx = digits $ Prelude.length xx
    digits = Prelude.length . show

    addOrder :: Int -> Scored CompletionItem -> CompletionItem
    addOrder n Scored{original = it@CompletionItem{_label,_sortText}} =
        it{_sortText = Just $
                T.pack(pad lxx n)
                }

    pad n x = let sx = show x in replicate (n - Prelude.length sx) '0' <> sx

----------------------------------------------------------------------------------------------------

toModueNameText :: KT.Target -> T.Text
toModueNameText target = case target of
  KT.TargetModule m -> T.pack $ moduleNameString m
  _                 -> T.empty
