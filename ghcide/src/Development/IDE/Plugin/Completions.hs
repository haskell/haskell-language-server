{-# LANGUAGE CPP          #-}
{-# LANGUAGE RankNTypes   #-}
{-# LANGUAGE TypeFamilies #-}

module Development.IDE.Plugin.Completions
    ( descriptor
    ) where

import           Control.Concurrent.Async                     (concurrently)
import           Control.Concurrent.STM.Stats                 (readTVarIO)
import           Control.Monad.Extra
import           Control.Monad.IO.Class
import           Control.Monad.Trans.Maybe
import           Data.Aeson
import qualified Data.HashMap.Strict                          as Map
import qualified Data.HashSet                                 as Set
import           Data.List                                    (find)
import           Data.Maybe
import qualified Data.Text                                    as T
import           Development.IDE.Core.PositionMapping
import           Development.IDE.Core.RuleTypes
import           Development.IDE.Core.Service
import           Development.IDE.Core.Shake
import           Development.IDE.GHC.Compat
import           Development.IDE.GHC.Error                    (rangeToSrcSpan)
import           Development.IDE.GHC.ExactPrint               (GetAnnotatedParsedSource (GetAnnotatedParsedSource))
import           Development.IDE.GHC.Util                     (prettyPrint)
import           Development.IDE.Graph
import           Development.IDE.Plugin.CodeAction.ExactPrint
import           Development.IDE.Plugin.Completions.Logic
import           Development.IDE.Plugin.Completions.Types
import           Development.IDE.Types.Exports
import           Development.IDE.Types.HscEnvEq               (HscEnvEq (envPackageExports),
                                                               hscEnv)
import qualified Development.IDE.Types.KnownTargets           as KT
import           Development.IDE.Types.Location
import           Development.IDE.Types.Logger                 (logDebug,
                                                               logError)
import           GHC.Exts                                     (IsList (fromList),
                                                               toList)
import           Ide.Plugin.Config                            (Config)
import           Ide.Types
import qualified Language.LSP.Server                          as LSP
import           Language.LSP.Types
import qualified Language.LSP.VFS                             as VFS
import           Text.Fuzzy.Parallel                          (Scored (..))

descriptor :: PluginId -> PluginDescriptor IdeState
descriptor plId = (defaultPluginDescriptor plId)
  { pluginRules = produceCompletions
  , pluginHandlers = mkPluginHandler STextDocumentCompletion getCompletionsLSP
  , pluginCommands = [extendImportCommand]
  , pluginConfigDescriptor = defaultConfigDescriptor {configCustomConfig = mkCustomConfig properties}
  }

produceCompletions :: Rules ()
produceCompletions = do
    define $ \LocalCompletions file -> do
        let uri = fromNormalizedUri $ normalizedFilePathToUri file
        pm <- useWithStale GetParsedModule file
        case pm of
            Just (pm, _) -> do
                let cdata = localCompletionsForParsedModule uri pm
                return ([], Just cdata)
            _ -> return ([], Nothing)
    define $ \NonLocalCompletions file -> do
        -- For non local completions we avoid depending on the parsed module,
        -- synthetizing a fake module with an empty body from the buffer
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
                      let uri = fromNormalizedUri $ normalizedFilePathToUri file
                      cdata <- liftIO $ cacheDataProducer uri sess (ms_mod msrModSummary) globalEnv inScopeEnv msrImports
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
        (ideOpts, compls, moduleExports) <- liftIO $ runIdeAction "Completion" (shakeExtras ide) $ do
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
                exportsCompItems = foldMap (map (fromIdentInfo uri) . Set.toList) . Map.elems . getExportsMap $ exportsMap
                exportsCompls = mempty{anyQualCompls = exportsCompItems}
            let compls = (fst <$> localCompls) <> (fst <$> nonLocalCompls) <> Just exportsCompls <> Just lModules

            pure (opts, fmap (,pm,binds) compls, moduleExports)
        case compls of
          Just (cci', parsedMod, bindMap) -> do
            pfix <- VFS.getCompletionPrefix position cnts
            case (pfix, completionContext) of
              (Just (VFS.PosPrefixInfo _ "" _ _), Just CompletionContext { _triggerCharacter = Just "."})
                -> return (InL $ List [])
              (Just pfix', _) -> do
                let clientCaps = clientCapabilities $ shakeExtras ide
                config <- getCompletionsConfig plId
                allCompletions <- liftIO $ getCompletions plId ideOpts cci' parsedMod bindMap pfix' clientCaps config moduleExports
                pure $ InL (List $ orderedCompletions allCompletions)
              _ -> return (InL $ List [])
          _ -> return (InL $ List [])
      _ -> return (InL $ List [])

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

extendImportCommand :: PluginCommand IdeState
extendImportCommand =
  PluginCommand (CommandId extendImportCommandId) "additional edits for a completion" extendImportHandler

extendImportHandler :: CommandFunction IdeState ExtendImport
extendImportHandler ideState edit@ExtendImport {..} = do
  res <- liftIO $ runMaybeT $ extendImportHandler' ideState edit
  whenJust res $ \(nfp, wedit@WorkspaceEdit {_changes}) -> do
    let (_, List (head -> TextEdit {_range})) = fromJust $ _changes >>= listToMaybe . toList
        srcSpan = rangeToSrcSpan nfp _range
    LSP.sendNotification SWindowShowMessage $
      ShowMessageParams MtInfo $
        "Import "
          <> maybe ("‘" <> newThing) (\x -> "‘" <> x <> " (" <> newThing <> ")") thingParent
          <> "’ from "
          <> importName
          <> " (at "
          <> T.pack (prettyPrint srcSpan)
          <> ")"
    void $ LSP.sendRequest SWorkspaceApplyEdit (ApplyWorkspaceEditParams Nothing wedit) (\_ -> pure ())
  return $ Right Null

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
            return (msr, ps, fromMaybe "" contents)
      let df = ms_hspp_opts msrModSummary
          wantedModule = mkModuleName (T.unpack importName)
          wantedQual = mkModuleName . T.unpack <$> importQual
          existingImport = find (isWantedModule wantedModule wantedQual) msrImports
          thingParent' = T.unpack <$> thingParent
          newThing' = T.unpack newThing
          importQual' = T.unpack <$> importQual
      case existingImport of
        Just imp -> do
            fmap (nfp,) $ liftEither $
              rewriteToWEdit df doc
#if !MIN_VERSION_ghc(9,2,0)
                (annsA ps)
#endif
                $
                  extendImport thingParent' newThing' (makeDeltaAst imp)
        Nothing -> do
            let newImport = maybe (NewUnqualifiedImportForIdentifier thingParent' newThing' False)
                    NewQualifiedImport importQual'
                editE = newImportToEdit NewImportContext
                    { nicFileContents = contents,
                      nicParsedSource = ps,
                      nicDynFlags = df,
                      nicModuleName = T.unpack importName
                    } newImport
            case editE of
                Left errMsg -> do
                    liftIO $ logError (ideLogger ideState) $ "[extendImport] error: " <> T.pack errMsg
                    mzero
                Right edit -> do
                    let workspaceEdit = WorkspaceEdit
                            { _changes = Just (fromList [(doc, List[edit])]),
                              _documentChanges = Nothing,
                              _changeAnnotations = Nothing
                            }
                    liftIO $ logDebug (ideLogger ideState) $ "[extendImport] workspace edit: " <> T.pack (show workspaceEdit)
                    pure (nfp, workspaceEdit)
  | otherwise =
    mzero

isWantedModule :: ModuleName -> Maybe ModuleName -> GenLocated l (ImportDecl GhcPs) -> Bool
isWantedModule wantedModule Nothing (L _ it@ImportDecl{ideclName, ideclHiding = Just (False, _)}) =
    not (isQualifiedImport it) && unLoc ideclName == wantedModule
isWantedModule wantedModule (Just qual) (L _ ImportDecl{ideclAs, ideclName, ideclHiding = Just (False, _)}) =
    unLoc ideclName == wantedModule && (wantedModule == qual || (unLoc . reLoc <$> ideclAs) == Just qual)
isWantedModule _ _ _ = False

liftEither :: Monad m => Either e a -> MaybeT m a
liftEither (Left _)  = mzero
liftEither (Right x) = return x
