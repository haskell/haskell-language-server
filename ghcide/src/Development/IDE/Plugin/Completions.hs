{-# LANGUAGE CPP          #-}
{-# LANGUAGE RankNTypes   #-}
{-# LANGUAGE TypeFamilies #-}

module Development.IDE.Plugin.Completions
    ( descriptor
    , LocalCompletions(..)
    , NonLocalCompletions(..)
    ) where

import           Control.Concurrent.Async                     (concurrently)
import           Control.Monad.Extra
import           Control.Monad.IO.Class
import           Control.Monad.Trans.Maybe
import           Data.Aeson
import           Data.List                                    (find)
import           Data.Maybe
import qualified Data.Text                                    as T
import           Development.IDE.Core.PositionMapping
import           Development.IDE.Core.RuleTypes
import           Development.IDE.Core.Service
import           Development.IDE.Core.Shake
import           Development.IDE.GHC.Compat
import           Development.IDE.GHC.Error                    (rangeToSrcSpan)
import           Development.IDE.GHC.ExactPrint               (Annotated (annsA),
                                                               GetAnnotatedParsedSource (GetAnnotatedParsedSource))
import           Development.IDE.GHC.Util                     (prettyPrint)
import           Development.IDE.Graph
import           Development.IDE.Graph.Classes
import           Development.IDE.Plugin.CodeAction.ExactPrint
import           Development.IDE.Plugin.Completions.Logic
import           Development.IDE.Plugin.Completions.Types
import           Development.IDE.Types.HscEnvEq               (hscEnv)
import           Development.IDE.Types.Location
import           GHC.Exts                                     (toList)
import           GHC.Generics
import           Ide.Plugin.Config                            (Config)
import           Ide.Types
import qualified Language.LSP.Server                          as LSP
import           Language.LSP.Types
import qualified Language.LSP.VFS                             as VFS
#if MIN_VERSION_ghc(9,0,0)
import           GHC.Tc.Module                                (tcRnImportDecls)
#else
import           TcRnDriver                                   (tcRnImportDecls)
#endif

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
dropListFromImportDecl :: GenLocated SrcSpan (ImportDecl GhcPs) -> GenLocated SrcSpan (ImportDecl GhcPs)
dropListFromImportDecl iDecl = let
    f d@ImportDecl {ideclHiding} = case ideclHiding of
        Just (False, _) -> d {ideclHiding=Nothing}
        -- if hiding or Nothing just return d
        _               -> d
    f x = x
    in f <$> iDecl

-- | Produce completions info for a file
type instance RuleResult LocalCompletions = CachedCompletions
type instance RuleResult NonLocalCompletions = CachedCompletions

data LocalCompletions = LocalCompletions
    deriving (Eq, Show, Typeable, Generic)
instance Hashable LocalCompletions
instance NFData   LocalCompletions
instance Binary   LocalCompletions

data NonLocalCompletions = NonLocalCompletions
    deriving (Eq, Show, Typeable, Generic)
instance Hashable NonLocalCompletions
instance NFData   NonLocalCompletions
instance Binary   NonLocalCompletions

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
        (ideOpts, compls) <- liftIO $ runIdeAction "Completion" (shakeExtras ide) $ do
            opts <- liftIO $ getIdeOptionsIO $ shakeExtras ide
            localCompls <- useWithStaleFast LocalCompletions npath
            nonLocalCompls <- useWithStaleFast NonLocalCompletions npath
            pm <- useWithStaleFast GetParsedModule npath
            binds <- fromMaybe (mempty, zeroMapping) <$> useWithStaleFast GetBindings npath
            pure (opts, fmap (,pm,binds) ((fst <$> localCompls) <> (fst <$> nonLocalCompls)))
        case compls of
          Just (cci', parsedMod, bindMap) -> do
            pfix <- VFS.getCompletionPrefix position cnts
            case (pfix, completionContext) of
              (Just (VFS.PosPrefixInfo _ "" _ _), Just CompletionContext { _triggerCharacter = Just "."})
                -> return (InL $ List [])
              (Just pfix', _) -> do
                let clientCaps = clientCapabilities $ shakeExtras ide
                config <- getCompletionsConfig plId
                allCompletions <- liftIO $ getCompletions plId ideOpts cci' parsedMod bindMap pfix' clientCaps config
                pure $ InL (List allCompletions)
              _ -> return (InL $ List [])
          _ -> return (InL $ List [])
      _ -> return (InL $ List [])

----------------------------------------------------------------------------------------------------

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
      (ModSummaryResult {..}, ps) <- MaybeT $ liftIO $
        runAction "extend import" ideState $
          runMaybeT $ do
            -- We want accurate edits, so do not use stale data here
            msr <- MaybeT $ use GetModSummaryWithoutTimestamps nfp
            ps <- MaybeT $ use GetAnnotatedParsedSource nfp
            return (msr, ps)
      let df = ms_hspp_opts msrModSummary
          wantedModule = mkModuleName (T.unpack importName)
          wantedQual = mkModuleName . T.unpack <$> importQual
      imp <- liftMaybe $ find (isWantedModule wantedModule wantedQual) msrImports
      fmap (nfp,) $ liftEither $
        rewriteToWEdit df doc (annsA ps) $
          extendImport (T.unpack <$> thingParent) (T.unpack newThing) imp
  | otherwise =
    mzero

isWantedModule :: ModuleName -> Maybe ModuleName -> GenLocated l (ImportDecl pass) -> Bool
isWantedModule wantedModule Nothing (L _ it@ImportDecl{ideclName, ideclHiding = Just (False, _)}) =
    not (isQualifiedImport it) && unLoc ideclName == wantedModule
isWantedModule wantedModule (Just qual) (L _ ImportDecl{ideclAs, ideclName, ideclHiding = Just (False, _)}) =
    unLoc ideclName == wantedModule && (wantedModule == qual || (unLoc <$> ideclAs) == Just qual)
isWantedModule _ _ _ = False

liftMaybe :: Monad m => Maybe a -> MaybeT m a
liftMaybe a = MaybeT $ pure a

liftEither :: Monad m => Either e a -> MaybeT m a
liftEither (Left _)  = mzero
liftEither (Right x) = return x
