{-# LANGUAGE CPP          #-}
{-# LANGUAGE TypeFamilies #-}
#include "ghc-api-version.h"

module Development.IDE.Plugin.Completions
    ( descriptor
    , LocalCompletions(..)
    , NonLocalCompletions(..)
    ) where
import Language.Haskell.LSP.Types
import qualified Language.Haskell.LSP.Core as LSP
import qualified Language.Haskell.LSP.VFS as VFS

import Control.Monad
import Control.Monad.Trans.Maybe
import Data.Aeson
import Data.List (find)
import Data.Maybe
import qualified Data.Text as T
import Development.Shake.Classes
import Development.Shake
import GHC.Generics
import Development.IDE.Core.Service
import Development.IDE.Core.PositionMapping
import Development.IDE.Plugin.Completions.Logic
import Development.IDE.Types.Location
import Development.IDE.Core.RuleTypes
import Development.IDE.Core.Shake
import Development.IDE.GHC.Compat
import Development.IDE.GHC.ExactPrint (Annotated (annsA), GetAnnotatedParsedSource (GetAnnotatedParsedSource))
import Development.IDE.GHC.Util
import Development.IDE.Plugin.CodeAction.ExactPrint
import Development.IDE.Plugin.Completions.Types
import Ide.Plugin.Config (Config (completionSnippetsOn))
import Ide.PluginUtils (getClientConfig)
import Ide.Types
import TcRnDriver (tcRnImportDecls)
#if defined(GHC_LIB)
import Development.IDE.Import.DependencyInformation
#endif

descriptor :: PluginId -> PluginDescriptor IdeState
descriptor plId = (defaultPluginDescriptor plId)
    { pluginRules = produceCompletions,
      pluginCompletionProvider = Just (getCompletionsLSP plId),
      pluginCommands = [extendImportCommand]
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

-- When possible, rely on the haddocks embedded in our interface files
-- This creates problems on ghc-lib, see comment on 'getDocumentationTryGhc'
#if !defined(GHC_LIB)
        let parsedDeps = []
#else
        deps <- maybe (TransitiveDependencies [] [] []) fst <$> useWithStale GetDependencies file
        parsedDeps <- mapMaybe (fmap fst) <$> usesWithStale GetParsedModule (transitiveModuleDeps deps)
#endif

        case (ms, sess) of
            (Just (ms,imps), Just sess) -> do
              let env = hscEnv sess
              -- We do this to be able to provide completions of items that are not restricted to the explicit list
              res <- liftIO $ tcRnImportDecls env (dropListFromImportDecl <$> imps)
              case res of
                  (_, Just rdrEnv) -> do
                      let uri = fromNormalizedUri $ normalizedFilePathToUri file
                      cdata <- liftIO $ cacheDataProducer uri env (ms_mod ms) rdrEnv imps parsedDeps
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
        _ -> d
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
    :: PluginId
    -> LSP.LspFuncs Config
    -> IdeState
    -> CompletionParams
    -> IO (Either ResponseError CompletionResponseResult)
getCompletionsLSP plId lsp ide
  CompletionParams{_textDocument=TextDocumentIdentifier uri
                  ,_position=position
                  ,_context=completionContext} = do
    contents <- LSP.getVirtualFileFunc lsp $ toNormalizedUri uri
    fmap Right $ case (contents, uriToFilePath' uri) of
      (Just cnts, Just path) -> do
        let npath = toNormalizedFilePath' path
        (ideOpts, compls) <- runIdeAction "Completion" (shakeExtras ide) $ do
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
                -> return (Completions $ List [])
              (Just pfix', _) -> do
                let clientCaps = clientCapabilities $ shakeExtras ide
                config <- getClientConfig lsp
                let snippets = WithSnippets . completionSnippetsOn $ config
                allCompletions <- getCompletions plId ideOpts cci' parsedMod bindMap pfix' clientCaps snippets
                pure $ Completions (List allCompletions)
              _ -> return (Completions $ List [])
          _ -> return (Completions $ List [])
      _ -> return (Completions $ List [])

----------------------------------------------------------------------------------------------------

extendImportCommand :: PluginCommand IdeState
extendImportCommand =
  PluginCommand (CommandId extendImportCommandId) "additional edits for a completion" extendImportHandler

extendImportHandler :: CommandFunction IdeState ExtendImport
extendImportHandler _lsp ideState edit = do
  res <- runMaybeT $ extendImportHandler' ideState edit
  return (Right Null, res)

extendImportHandler' :: IdeState -> ExtendImport -> MaybeT IO (ServerMethod, ApplyWorkspaceEditParams)
extendImportHandler' ideState ExtendImport {..}
  | Just fp <- uriToFilePath doc,
    nfp <- toNormalizedFilePath' fp =
    do
      (ms, ps, imps) <- MaybeT $
        runAction "extend import" ideState $
          runMaybeT $ do
            -- We want accurate edits, so do not use stale data here
            (ms, imps) <- MaybeT $ use GetModSummaryWithoutTimestamps nfp
            ps <- MaybeT $ use GetAnnotatedParsedSource nfp
            return (ms, ps, imps)
      let df = ms_hspp_opts ms
          wantedModule = mkModuleName (T.unpack importName)
      imp <- liftMaybe $ find (isWantedModule wantedModule) imps
      wedit <-
        liftEither $
          rewriteToEdit df doc (annsA ps) $
            extendImport (T.unpack <$> thingParent) (T.unpack newThing) imp
      return (WorkspaceApplyEdit, ApplyWorkspaceEditParams wedit)
  | otherwise =
    mzero

isWantedModule :: ModuleName -> GenLocated l (ImportDecl pass) -> Bool
isWantedModule wantedModule (L _ ImportDecl {..}) = unLoc ideclName == wantedModule
isWantedModule _ _ = False

liftMaybe :: Monad m => Maybe a -> MaybeT m a
liftMaybe a = MaybeT $ pure a

liftEither :: Monad m => Either e a -> MaybeT m a
liftEither (Left _) = mzero
liftEither (Right x) = return x
