{-# LANGUAGE CPP          #-}
{-# LANGUAGE TypeFamilies #-}
#include "ghc-api-version.h"

module Development.IDE.Plugin.Completions
    (
      plugin
    , getCompletionsLSP
    ) where

import Language.Haskell.LSP.Messages
import Language.Haskell.LSP.Types
import qualified Language.Haskell.LSP.Core as LSP
import qualified Language.Haskell.LSP.VFS as VFS
import Language.Haskell.LSP.Types.Capabilities
import Development.Shake.Classes
import Development.Shake
import GHC.Generics

import Development.IDE.Plugin
import Development.IDE.Core.Service
import Development.IDE.Core.PositionMapping
import Development.IDE.Plugin.Completions.Logic
import Development.IDE.Types.Location
import Development.IDE.Core.RuleTypes
import Development.IDE.Core.Shake
import Development.IDE.GHC.Compat

import Development.IDE.GHC.Util
import Development.IDE.LSP.Server
import TcRnDriver (tcRnImportDecls)
import Data.Maybe

#if defined(GHC_LIB)
import Development.IDE.Import.DependencyInformation
#endif

plugin :: Plugin c
plugin = Plugin produceCompletions setHandlersCompletion

produceCompletions :: Rules ()
produceCompletions = do
    define $ \ProduceCompletions file -> do
        local <- useWithStale LocalCompletions file
        nonLocal <- useWithStale NonLocalCompletions file
        let extract = fmap fst
        return ([], extract local <> extract nonLocal)
    define $ \LocalCompletions file -> do
        pm <- useWithStale GetParsedModule file
        case pm of
            Just (pm, _) -> do
                let cdata = localCompletionsForParsedModule pm
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
              let imps' = map dropListFromImportDecl imps
              res <- liftIO $ tcRnImportDecls env imps'
              case res of
                  (_, Just rdrEnv) -> do
                      cdata <- liftIO $ cacheDataProducer env (ms_mod ms) rdrEnv imps' parsedDeps
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
type instance RuleResult ProduceCompletions = CachedCompletions
type instance RuleResult LocalCompletions = CachedCompletions
type instance RuleResult NonLocalCompletions = CachedCompletions

data ProduceCompletions = ProduceCompletions
    deriving (Eq, Show, Typeable, Generic)
instance Hashable ProduceCompletions
instance NFData   ProduceCompletions
instance Binary   ProduceCompletions

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
    :: LSP.LspFuncs cofd
    -> IdeState
    -> CompletionParams
    -> IO (Either ResponseError CompletionResponseResult)
getCompletionsLSP lsp ide
  CompletionParams{_textDocument=TextDocumentIdentifier uri
                  ,_position=position
                  ,_context=completionContext} = do
    contents <- LSP.getVirtualFileFunc lsp $ toNormalizedUri uri
    fmap Right $ case (contents, uriToFilePath' uri) of
      (Just cnts, Just path) -> do
        let npath = toNormalizedFilePath' path
        (ideOpts, compls) <- runIdeAction "Completion" (shakeExtras ide) $ do
            opts <- liftIO $ getIdeOptionsIO $ shakeExtras ide
            compls <- useWithStaleFast ProduceCompletions npath
            pm <- useWithStaleFast GetParsedModule npath
            binds <- fromMaybe (mempty, zeroMapping) <$> useWithStaleFast GetBindings npath
            pure (opts, fmap (,pm,binds) compls )
        case compls of
          Just ((cci', _), parsedMod, bindMap) -> do
            pfix <- VFS.getCompletionPrefix position cnts
            case (pfix, completionContext) of
              (Just (VFS.PosPrefixInfo _ "" _ _), Just CompletionContext { _triggerCharacter = Just "."})
                -> return (Completions $ List [])
              (Just pfix', _) -> do
                  -- TODO pass the real capabilities here (or remove the logic for snippets)
                let fakeClientCapabilities = ClientCapabilities Nothing Nothing Nothing Nothing
                Completions . List <$> getCompletions ideOpts cci' parsedMod bindMap pfix' fakeClientCapabilities (WithSnippets True)
              _ -> return (Completions $ List [])
          _ -> return (Completions $ List [])
      _ -> return (Completions $ List [])

setHandlersCompletion :: PartialHandlers c
setHandlersCompletion = PartialHandlers $ \WithMessage{..} x -> return x{
    LSP.completionHandler = withResponse RspCompletion getCompletionsLSP
    }
