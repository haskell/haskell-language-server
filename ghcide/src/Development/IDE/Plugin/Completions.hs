{-# LANGUAGE CPP          #-}
{-# LANGUAGE TypeFamilies #-}
#include "ghc-api-version.h"

module Development.IDE.Plugin.Completions(plugin) where

import Control.Applicative
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
import Development.IDE.Plugin.Completions.Logic
import Development.IDE.Types.Location
import Development.IDE.Types.Options
import Development.IDE.Core.Compile
import Development.IDE.Core.PositionMapping
import Development.IDE.Core.RuleTypes
import Development.IDE.Core.Shake
import Development.IDE.GHC.Compat (hsmodExports, ParsedModule(..), ModSummary (ms_hspp_buf))

import Development.IDE.GHC.Util
import Development.IDE.LSP.Server
import Control.Monad.Trans.Except (runExceptT)
import HscTypes (HscEnv(hsc_dflags))
import Data.Maybe
import Data.Functor ((<&>))

#if !MIN_GHC_API_VERSION(8,6,0) || defined(GHC_LIB)
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
#if MIN_GHC_API_VERSION(8,6,0) && !defined(GHC_LIB)
        let parsedDeps = []
#else
        deps <- maybe (TransitiveDependencies [] [] []) fst <$> useWithStale GetDependencies file
        parsedDeps <- mapMaybe (fmap fst) <$> usesWithStale GetParsedModule (transitiveModuleDeps deps)
#endif

        case (ms, sess) of
            (Just ms, Just sess) -> do
                -- After parsing the module remove all package imports referring to
                -- these packages as we have already dealt with what they map to.
                let env = hscEnv sess
                    buf = fromJust $ ms_hspp_buf ms
                    f = fromNormalizedFilePath file
                    dflags = hsc_dflags env
                pm <- liftIO $ evalGhcEnv env $ runExceptT $ parseHeader dflags f buf
                case pm of
                    Right (_diags, hsMod) -> do
                        let hsModNoExports = hsMod <&> \x -> x{hsmodExports = Nothing}
                            pm = ParsedModule
                                    { pm_mod_summary = ms
                                    , pm_parsed_source = hsModNoExports
                                    , pm_extra_src_files = [] -- src imports not allowed
                                    , pm_annotations = mempty
                                    }
                        tm <- liftIO $ typecheckModule (IdeDefer True) env pm
                        case tm of
                            (_, Just (_,TcModuleResult{..})) -> do
                                cdata <- liftIO $ cacheDataProducer env tmrModule parsedDeps
                                -- Do not return diags from parsing as they would duplicate
                                -- the diagnostics from typechecking
                                return ([], Just cdata)
                            (_diag, _) ->
                                return ([], Nothing)
                    Left _diag ->
                        return ([], Nothing)
            _ -> return ([], Nothing)

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
    :: LSP.LspFuncs c
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
            pure (opts, liftA2 (,) compls pm)
        case compls of
          Just ((cci', _), (pm, mapping)) -> do
            let !position' = fromCurrentPosition mapping position
            pfix <- maybe (return Nothing) (flip VFS.getCompletionPrefix cnts) position'
            case (pfix, completionContext) of
              (Just (VFS.PosPrefixInfo _ "" _ _), Just CompletionContext { _triggerCharacter = Just "."})
                -> return (Completions $ List [])
              (Just pfix', _) -> do
                  -- TODO pass the real capabilities here (or remove the logic for snippets)
                let fakeClientCapabilities = ClientCapabilities Nothing Nothing Nothing Nothing
                Completions . List <$> getCompletions ideOpts cci' pm pfix' fakeClientCapabilities (WithSnippets True)
              _ -> return (Completions $ List [])
          _ -> return (Completions $ List [])
      _ -> return (Completions $ List [])

setHandlersCompletion :: PartialHandlers c
setHandlersCompletion = PartialHandlers $ \WithMessage{..} x -> return x{
    LSP.completionHandler = withResponse RspCompletion getCompletionsLSP
    }
