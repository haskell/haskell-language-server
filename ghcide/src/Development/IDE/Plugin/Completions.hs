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
import Development.IDE.Core.PositionMapping
import Development.IDE.Core.RuleTypes
import Development.IDE.Core.Shake
import Development.IDE.GHC.Util
import Development.IDE.LSP.Server

#if !MIN_GHC_API_VERSION(8,6,0) || defined(GHC_LIB)
import Data.Maybe
import Development.IDE.Import.DependencyInformation
#endif

plugin :: Plugin c
plugin = Plugin produceCompletions setHandlersCompletion

produceCompletions :: Rules ()
produceCompletions =
    define $ \ProduceCompletions file -> do

-- When possible, rely on the haddocks embedded in our interface files
-- This creates problems on ghc-lib, see comment on 'getDocumentationTryGhc'
#if MIN_GHC_API_VERSION(8,6,0) && !defined(GHC_LIB)
        let parsedDeps = []
#else
        deps <- maybe (TransitiveDependencies [] [] []) fst <$> useWithStale GetDependencies file
        parsedDeps <- mapMaybe (fmap fst) <$> usesWithStale GetParsedModule (transitiveModuleDeps deps)
#endif
        tm <- fmap fst <$> useWithStale TypeCheck file
        packageState <- fmap (hscEnv . fst) <$> useWithStale GhcSession file
        case (tm, packageState) of
            (Just tm', Just packageState') -> do
                cdata <- liftIO $ cacheDataProducer packageState'
                                                    (tmrModule tm') parsedDeps
                return ([], Just cdata)
            _ -> return ([], Nothing)


-- | Produce completions info for a file
type instance RuleResult ProduceCompletions = CachedCompletions

data ProduceCompletions = ProduceCompletions
    deriving (Eq, Show, Typeable, Generic)
instance Hashable ProduceCompletions
instance NFData   ProduceCompletions
instance Binary   ProduceCompletions


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
        (ideOpts, compls) <- runAction ide $ do
            opts <- getIdeOptions
            compls <- useWithStale ProduceCompletions npath
            pm <- useWithStale GetParsedModule npath
            pure (opts, liftA2 (,) compls pm)
        case compls of
          Just ((cci', _), (pm, mapping)) -> do
            let !position' = fromCurrentPosition mapping position
            pfix <- maybe (return Nothing) (flip VFS.getCompletionPrefix cnts) position'
            case (pfix, completionContext) of
              (Just (VFS.PosPrefixInfo _ "" _ _), Just CompletionContext { _triggerCharacter = Just "."})
                -> return (Completions $ List [])
              (Just pfix', _) -> do
                let fakeClientCapabilities = ClientCapabilities Nothing Nothing Nothing Nothing
                Completions . List <$> getCompletions ideOpts cci' pm pfix' fakeClientCapabilities (WithSnippets True)
              _ -> return (Completions $ List [])
          _ -> return (Completions $ List [])
      _ -> return (Completions $ List [])

setHandlersCompletion :: PartialHandlers c
setHandlersCompletion = PartialHandlers $ \WithMessage{..} x -> return x{
    LSP.completionHandler = withResponse RspCompletion getCompletionsLSP
    }
