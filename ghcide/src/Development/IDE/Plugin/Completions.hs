{-# LANGUAGE TypeFamilies #-}

module Development.IDE.Plugin.Completions(plugin) where

import Language.Haskell.LSP.Messages
import Language.Haskell.LSP.Types
import qualified Language.Haskell.LSP.Core as LSP
import qualified Language.Haskell.LSP.VFS as VFS
import Language.Haskell.LSP.Types.Capabilities
import Development.Shake.Classes
import Development.Shake
import GHC.Generics
import Data.Maybe
import HscTypes

import Development.IDE.Plugin
import Development.IDE.Core.Service
import Development.IDE.Plugin.Completions.Logic
import Development.IDE.Types.Location
import Development.IDE.Core.PositionMapping
import Development.IDE.Core.RuleTypes
import Development.IDE.Core.Shake
import Development.IDE.GHC.Util
import Development.IDE.LSP.Server
import Development.IDE.Import.DependencyInformation


plugin :: Plugin c
plugin = Plugin produceCompletions setHandlersCompletion

produceCompletions :: Rules ()
produceCompletions =
    define $ \ProduceCompletions file -> do
        deps <- maybe (TransitiveDependencies [] []) fst <$> useWithStale GetDependencies file
        parsedDeps <- mapMaybe (fmap fst) <$> usesWithStale GetParsedModule (transitiveModuleDeps deps)
        tm <- fmap fst <$> useWithStale TypeCheck file
        packageState <- fmap (hscEnv . fst) <$> useWithStale GhcSession file
        case (tm, packageState) of
            (Just tm', Just packageState') -> do
                cdata <- liftIO $ cacheDataProducer packageState' (hsc_dflags packageState')
                                                    (tmrModule tm') parsedDeps
                return ([], Just (cdata, tm'))
            _ -> return ([], Nothing)


-- | Produce completions info for a file
type instance RuleResult ProduceCompletions = (CachedCompletions, TcModuleResult)

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
        let npath = toNormalizedFilePath path
        (ideOpts, compls) <- runAction ide ((,) <$> getIdeOptions <*> useWithStale ProduceCompletions npath)
        case compls of
          Just ((cci', tm'), mapping) -> do
            let position' = fromCurrentPosition mapping position
            pfix <- maybe (return Nothing) (flip VFS.getCompletionPrefix cnts) position'
            case (pfix, completionContext) of
              (Just (VFS.PosPrefixInfo _ "" _ _), Just CompletionContext { _triggerCharacter = Just "."})
                -> return (Completions $ List [])
              (Just pfix', _) -> do
                let fakeClientCapabilities = ClientCapabilities Nothing Nothing Nothing Nothing
                Completions . List <$> getCompletions ideOpts cci' (tmrModule tm') pfix' fakeClientCapabilities (WithSnippets True)
              _ -> return (Completions $ List [])
          _ -> return (Completions $ List [])
      _ -> return (Completions $ List [])

setHandlersCompletion :: PartialHandlers c
setHandlersCompletion = PartialHandlers $ \WithMessage{..} x -> return x{
    LSP.completionHandler = withResponse RspCompletion getCompletionsLSP
    }
