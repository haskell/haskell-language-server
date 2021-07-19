{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE NamedFieldPuns    #-}
{-# LANGUAGE OverloadedStrings #-}

module Ide.Plugin.Rename (descriptor) where

import           Control.Monad.IO.Class
import           Control.Monad.Trans.Class
import           Control.Monad.Trans.Maybe
import qualified Data.Bifunctor
import           Data.Char
import           Data.Containers.ListUtils
import           Data.HashMap.Internal                (fromList)
import qualified Data.HashMap.Strict                  as HM
import qualified Data.Map                             as M
import           Data.Maybe
import qualified Data.Text                            as T
import           Debug.Trace
import           Development.IDE                      hiding (pluginHandlers)
import           Development.IDE.Core.Actions         (refsAtPoint)
import           Development.IDE.Core.PositionMapping
import           Development.IDE.Core.Shake
import           Development.IDE.GHC.Compat
import           Development.IDE.Spans.AtPoint
import           HieDb.Query
import           Ide.Plugin.Retrie                    hiding (descriptor)
import           Ide.Types
import           Language.LSP.Types
import           Name
import           Retrie
import           Retrie.ExactPrint
import Retrie.Universe

type HiePosMap = HM.HashMap NormalizedFilePath (HieAstResult, PositionMapping)

descriptor :: PluginId -> PluginDescriptor IdeState
descriptor pluginId = (defaultPluginDescriptor pluginId) {
    pluginHandlers = mkPluginHandler STextDocumentRename renameProvider
}

renameProvider :: PluginMethodHandler IdeState TextDocumentRename
renameProvider state pluginId (RenameParams tdi@(TextDocumentIdentifier uri) pos _progToken newName) = response $ do
    let Just nfp = uriToNormalizedFilePath $ toNormalizedUri uri
    session <- liftIO $ runAction "Rename.GhcSessionDeps" state (useWithStale GhcSessionDeps nfp)
    oldName <- liftIO $ runAction "Rename.nameAtPos" state (nameAtPos pos nfp)
    refs <- liftIO $ runAction "Rename.references" state (refsAtName nfp oldName)

    let emptyContextUpdater c i = const (return c)
        isType = isUpper $ head oldNameStr
        oldNameStr = getOccString oldName
        -- rewrite = Unfold "Main.foo"
        rewrite = (if isType then AdhocType else Adhoc) (oldNameStr ++ " = " ++ T.unpack newName)
    (_errors, edits) <- liftIO $
        callRetrieWithTransformerAndUpdates
            (referenceTransformer refs)
            emptyContextUpdater
            state
            (hscEnv $ fst $ fromJust session)
            [Right rewrite]
            nfp
            True
    return edits

referenceTransformer :: [Location] -> MatchResultTransformer
referenceTransformer refs _ctxt match
  | MatchResult _substitution template <- match
  , Just loc <- srcSpanToLocation $ getOrigin $ astA $ tTemplate template -- Bug: incorrect loc
  , loc `elem` refs = return match
  | otherwise = return NoMatch

nameAtPos :: Position -> NormalizedFilePath -> Action Name
nameAtPos pos nfp = do
    Just (HAR _ asts _ _ _, mapping) <- head <$> usesWithStale GetHieAst [nfp]
    return $ head $ getNamesAtPoint asts pos mapping

refsAtName :: NormalizedFilePath -> Name -> Action [Location]
refsAtName nfp name = do
    ShakeExtras{hiedb} <- getShakeExtras
    fois <- HM.keys <$> getFilesOfInterest
    asts <- HM.fromList . mapMaybe sequence . zip fois <$> usesWithStale GetHieAst fois
    let foiRefs = concat $ mapMaybe (getNameAstLocations name) (HM.elems asts)
    refs <- nameDbRefs (HM.keys asts) name hiedb
    pure $ nubOrd $ foiRefs ++ refs

nameDbRefs :: [NormalizedFilePath] -> Name -> HieDb -> Action [Location]
nameDbRefs fois name hiedb =
    case nameModule_maybe name of
        Nothing -> pure []
        Just mod -> do
            let exclude = map fromNormalizedFilePath fois
            rows <- liftIO $ findReferences hiedb True (nameOccName name) (Just $ moduleName mod) (Just $ moduleUnitId mod) exclude
            pure $ mapMaybe rowToLoc rows

getNameAstLocations :: Name -> (HieAstResult, PositionMapping) -> Maybe [Location]
getNameAstLocations name (HAR _ _ rm _ _, mapping) =
    mapMaybe (toCurrentLocation mapping . realSrcSpanToLocation . fst) <$> M.lookup (Right name) rm

-- Debugging
showMatch :: MatchResult Universe -> [Char]
showMatch NoMatch = "Nomatch"
showMatch (MatchResult sub temp) =
    "Sub: " ++ show sub ++
    " \ntemp: " ++ showTemp (astA $ tTemplate temp) ++
    " \nAnns: " ++ show (annsA (tTemplate temp))

showTemp (ULHsExpr _) = "ULHsExpr"
showTemp (ULStmt _) = "ULStmt"
showTemp (ULType _) = "ULType"
showTemp (ULPat _) = "ULPat"
