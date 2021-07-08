{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE NamedFieldPuns    #-}
{-# LANGUAGE OverloadedStrings #-}

module Ide.Plugin.Rename (descriptor) where

import           Control.Monad.IO.Class
import           Control.Monad.Trans.Class
import qualified Data.Bifunctor
import           Data.Either
import           Data.HashMap.Internal                (fromList)
import qualified Data.HashMap.Strict                  as HM
import qualified Data.Map                             as M
import           Data.Maybe
import qualified Data.Text                            as T
import           Development.IDE                      hiding (pluginHandlers)
import           Development.IDE.Core.PositionMapping
import           Development.IDE.Core.Shake
import           Development.IDE.GHC.Compat
import           Development.IDE.LSP.HoverDefinition  (references)
import           Development.IDE.Spans.AtPoint
import           Ide.Plugin.Retrie                    hiding (descriptor)
import           Ide.Types
import           Language.LSP.Types
import           Name
import           Retrie
import Data.Char

type HiePosMap = HM.HashMap NormalizedFilePath (HieAstResult, PositionMapping)

descriptor :: PluginId -> PluginDescriptor IdeState
descriptor pluginId = (defaultPluginDescriptor pluginId) {
    pluginHandlers = mkPluginHandler STextDocumentRename renameProvider
}

renameProvider :: PluginMethodHandler IdeState TextDocumentRename
renameProvider state pluginId (RenameParams tdi@(TextDocumentIdentifier uri) pos _progToken newName) = response $ do
    let Just nfp = uriToNormalizedFilePath $ toNormalizedUri uri
    session <- liftIO $ runAction "Rename.GhcSessionDeps" state (useWithStale GhcSessionDeps nfp)
    oldName <- liftIO $ getOccString <$> runAction "Rename.nameAtPos" state (nameAtPos pos nfp)
    mRefs <- lift $ references state (ReferenceParams tdi pos Nothing Nothing (ReferenceContext False))
    let isType = isUpper $ head oldName
        rewrite = (if isType then AdhocType else Adhoc) (oldName ++ " = " ++ T.unpack newName)
        List refs = fromRight (List []) mRefs
    (_errors, edits) <- liftIO $
            callRetrie
                state
                (hscEnv $ fst $ fromJust session)
                [Right rewrite]
                nfp
                True
                (Just $ isReference refs)
    return edits

isReference :: [Location] -> MatchResultTransformer
isReference refs _ctxt match
  | MatchResult substitution template <- match
  , Just loc <- srcSpanToLocation $ getOrigin $ astA $ tTemplate template -- Bug: incorrect loc
  , loc `elem` refs = return match
  | otherwise = return NoMatch

nameAtPos :: Position -> NormalizedFilePath -> Action Name
nameAtPos pos nfp = do
    ShakeExtras{hiedb} <- getShakeExtras
    fois <- HM.keys <$> getFilesOfInterest
    asts <- HM.fromList . mapMaybe sequence . zip fois <$> usesWithStale GetHieAst fois
    let getName (HAR _ ast _ _ _, mapping) = listToMaybe $ getNamesAtPoint ast pos mapping
    return $ fromJust $ getName (fromJust $ HM.lookup nfp asts)
