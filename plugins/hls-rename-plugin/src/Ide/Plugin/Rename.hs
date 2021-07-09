{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE NamedFieldPuns    #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Ide.Plugin.Rename (descriptor) where

import           Control.Monad.IO.Class
import           Control.Monad.Trans.Class
import qualified Data.Bifunctor
import           Data.Char
import           Data.Either
import           Data.HashMap.Internal                (fromList)
import qualified Data.HashMap.Strict                  as HM
import           Data.List
import qualified Data.Map                             as M
import           Data.Maybe
import qualified Data.Text                            as T
import           Development.IDE                      hiding (pluginHandlers)
import           Development.IDE.Core.Actions         (refsAtPoint)
import           Development.IDE.Core.PositionMapping
import           Development.IDE.Core.Shake
import           Development.IDE.GHC.Compat
import           Development.IDE.Spans.AtPoint
import           Ide.Plugin.Retrie                    hiding (descriptor)
import           Ide.Types
import           Language.LSP.Types
import           Name
import           Retrie
import Control.Monad.Trans.Maybe

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
    refs <- liftIO $ runAction "Rename.refsAtPoint" state $ refsAtPoint nfp pos

    let emptyContextUpdater c i = const (return c)
        isType = isUpper $ head oldNameStr
        oldNameStr = getOccString oldName
        rewrite = (if isType then AdhocType else Adhoc) (oldNameStr ++ " = " ++ T.unpack newName)
    (_errors, edits) <- liftIO $
        callRetrieWithTransformerAndUpdates
            (isReference refs)
            emptyContextUpdater
            state
            (hscEnv $ fst $ fromJust session)
            [Right rewrite]
            nfp
            True
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
    Just (HAR _ asts _ _ _, mapping) <- head <$> usesWithStale GetHieAst [nfp]
    return $ head $ getNamesAtPoint asts pos mapping

------ Debugging
showAstNode :: Int -> HieAST a -> [Char]
showAstNode n ast =
    intercalate indentation [
        "Ast Span: " ++ show (nodeSpan ast),
        "Info: " ++  show (map showName (M.keys (nodeIdentifiers (nodeInfo ast)))),
        "Children: " ++ concatMap (showAstNode (succ n)) (nodeChildren ast)
        ]
            where indentation = '\n' : replicate (n * 4) ' '

showName :: NamedThing a => Either ModuleName a -> String
showName (Left mn) = moduleNameString mn
showName (Right n) = getOccString n
------
