{-# LANGUAGE ExplicitNamespaces #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE NamedFieldPuns     #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
module Ide.Plugin.CallHierarchy where

import           Control.Lens                  ((^.))
import           Control.Monad.IO.Class
import           Data.List
import qualified Data.Map                      as M
import           Data.Maybe
import qualified Data.Text                     as T
import           Development.IDE
import           Development.IDE.Core.Shake
import           Development.IDE.GHC.Compat
import           Development.IDE.Spans.AtPoint
import           HieDb                         (searchDef, type (:.) ((:.)))
import           Ide.Types
import           Language.LSP.Types
import qualified Language.LSP.Types.Lens       as L
import           Name

descriptor :: PluginId -> PluginDescriptor IdeState
descriptor plId = (defaultPluginDescriptor plId)
  { Ide.Types.pluginHandlers = mkPluginHandler STextDocumentPrepareCallHierarchy prepareCallHierarchy
  }

prepareCallHierarchy :: PluginMethodHandler IdeState TextDocumentPrepareCallHierarchy
prepareCallHierarchy state pluginId param
  | Just nfp <- uriToNormalizedFilePath $ toNormalizedUri uri =
    liftIO $ prepareCallHierarchyItem state nfp pos >>=
    \case
      Just item -> pure $ Right $ Just $ List item
      Nothing   -> pure $ Left $ responseError "Call Hierarchy: No result"
  | otherwise = pure $ Left $ responseError $ T.pack $ "Call Hierarchy: uriToNormalizedFilePath failed for: " <> show uri
  where
    uri = param ^. (L.textDocument . L.uri)
    pos = param ^. L.position



incomingCalls :: PluginMethodHandler IdeState CallHierarchyIncomingCalls
incomingCalls = undefined

outgoingCalls :: PluginMethodHandler IdeState CallHierarchyOutgoingCalls
outgoingCalls = undefined

prepareCallHierarchyItem :: IdeState -> NormalizedFilePath -> Position -> IO (Maybe [CallHierarchyItem])
prepareCallHierarchyItem state filepath pos = do
  let ShakeExtras{hiedb} = shakeExtras state
  runAction "CallHierarchy.mkCallHierarchyItem" state (use GetHieAst filepath) >>=
    \case
      Nothing -> pure Nothing
      Just (HAR _ hf _ _ _) -> do
        case listToMaybe $
          pointCommand hf pos
            (\ast -> ((M.keys . nodeIdentifiers . nodeInfo) ast, nodeSpan ast)) of
              Just res -> pure $ Just $ nub $ map (\x -> construct x (snd res)) (fst res)
              _ -> pure Nothing
  where
    getSymbolKind occName
      | isVarOcc occName = SkVariable
      | isDataOcc occName = SkConstructor
      | isTvOcc occName = SkStruct
      | otherwise = SkUnknown 27 -- avoid duplication

    construct :: Identifier -> Span -> CallHierarchyItem
    construct identifer span = case identifer of
      Left modName -> mkCallHierarchyItem (moduleNameString modName) SkModule span
      Right name -> let occName = nameOccName name
                    in mkCallHierarchyItem (occNameString occName) (getSymbolKind occName) span

    mkCallHierarchyItem :: String -> SymbolKind -> Span -> CallHierarchyItem
    mkCallHierarchyItem name kind span =
      CallHierarchyItem
        (T.pack name)
        kind
        Nothing
        Nothing
        (fromNormalizedUri $ normalizedFilePathToUri filepath)
        (realSrcSpanToRange span)
        (realSrcSpanToRange span)
        Nothing


