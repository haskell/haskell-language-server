{-# LANGUAGE ExplicitNamespaces #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE NamedFieldPuns     #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
module Ide.Plugin.CallHierarchy where

import           Control.Lens                  ((^.))
import           Control.Monad.IO.Class
import qualified Data.Map                      as M
import           Data.Maybe
import qualified Data.Set                      as S
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
        Just items -> pure $ Right $ Just $ List items
        Nothing    -> pure $ Left $ responseError "Call Hierarchy: No result"
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
  runAction "CallHierarchy.prepareCallHierarchyItem" state (use GetHieAst filepath) >>=
    \case
      Nothing -> pure Nothing
      Just (HAR _ hf _ _ _) -> do
        case listToMaybe $ pointCommand hf pos extract of
          Just res -> pure $ Just $ mapMaybe construct res
          _        -> pure Nothing
  where

    extract :: HieAST a -> [(Identifier, S.Set ContextInfo, Span)]
    extract ast = let span = nodeSpan ast
                      infos = M.toList $ M.map identInfo (nodeIdentifiers $ nodeInfo ast)
                  in  [ (ident, contexts, span) | (ident, contexts) <- infos ]

    identifierName :: Identifier -> String
    identifierName = \case
      Left modName -> moduleNameString modName
      Right name   -> occNameString $ nameOccName name

    recFieldInfo :: S.Set ContextInfo -> Maybe ContextInfo
    recFieldInfo ctxs = listToMaybe [ctx | ctx@RecField{} <- S.toList ctxs]

    declInfo :: S.Set ContextInfo -> Maybe ContextInfo
    declInfo ctxs = listToMaybe [ctx | ctx@Decl{} <- S.toList ctxs]

    valBindInfo :: S.Set ContextInfo -> Maybe ContextInfo
    valBindInfo ctxs = listToMaybe [ctx | ctx@ValBind{} <- S.toList ctxs]

    classTyDeclInfo :: S.Set ContextInfo -> Maybe ContextInfo
    classTyDeclInfo ctxs = listToMaybe [ctx | ctx@ClassTyDecl{} <- S.toList ctxs]

    useInfo :: S.Set ContextInfo -> Maybe ContextInfo
    useInfo ctxs = listToMaybe [Use | Use <- S.toList ctxs]

    patternBindInfo :: S.Set ContextInfo -> Maybe ContextInfo
    patternBindInfo ctxs = listToMaybe [ctx | ctx@PatternBind{} <- S.toList ctxs]

    construct :: (Identifier, S.Set ContextInfo, Span) -> Maybe CallHierarchyItem
    construct (ident, contexts, ssp)
      | Just (RecField RecFieldDecl _) <- recFieldInfo contexts
        -- ignored type span
        = Just $ mkCallHierarchyItem name SkField ssp ssp

      | Just ctx <- valBindInfo contexts
        = Just $ case ctx of
            ValBind _ _ span -> mkCallHierarchyItem name SkFunction (renderSpan span) ssp
            _ -> mkCallHierarchyItem name skUnknown ssp ssp

      | Just ctx <- declInfo contexts
        = Just $ case ctx of
            -- TODO: sort in alphabetical order
            Decl DataDec span -> mkCallHierarchyItem name SkStruct (renderSpan span) ssp
            Decl ConDec span -> mkCallHierarchyItem name SkConstructor (renderSpan span) ssp
            Decl SynDec span -> mkCallHierarchyItem name SkTypeParameter (renderSpan span) ssp
            Decl ClassDec span -> mkCallHierarchyItem name SkInterface (renderSpan span) ssp
            Decl FamDec span -> mkCallHierarchyItem name SkFunction (renderSpan span) ssp
            Decl InstDec span -> mkCallHierarchyItem name SkInterface (renderSpan span) ssp
            _ -> mkCallHierarchyItem name skUnknown ssp ssp

      | Just (ClassTyDecl span) <- classTyDeclInfo contexts
        = Just $ mkCallHierarchyItem name SkMethod (renderSpan span) ssp

      | Just (PatternBind _ _ span) <- patternBindInfo contexts
        = Just $ mkCallHierarchyItem name SkFunction (renderSpan span) ssp

      | Just Use <- useInfo contexts
        = Just $ mkCallHierarchyItem name SkInterface ssp ssp

      | otherwise = Nothing
      where
        name = identifierName ident
        renderSpan = \case Just span -> span
                           _         -> ssp
        skUnknown = SkUnknown 27

    mkCallHierarchyItem :: String -> SymbolKind -> Span -> Span -> CallHierarchyItem
    mkCallHierarchyItem name kind span selSpan =
      CallHierarchyItem
        (T.pack name)
        kind
        Nothing
        Nothing
        (fromNormalizedUri $ normalizedFilePathToUri filepath)
        (realSrcSpanToRange span)
        (realSrcSpanToRange selSpan)
        Nothing
