{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE NamedFieldPuns      #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Ide.Plugin.CallHierarchy.Internal where

import           Control.Lens                   (Field1 (_1), Field3 (_3), (^.))
import           Control.Monad.IO.Class
import           Data.Aeson                     as A
import qualified Data.HashMap.Strict            as HM
import qualified Data.Map                       as M
import           Data.Maybe
import qualified Data.Set                       as S
import qualified Data.Text                      as T
import           Development.IDE
import           Development.IDE.Core.Shake
import           Development.IDE.GHC.Compat
import           Development.IDE.Spans.AtPoint
import           HieDb                          (Symbol (Symbol))
import qualified Ide.Plugin.CallHierarchy.Query as Q
import           Ide.Plugin.CallHierarchy.Types
import           Ide.Types
import           Language.LSP.Types
import qualified Language.LSP.Types.Lens        as L
import           Name
import           Text.Read                      (readMaybe)

prepareCallHierarchy :: PluginMethodHandler IdeState TextDocumentPrepareCallHierarchy
prepareCallHierarchy state pluginId param
  | Just nfp <- uriToNormalizedFilePath $ toNormalizedUri uri =
    liftIO (runAction "" state (prepareCallHierarchyItem nfp pos)) >>=
      \case
        Just items -> pure $ Right $ Just $ List items
        Nothing    -> pure $ Left $ responseError "Call Hierarchy: No result"
  | otherwise = pure $ Left $ responseError $ T.pack $ "Call Hierarchy: uriToNormalizedFilePath failed for: " <> show uri
  where
    uri = param ^. (L.textDocument . L.uri)
    pos = param ^. L.position

prepareCallHierarchyItem :: NormalizedFilePath -> Position -> Action (Maybe [CallHierarchyItem])
prepareCallHierarchyItem = constructFromAst

constructFromAst ::NormalizedFilePath -> Position -> Action (Maybe [CallHierarchyItem])
constructFromAst nfp pos =
  use GetHieAst nfp >>=
    \case
      Nothing -> pure Nothing
      Just (HAR _ hf _ _ _) -> do
        case listToMaybe $ pointCommand hf pos extract of
          Just res -> pure $ Just $ mapMaybe (construct nfp) res
          Nothing  -> pure Nothing

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

construct :: NormalizedFilePath -> (Identifier, S.Set ContextInfo, Span) -> Maybe CallHierarchyItem
construct nfp (ident, contexts, ssp)
  | Just (RecField RecFieldDecl _) <- recFieldInfo contexts
    -- ignored type span
    = Just $ mkCallHierarchyItem' ident SkField ssp ssp

  | Just ctx <- valBindInfo contexts
    = Just $ case ctx of
        ValBind _ _ span -> mkCallHierarchyItem' ident SkFunction (renderSpan span) ssp
        _ -> mkCallHierarchyItem' ident skUnknown ssp ssp

  | Just ctx <- declInfo contexts
    = Just $ case ctx of
        -- TODO: sort in alphabetical order
        Decl DataDec span -> mkCallHierarchyItem' ident SkStruct (renderSpan span) ssp
        Decl ConDec span -> mkCallHierarchyItem' ident SkConstructor (renderSpan span) ssp
        Decl SynDec span -> mkCallHierarchyItem' ident SkTypeParameter (renderSpan span) ssp
        Decl ClassDec span -> mkCallHierarchyItem' ident SkInterface (renderSpan span) ssp
        Decl FamDec span -> mkCallHierarchyItem' ident SkFunction (renderSpan span) ssp
        Decl InstDec span -> mkCallHierarchyItem' ident SkInterface (renderSpan span) ssp
        _ -> mkCallHierarchyItem' ident skUnknown ssp ssp

  | Just (ClassTyDecl span) <- classTyDeclInfo contexts
    = Just $ mkCallHierarchyItem' ident SkMethod (renderSpan span) ssp

  | Just (PatternBind _ _ span) <- patternBindInfo contexts
    = Just $ mkCallHierarchyItem' ident SkFunction (renderSpan span) ssp

  | Just Use <- useInfo contexts
    = Just $ mkCallHierarchyItem' ident SkInterface ssp ssp

  | otherwise = Nothing
  where
    renderSpan = \case Just span -> span
                       _         -> ssp
    skUnknown = SkUnknown 27
    mkCallHierarchyItem' = mkCallHierarchyItem nfp

mkCallHierarchyItem :: NormalizedFilePath -> Identifier -> SymbolKind -> Span -> Span -> CallHierarchyItem
mkCallHierarchyItem nfp ident kind span selSpan =
  CallHierarchyItem
    (T.pack $ identifierName ident)
    kind
    Nothing
    (T.pack . show <$> mkSymbol ident)
    (fromNormalizedUri $ normalizedFilePathToUri nfp)
    (realSrcSpanToRange span)
    (realSrcSpanToRange selSpan)
    (toJSON . show <$> mkSymbol ident)

mkSymbol :: Identifier -> Maybe Symbol
mkSymbol = \case
  Left _     -> Nothing
  Right name -> Just $ Symbol (occName name) (nameModule name)

incomingCalls = undefined

outgoingCalls :: PluginMethodHandler IdeState CallHierarchyOutgoingCalls
outgoingCalls state pluginId param = do
  liftIO $ runAction "CallHierarchy.outgoingCalls" state $ queryCalls state (param ^. L.item) >>=
    \case
      Just x  -> pure $ Right $ Just $ List x
      Nothing -> pure $ Left $ responseError "CallHierarchy.outgoingCalls error"

queryCalls :: IdeState -> CallHierarchyItem -> Action (Maybe [CallHierarchyOutgoingCall])
queryCalls state item
  | Just nfp <- uriToNormalizedFilePath $ toNormalizedUri uri = do
    ShakeExtras{hiedb} <- getShakeExtras
    maySymbol <- getSymbol nfp
    case maySymbol of
      Nothing -> error "CallHierarchy.Impossible"
      Just symbol -> do
        vs <- liftIO $ Q.outgoingCalls hiedb symbol
        nonFOIItems <- mapM mkCallHierarchyOutgoingCall vs
        foiRes <- foiOutgoingCalls nfp pos
        let nonFOIRes = Just $ catMaybes nonFOIItems
        pure (nonFOIRes <> foiRes)
  | otherwise = pure Nothing
  where
    uri = item ^. L.uri
    xdata = item ^. L.xdata
    pos = item ^. (L.selectionRange . L.start)

    getSymbol nfp =
      case item ^. L.xdata of
        Just xdata -> case fromJSON xdata of
          Success (symbolStr :: String) ->
            case readMaybe symbolStr of
              Just symbol -> pure $ Just symbol
              Nothing     -> getSymbolFromAst nfp pos
          A.Error _ -> getSymbolFromAst nfp pos
        Nothing -> getSymbolFromAst nfp pos

    mkCallHierarchyOutgoingCall :: Vertex -> Action (Maybe CallHierarchyOutgoingCall)
    mkCallHierarchyOutgoingCall Vertex{..} = do
      let pos = Position (sl - 1) (sc - 1)
          nfp = toNormalizedFilePath' hieSrc
          range = mkRange (casl - 1) (casc - 1) (cael - 1) (caec - 1)
      items <- prepareCallHierarchyItem nfp pos
      case items of
        Just [item] -> pure $ Just $ CallHierarchyOutgoingCall item (List [range])
        _           -> pure Nothing

-- Incoming calls for FOIs, caller range is broken apparently.
foiOutgoingCalls :: NormalizedFilePath -> Position -> Action (Maybe [CallHierarchyOutgoingCall])
foiOutgoingCalls nfp pos =
  use GetHieAst nfp >>=
    \case
      Nothing -> pure Nothing
      Just (HAR _ hf _ _ _) -> do
        case listToMaybe $ pointCommand hf pos nodeChildren of
          Nothing       -> pure Nothing
          Just children -> pure $ Just $ mkOutgoingCalls children
  where
    mkOutgoingCalls asts = let infos = concatMap extract asts
                               items = mapMaybe (construct nfp) infos
                           in  map (\item -> CallHierarchyOutgoingCall item (List [item ^. L.selectionRange])) items -- obvious error of range

getSymbolFromAst :: NormalizedFilePath -> Position -> Action (Maybe Symbol)
getSymbolFromAst nfp pos =
  use GetHieAst nfp >>=
    \case
      Nothing -> pure Nothing
      Just (HAR _ hf _ _ _) -> do
        case listToMaybe $ pointCommand hf pos extract of
          Just infos -> case (\(ident, _, _) -> mkSymbol ident) <$> listToMaybe infos of
            Nothing  -> pure Nothing
            Just res -> pure res
          Nothing -> pure Nothing

-- withHieAst :: NormalizedFilePath -> Position -> (HieAST a -> b) -> (b -> c) -> Action (Maybe c)
-- withHieAst nfp pos f trans =
--   use GetHieAst nfp >>=
--     \case
--       Nothing -> pure Nothing
--       Just (HAR _ hf _ _ _) -> do
--         case listToMaybe $ pointCommand hf pos f of
--           Nothing -> pure Nothing
--           Just res -> pure $ Just (trans res)
--   where
--     getAst :: forall aa.Action (Maybe (HieASTs aa))
--     getAst =
--       use GetHieAst nfp >>=
--         \case
--           Nothing -> pure Nothing
--           Just (HAR _ hf _ _ _) -> pure $ Just hf
