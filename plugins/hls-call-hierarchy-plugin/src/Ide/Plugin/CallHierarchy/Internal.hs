{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE NamedFieldPuns      #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving  #-}

module Ide.Plugin.CallHierarchy.Internal (
  prepareCallHierarchy
, incomingCalls
, outgoingCalls
) where

import           Control.Lens                   ((^.))
import           Control.Monad.Extra
import           Control.Monad.IO.Class
import           Data.Aeson                     as A
import qualified Data.ByteString                as BS
import qualified Data.HashMap.Strict            as HM
import           Data.List                      (groupBy, sortBy)
import qualified Data.Map                       as M
import           Data.Maybe
import qualified Data.Set                       as S
import qualified Data.Text                      as T
import qualified Data.Text.Encoding             as T
import           Data.Tuple.Extra
import           Development.IDE
import           Development.IDE.Core.Compile
import           Development.IDE.Core.Shake
import           Development.IDE.GHC.Compat     as Compat
import           Development.IDE.Spans.AtPoint
import           GHC.Conc.Sync
import           HieDb                          (Symbol (Symbol))
import qualified Ide.Plugin.CallHierarchy.Query as Q
import           Ide.Plugin.CallHierarchy.Types
import           Ide.Types
import           Language.LSP.Types
import qualified Language.LSP.Types.Lens        as L
import           Text.Read                      (readMaybe)

-- | Render prepare call hierarchy request.
prepareCallHierarchy :: PluginMethodHandler IdeState TextDocumentPrepareCallHierarchy
prepareCallHierarchy state pluginId param
  | Just nfp <- uriToNormalizedFilePath $ toNormalizedUri uri =
    liftIO (runAction "CallHierarchy.prepareHierarchy" state (prepareCallHierarchyItem nfp pos)) >>=
      \case
        Just items -> pure $ Right $ Just $ List items
        Nothing    -> pure $ Right Nothing
  | otherwise = pure $ Left $ responseError $ T.pack $ "Call Hierarchy: uriToNormalizedFilePath failed for: " <> show uri
  where
    uri = param ^. (L.textDocument . L.uri)
    pos = param ^. L.position

prepareCallHierarchyItem :: NormalizedFilePath -> Position -> Action (Maybe [CallHierarchyItem])
prepareCallHierarchyItem = constructFromAst

constructFromAst :: NormalizedFilePath -> Position -> Action (Maybe [CallHierarchyItem])
constructFromAst nfp pos =
  use GetHieAst nfp >>=
    \case
      Nothing -> pure Nothing
      Just (HAR _ hf _ _ _) -> do
        resolveIntoCallHierarchy hf pos nfp

resolveIntoCallHierarchy :: Applicative f => HieASTs a -> Position -> NormalizedFilePath -> f (Maybe [CallHierarchyItem])
resolveIntoCallHierarchy hf pos nfp =
  case listToMaybe $ pointCommand hf pos extract of
    Nothing    -> pure Nothing
    Just infos ->
      case mapMaybe (construct nfp hf) infos of
        []  -> pure Nothing
        res -> pure $ Just res

extract :: HieAST a -> [(Identifier, S.Set ContextInfo, Span)]
extract ast = let span = nodeSpan ast
                  infos = M.toList $ M.map identInfo (Compat.getNodeIds ast)
              in  [ (ident, contexts, span) | (ident, contexts) <- infos ]

recFieldInfo, declInfo, valBindInfo, classTyDeclInfo,
  useInfo, patternBindInfo, tyDeclInfo, matchBindInfo
    :: [ContextInfo] -> Maybe ContextInfo
recFieldInfo    ctxs = listToMaybe [ctx       | ctx@RecField{}    <- ctxs]
declInfo        ctxs = listToMaybe [ctx       | ctx@Decl{}        <- ctxs]
valBindInfo     ctxs = listToMaybe [ctx       | ctx@ValBind{}     <- ctxs]
classTyDeclInfo ctxs = listToMaybe [ctx       | ctx@ClassTyDecl{} <- ctxs]
useInfo         ctxs = listToMaybe [Use       | Use               <- ctxs]
patternBindInfo ctxs = listToMaybe [ctx       | ctx@PatternBind{} <- ctxs]
tyDeclInfo      ctxs = listToMaybe [TyDecl    | TyDecl            <- ctxs]
matchBindInfo   ctxs = listToMaybe [MatchBind | MatchBind         <- ctxs]

construct :: NormalizedFilePath -> HieASTs a -> (Identifier, S.Set ContextInfo, Span) -> Maybe CallHierarchyItem
construct nfp hf (ident, contexts, ssp)
  | isInternalIdentifier ident = Nothing

  | Just (RecField RecFieldDecl _) <- recFieldInfo ctxList
    -- ignored type span
    = Just $ mkCallHierarchyItem' ident SkField ssp ssp

  | isJust (matchBindInfo ctxList) && isNothing (valBindInfo ctxList)
    = Just $ mkCallHierarchyItem' ident SkFunction ssp ssp

  | Just ctx <- valBindInfo ctxList
    = Just $ case ctx of
        ValBind _ _ span -> mkCallHierarchyItem' ident SkFunction (renderSpan span) ssp
        _                -> mkCallHierarchyItem' ident skUnknown ssp ssp

  | Just ctx <- declInfo ctxList
    = Just $ case ctx of
        Decl ClassDec span -> mkCallHierarchyItem' ident SkInterface     (renderSpan span) ssp
        Decl ConDec   span -> mkCallHierarchyItem' ident SkConstructor   (renderSpan span) ssp
        Decl DataDec  span -> mkCallHierarchyItem' ident SkStruct        (renderSpan span) ssp
        Decl FamDec   span -> mkCallHierarchyItem' ident SkFunction      (renderSpan span) ssp
        Decl InstDec  span -> mkCallHierarchyItem' ident SkInterface     (renderSpan span) ssp
        Decl SynDec   span -> mkCallHierarchyItem' ident SkTypeParameter (renderSpan span) ssp
        _ -> mkCallHierarchyItem' ident skUnknown ssp ssp

  | Just (ClassTyDecl span) <- classTyDeclInfo ctxList
    = Just $ mkCallHierarchyItem' ident SkMethod (renderSpan span) ssp

  | Just (PatternBind _ _ span) <- patternBindInfo ctxList
    = Just $ mkCallHierarchyItem' ident SkFunction (renderSpan span) ssp

  | Just Use <- useInfo ctxList
    = Just $ mkCallHierarchyItem' ident SkInterface ssp ssp

  | Just _ <- tyDeclInfo ctxList
    = renderTyDecl

  | otherwise = Nothing
  where
    renderSpan = \case Just span -> span
                       _         -> ssp

    skUnknown = SkUnknown 27

    mkCallHierarchyItem' = mkCallHierarchyItem nfp

    isInternalIdentifier = \case
      Left _     -> False
      Right name -> isInternalName name

    ctxList = S.toList contexts

    renderTyDecl = case ident of
      Left _ -> Nothing
      Right name -> case getNameBindingInClass name ssp (getAsts hf) of
        Nothing -> Nothing
        Just sp -> case resolveIntoCallHierarchy hf (realSrcSpanToRange sp ^. L.start) nfp of
          Just (Just items) -> listToMaybe items
          _                 -> Nothing

mkCallHierarchyItem :: NormalizedFilePath -> Identifier -> SymbolKind -> Span -> Span -> CallHierarchyItem
mkCallHierarchyItem nfp ident kind span selSpan =
  CallHierarchyItem
    (T.pack $ optimize $ identifierName ident)
    kind
    Nothing
    (Just $ T.pack $ identifierToDetail ident)
    (fromNormalizedUri $ normalizedFilePathToUri nfp)
    (realSrcSpanToRange span)
    (realSrcSpanToRange selSpan)
    (toJSON . show <$> mkSymbol ident)
  where
    identifierToDetail :: Identifier -> String
    identifierToDetail = \case
      Left modName -> moduleNameString modName
      Right name   -> (moduleNameString . moduleName . nameModule) name

    identifierName :: Identifier -> String
    identifierName = \case
      Left modName -> moduleNameString modName
      Right name   -> occNameString $ nameOccName name

    optimize :: String -> String
    optimize name -- optimize display for DuplicateRecordFields
        | "$sel:" == take 5 name = drop 5 name
        | otherwise = name

mkSymbol :: Identifier -> Maybe Symbol
mkSymbol = \case
  Left _     -> Nothing
  Right name -> Just $ Symbol (occName name) (nameModule name)

----------------------------------------------------------------------
-------------- Incoming calls and outgoing calls ---------------------
----------------------------------------------------------------------

deriving instance Ord SymbolKind
deriving instance Ord SymbolTag
deriving instance Ord CallHierarchyItem

-- | Render incoming calls request.
incomingCalls :: PluginMethodHandler IdeState CallHierarchyIncomingCalls
incomingCalls state pluginId param = do
  liftIO $ runAction "CallHierarchy.incomingCalls" state $
      queryCalls (param ^. L.item) Q.incomingCalls mkCallHierarchyIncomingCall
        mergeIncomingCalls >>=
    \case
      Just x  -> pure $ Right $ Just $ List x
      Nothing -> pure $ Left $ responseError "CallHierarchy: IncomingCalls internal error"
  where
    mkCallHierarchyIncomingCall :: Vertex -> Action (Maybe CallHierarchyIncomingCall)
    mkCallHierarchyIncomingCall = mkCallHierarchyCall CallHierarchyIncomingCall

    mergeIncomingCalls :: [CallHierarchyIncomingCall] -> [CallHierarchyIncomingCall]
    mergeIncomingCalls = map merge
                       . groupBy (\a b -> a ^. L.from == b ^. L.from)
                       . sortBy (\a b -> (a ^. L.from) `compare` (b ^. L.from))
      where
        merge calls = let ranges = concatMap ((\(List x) -> x) . (^. L.fromRanges)) calls
                      in  CallHierarchyIncomingCall (head calls ^. L.from) (List ranges)

-- Render outgoing calls request.
outgoingCalls :: PluginMethodHandler IdeState CallHierarchyOutgoingCalls
outgoingCalls state pluginId param = do
  liftIO $ runAction "CallHierarchy.outgoingCalls" state $
      queryCalls (param ^. L.item) Q.outgoingCalls mkCallHierarchyOutgoingCall
        mergeOutgoingCalls >>=
    \case
      Just x  -> pure $ Right $ Just $ List x
      Nothing -> pure $ Left $ responseError "CallHierarchy: OutgoingCalls internal error"
  where
    mkCallHierarchyOutgoingCall :: Vertex -> Action (Maybe CallHierarchyOutgoingCall)
    mkCallHierarchyOutgoingCall = mkCallHierarchyCall CallHierarchyOutgoingCall

    mergeOutgoingCalls :: [CallHierarchyOutgoingCall] -> [CallHierarchyOutgoingCall]
    mergeOutgoingCalls = map merge
                       . groupBy (\a b -> a ^. L.to == b ^. L.to)
                       . sortBy (\a b -> (a ^. L.to) `compare` (b ^. L.to))
      where
        merge calls = let ranges = concatMap ((\(List x) -> x) . (^. L.fromRanges)) calls
                      in  CallHierarchyOutgoingCall (head calls ^. L.to) (List ranges)

mkCallHierarchyCall :: (CallHierarchyItem -> List Range -> a) -> Vertex -> Action (Maybe a)
mkCallHierarchyCall mk v@Vertex{..} = do
  let pos = Position (sl - 1) (sc - 1)
      nfp = toNormalizedFilePath' hieSrc
      range = mkRange (casl - 1) (casc - 1) (cael - 1) (caec - 1)

  prepareCallHierarchyItem nfp pos >>=
    \case
      Just [item] -> pure $ Just $ mk item (List [range])
      _           -> do
        ShakeExtras{hiedb} <- getShakeExtras
        liftIO (Q.getSymbolPosition hiedb v) >>=
          \case
            (x:_) ->
              prepareCallHierarchyItem nfp (Position (psl x - 1) (psc x - 1)) >>=
                \case
                  Just [item] -> pure $ Just $ mk item (List [range])
                  _           -> pure Nothing
            _     -> pure Nothing

-- | Unified queries include incoming calls and outgoing calls.
queryCalls :: (Show a)
  => CallHierarchyItem
  -> (HieDb -> Symbol -> IO [Vertex])
  -> (Vertex -> Action (Maybe a))
  -> ([a] -> [a])
  -> Action (Maybe [a])
queryCalls item queryFunc makeFunc merge
  | Just nfp <- uriToNormalizedFilePath $ toNormalizedUri uri = do
    refreshHieDb

    ShakeExtras{hiedb} <- getShakeExtras
    maySymbol <- getSymbol nfp
    case maySymbol of
      Nothing -> error "CallHierarchy.Impossible"
      Just symbol -> do
        vs <- liftIO $ queryFunc hiedb symbol
        items <- Just . catMaybes <$> mapM makeFunc vs
        pure $ merge <$> items
  | otherwise = pure Nothing
  where
    uri = item ^. L.uri
    xdata = item ^. L.xdata
    pos = item ^. (L.selectionRange . L.start)

    getSymbol nfp =
      case item ^. L.xdata of
        Just xdata -> case fromJSON xdata of
          A.Success (symbolStr :: String) ->
            case readMaybe symbolStr of
              Just symbol -> pure $ Just symbol
              Nothing     -> getSymbolFromAst nfp pos
          A.Error _ -> getSymbolFromAst nfp pos
        Nothing -> getSymbolFromAst nfp pos

    getSymbolFromAst :: NormalizedFilePath -> Position -> Action (Maybe Symbol)
    getSymbolFromAst nfp pos =
      use GetHieAst nfp >>=
        \case
          Nothing -> pure Nothing
          Just (HAR _ hf _ _ _) -> do
            case listToMaybe $ pointCommand hf pos extract of
              Just infos -> case mkSymbol . fst3 <$> listToMaybe infos of
                Nothing  -> pure Nothing
                Just res -> pure res
              Nothing -> pure Nothing

-- Write modified foi files before queries.
refreshHieDb :: Action ()
refreshHieDb = do
    fs <- HM.keys . HM.filter (/= OnDisk) <$> getFilesOfInterestUntracked
    forM_ fs (\f -> do
        tmr <- use_ TypeCheck f
        hsc <- hscEnv <$> use_ GhcSession f
        (_, masts) <- liftIO $ generateHieAsts hsc tmr
        se <- getShakeExtras
        case masts of
            Nothing -> pure ()
            Just asts -> do
                source <- getSourceFileSource f
                let exports = tcg_exports $ tmrTypechecked tmr
                    msum = tmrModSummary tmr
                liftIO $ writeAndIndexHieFile hsc se msum f exports asts source
                pure ()
        )
    ShakeExtras{hiedbWriter} <- getShakeExtras
    liftIO $ atomically $ check $ indexPending hiedbWriter
    where
      check p = do
        v <- readTVar p
        if HM.null v then pure () else retry

-- Copy unexport function form `ghcide/src/Development/IDE/Core/Rules.hs`
getSourceFileSource :: NormalizedFilePath -> Action BS.ByteString
getSourceFileSource nfp = do
    (_, msource) <- getFileContents nfp
    case msource of
        Nothing     -> liftIO $ BS.readFile (fromNormalizedFilePath nfp)
        Just source -> pure $ T.encodeUtf8 source
