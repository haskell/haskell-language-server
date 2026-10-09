{-# LANGUAGE CPP               #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

module Ide.Plugin.CallHierarchy.Internal (
  prepareCallHierarchy
, incomingCalls
, outgoingCalls
) where

import           Control.Lens                   (Lens', (^.))
import           Control.Monad.IO.Class
import           Data.Aeson                     as A
import           Data.Functor                   ((<&>))
import           Data.List                      (groupBy, sortBy)
import qualified Data.Map                       as M
import           Data.Maybe
import           Data.Ord                       (comparing)
import qualified Data.Set                       as S
import qualified Data.Text                      as T
import           Data.Tuple.Extra
import           Development.IDE
import           Development.IDE.Core.Shake
import           Development.IDE.GHC.Compat     as Compat
import           Development.IDE.Spans.AtPoint
import           GHC.Iface.Ext.Types            (ContextInfo (..),
                                                 DeclType (..), HieAST (..),
                                                 HieASTs (..), Identifier,
                                                 IdentifierDetails (..),
                                                 RecFieldContext (..), Span)
import           GHC.Iface.Ext.Utils            (getNameBinding)
import           HieDb                          (Symbol (Symbol))
import qualified Ide.Plugin.CallHierarchy.Query as Q
import           Ide.Plugin.CallHierarchy.Types
import           Ide.Plugin.Error
import           Ide.Types
import qualified Language.LSP.Protocol.Lens     as L
import           Language.LSP.Protocol.Message
import           Language.LSP.Protocol.Types
import           Prelude                        hiding (mod, span)
import           Text.Read                      (readMaybe)

-- | Render prepare call hierarchy request.
prepareCallHierarchy :: PluginMethodHandler IdeState Method_TextDocumentPrepareCallHierarchy
prepareCallHierarchy state _ param = do
    nfp <- getNormalizedFilePathE (param ^. (L.textDocument . L.uri))
    items <- liftIO
        $ runAction "CallHierarchy.prepareHierarchy" state
        $ prepareCallHierarchyItem nfp (param ^. L.position)
    pure $ InL items

prepareCallHierarchyItem :: NormalizedFilePath -> Position -> Action [CallHierarchyItem]
prepareCallHierarchyItem nfp pos = use GetHieAst nfp <&> \case
    Nothing               -> mempty
    Just (HAR _ hf _ _ _) -> prepareByAst hf pos nfp

prepareByAst :: HieASTs a -> Position -> NormalizedFilePath -> [CallHierarchyItem]
prepareByAst hf pos nfp =
    case listToMaybe $ pointCommand hf pos extract of
        Nothing    -> mempty
        Just infos -> mapMaybe (construct nfp hf) infos

extract :: HieAST a -> [(Identifier, [ContextInfo], Span)]
extract ast = let span = nodeSpan ast
                  infos = M.toList $ M.map (S.toList . identInfo) (Compat.getNodeIds ast)
              in  [(ident, contexts, span) | (ident, contexts) <- infos]

recFieldInfo, declInfo, valBindInfo, classTyDeclInfo,
    useInfo, patternBindInfo, tyDeclInfo, matchBindInfo :: [ContextInfo] -> Maybe ContextInfo
recFieldInfo    ctxs = listToMaybe [ctx       | ctx@RecField{}    <- ctxs]
declInfo        ctxs = listToMaybe [ctx       | ctx@Decl{}        <- ctxs]
valBindInfo     ctxs = listToMaybe [ctx       | ctx@ValBind{}     <- ctxs]
classTyDeclInfo ctxs = listToMaybe [ctx       | ctx@ClassTyDecl{} <- ctxs]
useInfo         ctxs = listToMaybe [Use       | Use               <- ctxs]
patternBindInfo ctxs = listToMaybe [ctx       | ctx@PatternBind{} <- ctxs]
tyDeclInfo      ctxs = listToMaybe [TyDecl    | TyDecl            <- ctxs]
matchBindInfo   ctxs = listToMaybe [MatchBind | MatchBind         <- ctxs]

construct :: NormalizedFilePath -> HieASTs a -> (Identifier, [ContextInfo], Span) -> Maybe CallHierarchyItem
construct nfp hf (ident, contexts, ssp)
    | isInternalIdentifier ident = Nothing

    | Just (RecField RecFieldDecl _) <- recFieldInfo contexts
        -- ignored type span
        = Just $ mkCallHierarchyItem' ident SymbolKind_Field ssp ssp

    | isJust (matchBindInfo contexts) && isNothing (valBindInfo contexts)
        = Just $ mkCallHierarchyItem' ident SymbolKind_Function ssp ssp

    | Just ctx <- valBindInfo contexts
        = Just $ case ctx of
            ValBind _ _ span -> mkCallHierarchyItem' ident SymbolKind_Function (renderSpan span) ssp
            _                -> mkCallHierarchyItem' ident skUnknown ssp ssp

    | Just ctx <- declInfo contexts
        = Just $ case ctx of
            Decl ClassDec span -> mkCallHierarchyItem' ident SymbolKind_Interface     (renderSpan span) ssp
            Decl ConDec   span -> mkCallHierarchyItem' ident SymbolKind_Constructor   (renderSpan span) ssp
            Decl DataDec  span -> mkCallHierarchyItem' ident SymbolKind_Struct        (renderSpan span) ssp
            Decl FamDec   span -> mkCallHierarchyItem' ident SymbolKind_Function      (renderSpan span) ssp
            Decl InstDec  span -> mkCallHierarchyItem' ident SymbolKind_Interface     (renderSpan span) ssp
            Decl SynDec   span -> mkCallHierarchyItem' ident SymbolKind_TypeParameter (renderSpan span) ssp
            _ -> mkCallHierarchyItem' ident skUnknown ssp ssp

    | Just (ClassTyDecl span) <- classTyDeclInfo contexts
        = Just $ mkCallHierarchyItem' ident SymbolKind_Method (renderSpan span) ssp

    | Just (PatternBind _ _ span) <- patternBindInfo contexts
        = Just $ mkCallHierarchyItem' ident SymbolKind_Function (renderSpan span) ssp

    | Just _ <- useInfo contexts = Just $ mkCallHierarchyItem' ident SymbolKind_Interface ssp ssp

    | Just _ <- tyDeclInfo contexts = renderTyDecl

    | otherwise = Nothing
    where
        renderSpan (Just span) = span
        renderSpan _           = ssp

        -- https://github.com/haskell/lsp/blob/e11b7c09658610f6d815d04db08a64e7cf6b4467/lsp-types/src/Language/LSP/Types/DocumentSymbol.hs#L97
        -- There is no longer an unknown symbol, thus using SymbolKind_Function
        -- as this is the call-hierarchy plugin
        skUnknown = SymbolKind_Function

        mkCallHierarchyItem' = mkCallHierarchyItem nfp

        isInternalIdentifier = \case
            Left _     -> False
            Right name -> isInternalName name

        renderTyDecl = case ident of
            Left _ -> Nothing
            Right name -> case getNameBinding name (getAsts hf) of
                Nothing -> Nothing
                Just sp -> listToMaybe $ prepareByAst hf (realSrcSpanToRange sp ^. L.start) nfp

mkCallHierarchyItem :: NormalizedFilePath -> Identifier -> SymbolKind -> Span -> Span -> CallHierarchyItem
mkCallHierarchyItem nfp ident kind span selSpan =
    CallHierarchyItem
        (T.pack $ optimizeDisplay $ identifierName ident)
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

        optimizeDisplay :: String -> String
        optimizeDisplay name -- Optimize display for DuplicateRecordFields
            | "$sel:" == take 5 name = drop 5 name
            | otherwise = name

mkSymbol :: Identifier -> Maybe Symbol
mkSymbol = \case
    Left _     -> Nothing
    Right name -> Just $ Symbol (occName name) (nameModule name)

----------------------------------------------------------------------
-------------- Incoming calls and outgoing calls ---------------------
----------------------------------------------------------------------

-- | Render incoming calls request.
incomingCalls :: PluginMethodHandler IdeState Method_CallHierarchyIncomingCalls
incomingCalls state _pluginId param = do
    calls <- liftIO
        $ runAction "CallHierarchy.incomingCalls" state
        $ queryCalls
            (param ^. L.item)
            Q.incomingCalls
            mkCallHierarchyIncomingCall
            (mergeCalls CallHierarchyIncomingCall L.from)
    pure $ InL calls
    where
        mkCallHierarchyIncomingCall :: Vertex -> Action (Maybe CallHierarchyIncomingCall)
        mkCallHierarchyIncomingCall = mkCallHierarchyCall CallHierarchyIncomingCall

-- | Render outgoing calls request.
outgoingCalls :: PluginMethodHandler IdeState Method_CallHierarchyOutgoingCalls
outgoingCalls state _pluginId param = do
    calls <- liftIO
        $ runAction "CallHierarchy.outgoingCalls" state
        $ queryCalls
            (param ^. L.item)
            Q.outgoingCalls
            mkCallHierarchyOutgoingCall
            (mergeCalls CallHierarchyOutgoingCall L.to)
    pure $ InL calls
    where
        mkCallHierarchyOutgoingCall :: Vertex -> Action (Maybe CallHierarchyOutgoingCall)
        mkCallHierarchyOutgoingCall = mkCallHierarchyCall CallHierarchyOutgoingCall

-- | Merge calls from the same place
mergeCalls ::
    L.HasFromRanges s [Range]
    => (CallHierarchyItem -> [Range] -> s)
    -> Lens' s CallHierarchyItem
    -> [s]
    -> [s]
mergeCalls constructor target =
    concatMap merge
        . groupBy (\a b -> a ^. target == b ^. target)
        . sortBy (comparing (^. target))
    where
        merge [] = []
        merge calls@(call:_) =
            let ranges = concatMap (^. L.fromRanges) calls
            in  [constructor (call ^. target) ranges]

mkCallHierarchyCall :: (CallHierarchyItem ->  [Range] -> a) -> Vertex -> Action (Maybe a)
mkCallHierarchyCall mk v@Vertex{..} = do
    let pos = Position (fromIntegral $ sl - 1) (fromIntegral $ sc - 1)
        nfp = toNormalizedFilePath' hieSrc
        range = mkRange
                    (fromIntegral $ casl - 1)
                    (fromIntegral $ casc - 1)
                    (fromIntegral $ cael - 1)
                    (fromIntegral $ caec - 1)

    prepareCallHierarchyItem nfp pos >>=
        \case
            [item] -> pure $ Just $ mk item [range]
            _      -> do
                ShakeExtras{withHieDb} <- getShakeExtras
                sps <- liftIO (withHieDb (`Q.getSymbolPosition` v))
                case sps of
                    (x:_) -> do
                        items <- prepareCallHierarchyItem
                                    nfp
                                    (Position (fromIntegral $ psl x - 1) (fromIntegral $ psc x - 1))
                        case items of
                            [item] -> pure $ Just $ mk item [range]
                            _      -> pure Nothing
                    []     -> pure Nothing

-- | Unified queries include incoming calls and outgoing calls.
queryCalls ::
    CallHierarchyItem
    -> (HieDb -> Symbol -> IO [Vertex])
    -> (Vertex -> Action (Maybe a))
    -> ([a] -> [a])
    -> Action [a]
queryCalls item queryFunc makeFunc merge
    | Just nfp <- uriToNormalizedFilePath $ toNormalizedUri uri = do
        ShakeExtras{withHieDb} <- getShakeExtras
        maySymbol <- getSymbol nfp
        case maySymbol of
            Nothing -> pure mempty
            Just symbol -> do
                vs <- liftIO $ withHieDb (`queryFunc` symbol)
                items <- catMaybes <$> mapM makeFunc vs
                pure $ merge items
    | otherwise = pure mempty
    where
        uri = item ^. L.uri
        pos = item ^. (L.selectionRange . L.start)

        getSymbol nfp = case item ^. L.data_ of
            Just xdata -> case fromJSON xdata of
                A.Success (symbolStr :: String) -> maybe (getSymbolFromAst nfp pos) (pure . pure) $ readMaybe symbolStr
                A.Error _ -> getSymbolFromAst nfp pos
            Nothing -> getSymbolFromAst nfp pos -- Fallback if xdata lost, some editor(VSCode) will drop it

        getSymbolFromAst :: NormalizedFilePath -> Position -> Action (Maybe Symbol)
        getSymbolFromAst nfp pos_ = use GetHieAst nfp <&> \case
            Nothing -> Nothing
            Just (HAR _ hf _ _ _) -> do
                case listToMaybe $ pointCommand hf pos_ extract of
                    Just infos -> mkSymbol . fst3 =<< listToMaybe infos
                    Nothing    -> Nothing
