{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs     #-}

module Ide.Plugin.SignatureHelp (descriptor) where

import           Control.Arrow                        ((>>>))
import           Control.Monad.Trans.Except           (ExceptT (ExceptT))
import           Data.Bifunctor                       (bimap)
import           Data.Function                        ((&))
import           Data.IntMap                          (IntMap)
import qualified Data.IntMap                          as IntMap
import qualified Data.Map.Strict                      as M
import qualified Data.Set                             as S
import           Data.Text                            (Text)
import qualified Data.Text                            as T
import           Development.IDE                      (DocAndTyThingMap (DKMap),
                                                       GetDocMap (GetDocMap),
                                                       GetHieAst (GetHieAst),
                                                       HieAstResult (HAR, hieAst, hieKind),
                                                       HieKind (..),
                                                       IdeState (shakeExtras),
                                                       Pretty (pretty),
                                                       Recorder, WithPriority,
                                                       printOutputableOneLine,
                                                       useWithStaleFast)
import           Development.IDE.Core.PluginUtils     (runIdeActionE,
                                                       useWithStaleFastE)
import           Development.IDE.Core.PositionMapping (fromCurrentPosition)
import           Development.IDE.GHC.Compat           (FastStringCompat, Name,
                                                       RealSrcSpan,
                                                       getSourceNodeIds,
                                                       isAnnotationInNodeInfo,
                                                       mkRealSrcLoc,
                                                       mkRealSrcSpan, ppr,
                                                       sourceNodeInfo)
import           Development.IDE.GHC.Compat.Util      (LexicalFastString (LexicalFastString))
import           Development.IDE.Spans.Common         (ArgDocMap, DocMap,
                                                       SpanDoc,
                                                       spanDocToMarkdown)
import           GHC.Core.Map.Type                    (deBruijnize)
import           GHC.Core.Type                        (FunTyFlag (FTF_T_T),
                                                       Type, dropForAlls,
                                                       splitFunTy_maybe)
import           GHC.Data.Maybe                       (rightToMaybe)
import           GHC.Iface.Ext.Types                  (ContextInfo (Use),
                                                       HieAST (nodeChildren, nodeSpan),
                                                       HieASTs (getAsts),
                                                       IdentifierDetails (identInfo, identType),
                                                       nodeType)
import           GHC.Iface.Ext.Utils                  (smallestContainingSatisfying)
import           GHC.Types.Name.Env                   (lookupNameEnv)
import           GHC.Types.SrcLoc                     (isRealSubspanOf)
import           Ide.Plugin.Error                     (getNormalizedFilePathE)
import           Ide.Types                            (PluginDescriptor (pluginHandlers),
                                                       PluginId,
                                                       PluginMethodHandler,
                                                       defaultPluginDescriptor,
                                                       mkPluginHandler)
import           Language.LSP.Protocol.Message        (Method (Method_TextDocumentSignatureHelp),
                                                       SMethod (SMethod_TextDocumentSignatureHelp))
import           Language.LSP.Protocol.Types          (MarkupContent (MarkupContent),
                                                       MarkupKind (MarkupKind_Markdown),
                                                       Null (Null),
                                                       ParameterInformation (ParameterInformation),
                                                       Position (Position),
                                                       SignatureHelp (SignatureHelp),
                                                       SignatureHelpParams (SignatureHelpParams),
                                                       SignatureInformation (SignatureInformation),
                                                       TextDocumentIdentifier (TextDocumentIdentifier),
                                                       UInt,
                                                       type (|?) (InL, InR))

data Log = LogDummy

instance Pretty Log where
    pretty = \case
        LogDummy -> "TODO(@linj) remove this dummy log"

descriptor :: Recorder (WithPriority Log) -> PluginId -> PluginDescriptor IdeState
descriptor _recorder pluginId =
    (defaultPluginDescriptor pluginId "Provides signature help of something callable")
        { Ide.Types.pluginHandlers = mkPluginHandler SMethod_TextDocumentSignatureHelp signatureHelpProvider
        }

-- TODO(@linj) get doc
signatureHelpProvider :: PluginMethodHandler IdeState Method_TextDocumentSignatureHelp
signatureHelpProvider ideState _pluginId (SignatureHelpParams (TextDocumentIdentifier uri) position _mProgreeToken _mContext) = do
    nfp <- getNormalizedFilePathE uri
    results <- runIdeActionE "signatureHelp.ast" (shakeExtras ideState) $ do
        -- TODO(@linj) why HAR {hieAst} may have more than one AST?
        (HAR {hieAst, hieKind}, positionMapping) <- useWithStaleFastE GetHieAst nfp
        case fromCurrentPosition positionMapping position of
            Nothing -> pure []
            Just oldPosition -> do
                pure $
                    extractInfoFromSmallestContainingFunctionApplicationAst
                        oldPosition
                        hieAst
                        ( \span hieAst -> do
                              let functionNode = getLeftMostNode hieAst
                              (functionName, functionTypes) <- getNodeNameAndTypes hieKind functionNode
                              argumentNumber <- getArgumentNumber span hieAst
                              Just (functionName, functionTypes, argumentNumber)
                        )
    (docMap, argDocMap) <- runIdeActionE "signatureHelp.docMap" (shakeExtras ideState) $ do
        mResult <- ExceptT $ Right <$> useWithStaleFast GetDocMap nfp
        case mResult of
            Just (DKMap docMap _tyThingMap argDocMap, _positionMapping) -> pure (docMap, argDocMap)
            Nothing -> pure (mempty, mempty)
    case results of
        [(_functionName, [], _argumentNumber)] -> pure $ InR Null
        [(functionName, functionTypes, argumentNumber)] ->
            pure $ InL $ mkSignatureHelp docMap argDocMap (fromIntegral argumentNumber - 1) functionName functionTypes
        -- TODO(@linj) what does non-singleton list mean?
        _ -> pure $ InR Null

mkSignatureHelp :: DocMap -> ArgDocMap -> UInt -> Name -> [Type] -> SignatureHelp
mkSignatureHelp docMap argDocMap argumentNumber functionName functionTypes =
    SignatureHelp
        (mkSignatureInformation docMap argDocMap argumentNumber functionName <$> functionTypes)
        (Just 0)
        (Just $ InL argumentNumber)

mkSignatureInformation :: DocMap -> ArgDocMap -> UInt -> Name -> Type -> SignatureInformation
mkSignatureInformation docMap argDocMap argumentNumber functionName functionType =
    let functionNameLabelPrefix = printOutputableOneLine (ppr functionName) <> " :: "
        mFunctionDoc = case lookupNameEnv docMap functionName of
            Nothing      -> Nothing
            Just spanDoc -> Just $ InR $ mkMarkdownDoc spanDoc
        thisArgDocMap = case lookupNameEnv argDocMap functionName of
            Nothing             -> mempty
            Just thisArgDocMap' -> thisArgDocMap'
     in SignatureInformation
            (functionNameLabelPrefix <> printOutputableOneLine functionType)
            mFunctionDoc
            (Just $ mkArguments thisArgDocMap (fromIntegral $ T.length functionNameLabelPrefix) functionType)
            (Just $ InL argumentNumber)

mkArguments :: IntMap SpanDoc -> UInt -> Type -> [ParameterInformation]
mkArguments thisArgDocMap offset functionType =
    [ ParameterInformation (InR range) mArgDoc
    | (argIndex, range) <- zip [0..] (bimap (+offset) (+offset) <$> findArgumentRanges functionType)
    , let mArgDoc = case IntMap.lookup argIndex thisArgDocMap of
              Nothing      -> Nothing
              Just spanDoc -> Just $ InR $ mkMarkdownDoc spanDoc
    ]

mkMarkdownDoc :: SpanDoc -> MarkupContent
mkMarkdownDoc = spanDocToMarkdown >>> T.unlines >>> MarkupContent MarkupKind_Markdown

findArgumentRanges :: Type -> [(UInt, UInt)]
findArgumentRanges functionType =
    let functionTypeString = printOutputableOneLine functionType
        functionTypeStringLength = fromIntegral $ T.length functionTypeString
        splitFunctionTypes = filter notTypeConstraint $ splitFunTysIgnoringForAll functionType
        splitFunctionTypeStrings = printOutputableOneLine . fst <$> splitFunctionTypes
        -- reverse to avoid matching "a" of "forall a" in "forall a. a -> a"
        reversedRanges =
            drop 1 $ -- do not need the range of the result (last) type
                findArgumentStringRanges
                    0
                    (T.reverse functionTypeString)
                    (T.reverse <$> reverse splitFunctionTypeStrings)
     in reverse $ modifyRange functionTypeStringLength <$> reversedRanges
    where
        modifyRange functionTypeStringLength (start, end) =
            (functionTypeStringLength - end, functionTypeStringLength - start)

{-
The implemented method uses both structured type and unstructured type string.
It provides good enough results and is easier to implement than alternative
method 1 or 2.

Alternative method 1: use only structured type
This method is hard to implement because we need to duplicate some logic of 'ppr' for 'Type'.
Some tricky cases are as follows:
- 'Eq a => Num b -> c' is shown as '(Eq a, Numb) => c'
- 'forall' can appear anywhere in a type when RankNTypes is enabled
  f :: forall a. Maybe a -> forall b. (a, b) -> b
- '=>' can appear anywhere in a type
  g :: forall a b. Eq a => a -> Num b => b -> b
- ppr the first argument type of '(a -> b) -> a -> b' is 'a -> b' (no parentheses)
- 'forall' is not always shown

Alternative method 2: use only unstructured type string
This method is hard to implement because we need to parse the type string.
Some tricky cases are as follows:
- h :: forall a (m :: Type -> Type). Monad m => a -> m a
-}
findArgumentStringRanges :: UInt -> Text -> [Text] -> [(UInt, UInt)]
findArgumentStringRanges _totalPrefixLength _functionTypeString [] = []
findArgumentStringRanges totalPrefixLength functionTypeString (argumentTypeString:restArgumentTypeStrings) =
    let (prefix, match) = T.breakOn argumentTypeString functionTypeString
        prefixLength = fromIntegral $ T.length prefix
        argumentTypeStringLength = fromIntegral $ T.length argumentTypeString
        start = totalPrefixLength + prefixLength
     in (start, start + argumentTypeStringLength)
            : findArgumentStringRanges
              (totalPrefixLength + prefixLength + argumentTypeStringLength)
              (T.drop (fromIntegral argumentTypeStringLength) match)
              restArgumentTypeStrings

-- similar to 'splitFunTys' but
--   1) the result (last) type is included and
--   2) toplevel foralls are ignored
splitFunTysIgnoringForAll :: Type -> [(Type, Maybe FunTyFlag)]
splitFunTysIgnoringForAll ty = case ty & dropForAlls & splitFunTy_maybe of
    Just (funTyFlag, _mult, argumentType, resultType) ->
        (argumentType, Just funTyFlag) : splitFunTysIgnoringForAll resultType
    Nothing -> [(ty, Nothing)]

notTypeConstraint :: (Type, Maybe FunTyFlag) -> Bool
notTypeConstraint (_type, Just FTF_T_T) = True
notTypeConstraint (_type, Nothing)      = True
notTypeConstraint _                     = False

extractInfoFromSmallestContainingFunctionApplicationAst ::
    Position -> HieASTs a -> (RealSrcSpan -> HieAST a -> Maybe b) -> [b]
extractInfoFromSmallestContainingFunctionApplicationAst position hieAsts extractInfo =
    M.elems $ flip M.mapMaybeWithKey (getAsts hieAsts) $ \hiePath hieAst ->
        smallestContainingSatisfying (positionToSpan hiePath position) (nodeHasAnnotation ("HsApp", "HsExpr")) hieAst
            >>= extractInfo (positionToSpan hiePath position)
    where
        positionToSpan hiePath position =
            let loc = mkLoc hiePath position in mkRealSrcSpan loc loc
        mkLoc (LexicalFastString hiePath) (Position line character) =
            mkRealSrcLoc hiePath (fromIntegral line + 1) (fromIntegral character + 1)

type Annotation = (FastStringCompat, FastStringCompat)

nodeHasAnnotation :: Annotation -> HieAST a -> Bool
nodeHasAnnotation annotation hieAst = case sourceNodeInfo hieAst of
    Nothing       -> False
    Just nodeInfo -> isAnnotationInNodeInfo annotation nodeInfo

-- TODO(@linj): the left most node may not be the function node. example: (if True then f else g) x
getLeftMostNode :: HieAST a -> HieAST a
getLeftMostNode thisNode =
    case nodeChildren thisNode of
        []           -> thisNode
        leftChild: _ -> getLeftMostNode leftChild

getNodeNameAndTypes :: HieKind a -> HieAST a -> Maybe (Name, [Type])
getNodeNameAndTypes hieKind hieAst =
    if nodeHasAnnotation ("HsVar", "HsExpr") hieAst
        then case hieAst & getSourceNodeIds & M.filter isUse & M.assocs of
            [(identifier, identifierDetails)] ->
                case extractName identifier of
                    Nothing -> Nothing
                    Just name ->
                        let mTypeOfName = identType identifierDetails
                            typesOfNode = case sourceNodeInfo hieAst of
                                Nothing       -> []
                                Just nodeInfo -> nodeType nodeInfo
                            allTypes = case mTypeOfName of
                                Nothing -> typesOfNode
                                Just typeOfName -> typeOfName : filter (isDifferentType typeOfName) typesOfNode
                         in Just (name, filterCoreTypes allTypes)
            [] -> Nothing
            _ -> Nothing -- seems impossible
        else Nothing -- TODO(@linj) must function node be HsVar?
    where
        extractName = rightToMaybe

        isDifferentType type1 type2 = case hieKind of
            HieFresh       -> deBruijnize type1 /= deBruijnize type2
            HieFromDisk {} -> type1 /= type2

        filterCoreTypes types = case hieKind of
            HieFresh       -> types
            -- ignore this case since this only happens before we finish startup
            HieFromDisk {} -> []

isUse :: IdentifierDetails a -> Bool
isUse = identInfo >>> S.member Use

-- Just 1 means the first argument
getArgumentNumber :: RealSrcSpan -> HieAST a -> Maybe Integer
getArgumentNumber span hieAst =
    if nodeHasAnnotation ("HsApp", "HsExpr") hieAst
        then
            case nodeChildren hieAst of
                [leftChild, _] ->
                    if span `isRealSubspanOf` nodeSpan leftChild
                        then Nothing
                        else getArgumentNumber span leftChild >>= \argumentNumber -> Just (argumentNumber + 1)
                _ -> Nothing -- impossible
        else
            case nodeChildren hieAst of
                []      -> Just 0 -- the function is found
                [child] -> getArgumentNumber span child -- ignore irrelevant nodes
                _       -> Nothing -- TODO(@linj) handle more cases such as `if`
