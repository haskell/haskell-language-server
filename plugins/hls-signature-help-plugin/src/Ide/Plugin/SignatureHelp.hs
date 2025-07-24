{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs     #-}

module Ide.Plugin.SignatureHelp (descriptor) where

import           Control.Arrow                        ((>>>))
import           Data.Bifunctor                       (bimap)
import qualified Data.Map.Strict                      as M
import           Data.Maybe                           (mapMaybe)
import qualified Data.Set                             as S
import           Data.Text                            (Text)
import qualified Data.Text                            as T
import           Development.IDE                      (GetHieAst (GetHieAst),
                                                       HieAstResult (HAR, hieAst, hieKind),
                                                       HieKind (..),
                                                       IdeState (shakeExtras),
                                                       Pretty (pretty),
                                                       Recorder, WithPriority,
                                                       printOutputable)
import           Development.IDE.Core.PluginUtils     (runIdeActionE,
                                                       useWithStaleFastE)
import           Development.IDE.Core.PositionMapping (fromCurrentPosition)
import           Development.IDE.GHC.Compat           (ContextInfo (Use),
                                                       FastStringCompat, HieAST,
                                                       HieASTs,
                                                       IdentifierDetails, Name,
                                                       RealSrcSpan, SDoc,
                                                       getAsts,
                                                       getSourceNodeIds,
                                                       hieTypeToIface,
                                                       hie_types, identInfo,
                                                       identType,
                                                       isAnnotationInNodeInfo,
                                                       mkRealSrcLoc,
                                                       mkRealSrcSpan,
                                                       nodeChildren, nodeSpan,
                                                       ppr, recoverFullType,
                                                       smallestContainingSatisfying,
                                                       sourceNodeInfo)
import           Development.IDE.GHC.Compat.Util      (LexicalFastString (LexicalFastString))
import           GHC.Data.Maybe                       (rightToMaybe)
import           GHC.Types.SrcLoc                     (isRealSubspanOf)
import           Ide.Plugin.Error                     (getNormalizedFilePathE)
import           Ide.Types                            (PluginDescriptor (pluginHandlers),
                                                       PluginId,
                                                       PluginMethodHandler,
                                                       defaultPluginDescriptor,
                                                       mkPluginHandler)
import           Language.LSP.Protocol.Message        (Method (Method_TextDocumentSignatureHelp),
                                                       SMethod (SMethod_TextDocumentSignatureHelp))
import           Language.LSP.Protocol.Types          (Null (Null),
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
    mResult <- runIdeActionE "signatureHelp" (shakeExtras ideState) $ do
        -- TODO(@linj) why HAR {hieAst} may have more than one AST?
        (HAR {hieAst, hieKind}, positionMapping) <- useWithStaleFastE GetHieAst nfp
        case fromCurrentPosition positionMapping position of
            Nothing -> pure Nothing
            Just oldPosition -> do
                let functionName =
                        extractInfoFromSmallestContainingFunctionApplicationAst
                            oldPosition
                            hieAst
                            (\span -> getLeftMostNode >>> getNodeName span)
                    functionType =
                        extractInfoFromSmallestContainingFunctionApplicationAst
                            oldPosition
                            hieAst
                            (\span -> getLeftMostNode >>> getNodeType hieKind span)
                    argumentNumber =
                        extractInfoFromSmallestContainingFunctionApplicationAst
                            oldPosition
                            hieAst
                            getArgumentNumber
                pure $ Just (functionName, functionType, argumentNumber)
    case mResult of
        -- TODO(@linj) what do non-singleton lists mean?
        Just (functionName : _, functionType : _, argumentNumber : _) -> do
            pure $ InL $ mkSignatureHelp functionName functionType (fromIntegral argumentNumber - 1)
        _ -> pure $ InR Null

mkSignatureHelp :: Name -> Text -> UInt -> SignatureHelp
mkSignatureHelp functionName functionType argumentNumber =
    let functionNameLabelPrefix = printOutputable (ppr functionName) <> " :: "
     in SignatureHelp
            [ SignatureInformation
                  (functionNameLabelPrefix <> functionType)
                  Nothing
                  (Just $ mkArguments (fromIntegral $ T.length functionNameLabelPrefix) functionType)
                  (Just $ InL argumentNumber)
            ]
            (Just 0)
            (Just $ InL argumentNumber)

-- TODO(@linj) can type string be a multi-line string?
mkArguments :: UInt -> Text -> [ParameterInformation]
mkArguments offset functionType =
    let separator = " -> "
        separatorLength = fromIntegral $ T.length separator
        splits = T.breakOnAll separator functionType
        prefixes = fst <$> splits
        prefixLengths = fmap (T.length >>> fromIntegral) prefixes
        ranges =
            [ ( if previousPrefixLength == 0 then 0 else previousPrefixLength + separatorLength,
                currentPrefixLength
              )
            | (previousPrefixLength, currentPrefixLength) <- zip (0: prefixLengths) prefixLengths
            ]
     in [ ParameterInformation (InR range) Nothing
        | range <- bimap (+offset) (+offset) <$> ranges
        ]

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

getNodeName :: RealSrcSpan -> HieAST a -> Maybe Name
getNodeName _span hieAst =
    if nodeHasAnnotation ("HsVar", "HsExpr") hieAst
        then
            case mapMaybe extractName $ M.keys $ M.filter isUse $ getSourceNodeIds hieAst of
                [name] -> Just name -- TODO(@linj) will there be more than one name?
                _      -> Nothing
        else Nothing -- TODO(@linj) must function node be HsVar?
    where
        extractName = rightToMaybe

-- TODO(@linj) share code with getNodeName
getNodeType :: HieKind a -> RealSrcSpan -> HieAST a -> Maybe Text
getNodeType (hieKind :: HieKind a) _span hieAst =
    if nodeHasAnnotation ("HsVar", "HsExpr") hieAst
       then
           case M.elems $ M.filter isUse $ getSourceNodeIds hieAst of
               [identifierDetails] -> identType identifierDetails >>= (prettyType >>> Just)
               _ -> Nothing -- TODO(@linj) will there be more than one identifierDetails?
       else Nothing
    where
        -- modified from Development.IDE.Spans.AtPoint.atPoint
        prettyType :: a -> Text
        prettyType = expandType >>> printOutputable

        expandType :: a -> SDoc
        expandType t = case hieKind of
            HieFresh -> ppr t
            HieFromDisk hieFile -> ppr $ hieTypeToIface $ recoverFullType t (hie_types hieFile)

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
