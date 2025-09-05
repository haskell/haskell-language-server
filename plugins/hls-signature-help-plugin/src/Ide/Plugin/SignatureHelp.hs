{-# LANGUAGE DataKinds                #-}
{-# LANGUAGE DisambiguateRecordFields #-}
{-# LANGUAGE GADTs                    #-}

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
                                                       SpanDoc (..),
                                                       SpanDocUris (SpanDocUris),
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
                                                       SignatureHelp (..),
                                                       SignatureHelpContext (..),
                                                       SignatureHelpParams (SignatureHelpParams),
                                                       SignatureInformation (..),
                                                       TextDocumentIdentifier (TextDocumentIdentifier),
                                                       UInt,
                                                       type (|?) (InL, InR))

data Log

instance Pretty Log where
  pretty = \case {}

descriptor :: Recorder (WithPriority Log) -> PluginId -> PluginDescriptor IdeState
descriptor _recorder pluginId =
  (defaultPluginDescriptor pluginId "Provides signature help of something callable")
    { Ide.Types.pluginHandlers = mkPluginHandler SMethod_TextDocumentSignatureHelp signatureHelpProvider
    }

{- Note [Stale Results in Signature Help]
   ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Stale results work well when users are reading code.

When we add support for writing code, such as automatically triggering signature
help when a space char is inserted, we probably have to use up-to-date results.
-}

{-
Here is a brief description of the algorithm of finding relevant bits from HIE AST
1. let 'hsAppNode' = the smallest 'HsApp' AST node which contains the cursor postion
   See 'extractInfoFromSmallestContainingFunctionApplicationAst'
2. let 'functionNode' = the left-most node of 'hsAppNode'
   See 'getLeftMostNode'
3. try to get 'functionName' and 'functionTypes' from 'functionNode'
   We get 'Nothing' when we cannot get that info
   See 'getNodeNameAndTypes'
4. count 'parameterIndex' by traversing the 'hsAppNode' subtree from its root to the cursor position
   We get 'Nothing' when either the cursor position is at 'functionNode'
   or we encounter some AST node we do not yet know how to continue our traversal
   See 'getParameterIndex'
-}
signatureHelpProvider :: PluginMethodHandler IdeState Method_TextDocumentSignatureHelp
signatureHelpProvider ideState _pluginId (SignatureHelpParams (TextDocumentIdentifier uri) position _mProgreeToken mSignatureHelpContext) = do
  nfp <- getNormalizedFilePathE uri
  results <- runIdeActionE "signatureHelp.ast" (shakeExtras ideState) $ do
    -- see Note [Stale Results in Signature Help]
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
                parameterIndex <- getParameterIndex span hieAst
                Just (functionName, functionTypes, parameterIndex)
            )
  (docMap, argDocMap) <- runIdeActionE "signatureHelp.docMap" (shakeExtras ideState) $ do
    -- see Note [Stale Results in Signature Help]
    mResult <- ExceptT $ Right <$> useWithStaleFast GetDocMap nfp
    case mResult of
      Just (DKMap docMap _tyThingMap argDocMap, _positionMapping) -> pure (docMap, argDocMap)
      Nothing -> pure (mempty, mempty)
  case results of
    [(_functionName, [], _parameterIndex)] -> pure $ InR Null
    [(functionName, functionTypes, parameterIndex)] ->
      pure $ InL $ mkSignatureHelp mSignatureHelpContext docMap argDocMap (fromIntegral parameterIndex - 1) functionName functionTypes
    _ -> pure $ InR Null

mkSignatureHelp :: Maybe SignatureHelpContext -> DocMap -> ArgDocMap -> UInt -> Name -> [Type] -> SignatureHelp
mkSignatureHelp mSignatureHelpContext docMap argDocMap parameterIndex functionName functionTypes =
  SignatureHelp
    { _signatures = mkSignatureInformation docMap argDocMap parameterIndex functionName <$> functionTypes,
      _activeSignature = activeSignature,
      _activeParameter = Just $ InL parameterIndex
    }
  where
    activeSignature = case mSignatureHelpContext of
      Just
        ( SignatureHelpContext
            { _triggerKind,
              _triggerCharacter,
              _isRetrigger = True,
              _activeSignatureHelp = Just (SignatureHelp _signatures oldActivateSignature _activeParameter)
            }
          ) -> oldActivateSignature
      _ -> Just 0

mkSignatureInformation :: DocMap -> ArgDocMap -> UInt -> Name -> Type -> SignatureInformation
mkSignatureInformation docMap argDocMap parameterIndex functionName functionType =
  let functionNameLabelPrefix = printOutputableOneLine (ppr functionName) <> " :: "
      mFunctionDoc = case lookupNameEnv docMap functionName of
        Nothing      -> Nothing
        Just spanDoc -> Just $ InR $ mkMarkdownDoc spanDoc
      thisArgDocMap = case lookupNameEnv argDocMap functionName of
        Nothing             -> mempty
        Just thisArgDocMap' -> thisArgDocMap'
   in SignatureInformation
        { -- Server-side line wrapping may be better since more context is available.
          -- However, server-side line wrapping may make it harder to calculate
          -- parameter ranges.  In addition, some clients, such as vscode, ignore
          -- server-side line wrapping and instead does client-side line wrapping.
          -- So we choose not to do server-side line wrapping.
          _label = functionNameLabelPrefix <> printOutputableOneLine functionType,
          _documentation = mFunctionDoc,
          _parameters = Just $ mkParameterInformations thisArgDocMap (fromIntegral $ T.length functionNameLabelPrefix) functionType,
          _activeParameter = Just $ InL parameterIndex
        }

mkParameterInformations :: IntMap SpanDoc -> UInt -> Type -> [ParameterInformation]
mkParameterInformations thisArgDocMap offset functionType =
  [ ParameterInformation (InR range) mParameterDoc
    | (parameterIndex, range) <- zip [0 ..] (bimap (+ offset) (+ offset) <$> findParameterRanges functionType),
      let mParameterDoc = case IntMap.lookup parameterIndex thisArgDocMap of
            Nothing      -> Nothing
            Just spanDoc -> Just $ InR $ mkMarkdownDoc $ removeUris spanDoc
  ]
  where
    -- we already show uris in the function doc, no need to duplicate them in the parameter doc
    removeUris (SpanDocString docs _uris) = SpanDocString docs emptyUris
    removeUris (SpanDocText docs _uris)   = SpanDocText docs emptyUris

    emptyUris = SpanDocUris Nothing Nothing

mkMarkdownDoc :: SpanDoc -> MarkupContent
mkMarkdownDoc = spanDocToMarkdown >>> T.unlines >>> MarkupContent MarkupKind_Markdown

findParameterRanges :: Type -> [(UInt, UInt)]
findParameterRanges functionType =
  let functionTypeString = printOutputableOneLine functionType
      functionTypeStringLength = fromIntegral $ T.length functionTypeString
      splitFunctionTypes = filter notTypeConstraint $ splitFunTysIgnoringForAll functionType
      splitFunctionTypeStrings = printOutputableOneLine . fst <$> splitFunctionTypes
      -- reverse to avoid matching "a" of "forall a" in "forall a. a -> a"
      reversedRanges =
        drop 1 $ -- do not need the range of the result (last) type
          findParameterStringRanges
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
- ppr the first parameter type of '(a -> b) -> a -> b' is 'a -> b' (no parentheses)
- 'forall' is not always shown

Alternative method 2: use only unstructured type string
This method is hard to implement because we need to parse the type string.
Some tricky cases are as follows:
- h :: forall a (m :: Type -> Type). Monad m => a -> m a
-}
findParameterStringRanges :: UInt -> Text -> [Text] -> [(UInt, UInt)]
findParameterStringRanges _totalPrefixLength _functionTypeString [] = []
findParameterStringRanges totalPrefixLength functionTypeString (parameterTypeString : restParameterTypeStrings) =
  let (prefix, match) = T.breakOn parameterTypeString functionTypeString
      prefixLength = fromIntegral $ T.length prefix
      parameterTypeStringLength = fromIntegral $ T.length parameterTypeString
      start = totalPrefixLength + prefixLength
   in (start, start + parameterTypeStringLength)
        : findParameterStringRanges
          (totalPrefixLength + prefixLength + parameterTypeStringLength)
          (T.drop (fromIntegral parameterTypeStringLength) match)
          restParameterTypeStrings

-- similar to 'splitFunTys' but
--   1) the result (last) type is included and
--   2) toplevel foralls are ignored
splitFunTysIgnoringForAll :: Type -> [(Type, Maybe FunTyFlag)]
splitFunTysIgnoringForAll ty = case ty & dropForAlls & splitFunTy_maybe of
  Just (funTyFlag, _mult, parameterType, resultType) ->
    (parameterType, Just funTyFlag) : splitFunTysIgnoringForAll resultType
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

getLeftMostNode :: HieAST a -> HieAST a
getLeftMostNode thisNode =
  case nodeChildren thisNode of
    []            -> thisNode
    leftChild : _ -> getLeftMostNode leftChild

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
                  -- (the last?) one type of 'typesOfNode' may (always?) be the same as 'typeOfName'
                  -- To avoid generating two identical signature helps, we do a filtering here
                  -- This is similar to 'dropEnd1' in Development.IDE.Spans.AtPoint.atPoint
                  -- TODO perhaps extract a common function
                  Just typeOfName -> typeOfName : filter (isDifferentType typeOfName) typesOfNode
             in Just (name, filterCoreTypes allTypes)
      [] -> Nothing
      _ -> Nothing -- seems impossible
    else Nothing
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

-- Just 1 means the first parameter
getParameterIndex :: RealSrcSpan -> HieAST a -> Maybe Integer
getParameterIndex span hieAst
  | nodeHasAnnotation ("HsApp", "HsExpr") hieAst =
      case nodeChildren hieAst of
        [leftChild, _] ->
          if span `isRealSubspanOf` nodeSpan leftChild
            then Nothing
            else getParameterIndex span leftChild >>= \parameterIndex -> Just (parameterIndex + 1)
        _ -> Nothing -- impossible
  | nodeHasAnnotation ("HsAppType", "HsExpr") hieAst =
      case nodeChildren hieAst of
        [leftChild, _] -> getParameterIndex span leftChild
        _              -> Nothing -- impossible
  | otherwise =
      case nodeChildren hieAst of
        []      -> Just 0 -- the function is found
        [child] -> getParameterIndex span child -- ignore irrelevant nodes
        _       -> Nothing
