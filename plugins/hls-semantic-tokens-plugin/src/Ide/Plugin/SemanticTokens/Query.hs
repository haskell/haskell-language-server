{-
    The query module is used to query the semantic tokens from the AST
-}
{-# LANGUAGE ExplicitNamespaces  #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections       #-}

module Ide.Plugin.SemanticTokens.Query where

import           Data.Either                        (rights)
import           Data.Generics                      (everything)
import qualified Data.List                          as List
import qualified Data.List.NonEmpty                 as NE
-- import HieDb.Types (DefRow (..))
import qualified Data.Map                           as M
import qualified Data.Map                           as Map
import           Data.Maybe                         (catMaybes, listToMaybe,
                                                     mapMaybe)
import qualified Data.Set                           as S
import qualified Data.Set                           as Set
import           Data.Text                          (Text)
import           Development.IDE                    (HieKind,
                                                     realSrcSpanToRange)
import           Development.IDE.GHC.Compat
import           Generics.SYB                       (mkQ)
import           Ide.Plugin.SemanticTokens.Mappings
import           Ide.Plugin.SemanticTokens.Types    (HsSemanticTokenType,
                                                     NameSemanticMap)
import           Language.LSP.Protocol.Types
import           Prelude                            hiding (span)

------------------------------------------------

-- * extract names from RenamedSource

------------------------------------------------

nameToCollect :: Name -> NameSet
nameToCollect = unitNameSet

nameGetter :: RenamedSource -> NameSet
nameGetter = everything unionNameSet (emptyNameSet `mkQ` nameToCollect)

---------------------------------------------------------

-- * extract semantic map from HieAst for local variables

---------------------------------------------------------

mkLocalNameSemanticFromAst :: HieKind a -> RefMap a -> NameSemanticMap
mkLocalNameSemanticFromAst hieKind rm = mkNameEnv (mapMaybe (nameNameSemanticFromHie hieKind rm) ns)
  where
    ns = rights $ M.keys rm

nameNameSemanticFromHie :: forall a. HieKind a -> RefMap a -> Name -> Maybe (Name, HsSemanticTokenType)
nameNameSemanticFromHie hieKind rm ns = do
  st <- -- traceShow ("to find Name", showName ns) $
    nameSemanticFromRefMap rm ns
  return
    -- \$ traceShow (showName ns, st)
    (ns, st)
  where
    nameSemanticFromRefMap :: RefMap a -> Name -> Maybe HsSemanticTokenType
    nameSemanticFromRefMap rm' name' = do
      spanInfos <- -- traceShow ("getting spans:", nameString) $
        Map.lookup (Right name') rm'
      let infos = S.unions $ map (identInfo . snd) spanInfos
      let typeTokenType = fmap (typeSemantic hieKind) $ listToMaybe $ mapMaybe (identType . snd) spanInfos
      let contextInfoTokenType = contextInfosMaybeTokenType infos
      maximum <$> NE.nonEmpty (catMaybes [typeTokenType, contextInfoTokenType])

    contextInfosMaybeTokenType :: Set.Set ContextInfo -> Maybe HsSemanticTokenType
    contextInfosMaybeTokenType details = case NE.nonEmpty $ Set.toList details of
      Just infos -> maximum $ NE.map infoTokenType infos
      Nothing    -> Nothing

-----------------------------------

-- * extract location from HieAST a

-----------------------------------

hieAstSpanNames :: NameSet -> HieAST a -> [(Range, Name)]
hieAstSpanNames nameSet ast =
  if null (nodeChildren ast)
    then getIds ast
    else concatMap (hieAstSpanNames nameSet) (nodeChildren ast)
  where
    getIds :: HieAST a -> [(Range, Name)]
    getIds ast' =
      [ (realSrcSpanToRange $ nodeSpan ast', c)
        | (Right c) <- S.toList $ getNodeIds' ast,
          elemNameSet c nameSet
      ]
    getNodeIds' :: HieAST a -> S.Set Identifier
    getNodeIds' =
      Map.foldl' combineNodeIds mempty
        . Map.filterWithKey (\k _ -> k == SourceInfo)
        . getSourcedNodeInfo
        . sourcedNodeInfo
    combineNodeIds :: S.Set Identifier -> NodeInfo a -> S.Set Identifier
    ad `combineNodeIds` (NodeInfo _ _ bd) = ad `S.union` xs
      where
        xs = M.keysSet bd

-------------------------------------------------

-- * extract semantic tokens from NameSemanticMap

-------------------------------------------------

semanticTokenAbsoluteSemanticTokens :: [(Range, HsSemanticTokenType)] -> Either Text SemanticTokens
semanticTokenAbsoluteSemanticTokens = makeSemanticTokens defaultSemanticTokensLegend . List.sort . map (uncurry toAbsSemanticToken)
  where
    toAbsSemanticToken :: Range -> HsSemanticTokenType -> SemanticTokenAbsolute
    toAbsSemanticToken (Range (Position startLine startColumn) (Position _endLine endColumn)) tokenType =
      let len = endColumn - startColumn
       in SemanticTokenAbsolute
            (fromIntegral startLine)
            (fromIntegral startColumn)
            (fromIntegral len)
            (toLspTokenType tokenType)
            []

extractSemanticTokensFromNames :: NameSemanticMap -> [(Range, Name)] -> [(Range, HsSemanticTokenType)]
extractSemanticTokensFromNames nsm = mergeNameFromSamSpan . mapMaybe (getSemantic nsm)
  where
    -- merge all tokens with same span
    -- mergeNameFromSamSpan :: [(Span, SemanticTokenType)] -> [(Span, SemanticTokenType)]
    mergeNameFromSamSpan xs = Map.toList $ Map.fromListWith (<>) xs

    getSemantic nameMap (span, name) = do
      tokenType <- lookupNameEnv nameMap name
      pure (span, tokenType)
