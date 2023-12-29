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
import           Data.Foldable                      (fold)
import qualified Data.Map                           as M
import qualified Data.Map                           as Map
import           Data.Maybe                         (listToMaybe, mapMaybe)
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
      fold [typeTokenType, contextInfoTokenType]

    contextInfosMaybeTokenType :: Set.Set ContextInfo -> Maybe HsSemanticTokenType
    contextInfosMaybeTokenType details = foldMap infoTokenType (Set.toList details)

-----------------------------------

-- * extract location from HieAST a

-----------------------------------

hieAstSpanNames :: HieAST a -> M.Map Range NameSet
hieAstSpanNames ast =
  if null (nodeChildren ast)
    then getIds ast
    else M.unionsWith unionNameSet $ map hieAstSpanNames (nodeChildren ast)
  where
    getIds ast' =  M.singleton (realSrcSpanToRange $ nodeSpan ast') (getNodeIds' ast')
    getNodeIds' =
      Map.foldl' combineNodeIds mempty
        . Map.filterWithKey (\k _ -> k == SourceInfo)
        . getSourcedNodeInfo
        . sourcedNodeInfo
    combineNodeIds :: NameSet -> NodeInfo a -> NameSet
    ad `combineNodeIds` (NodeInfo _ _ bd) = ad `unionNameSet` xs
      where xs =  mkNameSet $ rights $ M.keys bd


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

extractSemanticTokensFromNames :: NameSemanticMap -> M.Map Range NameSet -> [(Range, HsSemanticTokenType)]
extractSemanticTokensFromNames nsm rnMap = xs
    where xs = mapMaybe sequence (Map.toList $ Map.map (foldMap (lookupNameEnv nsm) . nameSetElemsStable) rnMap)
