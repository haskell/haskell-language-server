{-
    The query module is used to query the semantic tokens from the AST
-}
{-# LANGUAGE ExplicitNamespaces  #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ScopedTypeVariables #-}


module Ide.Plugin.SemanticTokens.Query where

import           Data.Either                        (rights)
import           Data.Foldable                      (fold)
import qualified Data.List                          as List
import qualified Data.Map                           as M
import qualified Data.Map                           as Map
import           Data.Maybe                         (fromMaybe, listToMaybe,
                                                     mapMaybe)
import qualified Data.Set                           as S
import qualified Data.Set                           as Set
import           Data.Text                          (Text)
import           Development.IDE                    (realSrcSpanToRange)
import           Development.IDE.GHC.Compat
import           Ide.Plugin.SemanticTokens.Mappings
import           Ide.Plugin.SemanticTokens.Types    (HieFunMaskKind,
                                                     HsSemanticTokenType,
                                                     NameSemanticMap)
import           Ide.Plugin.SemanticTokens.Utils    (realSrcSpanToCodePointRange)
import           Language.LSP.Protocol.Types
import           Language.LSP.VFS                   (CodePointRange,
                                                     VirtualFile,
                                                     codePointRangeToRange)
import           Prelude                            hiding (span)

---------------------------------------------------------

-- * extract semantic map from HieAst for local variables

---------------------------------------------------------

mkLocalNameSemanticFromAst :: [Name] -> HieFunMaskKind a -> RefMap a -> NameSemanticMap
mkLocalNameSemanticFromAst names hieKind rm = mkNameEnv (mapMaybe (nameNameSemanticFromHie hieKind rm) names)

nameNameSemanticFromHie :: forall a. HieFunMaskKind a -> RefMap a -> Name -> Maybe (Name, HsSemanticTokenType)
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
      let typeTokenType = foldMap (typeSemantic hieKind) $ listToMaybe $ mapMaybe (identType . snd) spanInfos
      contextInfoTokenType <- foldMap (contextInfosMaybeTokenType . identInfo . snd) spanInfos
      fold [typeTokenType, Just contextInfoTokenType]

    contextInfosMaybeTokenType :: Set.Set ContextInfo -> Maybe HsSemanticTokenType
    contextInfosMaybeTokenType details = foldMap infoTokenType (Set.toList details)

-----------------------------------

-- * extract location from HieAST a

-----------------------------------

-- | get only visible names from HieAST
-- we care only the leaf node of the AST
-- and filter out the derived and evidence names
hieAstSpanNames :: VirtualFile -> HieAST a -> M.Map Range NameSet
hieAstSpanNames vf ast =
  if null (nodeChildren ast)
    then getIds ast
    else M.unionsWith unionNameSet $ map (hieAstSpanNames vf) (nodeChildren ast)
  where
    getIds ast' =  fromMaybe mempty $ do
        range <- codePointRangeToRange vf $ realSrcSpanToCodePointRange $ nodeSpan ast'
        return $ M.singleton range (getNodeIds' ast')
    getNodeIds' =
      Map.foldl' combineNodeIds mempty
        . Map.filterWithKey (\k _ -> k == SourceInfo)
        . getSourcedNodeInfo
        . sourcedNodeInfo
    combineNodeIds :: NameSet -> NodeInfo a -> NameSet
    ad `combineNodeIds` (NodeInfo _ _ bd) = ad `unionNameSet` xs
      where
        xs =  mkNameSet $ rights $ M.keys $ M.filterWithKey inclusion bd
        inclusion :: Identifier -> IdentifierDetails a -> Bool
        inclusion a b = not $ exclusion a b
        exclusion :: Identifier -> IdentifierDetails a -> Bool
        exclusion idt IdentifierDetails {identInfo = infos} = case idt of
            Left _ -> True
            Right name ->
                isDerivedOccName (nameOccName name)
                || any isEvidenceContext (S.toList infos)


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
