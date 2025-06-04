{-# LANGUAGE OverloadedStrings #-}

module Ide.Plugin.CodeRange.ASTPreProcess
    ( preProcessAST
    , PreProcessEnv(..)
    , isCustomNode
    , CustomNodeType(..)
    ) where

import           Control.Monad.Reader       (Reader, asks)
import           Data.Foldable
import           Data.Functor.Identity      (Identity (Identity, runIdentity))
import           Data.List                  (groupBy)
import           Data.List.NonEmpty         (NonEmpty)
import qualified Data.List.NonEmpty         as NonEmpty
import           Data.Map.Strict            (Map)
import qualified Data.Map.Strict            as Map
import           Data.Maybe                 (fromMaybe, mapMaybe)
import           Data.Semigroup             (First (First, getFirst))
import           Data.Semigroup.Foldable    (foldlM1)
import qualified Data.Set                   as Set
import           Development.IDE.GHC.Compat
import           GHC.Iface.Ext.Types        (ContextInfo (..), HieAST (..),
                                             Identifier, IdentifierDetails (..),
                                             NodeInfo (nodeIdentifiers), Span)
import           GHC.Iface.Ext.Utils        (RefMap, flattenAst)
import           Prelude                    hiding (span)

{-|
Extra arguments for 'preProcessAST'. It's expected to be used in a 'Reader' context
-}
newtype PreProcessEnv a = PreProcessEnv
    { preProcessEnvRefMap :: RefMap a
    }

{-|
Before converting the HieAST to selection range, we need to run some passes on it. Each pass potentially modifies
the AST to handle some special cases.

'preProcessAST' combines the passes. Refer to 'mergeImports' or 'mergeSignatureWithDefinition' as
a concrete example example.

Adding another manipulation to the AST is simple, just implement a function of type
`HieAST a -> Reader (PreProcessEnv a) (HieAST a)`, and append it to 'preProcessAST' with `>>=`.

If it goes more complex, it may be more appropriate to split different manipulations to different modules.
-}
preProcessAST :: HieAST a -> Reader (PreProcessEnv a) (HieAST a)
preProcessAST node = mergeImports node >>= mergeSignatureWithDefinition

{-|
Create a custom node in 'HieAST'. By "custom", we mean this node doesn't actually exist in the original 'HieAST'
provided by GHC, but created to suite the needs of hls-code-range-plugin.
-}
createCustomNode :: CustomNodeType -> NonEmpty (HieAST a) -> HieAST a
createCustomNode customNodeType children = mkAstNode customNodeInfo span' (NonEmpty.toList children)
  where
    span' :: RealSrcSpan
    span' = runIdentity . foldlM1 (\x y -> Identity (combineRealSrcSpans x y)) . fmap nodeSpan $ children

    customNodeInfo = simpleNodeInfoCompat "HlsCustom" (customNodeTypeToFastString customNodeType)

isCustomNode :: HieAST a -> Maybe CustomNodeType
isCustomNode node = do
    nodeInfo <- generatedNodeInfo node
    getFirst <$> foldMap go (nodeAnnotations nodeInfo)
  where
    go :: (FastStringCompat, FastStringCompat) -> Maybe (First CustomNodeType)
    go (k, v)
        | k == "HlsCustom", Just v' <- revCustomNodeTypeMapping Map.!? v = Just (First v')
        | otherwise = Nothing

data CustomNodeType =
    -- | a group of imports
    CustomNodeImportsGroup
    -- | adjacent type signature and value definition are paired under a custom parent node
  | CustomNodeAdjacentSignatureDefinition
    deriving (Show, Eq, Ord)

customNodeTypeMapping :: Map CustomNodeType FastStringCompat
customNodeTypeMapping = Map.fromList
    [ (CustomNodeImportsGroup, "Imports")
    , (CustomNodeAdjacentSignatureDefinition, "AdjacentSignatureDefinition")
    ]

revCustomNodeTypeMapping :: Map FastStringCompat CustomNodeType
revCustomNodeTypeMapping = Map.fromList . fmap (\(k, v) -> (v, k)) . Map.toList $ customNodeTypeMapping

customNodeTypeToFastString :: CustomNodeType -> FastStringCompat
customNodeTypeToFastString k = fromMaybe "" (customNodeTypeMapping Map.!? k)

{-|
Combines adjacent import declarations under a new parent node, so that the user will have an extra step selecting
the whole import area while expanding/shrinking the selection range.
-}
mergeImports :: forall a. HieAST a -> Reader (PreProcessEnv a) (HieAST a)
mergeImports node = pure $ node { nodeChildren = children }
  where
    children :: [HieAST a]
    children = mapMaybe merge
        . groupBy (\x y -> nodeIsImport x && nodeIsImport y)
        . nodeChildren $ node

    merge :: [HieAST a] -> Maybe (HieAST a)
    merge []     = Nothing
    merge [x]    = Just x
    merge (x:xs) = Just $ createCustomNode CustomNodeImportsGroup (x NonEmpty.:| xs)

nodeIsImport :: HieAST a -> Bool
nodeIsImport = isAnnotationInAstNode ("ImportDecl", "ImportDecl")

{-|
Combine type signature with variable definition under a new parent node, if the signature is placed right before the
definition. This allows the user to have a step selecting both type signature and its accompanying definition.
-}
mergeSignatureWithDefinition :: HieAST a -> Reader (PreProcessEnv a) (HieAST a)
mergeSignatureWithDefinition node = do
    refMap <- asks preProcessEnvRefMap
    -- Do this recursively for children, so that non top level functions can be handled.
    children' <- traverse mergeSignatureWithDefinition (nodeChildren node)
    pure $ node { nodeChildren = reverse $ foldl' (go refMap) [] children' }
  where
    -- For every two adjacent nodes, we try to combine them into one.
    go :: RefMap a -> [HieAST a] -> HieAST a -> [HieAST a]
    go _ [] node' = [node']
    go refMap (prev:others) node' =
        case mergeAdjacentSigDef refMap (prev, node') of
            Nothing   -> node':prev:others
            Just comb -> comb:others

-- | Merge adjacent type signature and variable/function definition, if the type signature belongs to that variable or
-- function.
--
-- The implementation potentially has some corner cases not handled properly.
mergeAdjacentSigDef :: RefMap a -> (HieAST a, HieAST a) -> Maybe (HieAST a)
mergeAdjacentSigDef refMap (n1, n2) = do
    -- Let's check the node's annotation. There should be a function binding following its type signature.
    checkAnnotation
    -- Find the identifier of the type signature.
    typeSigId <- identifierForTypeSig n1
    -- Does that identifier appear in the second AST node as a definition? If so, we combines the two nodes.
    refs <- Map.lookup typeSigId refMap
    if any (isIdentADef (nodeSpan n2)) refs
    then pure . createCustomNode CustomNodeAdjacentSignatureDefinition $ n1 NonEmpty.:| [n2]
    else Nothing
  where
    checkAnnotation :: Maybe ()
    checkAnnotation =
      if ("TypeSig", "Sig") `isAnnotationInAstNode` n1 &&
         (("FunBind", "HsBindLR") `isAnnotationInAstNode` n2 || ("VarBind", "HsBindLR") `isAnnotationInAstNode` n2)
      then Just ()
      else Nothing

{-|
Given the AST node of a type signature, tries to find the identifier of it.
-}
identifierForTypeSig :: forall a. HieAST a -> Maybe Identifier
identifierForTypeSig node =
    {-
        It seems that the identifier lives in one of the children, so we search for the first 'TyDecl' node in
        its children recursively.
    -}
    case mapMaybe extractIdentifier nodes of
      []        -> Nothing
      (ident:_) -> Just ident
  where
    nodes = flattenAst node

    extractIdentifier :: HieAST a -> Maybe Identifier
    extractIdentifier node' = sourceNodeInfo node' >>=
        (fmap fst . find (\(_, detail) -> TyDecl `Set.member` identInfo detail)
        . Map.toList . nodeIdentifiers)

-- | Determines if the given occurrence of an identifier is a function/variable definition in the outer span
isIdentADef :: Span -> (Span, IdentifierDetails a) -> Bool
isIdentADef outerSpan (span, detail) =
    realSrcSpanStart span >= realSrcSpanStart outerSpan && realSrcSpanEnd span <= realSrcSpanEnd outerSpan
    && isDef
  where
    isDef :: Bool
    isDef = any isContextInfoDef $ identInfo detail

    -- Determines if the 'ContextInfo' represents a variable/function definition
    isContextInfoDef :: ContextInfo -> Bool
    isContextInfoDef ValBind{} = True
    isContextInfoDef MatchBind = True
    isContextInfoDef _         = False

isAnnotationInAstNode :: (FastStringCompat, FastStringCompat) -> HieAST a -> Bool
isAnnotationInAstNode p = maybe False (isAnnotationInNodeInfo p) . sourceNodeInfo
