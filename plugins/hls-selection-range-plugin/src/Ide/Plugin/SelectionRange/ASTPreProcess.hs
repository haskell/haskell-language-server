{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Ide.Plugin.SelectionRange.ASTPreProcess
    ( preProcessAST
    , PreProcessEnv(..)
    ) where

import           Control.Monad.Reader            (Reader, asks)
import           Data.Foldable                   (find, foldl')
import           Data.List                       (foldl1', groupBy)
import qualified Data.Map.Strict                 as Map
import           Data.Maybe                      (mapMaybe)
import qualified Data.Set                        as Set
import           Development.IDE.GHC.Compat      (ContextInfo (MatchBind, TyDecl, ValBind),
                                                  HieAST (..), Identifier,
                                                  IdentifierDetails (identInfo),
                                                  NodeInfo (NodeInfo, nodeIdentifiers),
                                                  RealSrcSpan, RefMap, Span,
                                                  combineRealSrcSpans',
                                                  flattenAst,
                                                  isAnnotationInNodeInfo,
                                                  mkAstNode, nodeInfoFromSource,
                                                  realSrcSpanEnd,
                                                  realSrcSpanStart)
import           Development.IDE.GHC.Compat.Util (FastString)
import           Prelude                         hiding (span)

{-|
Extra arguments for 'preaProcessAST', meant to be used in a 'Reader' context. We use 'Reader' to combine
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
    merge []  = Nothing
    merge [x] = Just x
    merge xs  = createVirtualNode xs

nodeIsImport :: HieAST a -> Bool
nodeIsImport = isAnnotationInAstNode ("ImportDecl", "ImportDecl")

createVirtualNode :: [HieAST a] -> Maybe (HieAST a)
createVirtualNode [] = Nothing
createVirtualNode children = Just $ mkAstNode (NodeInfo mempty mempty mempty) span' children
  where
    span' :: RealSrcSpan
    span' = foldl1' combineRealSrcSpans' . fmap nodeSpan $ children

{-|
Combine type signature with variable definition under a new parent node, if the signature is placed right before the
definition. This allows the user to have a step selecting both type signature and its accompanying definition.
-}
mergeSignatureWithDefinition :: HieAST a -> Reader (PreProcessEnv a) (HieAST a)
mergeSignatureWithDefinition node = do
    refMap <- asks preProcessEnvRefMap
    -- do this recursively for children, so that non top level functions can be handled.
    children' <- traverse mergeSignatureWithDefinition (nodeChildren node)
    pure $ node { nodeChildren = reverse $ foldl' (go refMap) [] children' }
  where
    -- for every two adjacent nodes, we try to combine them into one
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
    then createVirtualNode [n1, n2]
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
    extractIdentifier node' = nodeInfoFromSource node' >>=
        (fmap fst . find (\(_, detail) -> TyDecl `Set.member` identInfo detail)
        . Map.toList . nodeIdentifiers)

-- | Determines if the given occurence of an identifier is a function/variable definition in the outer span
isIdentADef :: Span -> (Span, IdentifierDetails a) -> Bool
isIdentADef outerSpan (span, detail) =
    realSrcSpanStart span >= realSrcSpanStart outerSpan && realSrcSpanEnd span <= realSrcSpanEnd outerSpan
    && isDef
  where
    isDef :: Bool
    isDef = any isContextInfoDef . Set.toList . identInfo $ detail

    -- does the 'ContextInfo' represents a variable/function definition?
    isContextInfoDef :: ContextInfo -> Bool
    isContextInfoDef ValBind{} = True
    isContextInfoDef MatchBind = True
    isContextInfoDef _         = False

isAnnotationInAstNode :: (FastString, FastString) -> HieAST a -> Bool
isAnnotationInAstNode p = maybe False (isAnnotationInNodeInfo p) . nodeInfoFromSource
