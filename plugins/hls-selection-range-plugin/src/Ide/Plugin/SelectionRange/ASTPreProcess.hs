{-# LANGUAGE CPP                 #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Ide.Plugin.SelectionRange.ASTPreProcess
    ( preProcessAST
    , PreProcessEnv(..)
    ) where

import           Control.Monad.Reader            (Reader, asks)
import           Data.Foldable                   (find, foldl')
import           Data.List                       (groupBy)
import qualified Data.Map.Strict                 as Map
import           Data.Maybe                      (mapMaybe)
import qualified Data.Set                        as Set
import           Development.IDE.GHC.Compat      (ContextInfo (MatchBind, TyDecl, ValBind),
                                                  HieAST (..), Identifier,
                                                  IdentifierDetails (identInfo),
                                                  NodeInfo (NodeInfo, nodeIdentifiers),
                                                  RefMap, Span, flattenAst,
                                                  isAnnotationInNodeInfo,
                                                  mkRealSrcSpan,
                                                  nodeInfoFromSource,
                                                  realSrcSpanEnd,
                                                  realSrcSpanStart)
import           Development.IDE.GHC.Compat.Util (FastString)
import           Prelude                         hiding (span)
#if MIN_VERSION_GLASGOW_HASKELL(9,2,0,0)
import           Development.IDE.GHC.Compat      (SourcedNodeInfo (..))
#endif

newtype PreProcessEnv a = PreProcessEnv
    { preProcessEnvRefMap :: RefMap a
    }

-- | Make the AST more suitable for generating selection range.
preProcessAST :: HieAST a -> Reader (PreProcessEnv a) (HieAST a)
preProcessAST node = mergeImports node >>= mergeSignatureWithDefinition

mergeImports :: HieAST a -> Reader (PreProcessEnv a) (HieAST a)
mergeImports node = pure $ node { nodeChildren = children }
  where
    children = mapMaybe merge
        . groupBy (\x y -> nodeIsImport x && nodeIsImport y)
        . nodeChildren $ node

    merge []  = Nothing
    merge [x] = Just x
    merge xs  = Just (createVirtualNode xs)

nodeIsImport :: HieAST a -> Bool
nodeIsImport = isAnnotationInAstNode ("ImportDecl", "ImportDecl")

createNodeWithEmptyInfo :: Span -> [HieAST a] -> HieAST a
#if MIN_VERSION_GLASGOW_HASKELL(9,2,0,0)
createNodeWithEmptyInfo = Node (SourcedNodeInfo mempty)
#else
createNodeWithEmptyInfo = Node (NodeInfo mempty mempty mempty)
#endif

createVirtualNode :: [HieAST a] -> HieAST a
createVirtualNode nodes = createNodeWithEmptyInfo span' nodes
  where
    span' = mkRealSrcSpan (minimum locations) (maximum locations)
    locations = (\s -> [realSrcSpanStart s, realSrcSpanEnd s]) . nodeSpan =<< nodes

mergeSignatureWithDefinition :: HieAST a -> Reader (PreProcessEnv a) (HieAST a)
mergeSignatureWithDefinition node = do
    refMap <- asks preProcessEnvRefMap
    children' <- traverse mergeSignatureWithDefinition (nodeChildren node)
    pure $ node { nodeChildren = reverse $ foldl' (go refMap) [] children' }
  where
    go :: RefMap a -> [HieAST a] -> HieAST a -> [HieAST a]
    go _ [] node' = [node']
    go refMap (prev:others) node' =
        case mergeNearbySigDef refMap (prev, node') of
            Nothing   -> node':prev:others
            Just comb -> comb:others

mergeNearbySigDef :: RefMap a -> (HieAST a, HieAST a) -> Maybe (HieAST a)
mergeNearbySigDef refMap (n1, n2) = do
    if not (("TypeSig", "Sig") `isAnnotationInAstNode` n1 && ("FunBind", "HsBindLR") `isAnnotationInAstNode` n2)
    then Nothing
    else do
        typeSigId <- identifierForTypeSig n1
        refs <- Map.lookup typeSigId refMap
        if any (isIdentADef (nodeSpan n2)) refs
        then pure $ createVirtualNode [n1, n2]
        else Nothing

identifierForTypeSig :: forall a. HieAST a -> Maybe Identifier
identifierForTypeSig node =
    case mapMaybe extractIdentifier nodes of
      []        -> Nothing
      (ident:_) -> Just ident
  where
    nodes = flattenAst node

    extractIdentifier :: HieAST a -> Maybe Identifier
    extractIdentifier node' = nodeInfoFromSource node' >>=
        (fmap fst . find (\(_, detail) -> TyDecl `Set.member` identInfo detail)
        . Map.toList . nodeIdentifiers)

-- | is the given occurence of an identifier is a function/variable definition in the outer span
isIdentADef :: Span -> (Span, IdentifierDetails a) -> Bool
isIdentADef outerSpan (span, detail) =
    realSrcSpanStart span >= realSrcSpanStart outerSpan && realSrcSpanEnd span <= realSrcSpanEnd outerSpan
    && isDef
  where
    isDef = any isContextInfoDef . Set.toList . identInfo $ detail

    isContextInfoDef ValBind{} = True
    isContextInfoDef MatchBind = True
    isContextInfoDef _         = False

isAnnotationInAstNode :: (FastString, FastString) -> HieAST a -> Bool
isAnnotationInAstNode p = maybe False (isAnnotationInNodeInfo p) . nodeInfoFromSource
