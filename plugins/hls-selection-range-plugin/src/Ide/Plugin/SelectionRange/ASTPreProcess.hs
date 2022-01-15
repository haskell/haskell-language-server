{-# LANGUAGE ScopedTypeVariables #-}

module Ide.Plugin.SelectionRange.ASTPreProcess
    ( preProcessAST
    ) where

import           Data.List                       (groupBy)
import           Data.Maybe                      (mapMaybe)
import qualified Data.Set                        as Set
import           Development.IDE.GHC.Compat      (HieAST (..),
                                                  NodeInfo (NodeInfo, nodeAnnotations),
                                                  mkRealSrcSpan, realSrcSpanEnd,
                                                  realSrcSpanStart)
import           Development.IDE.GHC.Compat.Util (mkFastString)

-- | Make the AST more suitable for generating selection range.
preProcessAST :: HieAST a -> HieAST a
preProcessAST = mergeImports

mergeImports :: HieAST a -> HieAST a
mergeImports node = node { nodeChildren = children }
  where
    children = mapMaybe merge
        . groupBy (\x y -> nodeIsImport x && nodeIsImport y)
        . nodeChildren $ node

    merge []  = Nothing
    merge [x] = Just x
    merge xs  = Just (createVirtualNode xs)

nodeIsImport :: HieAST a -> Bool
nodeIsImport node = Set.member (mkFastString "ImportDecl", mkFastString "ImportDecl") annotations
  where
    annotations = nodeAnnotations . nodeInfo $ node

createVirtualNode :: [HieAST a] -> HieAST a
createVirtualNode nodes = Node (NodeInfo mempty mempty mempty) span' nodes
  where
    span' = mkRealSrcSpan (minimum locations) (maximum locations)
    locations = (\s -> [realSrcSpanStart s, realSrcSpanEnd s]) . nodeSpan =<< nodes
