{-# LANGUAGE ExplicitNamespaces  #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- |
-- The query module is used to query the semantic tokens from the AST
module Ide.Plugin.SemanticTokens.Query where

import           Data.Foldable                        (fold)
import qualified Data.Map                             as M
import qualified Data.Map                             as Map
import           Data.Maybe                           (fromMaybe, listToMaybe,
                                                       mapMaybe)
import           Data.Set                             (Set)
import qualified Data.Set                             as S
import qualified Data.Set                             as Set
import           Data.Text                            (Text)
import           Development.IDE.Core.PositionMapping (PositionMapping,
                                                       toCurrentRange)
import           Development.IDE.GHC.Compat
import           Development.IDE.GHC.Error            (realSrcSpanToCodePointRange)
import           Ide.Plugin.SemanticTokens.Mappings
import           Ide.Plugin.SemanticTokens.Types      (HieFunMaskKind,
                                                       HsSemanticTokenType (TModule),
                                                       NameSemanticMap,
                                                       SemanticTokensConfig)
import           Ide.Plugin.SemanticTokens.Utils      (splitAndBreakModuleNameAndOccName, getTextByCodePointRangeFromVfs)
import           Language.LSP.Protocol.Types          (Position (Position),
                                                       Range (Range),
                                                       SemanticTokenAbsolute (SemanticTokenAbsolute),
                                                       SemanticTokens,
                                                       defaultSemanticTokensLegend,
                                                       makeSemanticTokens)
import           Language.LSP.VFS
import           Prelude                              hiding (length, span)

---------------------------------------------------------

-- * extract semantic map from HieAst for local variables

---------------------------------------------------------

mkLocalNameSemanticFromAst :: [Identifier] -> HieFunMaskKind a -> RefMap a -> NameSemanticMap
mkLocalNameSemanticFromAst names hieKind rm = M.fromList (mapMaybe (nameNameSemanticFromHie hieKind rm) names)

nameNameSemanticFromHie :: forall a. HieFunMaskKind a -> RefMap a -> Identifier -> Maybe (Identifier, HsSemanticTokenType)
nameNameSemanticFromHie _ _ ns@(Left _) = Just (ns, TModule)
nameNameSemanticFromHie hieKind rm ns@(Right _) = do
  st <- nameSemanticFromRefMap rm ns
  return (ns, st)
  where
    nameSemanticFromRefMap :: RefMap a -> Identifier -> Maybe HsSemanticTokenType
    nameSemanticFromRefMap rm' name' = do
      spanInfos <- Map.lookup name' rm'
      let typeTokenType = foldMap (typeSemantic hieKind) $ listToMaybe $ mapMaybe (identType . snd) spanInfos
      contextInfoTokenType <- foldMap (contextInfosMaybeTokenType . identInfo . snd) spanInfos
      fold [typeTokenType, Just contextInfoTokenType]

    contextInfosMaybeTokenType :: Set.Set ContextInfo -> Maybe HsSemanticTokenType
    contextInfosMaybeTokenType details = foldMap infoTokenType (Set.toList details)


-------------------------------------------------

-- * extract semantic tokens from NameSemanticMap

-------------------------------------------------

extractSemanticTokensFromNames :: NameSemanticMap -> M.Map Range (Set Identifier) -> M.Map Range HsSemanticTokenType
extractSemanticTokensFromNames nsm = Map.mapMaybe (foldMap (`M.lookup` nsm))

rangeSemanticMapSemanticTokens :: SemanticTokensConfig -> PositionMapping -> M.Map Range HsSemanticTokenType -> Either Text SemanticTokens
rangeSemanticMapSemanticTokens stc mapping =
  makeSemanticTokens defaultSemanticTokensLegend
    . mapMaybe (\(range, ty) -> flip toAbsSemanticToken ty <$> range)
    . Map.toAscList
    . M.mapKeys (toCurrentRange mapping)
  where
    toAbsSemanticToken :: Range -> HsSemanticTokenType -> SemanticTokenAbsolute
    toAbsSemanticToken (Range (Language.LSP.Protocol.Types.Position startLine startColumn)
                              (Language.LSP.Protocol.Types.Position _endLine endColumn))
                              tokenType =
      let len = endColumn - startColumn
       in SemanticTokenAbsolute
            (fromIntegral startLine)
            (fromIntegral startColumn)
            (fromIntegral len)
            (toLspTokenType stc tokenType)
            []
