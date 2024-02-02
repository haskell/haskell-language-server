{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings   #-}

-- |
-- The query module is used to query the semantic tokens from the AST
module Ide.Plugin.SemanticTokens.Query where

import           Data.Foldable                        (fold)
import qualified Data.Map.Strict                      as M
import           Data.Maybe                           (listToMaybe, mapMaybe)
import           Data.Set                             (Set)
import qualified Data.Set                             as Set
import           Data.Text                            (Text)
import           Development.IDE.Core.PositionMapping (PositionMapping,
                                                       toCurrentRange)
import           Development.IDE.GHC.Compat
import           Ide.Plugin.SemanticTokens.Mappings
import           Ide.Plugin.SemanticTokens.Types      (HieFunMaskKind,
                                                       HsSemanticTokenType (TModule),
                                                       IdSemanticMap,
                                                       RangeIdSetMap,
                                                       SemanticTokensConfig)
import           Language.LSP.Protocol.Types          (Position (Position),
                                                       Range (Range),
                                                       SemanticTokenAbsolute (SemanticTokenAbsolute),
                                                       SemanticTokens,
                                                       defaultSemanticTokensLegend,
                                                       makeSemanticTokens)
import           Prelude                              hiding (length, span)

---------------------------------------------------------

-- * extract semantic map from HieAst for local variables

---------------------------------------------------------

mkLocalIdSemanticFromAst :: Set Identifier -> HieFunMaskKind a -> RefMap a -> IdSemanticMap
mkLocalIdSemanticFromAst names hieKind rm = M.mapMaybe (idIdSemanticFromHie hieKind rm) $ M.fromSet id names

idIdSemanticFromHie :: forall a. HieFunMaskKind a -> RefMap a -> Identifier -> Maybe HsSemanticTokenType
idIdSemanticFromHie _ _ (Left _) = Just TModule
idIdSemanticFromHie hieKind rm ns = do
  idSemanticFromRefMap rm ns
  where
    idSemanticFromRefMap :: RefMap a -> Identifier -> Maybe HsSemanticTokenType
    idSemanticFromRefMap rm' name' = do
      spanInfos <- M.lookup name' rm'
      let typeTokenType = foldMap (typeSemantic hieKind) $ listToMaybe $ mapMaybe (identType . snd) spanInfos
      contextInfoTokenType <- foldMap (contextInfosMaybeTokenType . identInfo . snd) spanInfos
      fold [typeTokenType, Just contextInfoTokenType, idInfixOperator ns]

    contextInfosMaybeTokenType :: Set.Set ContextInfo -> Maybe HsSemanticTokenType
    contextInfosMaybeTokenType details = foldMap infoTokenType (Set.toList details)


-------------------------------------------------

-- * extract semantic tokens from IdSemanticMap

-------------------------------------------------

extractSemanticTokensFromNames :: IdSemanticMap -> RangeIdSetMap -> M.Map Range HsSemanticTokenType
extractSemanticTokensFromNames nsm = M.mapMaybe (foldMap (`M.lookup` nsm))

rangeSemanticMapSemanticTokens :: SemanticTokensConfig -> PositionMapping -> M.Map Range HsSemanticTokenType -> Either Text SemanticTokens
rangeSemanticMapSemanticTokens stc mapping =
  makeSemanticTokens defaultSemanticTokensLegend
    . mapMaybe (\(range, ty) -> flip toAbsSemanticToken ty <$> range)
    . M.toAscList
    . M.mapKeys (toCurrentRange mapping)
  where
    toAbsSemanticToken :: Range -> HsSemanticTokenType -> SemanticTokenAbsolute
    toAbsSemanticToken (Range (Position startLine startColumn) (Position _endLine endColumn)) tokenType =
      let len = endColumn - startColumn
       in SemanticTokenAbsolute
            (fromIntegral startLine)
            (fromIntegral startColumn)
            (fromIntegral len)
            (toLspTokenType stc tokenType)
            []
