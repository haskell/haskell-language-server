{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings   #-}

-- |
-- The query module is used to query the semantic tokens from the AST
module Ide.Plugin.SemanticTokens.Query where

import           Control.Applicative                  ((<|>))
import           Data.Foldable                        (fold)
import qualified Data.Map.Strict                      as M
import           Data.Maybe                           (listToMaybe, mapMaybe)
import qualified Data.Set                             as Set
import           Data.Text                            (Text)
import           Development.IDE.Core.PositionMapping (PositionMapping,
                                                       toCurrentRange)
import           Development.IDE.GHC.Compat
import           Ide.Plugin.SemanticTokens.Mappings
import           Ide.Plugin.SemanticTokens.Types      (HieFunMaskKind,
                                                       HsSemanticTokenType (TModule),
                                                       RangeSemanticTokenTypeList,
                                                       SemanticTokensConfig)
import           Language.LSP.Protocol.Types          (Position (Position),
                                                       Range (Range),
                                                       SemanticTokenAbsolute (SemanticTokenAbsolute),
                                                       SemanticTokens,
                                                       defaultSemanticTokensLegend,
                                                       makeSemanticTokens)
import           Prelude                              hiding (length, span)

---------------------------------------------------------

-- * extract semantic

---------------------------------------------------------

idSemantic :: forall a. NameEnv TyThing -> HieFunMaskKind a -> RefMap a -> Identifier -> Maybe HsSemanticTokenType
idSemantic _ _ _ (Left _) = Just TModule
idSemantic tyThingMap hieKind rm (Right n) =
    nameSemanticFromHie hieKind rm n -- local name
    <|> (lookupNameEnv tyThingMap n >>= tyThingSemantic) -- global name


---------------------------------------------------------

-- * extract semantic from HieAst for local variables

---------------------------------------------------------

nameSemanticFromHie :: forall a. HieFunMaskKind a -> RefMap a -> Name -> Maybe HsSemanticTokenType
nameSemanticFromHie hieKind rm n = do
  idSemanticFromRefMap rm (Right n)
  where
    idSemanticFromRefMap :: RefMap a -> Identifier -> Maybe HsSemanticTokenType
    idSemanticFromRefMap rm' name' = do
      spanInfos <- M.lookup name' rm'
      let typeTokenType = foldMap (typeSemantic hieKind) $ listToMaybe $ mapMaybe (identType . snd) spanInfos
      contextInfoTokenType <- foldMap (contextInfosMaybeTokenType . identInfo . snd) spanInfos
      fold [typeTokenType, Just contextInfoTokenType, nameInfixOperator n]

    contextInfosMaybeTokenType :: Set.Set ContextInfo -> Maybe HsSemanticTokenType
    contextInfosMaybeTokenType details = foldMap infoTokenType (Set.toList details)


-------------------------------------------------

-- * extract lsp semantic tokens from RangeSemanticTokenTypeList

-------------------------------------------------

rangeSemanticsSemanticTokens :: SemanticTokensConfig -> PositionMapping -> RangeSemanticTokenTypeList -> Either Text SemanticTokens
rangeSemanticsSemanticTokens stc mapping =
  makeSemanticTokens defaultSemanticTokensLegend
    . mapMaybe (\(ran, tk) -> toAbsSemanticToken <$> toCurrentRange mapping ran <*> return tk)
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
