{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE OverloadedLabels      #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE TemplateHaskellQuotes #-}
{-# LANGUAGE TypeFamilies          #-}

module Ide.Plugin.SemanticTokens.SemanticConfig where

import           Data.Char                       (toLower)
import           Data.Default                    (def)
import qualified Data.Set                        as S
import           Data.Text                       (Text)
import qualified Data.Text                       as T
import           Development.IDE                 (Action, usePropertyAction)
import           GHC.TypeLits                    (KnownSymbol)
import           Ide.Plugin.Properties           (KeyNameProxy, NotElem,
                                                  Properties,
                                                  PropertyKey (type PropertyKey),
                                                  PropertyType (type TEnum),
                                                  defineEnumProperty,
                                                  emptyProperties)
import           Ide.Plugin.SemanticTokens.Types
import           Ide.Types                       (PluginId)
import           Language.Haskell.TH
import           Language.LSP.Protocol.Types     (LspEnum (..),
                                                  SemanticTokenTypes)

docName :: HsTokenType -> T.Text
docName tt = case tt of
  HsSemanticTokenType TVariable        -> "variables"
  HsSemanticTokenType TFunction        -> "functions"
  HsSemanticTokenType TDataConstructor -> "data constructors"
  HsSemanticTokenType TTypeVariable    -> "type variables"
  HsSemanticTokenType TClassMethod     -> "typeclass methods"
  HsSemanticTokenType TPatternSynonym  -> "pattern synonyms"
  HsSemanticTokenType TTypeConstructor -> "type constructors"
  HsSemanticTokenType TClass           -> "typeclasses"
  HsSemanticTokenType TTypeSynonym     -> "type synonyms"
  HsSemanticTokenType TTypeFamily      -> "type families"
  HsSemanticTokenType TRecordField     -> "record fields"
  HsSemanticTokenType TModule          -> "modules"
  HsSemanticTokenType TOperator        -> "operators"
  HsSyntacticTokenType TKeyword        -> "keyword"
  HsSyntacticTokenType TStringLit      -> "string literal"
  HsSyntacticTokenType TComment        -> "comment"
  HsSyntacticTokenType TCharLit        -> "char literal"
  HsSyntacticTokenType TNumberLit      -> "number literal"
  HsSyntacticTokenType TRecordSelector -> "record selector"

toConfigName :: String -> String
toConfigName = ("st" <>)

type LspTokenTypeDescriptions = [(SemanticTokenTypes, T.Text)]

lspTokenTypeDescriptions :: LspTokenTypeDescriptions
lspTokenTypeDescriptions =
  map
    ( \x ->
        (x, "LSP Semantic Token Type: " <> toEnumBaseType x)
    )
    $ S.toList knownValues

allHsTokenTypes :: [HsTokenType]
allHsTokenTypes = map HsSemanticTokenType (enumFrom minBound) <> map HsSyntacticTokenType (enumFrom minBound)

lowerFirst :: String -> String
lowerFirst []       = []
lowerFirst (x : xs) = toLower x : xs

-- TODO: drop the "syntax/semanticness" before showing
allHsTokenNameStrings :: [String]
allHsTokenNameStrings = map (unwrap $ drop 1 . show) allHsTokenTypes
  where
    unwrap :: (forall a. Show a => a -> String) -> HsTokenType -> String
    unwrap k tt' = case tt' of
      HsSemanticTokenType tt  -> k tt
      HsSyntacticTokenType tt -> k tt

defineSemanticProperty ::
  (NotElem s r, KnownSymbol s) =>
  (KeyNameProxy s, Text, SemanticTokenTypes) ->
  Properties r ->
  Properties ('PropertyKey s (TEnum SemanticTokenTypes) : r)
defineSemanticProperty (lb, tokenType, st) =
  defineEnumProperty
    lb
    tokenType
    lspTokenTypeDescriptions
    st

semanticDef :: SemanticTokensConfig
semanticDef = def

-- | it produces the following functions:
-- semanticConfigProperties :: Properties '[
-- 'PropertyKey "Variable" ('TEnum SemanticTokenTypes),
-- ...
-- ]
-- useSemanticConfigAction :: PluginId -> Action SemanticTokensConfig
mkSemanticConfigFunctions :: Q [Dec]
mkSemanticConfigFunctions = do
  let pid = mkName "pid"
  let semanticConfigPropertiesName = mkName "semanticConfigProperties"
  let useSemanticConfigActionName = mkName "useSemanticConfigAction"
  let allLabelStrs = map ((<> "Token") . lowerFirst) allHsTokenNameStrings
      allLabels = map (LabelE . (<> "Token") . lowerFirst) allHsTokenNameStrings
      allFieldsNames = map (mkName . toConfigName) allHsTokenNameStrings
      allVariableNames = map (mkName . ("_variable_" <>) . toConfigName) allHsTokenNameStrings
      --   <- useSemanticConfigAction label pid config
      mkGetProperty (variable, label) =
        BindS
          (VarP variable)
          (AppE (VarE 'usePropertyAction) label `AppE` VarE pid `AppE` VarE semanticConfigPropertiesName)
      getProperties = zipWith (curry mkGetProperty) allVariableNames allLabels
      recordUpdate =
        RecUpdE (VarE 'semanticDef) $
          zipWith (\fieldName variableName -> (fieldName, VarE variableName)) allFieldsNames allVariableNames
      -- get and then update record
      bb = DoE Nothing $ getProperties ++ [NoBindS $ AppE (VarE 'return) recordUpdate]
  let useSemanticConfigAction = FunD useSemanticConfigActionName [Clause [VarP pid] (NormalB bb) []]
  let useSemanticConfigActionSig = SigD useSemanticConfigActionName (ArrowT `AppT` ConT ''PluginId `AppT` (ConT ''Action `AppT` ConT ''SemanticTokensConfig))

  -- SemanticConfigProperties
  nameAndDescList <-
    mapM
      ( \(lb, x) -> do
          desc <- [|"LSP semantic token type to use for " <> docName x|]
          lspToken <- [|toLspTokenType def x|]
          return $ TupE [Just lb, Just desc, Just lspToken]
      )
      $ zip allLabels allHsTokenTypes
  let body = foldr (AppE . AppE (VarE 'defineSemanticProperty)) (VarE 'emptyProperties) nameAndDescList
  let propertiesType =
        foldr
          ( \la ->
              AppT
                ( PromotedConsT
                    `AppT` (AppT (ConT 'PropertyKey) (LitT (StrTyLit la)) `AppT` AppT (ConT 'TEnum) (ConT ''SemanticTokenTypes))
                )
          )
          PromotedNilT
          allLabelStrs
  let semanticConfigProperties = FunD semanticConfigPropertiesName [Clause [] (NormalB body) []]
  let semanticConfigPropertiesSig = SigD semanticConfigPropertiesName (AppT (ConT ''Properties) propertiesType)
  return [semanticConfigPropertiesSig, semanticConfigProperties, useSemanticConfigActionSig, useSemanticConfigAction]
