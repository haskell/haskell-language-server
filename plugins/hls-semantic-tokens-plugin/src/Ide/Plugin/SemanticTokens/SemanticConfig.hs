{-# LANGUAGE GADTs             #-}
{-# LANGUAGE OverloadedLabels  #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE TypeFamilies      #-}
{-# LANGUAGE TypeOperators     #-}

module Ide.Plugin.SemanticTokens.SemanticConfig where

import           Data.Default                    (def)
import qualified Data.Set                        as S
import qualified Data.Text                       as T
import           Development.IDE                 (usePropertyAction)
import           Ide.Plugin.Properties           (defineEnumProperty,
                                                  emptyProperties)
import           Ide.Plugin.SemanticTokens.Types
import           Language.Haskell.TH
import           Language.LSP.Protocol.Types     (LspEnum (..),
                                                  SemanticTokenTypes)

toConfigName :: String -> String
toConfigName = ("st" <>)

-- lspTokenTypeDescription :: [(SemanticTokenTypes, String)]
type LspTokenTypeDescriptions = [(SemanticTokenTypes, T.Text)]

lspTokenTypeDescriptions :: LspTokenTypeDescriptions
lspTokenTypeDescriptions =
  map
    ( \x ->
        (x, "LSP Semantic Token Type:" <> toEnumBaseType x)
    )
    $ S.toList knownValues

allHsTokenTypes :: [HsSemanticTokenType]
allHsTokenTypes = enumFrom minBound

allHsTokenNameStrings :: [String]
allHsTokenNameStrings = map (drop 1 . show) allHsTokenTypes

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
  let
      allLabels = map LabelE allHsTokenNameStrings
      allFieldsNames = map (mkName . toConfigName) allHsTokenNameStrings
      allVariableNames = map (mkName . ("variable_" <>) . toConfigName) allHsTokenNameStrings
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

  -- SemanticConfigProperties
  nameAndDescList <-
    mapM
      ( \(lb, x) -> do
          desc <- [|"LSP semantic token type to use for " <> T.pack (drop 1 $ show x)|]
          lspToken <- [|toLspTokenType def x|]
          return $ TupE [Just lb, Just desc, Just lspToken]
      )
      $ zip allLabels allHsTokenTypes
  let body = foldr (AppE . AppE (VarE 'defineSemanticProperty)) (VarE 'emptyProperties) nameAndDescList
  let semanticConfigProperties = FunD semanticConfigPropertiesName [Clause [] (NormalB body) []]
  return [semanticConfigProperties, useSemanticConfigAction]
