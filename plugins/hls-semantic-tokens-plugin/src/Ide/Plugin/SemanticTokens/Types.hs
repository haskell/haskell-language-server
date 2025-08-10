{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE DerivingVia       #-}
{-# LANGUAGE GADTs             #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StrictData        #-}
{-# LANGUAGE TypeFamilies      #-}

module Ide.Plugin.SemanticTokens.Types where

import           Control.DeepSeq               (NFData (rnf), rwhnf)
import qualified Data.Array                    as A
import           Data.Default                  (Default (def))
import           Data.Text                     (Text)
import           Development.IDE               (Pretty (pretty), RuleResult)
import qualified Development.IDE.Core.Shake    as Shake
import           Development.IDE.GHC.Compat    hiding (loc)
import           Development.IDE.Graph.Classes (Hashable)
import           GHC.Generics                  (Generic)
import           GHC.Iface.Ext.Types           (TypeIndex)
import           Ide.Plugin.Error              (PluginError)
import           Language.Haskell.TH.Syntax    (Lift)
import           Language.LSP.Protocol.Types


-- !!!! order of declarations matters deriving enum and ord
-- since token may come from different source and we want to keep the most specific one
-- and we might want to merge them.
data HsSemanticTokenType
  = TVariable -- none function variable
  | TFunction -- function
  | TDataConstructor -- Data constructor
  | TTypeVariable -- Type variable
  | TClassMethod -- Class method
  | TPatternSynonym -- Pattern synonym
  | TTypeConstructor -- Type (Type constructor)
  | TClass -- Type class
  | TTypeSynonym -- Type synonym
  | TTypeFamily -- type family
  | TRecordField -- from match bind
  | TOperator-- operator
  | TModule -- module name
  deriving (Eq, Ord, Show, Enum, Bounded, Generic, Lift)

-- type SemanticTokensConfig = SemanticTokensConfig_ Identity
instance Default SemanticTokensConfig where
  def = STC
      { stFunction = SemanticTokenTypes_Function
      , stVariable = SemanticTokenTypes_Variable
      , stDataConstructor = SemanticTokenTypes_EnumMember
      , stTypeVariable = SemanticTokenTypes_TypeParameter
      , stClassMethod = SemanticTokenTypes_Method
      -- pattern syn is like a limited version of macro of constructing a term
      , stPatternSynonym = SemanticTokenTypes_Macro
        -- normal data type is a tagged union type look like enum type
        -- and a record is a product type like struct
        -- but we don't distinguish them yet
      , stTypeConstructor = SemanticTokenTypes_Enum
      , stClass = SemanticTokenTypes_Class
      , stTypeSynonym = SemanticTokenTypes_Type
      , stTypeFamily = SemanticTokenTypes_Interface
      , stRecordField = SemanticTokenTypes_Property
      , stModule = SemanticTokenTypes_Namespace
      , stOperator = SemanticTokenTypes_Operator
      }
-- | SemanticTokensConfig_ is a configuration for the semantic tokens plugin.
-- it contains map between the hs semantic token type and default token type.
data SemanticTokensConfig = STC
  { stFunction        :: !SemanticTokenTypes
  , stVariable        :: !SemanticTokenTypes
  , stDataConstructor :: !SemanticTokenTypes
  , stTypeVariable    :: !SemanticTokenTypes
  , stClassMethod     :: !SemanticTokenTypes
  , stPatternSynonym  :: !SemanticTokenTypes
  , stTypeConstructor :: !SemanticTokenTypes
  , stClass           :: !SemanticTokenTypes
  , stTypeSynonym     :: !SemanticTokenTypes
  , stTypeFamily      :: !SemanticTokenTypes
  , stRecordField     :: !SemanticTokenTypes
  , stModule          :: !SemanticTokenTypes
  , stOperator        :: !SemanticTokenTypes
  } deriving (Generic, Show)


instance Semigroup HsSemanticTokenType where
  -- one in higher enum is more specific
  a <> b = max a b

data SemanticTokenOriginal tokenType = SemanticTokenOriginal
  { _tokenType :: tokenType,
    _loc       :: Loc,
    _name      :: String
  }
  deriving (Eq, Ord)

--
instance (Show tokenType) => Show (SemanticTokenOriginal tokenType) where
  show (SemanticTokenOriginal tk loc name) = show loc <> " " <> show tk <> " " <> show name

data Loc = Loc
  { _line      :: UInt,
    _startChar :: UInt,
    _len       :: UInt
  }
  deriving (Eq, Ord)

instance Show Loc where
  show (Loc line startChar len) = show line <> ":" <> show startChar <> "-" <> show (startChar + len)

data GetSemanticTokens = GetSemanticTokens
  deriving (Eq, Show, Generic)

instance Hashable GetSemanticTokens

instance NFData GetSemanticTokens

type RangeSemanticTokenTypeList = [(Range, HsSemanticTokenType)]

newtype RangeHsSemanticTokenTypes = RangeHsSemanticTokenTypes {rangeSemanticList :: RangeSemanticTokenTypeList}

instance NFData RangeHsSemanticTokenTypes where
  rnf :: RangeHsSemanticTokenTypes -> ()
  rnf (RangeHsSemanticTokenTypes a) = rwhnf a

instance Show RangeHsSemanticTokenTypes where
  show (RangeHsSemanticTokenTypes xs) = unlines $ map showRangeToken xs

showRangeToken :: (Range, HsSemanticTokenType) -> String
showRangeToken (ran, tk) = showRange ran <> " " <> show tk
showRange :: Range -> String
showRange (Range (Position l1 c1) (Position l2 c2)) = show l1 <> ":" <> show c1 <> "-" <> show l2 <> ":" <> show c2

type instance RuleResult GetSemanticTokens = RangeHsSemanticTokenTypes

data HieFunMaskKind kind where
  HieFreshFun :: HieFunMaskKind Type
  HieFromDiskFun :: A.Array TypeIndex Bool -> HieFunMaskKind TypeIndex

data SemanticLog
  = LogShake Shake.Log
  | LogDependencyError PluginError
  | LogNoAST FilePath
  | LogConfig SemanticTokensConfig
  | LogMsg String
  | LogNoVF
  | LogSemanticTokensDeltaMisMatch Text (Maybe Text)

instance Pretty SemanticLog where
  pretty theLog = case theLog of
    LogShake shakeLog -> pretty shakeLog
    LogNoAST path     -> "no HieAst exist for file" <> pretty path
    LogNoVF           -> "no VirtualSourceFile exist for file"
    LogConfig config  -> "SemanticTokensConfig_: " <> pretty (show config)
    LogMsg msg        -> "SemanticLog Debug Message: " <> pretty msg
    LogSemanticTokensDeltaMisMatch previousIdFromRequest previousIdFromCache
                      -> "SemanticTokensDeltaMisMatch: previousIdFromRequest: " <> pretty previousIdFromRequest
                      <> " previousIdFromCache: " <> pretty previousIdFromCache
    LogDependencyError err -> "SemanticTokens' dependency error: " <> pretty err


type SemanticTokenId = Text
