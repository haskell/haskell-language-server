{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE DeriveGeneric       #-}
{-# LANGUAGE DerivingVia         #-}
{-# LANGUAGE GADTs               #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE InstanceSigs        #-}
{-# LANGUAGE OverloadedLabels    #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE StrictData          #-}
{-# LANGUAGE TypeFamilies        #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# LANGUAGE FlexibleInstances   #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE StandaloneDeriving  #-}
{-# LANGUAGE TemplateHaskell     #-}

module Ide.Plugin.SemanticTokens.Types where

import           Control.DeepSeq               (NFData (rnf), rwhnf)
import           Control.Monad.Identity        (Identity (..))
import           Data.Aeson                    (FromJSON (parseJSON),
                                                Options (..), ToJSON,
                                                defaultOptions,
                                                genericParseJSON, genericToJSON)
import           Data.Aeson.Types              (ToJSON (toJSON))
import qualified Data.Array                    as A
import           Data.Char                     (toLower)
import           Data.Default                  (Default (def))
import           Data.Generics                 (Typeable)
import qualified Data.Map                      as M
import           Data.Maybe                    (fromMaybe)
import           Development.IDE               (Pretty (pretty), RuleResult)
import qualified Development.IDE.Core.Shake    as Shake
import           Development.IDE.GHC.Compat    hiding (loc)
import           Development.IDE.Graph.Classes (Hashable)
import           GHC.Generics                  (Generic)
import           Ide.Plugin.Properties         (Properties,
                                                PropertyKey (PropertyKey),
                                                PropertyType (TObject),
                                                defineObjectProperty,
                                                emptyProperties, (&))
import           Language.LSP.Protocol.Types
import qualified Rank2
import qualified Rank2.TH

-- !!!! order of declarations matters deriving enum and ord
-- since token may come from different source and we want to keep the most specific one
-- and we might want to merge them.
data HsSemanticTokenType
  = TVariable -- none function variable
  | TFunction -- function
  | TDataCon -- Data constructor
  | TTypeVariable -- Type variable
  | TClassMethod -- Class method
  | TPatternSyn -- Pattern synonym
  | TTypeCon -- Type (Type constructor)
  | TClass -- Type class
  | TTypeSyn -- Type synonym
  | TTypeFamily -- type family
  | TRecField -- from match bind
  deriving (Eq, Ord, Show, Enum, Bounded)

type SemanticTokensConfig = SemanticTokensConfig_ Identity
instance Default SemanticTokensConfig where
  def = STC
      { stFunction = Identity SemanticTokenTypes_Function
      , stVariable = Identity SemanticTokenTypes_Variable
      , stDataCon = Identity SemanticTokenTypes_EnumMember
      , stTypeVariable = Identity SemanticTokenTypes_TypeParameter
      , stClassMethod = Identity SemanticTokenTypes_Method
      -- pattern syn is like a limited version of macro of constructing a term
      , stPatternSyn = Identity SemanticTokenTypes_Macro
        -- normal data type is a tagged union type look like enum type
        -- and a record is a product type like struct
        -- but we don't distinguish them yet
      , stTypeCon = Identity SemanticTokenTypes_Enum
      , stClass = Identity SemanticTokenTypes_Class
      , stTypeSyn = Identity SemanticTokenTypes_Type
      , stTypeFamily = Identity SemanticTokenTypes_Interface
      , stRecField = Identity SemanticTokenTypes_Property
      }
-- | SemanticTokensConfig_ is a configuration for the semantic tokens plugin.
-- it contains map between the hs semantic token type and default token type.
data SemanticTokensConfig_ f = STC
  { stFunction     :: !(f SemanticTokenTypes)
  , stVariable     :: !(f SemanticTokenTypes)
  , stDataCon      :: !(f SemanticTokenTypes)
  , stTypeVariable :: !(f SemanticTokenTypes)
  , stClassMethod  :: !(f SemanticTokenTypes)
  , stPatternSyn   :: !(f SemanticTokenTypes)
  , stTypeCon      :: !(f SemanticTokenTypes)
  , stClass        :: !(f SemanticTokenTypes)
  , stTypeSyn      :: !(f SemanticTokenTypes)
  , stTypeFamily   :: !(f SemanticTokenTypes)
  , stRecField     :: !(f SemanticTokenTypes)
  } deriving Generic
$(Rank2.TH.deriveAll ''SemanticTokensConfig_)

withDef :: SemanticTokensConfig  -> SemanticTokensConfig_ Maybe -> SemanticTokensConfig
withDef = Rank2.liftA2 (\x y -> Identity (fromMaybe (runIdentity x) y))
instance FromJSON SemanticTokensConfig where parseJSON = fmap (withDef def) . parseJSON
stOption :: Options
stOption = defaultOptions { fieldLabelModifier = map toLower . drop 2 }
instance FromJSON (SemanticTokensConfig_ Maybe) where parseJSON = genericParseJSON stOption
instance ToJSON (SemanticTokensConfig_ Maybe) where toJSON = genericToJSON stOption
instance ToJSON SemanticTokensConfig where toJSON = genericToJSON stOption

semanticConfigProperties :: Properties '[ 'PropertyKey "tokenMapping" ('TObject SemanticTokensConfig)]
semanticConfigProperties =
  emptyProperties
    & defineObjectProperty
      #tokenMapping
      "Configuration of map from hs semantic token type to LSP default token type"
      def
deriving instance Show SemanticTokensConfig

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

type NameSemanticMap = NameEnv HsSemanticTokenType

data GetSemanticTokens = GetSemanticTokens
  deriving (Eq, Show, Typeable, Generic)

instance Hashable GetSemanticTokens

instance NFData GetSemanticTokens

data RangeHsSemanticTokenTypes = RangeHsSemanticTokenTypes {rangeSemanticMap :: M.Map Range HsSemanticTokenType}

instance NFData RangeHsSemanticTokenTypes where
  rnf :: RangeHsSemanticTokenTypes -> ()
  rnf (RangeHsSemanticTokenTypes a) = rwhnf a

instance Show RangeHsSemanticTokenTypes where
  show = const "GlobalNameMap"

type instance RuleResult GetSemanticTokens = RangeHsSemanticTokenTypes

data HieFunMaskKind kind where
  HieFreshFun :: HieFunMaskKind Type
  HieFromDiskFun :: A.Array TypeIndex Bool -> HieFunMaskKind TypeIndex

data SemanticLog
  = LogShake Shake.Log
  | LogNoAST FilePath
  | LogConfig (SemanticTokensConfig )
  | LogMsg String
  | LogNoVF
  deriving (Show)

instance Pretty SemanticLog where
  pretty theLog = case theLog of
    LogShake shakeLog -> pretty shakeLog
    LogNoAST path     -> "no HieAst exist for file" <> pretty path
    LogNoVF           -> "no VirtualSourceFile exist for file"
    LogConfig config  -> "SemanticTokensConfig_: " <> pretty (show config)
    LogMsg msg        -> "SemanticLog Debug Message: " <> pretty msg

