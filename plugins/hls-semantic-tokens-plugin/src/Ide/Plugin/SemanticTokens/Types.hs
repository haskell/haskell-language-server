{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE DeriveGeneric       #-}
{-# LANGUAGE DerivingVia         #-}
{-# LANGUAGE FlexibleInstances   #-}
{-# LANGUAGE GADTs               #-}

{-# LANGUAGE InstanceSigs        #-}

{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE RankNTypes          #-}


{-# LANGUAGE StrictData          #-}

{-# LANGUAGE TypeFamilies        #-}


{-# LANGUAGE DeriveLift          #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Ide.Plugin.SemanticTokens.Types where

import           Control.DeepSeq               (NFData (rnf), rwhnf)
import qualified Data.Array                    as A
import           Data.Default                  (Default (def))
import           Data.Generics                 (Typeable)
import qualified Data.Map                      as M
import           Development.IDE               (Pretty (pretty), RuleResult)
import qualified Development.IDE.Core.Shake    as Shake
import           Development.IDE.GHC.Compat    hiding (loc)
import           Development.IDE.Graph.Classes (Hashable)
import           GHC.Generics                  (Generic)
import           Language.LSP.Protocol.Types
-- import template haskell
import           Language.Haskell.TH.Syntax    (Lift)


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
  deriving (Eq, Ord, Show, Enum, Bounded, Generic, Lift)

-- type SemanticTokensConfig = SemanticTokensConfig_ Identity
instance Default SemanticTokensConfig where
  def = STC
      { stFunction = SemanticTokenTypes_Function
      , stVariable = SemanticTokenTypes_Variable
      , stDataCon = SemanticTokenTypes_EnumMember
      , stTypeVariable = SemanticTokenTypes_TypeParameter
      , stClassMethod = SemanticTokenTypes_Method
      -- pattern syn is like a limited version of macro of constructing a term
      , stPatternSyn = SemanticTokenTypes_Macro
        -- normal data type is a tagged union type look like enum type
        -- and a record is a product type like struct
        -- but we don't distinguish them yet
      , stTypeCon = SemanticTokenTypes_Enum
      , stClass = SemanticTokenTypes_Class
      , stTypeSyn = SemanticTokenTypes_Type
      , stTypeFamily = SemanticTokenTypes_Interface
      , stRecField = SemanticTokenTypes_Property
      }
-- | SemanticTokensConfig_ is a configuration for the semantic tokens plugin.
-- it contains map between the hs semantic token type and default token type.
data SemanticTokensConfig = STC
  { stFunction     :: !SemanticTokenTypes
  , stVariable     :: !SemanticTokenTypes
  , stDataCon      :: !SemanticTokenTypes
  , stTypeVariable :: !SemanticTokenTypes
  , stClassMethod  :: !SemanticTokenTypes
  , stPatternSyn   :: !SemanticTokenTypes
  , stTypeCon      :: !SemanticTokenTypes
  , stClass        :: !SemanticTokenTypes
  , stTypeSyn      :: !SemanticTokenTypes
  , stTypeFamily   :: !SemanticTokenTypes
  , stRecField     :: !SemanticTokenTypes
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
  | LogConfig SemanticTokensConfig
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

