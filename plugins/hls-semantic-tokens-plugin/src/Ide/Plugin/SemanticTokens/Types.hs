{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE StrictData    #-}
{-# LANGUAGE TypeFamilies  #-}

module Ide.Plugin.SemanticTokens.Types where

import           Control.DeepSeq               (NFData (rnf), rwhnf)
import           Data.Generics                 (Typeable)
import           Development.IDE               (RuleResult)
import           Development.IDE.GHC.Compat    hiding (loc)
import           Development.IDE.Graph.Classes (Hashable)
import           GHC.Generics                  (Generic)
import           Language.LSP.Protocol.Types

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

instance Semigroup HsSemanticTokenType where
  -- one in higher enum is more specific
  a <> b = max a b

data SemanticTokenOriginal = SemanticTokenOriginal
  { _tokenType :: HsSemanticTokenType,
    _loc       :: Loc,
    _name      :: String
  }
  deriving (Eq, Ord)

--
instance Show SemanticTokenOriginal where
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

data RangeHsSemanticTokenTypes = RangeHsSemanticTokenTypes {tokens :: [(Range, HsSemanticTokenType)]}

instance NFData RangeHsSemanticTokenTypes where
  rnf (RangeHsSemanticTokenTypes a) = rwhnf a

instance Show RangeHsSemanticTokenTypes where
  show = const "GlobalNameMap"

type instance RuleResult GetSemanticTokens = RangeHsSemanticTokenTypes
