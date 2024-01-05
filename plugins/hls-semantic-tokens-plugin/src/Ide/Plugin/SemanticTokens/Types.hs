{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE GADTs             #-}
{-# LANGUAGE InstanceSigs      #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StrictData        #-}
{-# LANGUAGE TypeFamilies      #-}

module Ide.Plugin.SemanticTokens.Types where

import           Control.DeepSeq               (NFData (rnf), rwhnf)
import qualified Data.Array                    as A
import           Data.Generics                 (Typeable)
import qualified Data.Map                      as M
import           Development.IDE               (Pretty (pretty), RuleResult)
import qualified Development.IDE.Core.Shake    as Shake
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

data SemanticLog = LogShake Shake.Log
    | LogNoAST FilePath
    | LogNoVF
      deriving Show

instance Pretty SemanticLog where
    pretty theLog = case theLog of
        LogShake shakeLog -> pretty shakeLog
        LogNoAST path     -> "no HieAst exist for file" <> pretty path
        LogNoVF           -> "no VirtualSourceFile exist for file"

