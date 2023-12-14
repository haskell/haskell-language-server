{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE StrictData         #-}

module Ide.Plugin.SemanticTokens.Types where


import qualified Data.List                   as List
import qualified Data.List.Extra             as List
import qualified Data.List.NonEmpty          as NE
import qualified Data.Map                    as Map
import           Data.Maybe                  (fromMaybe)
import qualified Data.Set                    as Set
import           Development.IDE.GHC.Compat
-- import           GHC.Enum                    (boundedEnumFrom)
import           Language.LSP.Protocol.Types


-- data SemanticTokenType = SClass | SVariable | STypeVar | SDataCon | SNothing | SFieldName  deriving (Eq, Ord)
-- !!!! order of declarations matters deriving enum and ord
-- since token may come from different source and we want to keep the most specific one
-- and we might want to merge them.
data SemanticTokenType =
    TNothing -- unknown
    -- | TVariable -- fallback
    -- since many thing can be valbind. we put it as less priority
    | TValBind -- valBind instance bind or regular bind
    | TPatternBind -- PatternBind, parameters, as values in let/in, case, lambda
    | TDataCon -- Data constructor
    | TTypeVariable -- Type variable
    | TClassMethod -- Class method
    | TPatternSyn -- Pattern synonym
    | TTypeCon -- Type (Type constructor)
    | TClass -- Class (ConstraUInt constructor)
    | TTypeSyn -- Type synonym (Non-local is not captured)
    | TTypeFamily -- type family
    | TRecField -- from match bind
    deriving (Eq, Ord, Show, Enum, Bounded)

instance Semigroup SemanticTokenType where
    -- one in higher enum is more specific
    a <> b = max a b
instance Monoid SemanticTokenType where
    mempty = TNothing


type ActualToken = (UInt, UInt, UInt, SemanticTokenType, UInt)
-- { line: 2, startChar 5, length: 3, tokenType: SemanticTokenType, tokenModifiers: 3, string},

data SemanticTokenOriginal =  SemanticTokenOriginal
  { tokenType :: SemanticTokenType,
    loc       :: Loc,
    name      :: String
  }
  deriving (Show, Eq, Ord)

data Loc = Loc
  { line      :: UInt,
    startChar :: UInt,
    len       :: UInt
  }
  deriving (Show, Eq, Ord)


