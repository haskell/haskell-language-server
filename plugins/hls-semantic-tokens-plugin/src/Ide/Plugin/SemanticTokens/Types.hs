{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE StrictData         #-}
{-# LANGUAGE TypeFamilies       #-}

module Ide.Plugin.SemanticTokens.Types where


import qualified Data.List                     as List
import qualified Data.List.Extra               as List
import qualified Data.List.NonEmpty            as NE
import qualified Data.Map                      as Map
import           Data.Maybe                    (fromMaybe)
import qualified Data.Set                      as Set
import           Development.IDE.GHC.Compat
-- import           GHC.Enum                    (boundedEnumFrom)
import           Control.DeepSeq               (NFData (rnf), rwhnf)
import           Data.Generics                 (Typeable)
import           Development.IDE               (RuleResult)
import           Development.IDE.Graph.Classes (Hashable)
import           GHC.Generics                  (Generic)
import           Language.LSP.Protocol.Types


-- !!!! order of declarations matters deriving enum and ord
-- since token may come from different source and we want to keep the most specific one
-- and we might want to merge them.
data SemanticTokenType =
    TNothing -- unknown
    -- | TVariable -- fallback
    -- since many thing can be valbind. we put it as less priority
    | TVariable -- valBind instance bind or regular bind
    | TFunction -- function
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


type NameSemanticMap = NameEnv SemanticTokenType
data GetGlobalNameSemantic = GetGlobalNameSemantic
    deriving (Eq, Show, Typeable, Generic)
instance Hashable GetGlobalNameSemantic
instance NFData   GetGlobalNameSemantic

data GlobalTokenTypeMap = GTTMap {getNameSemanticMap :: !NameSemanticMap }
instance NFData GlobalTokenTypeMap where
    rnf (GTTMap a) = rwhnf a
instance Show GlobalTokenTypeMap where
    show = const "GlobalNameMap"
type instance RuleResult GetGlobalNameSemantic = GlobalTokenTypeMap
