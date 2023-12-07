{-# LANGUAGE StandaloneDeriving #-}
module Ide.Plugin.SemanticTokens.Types where


import qualified Data.List                   as List
import qualified Data.Set                    as Set
import           Development.IDE.GHC.Compat
import           Language.LSP.Protocol.Types

x = defaultSemanticTokensLegend

-- mapping from our token type to LSP default token type
toLspTokenType :: SemanticTokenType -> SemanticTokenTypes
toLspTokenType tk = case tk of
    -------
    -- term
    -------
    TVariable     -> SemanticTokenTypes_Variable
    TDataCon      -> SemanticTokenTypes_EnumMember
    TRecField     -> SemanticTokenTypes_Property
    -- most likely a function
    TValBind      -> SemanticTokenTypes_Function

    -------
    -- type
    -------
    TClass        -> SemanticTokenTypes_Class
    TClassMethod  -> SemanticTokenTypes_Method
    TPatternBind  -> SemanticTokenTypes_Parameter
    TTypeVariable -> SemanticTokenTypes_TypeParameter

    -- data type is a likely a tagged sum type since we choose data constructor as enum member
    -- we choose enum for data type

    -- but we are not distinguishing between data and record
    -- maybe a record type - struct ?
    TTypeCon      -> SemanticTokenTypes_Enum

    -------------------------------------
    -- wiggle not sure if this is correct choice
    -------------------------------------
    TTypeFamily   -> SemanticTokenTypes_Interface
    -- is a likely a data constructor result
    TPatternSyn   -> SemanticTokenTypes_Event
    -- let it falls to other type
    TTypeSyn      -> SemanticTokenTypes_Type
    TNothing      -> SemanticTokenTypes_Namespace

intToType :: UInt -> SemanticTokenTypes
intToType i = Set.elemAt (fromIntegral i) knownValues

fromLspTokenType :: SemanticTokenTypes -> SemanticTokenType
fromLspTokenType tk = case tk of
    SemanticTokenTypes_Variable      -> TVariable
    SemanticTokenTypes_EnumMember    -> TDataCon
    SemanticTokenTypes_Property      -> TRecField
    SemanticTokenTypes_Function      -> TValBind
    SemanticTokenTypes_Class         -> TClass
    SemanticTokenTypes_Method        -> TClassMethod
    SemanticTokenTypes_Parameter     -> TPatternBind
    SemanticTokenTypes_TypeParameter -> TTypeVariable
    SemanticTokenTypes_Enum          -> TTypeCon
    SemanticTokenTypes_Interface     -> TTypeFamily
    SemanticTokenTypes_Event         -> TPatternSyn
    SemanticTokenTypes_Type          -> TTypeSyn
    SemanticTokenTypes_Namespace     -> TNothing
    _                                -> TNothing

-- data SemanticTokenType = SClass | SVariable | STypeVar | SDataCon | SNothing | SFieldName  deriving (Eq, Ord)
--
data SemanticTokenType =
    TVariable -- fallback to variable for all other cases
    | TTypeVariable -- Type variable
    -- by match
    | TPatternBind -- PatternBind, parameters, as values in let/in, case, lambda
    | TValBind -- MatchBind, valBind instance bind or regular bind
    | TRecField -- from match bind
    | TPatternSyn -- Pattern synonym

    | TClassMethod -- Class method
    -- by decls
    | TTypeSyn -- Type synonym (Non-local is not captured)
    | TClass -- Class (ConstraUInt constructor)
    | TTypeCon -- Type (Type constructor)
    | TDataCon -- Data constructor
    | TTypeFamily -- type family
    | TNothing -- unknown
    deriving (Eq, Ord, Show, Enum)

-- toAbs :: SemanticTokenData -> SemanticTokenAbsolute
-- toAbs (line, startChar, len, tokenType, tokenModifiers) =
--     SemanticTokenAbsolute line startChar len (toLspTokenType tokenType) [SemanticTokenModifiers_Declaration]

type SemanticCollect = (SemanticTokenType, LIdP GhcRn)
-- { line: 2, startChar 5, length: 3, tokenType: SemanticTokenType, tokenModifiers: 3, string},
-- type SemanticToken = (SemanticTokenData, SemanticCollect)
-- type SemanticTokenData = (UInt, UInt, UInt, SemanticTokenType, UInt)

type SemanticTokenUInt = UInt

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
