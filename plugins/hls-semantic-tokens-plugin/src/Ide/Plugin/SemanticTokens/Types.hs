{-# LANGUAGE StandaloneDeriving #-}
module Ide.Plugin.SemanticTokens.Types where


import qualified Data.List                   as List
import qualified Data.List.Extra             as List
import qualified Data.Map                    as Map
import           Data.Maybe                  (fromMaybe)
import qualified Data.Set                    as Set
import           Development.IDE.GHC.Compat
import           GHC.Enum                    (boundedEnumFrom)
import           Language.LSP.Protocol.Types

-- legends :: SemanticTokensLegend
fromInt :: Int -> SemanticTokenTypes
fromInt i = Set.elemAt i knownValues

-- mapping from our token type to LSP default token type
toLspTokenType :: SemanticTokenType -> SemanticTokenTypes
toLspTokenType tk = case tk of
    -------
    -- term
    -------
    TVariable     -> SemanticTokenTypes_Variable
    TRecField     -> SemanticTokenTypes_Property
    -- most likely a function
    TValBind      -> SemanticTokenTypes_Function
    TClassMethod  -> SemanticTokenTypes_Method

    -------
    -- type
    -------
    TClass        -> SemanticTokenTypes_Class
    TPatternBind  -> SemanticTokenTypes_Parameter
    TTypeVariable -> SemanticTokenTypes_TypeParameter

    -- data type is a likely a tagged sum type since we choose data constructor as enum member
    -- we choose enum for data type

    -- but we are not distinguishing between data and record
    -- maybe a record type - struct ?
    -- TTypeCon      -> SemanticTokenTypes_Enum
    -- TDataCon      -> SemanticTokenTypes_EnumMember

    TTypeCon      -> SemanticTokenTypes_Enum
    TDataCon      -> SemanticTokenTypes_EnumMember

    -------------------------------------
    -- wiggle not sure if this is correct choice
    -------------------------------------
    TTypeFamily   -> SemanticTokenTypes_Interface
    -- is a likely a data constructor result
    TPatternSyn   -> SemanticTokenTypes_Event
    -- let it falls to other type
    TTypeSyn      -> SemanticTokenTypes_Type
    TNothing      -> SemanticTokenTypes_Namespace

lspTokenReverseMap :: Map.Map SemanticTokenTypes SemanticTokenType
lspTokenReverseMap = Map.fromList $ List.map (\x -> (toLspTokenType x, x)) $ boundedEnumFrom minBound

intToType :: UInt -> SemanticTokenTypes
intToType i = Set.elemAt (fromIntegral i) knownValues

fromLspTokenType :: SemanticTokenTypes -> SemanticTokenType
fromLspTokenType tk = fromMaybe TNothing $ Map.lookup tk lspTokenReverseMap

-- data SemanticTokenType = SClass | SVariable | STypeVar | SDataCon | SNothing | SFieldName  deriving (Eq, Ord)
-- order of declarations matters
data SemanticTokenType =
    TNothing -- unknown
    | TVariable -- fallback to variable for all other cases
    | TTypeVariable -- Type variable
    | TPatternBind -- PatternBind, parameters, as values in let/in, case, lambda
    | TValBind -- MatchBind, valBind instance bind or regular bind
    | TRecField -- from match bind
    | TPatternSyn -- Pattern synonym
    | TClassMethod -- Class method
    -- by decls
    | TClass -- Class (ConstraUInt constructor)
    | TTypeSyn -- Type synonym (Non-local is not captured)
    | TTypeCon -- Type (Type constructor)
    | TDataCon -- Data constructor
    | TTypeFamily -- type family
    deriving (Eq, Ord, Show, Enum, Bounded)

instance Semigroup SemanticTokenType where
    -- one in higher enum is more specific
    a <> b = max a b
instance Monoid SemanticTokenType where
    mempty = TNothing


type SemanticCollect = (SemanticTokenType, LIdP GhcRn)
type ActualToken = (UInt, UInt, UInt, SemanticTokenType, UInt)

semanticTokenAbsoluteActualToken :: SemanticTokenAbsolute -> ActualToken
semanticTokenAbsoluteActualToken (SemanticTokenAbsolute line startChar len tokenType tokenModifiers) =
    (line, startChar, len, fromLspTokenType tokenType, 0)
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


instance Show (IdentifierDetails a) where
    show x = show $ identInfo x

deriving instance Show DeclType
deriving instance Show BindType
deriving instance Show RecFieldContext

instance Show ContextInfo where
    show x = case x of
        Use                   -> "Use"
        MatchBind             -> "MatchBind"
        IEThing _             -> "IEThing IEType" -- imported
        TyDecl                -> "TyDecl"
        ValBind bt _ _        -> "ValBind of " <> show bt
        PatternBind _ _ _     -> "PatternBind"
        ClassTyDecl _         -> "ClassTyDecl"
        Decl d _              -> "Decl of " <> show d
        TyVarBind _ _         -> "TyVarBind"
        RecField c _          -> "RecField of " <> show c
        EvidenceVarBind _ _ _ -> "EvidenceVarBind"
        EvidenceVarUse        -> "EvidenceVarUse"


printCompactSrcSpan :: SrcSpan -> String
printCompactSrcSpan (RealSrcSpan x _buf) = printCompactRealSrc x
printCompactSrcSpan x                    = "noSrc"

printCompactRealSrc :: RealSrcSpan -> String
printCompactRealSrc x = show (srcSpanStartLine x) <> ":" <> show (srcSpanStartCol x) <> "-" <> show (srcSpanEndCol x)

type IdentifierItem = (Span, SrcSpan, Name, [ContextInfo])
newtype NIdentifier = NIdentifier IdentifierItem

instance Show NIdentifier where
    show (NIdentifier (span, c, x, y)) = occNameString (nameOccName x) <> " " <> show y <> ""
            <> printCompactSrcSpan c <> " " <> printCompactRealSrc span


getOriginalTextFromId :: String -> NIdentifier -> String
getOriginalTextFromId sourceCode (NIdentifier (span, c, _, _)) = fromMaybe "" $ do
            tLine <- lines sourceCode List.!? (line-1)
            return $ take len $ drop (startChar-1) tLine
        where
              line = srcSpanStartLine span
              startChar = srcSpanStartCol span
              len= srcSpanEndCol span - startChar
