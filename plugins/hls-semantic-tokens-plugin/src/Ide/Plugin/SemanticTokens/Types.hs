{-# LANGUAGE StandaloneDeriving #-}
module Ide.Plugin.SemanticTokens.Types where


import qualified Data.List                   as List
import qualified Data.List.Extra             as List
import qualified Data.List.NonEmpty          as NE
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
    -- TVariable     -> SemanticTokenTypes_Variable
    -- left hand side of none pattern bind
    TValBind      -> SemanticTokenTypes_Function
    -- any pattern bind
    TPatternBind  -> SemanticTokenTypes_Parameter
    TClass        -> SemanticTokenTypes_Class
    TClassMethod  -> SemanticTokenTypes_Method
    TTypeVariable -> SemanticTokenTypes_TypeParameter
    -- normal data type is a tagged union type look like enum type
    -- and a record is a product type like struct
    -- but we don't distinguish them yet
    TTypeCon      -> SemanticTokenTypes_Enum
    TDataCon      -> SemanticTokenTypes_EnumMember
    TRecField     -> SemanticTokenTypes_Property
    -- pattern syn is like a limited version of macro of constructing a data type
    TPatternSyn   -> SemanticTokenTypes_Macro
    -- saturated type
    TTypeSyn      -> SemanticTokenTypes_Type
    -- not sure if this is correct choice
    TTypeFamily   -> SemanticTokenTypes_Interface
    TNothing      -> SemanticTokenTypes_Namespace

lspTokenReverseMap :: Map.Map SemanticTokenTypes SemanticTokenType
lspTokenReverseMap = Map.fromList $ List.map (\x -> (toLspTokenType x, x)) $ boundedEnumFrom minBound

fromLspTokenType :: SemanticTokenTypes -> SemanticTokenType
fromLspTokenType tk = fromMaybe TNothing $ Map.lookup tk lspTokenReverseMap

-- data SemanticTokenType = SClass | SVariable | STypeVar | SDataCon | SNothing | SFieldName  deriving (Eq, Ord)
-- !!!! order of declarations matters
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


type SemanticCollect = (SemanticTokenType, LIdP GhcRn)

showCollect :: SemanticCollect -> String
showCollect (tk, L _ x) = show tk <> " " <> showSDocUnsafe (ppr x)

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



type IdentifierItem = (Span, Name, NE.NonEmpty ContextInfo)
newtype NIdentifier = NIdentifier IdentifierItem

identifierTokenType :: Identifier -> SemanticTokenType
identifierTokenType (Right x) = toTokenType x
identifierTokenType (Left x)  = TNothing

toTokenType :: Name -> SemanticTokenType
toTokenType locName = case occNameSpace $ occName locName of
  x | isDataConNameSpace x -> TDataCon
  x | isTvNameSpace x      -> TTypeVariable
  x | isTcClsNameSpace x   -> TTypeCon -- Type constructors and classes in the same name space for now
  x | isVarNameSpace x     -> TValBind
  _                        -> TNothing

instance Show NIdentifier where
    show (NIdentifier (span, x, y)) =
        occNameString (nameOccName x)
        <> "&" <> (show $ break (==':') $ occNameString $ nameOccName x)
        <> "&" <> (show $ toTokenType x)
        <> " [" <> show y <> "]"
            <> " nameSrc: "<> printCompactSrcSpan (nameSrcSpan x) <> " " <> printCompactRealSrc span


getOriginalTextFromId :: String -> NIdentifier -> String
getOriginalTextFromId sourceCode (NIdentifier (span, _, _)) = fromMaybe "" $ do
            tLine <- lines sourceCode List.!? (line-1)
            return $ take len $ drop (startChar-1) tLine
        where
              line = srcSpanStartLine span
              startChar = srcSpanStartCol span
              len= srcSpanEndCol span - startChar


-----------------------
----- identifier token map
-----------------------

-- unsafePrintDoc :: SDoc -> String

-- from declaration site to token type
type NameTokenTypeMap  = Map.Map Identifier ([Span], SemanticTokenType)

showNameTokenTypeMap :: NameTokenTypeMap -> String
showNameTokenTypeMap m = unlines
    [
       showSDocUnsafe (pprIdentifier idn) ++ ":" ++ show v
       ++ "\n" ++ unlines [showSDocUnsafe (ppr span) | span <- spans]
    | (idn, (spans, v)) <- Map.toList m]


-- type RefMap a = M.Map Identifier [(Span, IdentifierDetails a)]
showRefMap :: RefMap a -> String
showRefMap m = unlines
    [
       showIdentifier idn ++ ":"
       ++ "\n" ++ unlines [showSDocUnsafe (ppr span) ++ "\n" ++ showIdentifierDetails v | (span, v) <- spans]
    | (idn, spans) <- Map.toList m]

showIdentifierDetails :: IdentifierDetails a -> String
showIdentifierDetails x = show $ identInfo x

showIdentifier :: Identifier -> String
showIdentifier (Left x)  = showSDocUnsafe (ppr x)
showIdentifier (Right x) = nameStableString x
