{-
    The query module is used to query the semantic tokens from the AST
-}
{-# LANGUAGE RankNTypes         #-}
{-# LANGUAGE StandaloneDeriving #-}
module Ide.Plugin.SemanticTokens.Query where
import           Control.Arrow                   ((&&&))
import           Data.Function                   (on)
import           Data.Generics                   (everything)
import qualified Data.List                       as List
import           Data.Map                        (Map)
import qualified Data.Map                        as Map
import           Data.Maybe                      (fromMaybe, mapMaybe)
import           Data.Ord                        (comparing)
import qualified Data.Set                        as Set
import           Development.IDE                 (realSpan)
import           Development.IDE.GHC.Compat
import qualified Extra                           as List
import           Generics.SYB                    (mkQ)
import           Ide.Plugin.SemanticTokens.Types


-----------------------
----- identifier token map
-----------------------

-- from declaration site to token type
type NameTokenTypeMap  = Map.Map Name SemanticTokenType
type NameTokenTypeItem = (Name, ContextInfo)
-- name is unique
type IdentifierItem = (Span, SrcSpan, Name, [ContextInfo])

identifierItemContext :: IdentifierItem -> [ContextInfo]
identifierItemContext (span, c, x, y) = y
identifierItemName :: IdentifierItem -> Name
identifierItemName (span, c, x, y) = x


newtype NIdentifier = NIdentifier IdentifierItem

instance Show NIdentifier where
    show (NIdentifier (span, c, x, y)) = occNameString (nameOccName x) <> " " <> show y <> ""
            <> printCompactSrcSpan c <> " " <> printCompactRealSrc span

-- determine the precedence of context info
-- since we have multiple context info, we need to choose
-- the one with the highest level
contextInfoLevel :: ContextInfo -> Int
contextInfoLevel x = case x of
    Use                   -> 0
    MatchBind             -> 1
    IEThing _             -> 2 -- imported
    TyDecl                -> 3 -- type signature
    ValBind bt _ _        -> 4 -- lhs of val bind overrided by
    PatternBind _ _ _     -> 5 -- as in pattern
    ClassTyDecl _         -> 6 -- class method
    TyVarBind _ _         -> 8
    RecField _ _          -> 9 -- record field
    -- data constructor, type constructor, type synonym, type family
    Decl _ _              -> 10
    EvidenceVarUse        -> -1
    EvidenceVarBind _ _ _ -> -1

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

mergeInfo :: ContextInfo -> ContextInfo -> ContextInfo
mergeInfo x y = if contextInfoLevel x > contextInfoLevel y then x else y


infoTokenType :: ContextInfo -> SemanticTokenType
infoTokenType x = case x of
    Use                   -> TVariable
    MatchBind             -> TValBind
    IEThing _             -> TVariable -- todo check names space to distinguish
    TyDecl                -> TVariable -- type signature
    ValBind bt _ _        -> TValBind
    PatternBind _ _ _     -> TPatternBind
    ClassTyDecl _         -> TClassMethod
    TyVarBind _ _         -> TTypeVariable
    RecField _ _          -> TRecField
    -- data constructor, type constructor, type synonym, type family
    Decl ClassDec _       -> TClass
    Decl DataDec  _       -> TDataCon
    Decl ConDec   _       -> TTypeCon
    Decl SynDec   _       -> TTypeSyn
    Decl FamDec   _       -> TTypeFamily
    -- instance dec is class method
    Decl InstDec  _       -> TClassMethod
    Decl PatSynDec _      ->  TPatternSyn
    EvidenceVarUse        -> TVariable
    EvidenceVarBind _ _ _ -> TVariable

-- one definition site might have multiple identifiers


collectInfo :: [IdentifierItem] -> [NameTokenTypeItem]
collectInfo = map (identifierItemName &&& (foldr mergeInfo Use . identifierItemContext))

toNameGroups :: [IdentifierItem] -> [[IdentifierItem]]
toNameGroups = List.groupBy ((==) `on` identifierItemName) . List.sortOn identifierItemName

printCompactSrcSpan :: SrcSpan -> String
printCompactSrcSpan (RealSrcSpan x _buf) = printCompactRealSrc x
printCompactSrcSpan x                    = "noSrc"

printCompactRealSrc :: RealSrcSpan -> String
printCompactRealSrc x = show (srcSpanStartLine x) <> ":" <> show (srcSpanStartCol x) <> "-" <> show (srcSpanEndCol x)

getOriginalTextFromId :: String -> NIdentifier -> String
getOriginalTextFromId sourceCode (NIdentifier (span, c, _, _)) = fromMaybe "" $ do
            tLine <- lines sourceCode List.!? (line-1)
            return $ take len $ drop (startChar-1) tLine
        where
              line = srcSpanStartLine span
              startChar = srcSpanStartCol span
              len= srcSpanEndCol span - startChar

-- | Recurses through the given AST to find identifiers which are
-- 'InstanceValBind's.
identifierGetter :: HieAST a -> [IdentifierItem]
identifierGetter ast =
    let ids = [ (nodeSpan ast, nameSrcSpan c, c, Set.toList $ identInfo d) | (Right c, d) <- Map.toList $ getNodeIds ast]
    in ids <> concatMap identifierGetter (nodeChildren ast)

identifierNameTokenTypeMap :: Map.Map Name ContextInfo -> NameTokenTypeMap
identifierNameTokenTypeMap xs =  fmap infoTokenType xs

toNameGroupsMap :: [NameTokenTypeItem] -> Map.Map Name ContextInfo
toNameGroupsMap = Map.fromListWith mergeInfo

constructIdentifierMap :: HieAST a -> NameTokenTypeMap
constructIdentifierMap ast = identifierNameTokenTypeMap $ toNameGroupsMap $ collectInfo $ identifierGetter ast


-----------------------------------------
---- collect all names from RenamedSource
-----------------------------------------


toTokenType :: LIdP GhcRn -> SemanticTokenType
toTokenType locName = case occNameSpace $ occName $ unLoc locName of
  x | isTcClsNameSpace x   -> TClass
  x | isTvNameSpace x      -> TTypeVariable
  x | isDataConNameSpace x -> TDataCon
  x | isVarNameSpace x     -> TVariable
  _                        -> TVariable

nameGetter :: RenamedSource -> [SemanticCollect]
nameGetter = everything (++) ([] `mkQ` nameToCollect)

nameToCollect :: LIdP GhcRn -> [SemanticCollect]
nameToCollect locName = [(toTokenType locName, locName)]

------------------------
-- convert to lsp format
------------------------

toSemanticToken :: SemanticCollect -> Maybe SemanticToken
toSemanticToken ori@(tokenType, locName) = do
    loc <- realSpan $ getLocA locName
    let line = srcSpanStartLine loc
    let startChar = srcSpanStartCol loc
    let len= srcSpanEndCol loc - startChar
    return
        -- vscode render col start from 0
        ((line, startChar-1, len, tokenType, 0), ori)

-- need to take offset
toSemanticTokens :: [SemanticCollect] -> [SemanticToken]
toSemanticTokens = computeOffsets . List.sortOn fst. mapMaybe toSemanticToken
    where
        computeOffsets :: [SemanticToken] -> [SemanticToken]
        computeOffsets = ls . foldl f (1, 0, [])
        f (lastLine, lastStartChar, acc) ((line, startChar, len, tokenType, tokenModifiers), locName)
            = ( line, startChar,
                (if lastLine == line then
                    (0, startChar - lastStartChar, len, tokenType, tokenModifiers)
                else
                    (line - lastLine, startChar, len, tokenType, tokenModifiers)
                    , locName) :acc)
        ls (_, _, acc) = List.reverse acc


-----------------------------------
-- extract semantic tokens from ast
-----------------------------------
refineTokenType ::  NameTokenTypeMap -> SemanticCollect -> SemanticCollect
refineTokenType m (tokenType, locName) = case Map.lookup (unLoc locName) m of
    Just x  -> (x, locName)
    Nothing -> (TNothing, locName)


extractSemanticTokens :: forall a. HieAST a -> RenamedSource -> [SemanticToken]
extractSemanticTokens ast rs = toSemanticTokens $ map (refineTokenType iMap) $ nameGetter rs
    where collections = nameGetter rs
          iMap = constructIdentifierMap ast
