{-
    The query module is used to query the semantic tokens from the AST
-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving  #-}
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
import           Data.Text                       (Text)
import           Development.IDE                 (realSpan)
import           Development.IDE.GHC.Compat
import qualified Extra                           as List
import           Generics.SYB                    (mkQ)
import           Ide.Plugin.SemanticTokens.Types
import           Language.LSP.Protocol.Types


-----------------------
----- identifier token map
-----------------------

-- from declaration site to token type
type NameTokenTypeMap  = Map.Map Name SemanticTokenType
type NameTokenTypeItem = (Name, ContextInfo)

identifierItemContext :: IdentifierItem -> [ContextInfo]
identifierItemContext (span, c, x, y) = y
identifierItemName :: IdentifierItem -> Name
identifierItemName (span, c, x, y) = x


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


mergeInfo :: ContextInfo -> ContextInfo -> ContextInfo
mergeInfo x y = if contextInfoLevel x > contextInfoLevel y then x else y


infoTokenType :: ContextInfo -> SemanticTokenType
infoTokenType x = case x of
    Use                   -> TVariable
    MatchBind             -> TValBind
    IEThing _             -> TNothing -- todo check names space to distinguish
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

-----------------------------------------
---- construct definition map from HieAST a
-----------------------------------------
collectInfo :: [IdentifierItem] -> [NameTokenTypeItem]
collectInfo = map (identifierItemName &&& (foldr mergeInfo Use . identifierItemContext))


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


-----------------------------------
-- extract semantic tokens from ast
-----------------------------------
refineTokenType ::  NameTokenTypeMap -> SemanticCollect -> SemanticCollect
refineTokenType m (tokenType, locName) = case Map.lookup (unLoc locName) m of
    Just x  -> (x <> tokenType, locName)
    Nothing -> (tokenType, locName)


extractSemanticTokens' :: forall a. HieAST a -> RenamedSource -> Either Text SemanticTokens
extractSemanticTokens' ast rs = makeSemanticTokens defaultSemanticTokensLegend
    $ List.sort $ mapMaybe (toAbsSemanticToken . refineTokenType iMap) (nameGetter rs)
    where collections = nameGetter rs
          iMap = constructIdentifierMap ast


toAbsSemanticToken :: SemanticCollect -> Maybe SemanticTokenAbsolute
toAbsSemanticToken ori@(tokenType, locName) = do
    loc <- realSpan $ getLocA locName
    let line = srcSpanStartLine loc - 1
    let startChar = srcSpanStartCol loc - 1
    let len= srcSpanEndCol loc - 1 - startChar
    return $ SemanticTokenAbsolute (fromIntegral line) (fromIntegral startChar)
        (fromIntegral len) (toLspTokenType tokenType) [SemanticTokenModifiers_Declaration]

