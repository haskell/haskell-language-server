{-
    The query module is used to query the semantic tokens from the AST
-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving  #-}
module Ide.Plugin.SemanticTokens.Query where
import           Control.Arrow                   ((&&&))
import           Data.Char                       (isAlphaNum)
import           Data.Function                   (on)
import           Data.Generics                   (everything)
import qualified Data.List                       as List
import qualified Data.List.NonEmpty              as NE
import           Data.Map                        (Map)
import qualified Data.Map                        as Map
import           Data.Ord                        (comparing)
import qualified Data.Set                        as Set
import           Data.Text                       (Text)
import qualified Data.Text                       as Text
import qualified Data.Text.Lazy.Builder          as Text
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
type IdentifierNameMap  = Set.Set Name
type NameTokenTypeItem = (Name, SemanticTokenType)

identifierItemName :: IdentifierItem -> Name
identifierItemName (span, x, y) = x


-- mergeInfo :: ContextInfo -> ContextInfo -> ContextInfo
mergeInfo x y = x `max` y


infoTokenType :: ContextInfo -> SemanticTokenType
infoTokenType x = case x of
    Use                   -> TNothing
    MatchBind             -> TNothing
    IEThing _             -> TNothing -- todo check names space to distinguish
    TyDecl                -> TNothing -- type signature
    ValBind bt _ _        -> TValBind
    PatternBind _ _ _     -> TPatternBind
    ClassTyDecl _         -> TClassMethod
    TyVarBind _ _         -> TTypeVariable
    RecField _ _          -> TRecField
    -- data constructor, type constructor, type synonym, type family
    Decl ClassDec _       -> TClass
    Decl DataDec  _       -> TTypeCon
    Decl ConDec   _       -> TDataCon
    Decl SynDec   _       -> TTypeSyn
    Decl FamDec   _       -> TTypeFamily
    -- instance dec is class method
    Decl InstDec  _       -> TClassMethod
    Decl PatSynDec _      -> TPatternSyn
    EvidenceVarUse        -> TNothing
    EvidenceVarBind _ _ _ -> TNothing

-- one definition site might have multiple identifiers

-----------------------------------------
---- construct definition map from HieAST a
-----------------------------------------
collectInfo :: [IdentifierItem] -> [NameTokenTypeItem]
collectInfo = map (\(_, name, ty) -> (name, foldr1 (<>) $ fmap infoTokenType ty))

isNotEvidence :: ContextInfo -> Bool
isNotEvidence EvidenceVarUse          = False
isNotEvidence (EvidenceVarBind _ _ _) = False
isNotEvidence _                       = True


getIds :: HieAST a -> [(Span, Name, NE.NonEmpty ContextInfo)]
getIds ast = [ (nodeSpan ast, c, infos)
            | (Right c, d) <- Map.toList $ getNodeIds ast
            , let (Just infos) = NE.nonEmpty $ Set.toList $ identInfo d
            , not $ isDerivedOccName (occName c)
            ]

leafIdentifierGetter :: HieAST a -> [IdentifierItem]
leafIdentifierGetter ast =
        if null $ nodeChildren ast
            then getIds ast
            else concatMap identifierGetter (nodeChildren ast)

identifierGetter :: HieAST a -> [IdentifierItem]
identifierGetter ast = getIds ast <> concatMap identifierGetter (nodeChildren ast)

toNameGroupsMap :: [NameTokenTypeItem] -> NameTokenTypeMap
toNameGroupsMap = Map.mapWithKey (\name tk -> if tk == TNothing then
    toTokenType name else tk) .  Map.fromListWith mergeInfo

constructIdentifierMap :: [IdentifierItem] -> NameTokenTypeMap
constructIdentifierMap nis = toNameGroupsMap $ collectInfo $ nis

-----------------------------------------
---- collect all names from RenamedSource
-----------------------------------------


nameToCollect :: LIdP GhcRn -> [Name]
nameToCollect locName = [unLoc locName]

nameGetter :: RenamedSource -> [Name]
nameGetter = everything (++) ([] `mkQ` nameToCollect)


-----------------------------------
-- extract semantic tokens from ast
-----------------------------------


extractSemanticTokens' :: forall a. HieAST a -> RenamedSource -> Either Text SemanticTokens
extractSemanticTokens' ast rs = makeSemanticTokens defaultSemanticTokensLegend
    $ List.sort $ Map.foldMapWithKey toAbsSemanticToken $ identifierToSemanticMap iMap ids
    where
        --   targetNames = nameGetter rs
          ids =
            -- filter (\(_, name, _) -> name `List.elem` targetNames) $
            leafIdentifierGetter ast
          iMap = constructIdentifierMap ids
          -- refine the semantic token type
          identifierToSemanticMap   :: NameTokenTypeMap -> [IdentifierItem] -> Map.Map Span SemanticTokenType
          identifierToSemanticMap nMap xs = Map.fromListWith (<>) $
                map (\(span, name, info) -> (span, (toTokenType name) <> Map.findWithDefault (toTokenType name) name nMap)) xs


            -- toAbsSemanticToken :: Span -> SemanticTokenType -> SemanticTokenAbsolute
          toAbsSemanticToken loc tokenType =
                let line = srcSpanStartLine loc - 1
                    startChar = srcSpanStartCol loc - 1
                    len = srcSpanEndCol loc - 1 - startChar
                in return $ SemanticTokenAbsolute (fromIntegral line) (fromIntegral startChar)
                    (fromIntegral len) (toLspTokenType tokenType) [SemanticTokenModifiers_Declaration]

