{- | Mappings.hs
This module contains the mapping
1. our token type from(to) LSP default token type
2. from ghc type to our token type
3. from hieAst identifier detail to our token type
4. from lsp token to our token
-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections     #-}

module Ide.Plugin.SemanticTokens.Mappings where
import qualified Data.List                       as List
import           Data.List.Extra                 (chunksOf, (!?))
import qualified Data.Map                        as Map
import           Data.Maybe                      (fromMaybe, mapMaybe)
import qualified Data.Set                        as Set
import           Data.Text                       (Text)
import           Development.IDE.GHC.Compat
import           Ide.Plugin.SemanticTokens.Types
import           Language.LSP.Protocol.Types     (LspEnum (knownValues),
                                                  SemanticTokenAbsolute (SemanticTokenAbsolute),
                                                  SemanticTokenRelative (SemanticTokenRelative),
                                                  SemanticTokenTypes (..),
                                                  SemanticTokens (SemanticTokens),
                                                  UInt, absolutizeTokens)


{- |
1. from our token type to LSP default token type
-}


-- mapping from our token type to LSP default token type
toLspTokenType :: SemanticTokenType -> Maybe SemanticTokenTypes
toLspTokenType tk = case tk of
    -- TVariable     -> SemanticTokenTypes_Variable
    -- left hand side of none pattern bind
    TFunction     -> Just SemanticTokenTypes_Function
    TVariable     -> Just SemanticTokenTypes_Variable
    TClass        -> Just SemanticTokenTypes_Class
    TClassMethod  -> Just SemanticTokenTypes_Method
    TTypeVariable -> Just SemanticTokenTypes_TypeParameter
    -- normal data type is a tagged union type look like enum type
    -- and a record is a product type like struct
    -- but we don't distinguish them yet
    TTypeCon      -> Just SemanticTokenTypes_Enum
    TDataCon      -> Just SemanticTokenTypes_EnumMember
    TRecField     -> Just SemanticTokenTypes_Property
    -- pattern syn is like a limited version of macro of constructing a data type
    TPatternSyn   -> Just SemanticTokenTypes_Macro
    -- saturated type
    TTypeSyn      -> Just SemanticTokenTypes_Type
    -- not sure if this is correct choice
    TTypeFamily   -> Just SemanticTokenTypes_Interface
    TNothing      -> Nothing

lspTokenReverseMap :: Map.Map SemanticTokenTypes SemanticTokenType
lspTokenReverseMap = Map.fromList $ mapMaybe (\x -> fmap (,x) (toLspTokenType x) ) $ enumFrom minBound

fromLspTokenType :: SemanticTokenTypes -> SemanticTokenType
fromLspTokenType tk = fromMaybe TNothing $ Map.lookup tk lspTokenReverseMap


{- |
2. from ghc type (TyThing or name) to our token type
-}

typeSemantic :: Type -> SemanticTokenType
typeSemantic  x =
     case x of
        ForAllTy _ a     -> typeSemantic a
        FunTy _ _        -> TFunction
        TyVarTy _        -> TNothing
        TyConApp tyCon _ -> TNothing
            -- | isTypeSynonymTyCon tyCon -> TTypeSyn
            -- | isTypeFamilyTyCon tyCon -> TTypeFamily
            -- | isClassTyCon tyCon -> TClass
            -- | otherwise -> TTypeCon
        AppTy a b        -> TNothing
        LitTy _          -> TNothing
        CastTy _ _       -> TNothing
        CoercionTy _     -> TNothing


toTokenType :: Name -> SemanticTokenType
toTokenType locName = case occNameSpace $ occName locName of
  x | isDataConNameSpace x -> TDataCon
  x | isTvNameSpace x      -> TTypeVariable
  x | isTcClsNameSpace x   -> TTypeCon -- Type constructors and classes in the same name space for now
  x | isVarNameSpace x     -> TVariable
  _                        -> TNothing


-- | tyThingSemantic
-- from ghc source code https://hackage.haskell.org/package/ghc-9.6.3/docs/src/GHC.Core.TyCon.html#isDataTyCon
tyThingSemantic :: TyThing -> SemanticTokenType
tyThingSemantic ty = case ty of
    AnId vid
        | isTyVar vid -> TTypeVariable
        | isRecordSelector vid -> TRecField
        | isClassOpId vid -> TClassMethod
        | isFunVar vid -> TFunction
        -- | isDFunId vid -> TClassMethod
        | otherwise -> TVariable
    AConLike con -> case con of
        RealDataCon _ -> TDataCon
        PatSynCon _   -> TPatternSyn
    ATyCon tyCon
        | isTypeSynonymTyCon tyCon -> TTypeSyn
        | isTypeFamilyTyCon tyCon -> TTypeFamily
        | isClassTyCon tyCon -> TClass
        -- | isVanillaAlgTyCon tyCon -> TTypeCon
        -- | isPrimTyCon tyCon -> TTypeCon
        -- fall back to TTypeCon for
        -- including defined by data, newtype, and type family instance
        | otherwise -> TTypeCon
    ACoAxiom _ -> TNothing

isFunVar :: Var -> Bool
isFunVar var = isFun $ varType var
isFun :: Type -> Bool
isFun a = case a of
    ForAllTy _ a -> isFun a
    x            -> isFunTy a


{- |
3. from hieAst identifier detail to our token type
-}


infoTokenType :: ContextInfo -> SemanticTokenType
infoTokenType x = case x of
    Use                      -> TNothing
    MatchBind                -> TNothing
    IEThing _                -> TNothing
    TyDecl                   -> TNothing -- type signature

    ValBind RegularBind _ _  -> TVariable
    ValBind InstanceBind _ _ -> TClassMethod
    PatternBind {}           -> TVariable
    ClassTyDecl _            -> TClassMethod
    TyVarBind _ _            -> TTypeVariable
    RecField _ _             -> TRecField
    -- data constructor, type constructor, type synonym, type family
    Decl ClassDec _          -> TClass
    Decl DataDec  _          -> TTypeCon
    Decl ConDec   _          -> TDataCon
    Decl SynDec   _          -> TTypeSyn
    Decl FamDec   _          -> TTypeFamily
    -- instance dec is class method
    Decl InstDec  _          -> TClassMethod
    Decl PatSynDec _         -> TPatternSyn

    EvidenceVarUse           -> TNothing
    EvidenceVarBind {}       -> TNothing


--------------------------------
---- from lsp token to our token
--------------------------------

-- line, startChar, len, tokenType, modifiers
type ActualToken = (UInt, UInt, UInt, SemanticTokenType, UInt)
-- | recoverSemanticTokens
-- used for debug and test
-- this function is used to recover the original tokens(with token in haskell token type zoon)
-- from the lsp semantic tokens(with token in lsp token type zoon)
recoverSemanticTokens :: String -> SemanticTokens -> Either Text [SemanticTokenOriginal]
recoverSemanticTokens sourceCode (SemanticTokens _ xs) = fmap (tokenOrigin sourceCode) <$> dataActualToken xs
    where
        tokenOrigin :: [Char] -> ActualToken -> SemanticTokenOriginal
        tokenOrigin sourceCode (line, startChar, len, tokenType, _) =
                -- convert back to count from 1
                SemanticTokenOriginal tokenType (Loc (line+1) (startChar+1) len) name
                where tLine = lines sourceCode !? fromIntegral line
                      name = maybe "no source" (take (fromIntegral len) . drop (fromIntegral startChar)) tLine


        dataActualToken :: [UInt] -> Either Text [ActualToken]
        dataActualToken xs = maybe decodeError (Right . fmap semanticTokenAbsoluteActualToken . absolutizeTokens)
                $ mapM fromTuple (chunksOf 5 $ map fromIntegral xs)
            where
                decodeError = Left "recoverSemanticTokenRelative: wrong token data"
                fromTuple [a, b, c, d, _] = SemanticTokenRelative a b c <$> fromInt (fromIntegral d) <*> return []
                fromTuple _               = Nothing

        semanticTokenAbsoluteActualToken :: SemanticTokenAbsolute -> ActualToken
        semanticTokenAbsoluteActualToken (SemanticTokenAbsolute line startChar len tokenType tokenModifiers) =
            (line, startChar, len, fromLspTokenType tokenType, 0)

        -- legends :: SemanticTokensLegend
        fromInt :: Int -> Maybe SemanticTokenTypes
        fromInt i = Set.toAscList knownValues !? i
