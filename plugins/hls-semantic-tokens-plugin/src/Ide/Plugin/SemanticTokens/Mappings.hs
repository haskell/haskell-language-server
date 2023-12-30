{-# LANGUAGE GADTs             #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections     #-}
{-# LANGUAGE TypeFamilies      #-}

-- |
-- This module provides mappings to convert token type information in the Haskell IDE plugin. It includes functions for:
--
-- 1. Mapping semantic token type to and from the LSP default token type.
-- 2. Mapping from GHC type and tyThing to semantic token type.
-- 3. Mapping from hieAst identifier details to haskell semantic token type.
-- 4. Mapping from LSP tokens to SemanticTokenOriginal.
module Ide.Plugin.SemanticTokens.Mappings where

import qualified Data.Array                      as A
import           Data.List.Extra                 (chunksOf, (!?))
import qualified Data.Map                        as Map
import qualified Data.Set                        as Set
import           Data.Text                       (Text)
import           Development.IDE                 (HieKind (HieFresh, HieFromDisk))
import           Development.IDE.GHC.Compat
import           Ide.Plugin.SemanticTokens.Types
import           Ide.Plugin.SemanticTokens.Utils (recoverFunMaskArray)
import           Language.LSP.Protocol.Types     (LspEnum (knownValues),
                                                  SemanticTokenAbsolute (SemanticTokenAbsolute),
                                                  SemanticTokenRelative (SemanticTokenRelative),
                                                  SemanticTokenTypes (..),
                                                  SemanticTokens (SemanticTokens),
                                                  UInt, absolutizeTokens)

-- * 1. Mapping semantic token type to and from the LSP default token type.

-- | map from haskell semantic token type to LSP default token type
toLspTokenType :: HsSemanticTokenType -> SemanticTokenTypes
toLspTokenType tk = case tk of
  -- TVariable     -> SemanticTokenTypes_Variable
  -- left hand side of none pattern bind
  TFunction     -> SemanticTokenTypes_Function
  TVariable     -> SemanticTokenTypes_Variable
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


lspTokenReverseMap :: Map.Map SemanticTokenTypes HsSemanticTokenType
lspTokenReverseMap = Map.fromList $ map (\x -> (toLspTokenType x, x)) $ enumFrom minBound

fromLspTokenType :: SemanticTokenTypes -> Maybe HsSemanticTokenType
fromLspTokenType tk = Map.lookup tk lspTokenReverseMap

-- * 2. Mapping from GHC type and tyThing to semantic token type.


-- | tyThingSemantic
tyThingSemantic :: TyThing -> Maybe HsSemanticTokenType
tyThingSemantic ty = case ty of
  AnId vid
    | isTyVar vid -> Just TTypeVariable
    | isRecordSelector vid -> Just TRecField
    | isClassOpId vid -> Just TClassMethod
    | isFunVar vid -> Just TFunction
    -- \| isDFunId vid -> TClassMethod
    | otherwise -> Just TVariable
  AConLike con -> case con of
    RealDataCon _ -> Just TDataCon
    PatSynCon _   -> Just TPatternSyn
  ATyCon tyCon
    | isTypeSynonymTyCon tyCon -> Just TTypeSyn
    | isTypeFamilyTyCon tyCon -> Just TTypeFamily
    | isClassTyCon tyCon -> Just TClass
    -- fall back to TTypeCon the result
    | otherwise -> Just TTypeCon
  ACoAxiom _ -> Nothing
  where
    isFunVar :: Var -> Bool
    isFunVar var = isFunType $ varType var

isFunType :: Type -> Bool
isFunType a = case a of
  ForAllTy _ t    -> isFunType t
--   Development.IDE.GHC.Compat.Core.FunTy(pattern synonym), FunTyFlag which is used to distinguish
--   (->, =>, etc..)
  FunTy flg _ rhs-> isVisibleFunArg flg || isFunType rhs
  _x              -> isFunTy a

hieKindFunMasksKind :: HieKind a -> HieFunMaskKind a
hieKindFunMasksKind hieKind = case hieKind of
  HieFresh -> HieFreshFun
  HieFromDisk full_file -> HieFromDiskFun $ recoverFunMaskArray (hie_types full_file)

typeSemantic :: HieFunMaskKind hType -> hType -> Maybe HsSemanticTokenType
typeSemantic kind t = case kind of
  HieFreshFun        -> if isFunType t then Just TFunction else Nothing
  HieFromDiskFun arr -> if arr A.! t then Just TFunction else Nothing

-- * 3. Mapping from hieAst ContextInfo to haskell semantic token type.

infoTokenType :: ContextInfo -> Maybe HsSemanticTokenType
infoTokenType x = case x of
  Use                      -> Nothing
  MatchBind                -> Nothing
  IEThing _                -> Nothing
  TyDecl                   -> Nothing -- type signature
  ValBind RegularBind _ _  -> Just TVariable
  ValBind InstanceBind _ _ -> Just TClassMethod
  PatternBind {}           -> Just TVariable
  ClassTyDecl _            -> Just TClassMethod
  TyVarBind _ _            -> Just TTypeVariable
  RecField _ _             -> Just TRecField
  -- data constructor, type constructor, type synonym, type family
  Decl ClassDec _          -> Just TClass
  Decl DataDec _           -> Just TTypeCon
  Decl ConDec _            -> Just TDataCon
  Decl SynDec _            -> Just TTypeSyn
  Decl FamDec _            -> Just TTypeFamily
  -- instance dec is class method
  Decl InstDec _           -> Just TClassMethod
  Decl PatSynDec _         -> Just TPatternSyn
  EvidenceVarUse           -> Nothing
  EvidenceVarBind {}       -> Nothing

-- * 4. Mapping from LSP tokens to SemanticTokenOriginal.

-- | line, startChar, len, tokenType, modifiers
type ActualToken = (UInt, UInt, UInt, HsSemanticTokenType, UInt)

-- | recoverSemanticTokens
-- for debug and test.
-- this function is used to recover the original tokens(with token in haskell token type zoon)
-- from the lsp semantic tokens(with token in lsp token type zoon)
recoverSemanticTokens :: String -> SemanticTokens -> Either Text [SemanticTokenOriginal]
recoverSemanticTokens sourceCode (SemanticTokens _ xs) = fmap (tokenOrigin sourceCode) <$> dataActualToken xs
  where
    tokenOrigin :: [Char] -> ActualToken -> SemanticTokenOriginal
    tokenOrigin sourceCode' (line, startChar, len, tokenType, _) =
      -- convert back to count from 1
      SemanticTokenOriginal tokenType (Loc (line + 1) (startChar + 1) len) name
      where
        tLine = lines sourceCode' !? fromIntegral line
        name = maybe "no source" (take (fromIntegral len) . drop (fromIntegral startChar)) tLine

    dataActualToken :: [UInt] -> Either Text [ActualToken]
    dataActualToken dt =
      maybe decodeError (Right . fmap semanticTokenAbsoluteActualToken . absolutizeTokens) $
        mapM fromTuple (chunksOf 5 $ map fromIntegral dt)
      where
        decodeError = Left "recoverSemanticTokenRelative: wrong token data"
        fromTuple [a, b, c, d, _] = SemanticTokenRelative a b c <$> fromInt (fromIntegral d) <*> return []
        fromTuple _ = Nothing

    semanticTokenAbsoluteActualToken :: SemanticTokenAbsolute -> ActualToken
    semanticTokenAbsoluteActualToken (SemanticTokenAbsolute line startChar len tokenType _tokenModifiers) =
      case fromLspTokenType tokenType of
        Just t  -> (line, startChar, len, t, 0)
        Nothing -> error "semanticTokenAbsoluteActualToken: unknown token type"

    -- legends :: SemanticTokensLegend
    fromInt :: Int -> Maybe SemanticTokenTypes
    fromInt i = Set.toAscList knownValues !? i
