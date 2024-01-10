{-# LANGUAGE GADTs             #-}
{-# LANGUAGE OverloadedStrings #-}
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
import           Data.Default                    (def)
import           Data.Functor.Identity           (Identity (runIdentity))
import           Data.List.Extra                 (chunksOf, (!?))
import qualified Data.Map                        as Map
import           Data.Maybe                      (mapMaybe)
import qualified Data.Set                        as Set
import           Data.Text                       (Text, unpack)
import           Development.IDE                 (HieKind (HieFresh, HieFromDisk))
import           Development.IDE.GHC.Compat
import           Ide.Plugin.SemanticTokens.Types
import           Ide.Plugin.SemanticTokens.Utils (mkRange)
import           Language.LSP.Protocol.Types     (LspEnum (knownValues),
                                                  SemanticTokenAbsolute (SemanticTokenAbsolute),
                                                  SemanticTokenRelative (SemanticTokenRelative),
                                                  SemanticTokenTypes (..),
                                                  SemanticTokens (SemanticTokens),
                                                  UInt, absolutizeTokens)
import           Language.LSP.VFS                hiding (line)

-- * 1. Mapping semantic token type to and from the LSP default token type.

-- | map from haskell semantic token type to LSP default token type
toLspTokenType :: SemanticTokensConfig  -> HsSemanticTokenType -> SemanticTokenTypes
toLspTokenType conf tk = case tk of
  TFunction     -> runIdentity $ stFunction conf
  TVariable     -> runIdentity $ stVariable conf
  TClassMethod  -> runIdentity $ stClassMethod conf
  TTypeVariable -> runIdentity $ stTypeVariable conf
  TDataCon      -> runIdentity $ stDataCon conf
  TClass        -> runIdentity $ stClass conf
  TTypeCon      -> runIdentity $ stTypeCon conf
  TTypeSyn      -> runIdentity $ stTypeSyn conf
  TTypeFamily   -> runIdentity $ stTypeFamily conf
  TRecField     -> runIdentity $ stRecField conf
  TPatternSyn   -> runIdentity $ stPatternSyn conf

lspTokenReverseMap :: Map.Map SemanticTokenTypes HsSemanticTokenType
lspTokenReverseMap = Map.fromList $ map (\x -> (toLspTokenType def x, x)) $ enumFrom minBound

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
  FunTy flg _ rhs -> isVisibleFunArg flg || isFunType rhs
  _x              -> isFunTy a

hieKindFunMasksKind :: HieKind a -> HieFunMaskKind a
hieKindFunMasksKind hieKind = case hieKind of
  HieFresh -> HieFreshFun
  HieFromDisk full_file -> HieFromDiskFun $ recoverFunMaskArray (hie_types full_file)

-- wz1000 offered
-- the idea from https://gitlab.haskell.org/ghc/haddock/-/blob/b0b0e0366457c9aefebcc94df74e5de4d00e17b7/haddock-api/src/Haddock/Backends/Hyperlinker/Utils.hs#L107
-- optimize version of looking for which types are functions without unfolding the whole type
recoverFunMaskArray ::
  -- | flat types
  A.Array TypeIndex HieTypeFlat ->
  -- | array of bool indicating whether the type is a function
  A.Array TypeIndex Bool
recoverFunMaskArray flattened = unflattened
  where
    -- The recursion in 'unflattened' is crucial - it's what gives us sharing
    -- function indicator check.
    unflattened :: A.Array TypeIndex Bool
    unflattened = fmap (\flatTy -> go (fmap (unflattened A.!) flatTy)) flattened

    -- Unfold an 'HieType' whose subterms have already been unfolded
    go :: HieType Bool -> Bool
    go (HTyVarTy _name)              = False
    go (HAppTy _f _x)                = False
    go (HLitTy _lit)                 = False
    go (HForAllTy ((_n, _k), _af) b) = b
    go (HFunTy _ _ _)                = True
    go (HQualTy _constraint b)       = b
    go (HCastTy b)                   = b
    go HCoercionTy                   = False
    go (HTyConApp _ _)               = False

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

-- | recoverSemanticTokens
-- for debug and test.
-- this function is used to recover the original tokens(with token in haskell token type zoon)
-- from the lsp semantic tokens(with token in lsp token type zoon)
-- this use the default token type mapping
recoverSemanticTokens :: VirtualFile -> SemanticTokens -> Either Text [SemanticTokenOriginal HsSemanticTokenType]
recoverSemanticTokens v s = do
    tks <- recoverLspSemanticTokens v s
    return $ map fromLspTokenTypeStrict tks

-- | fromLspTokenTypeStrict
-- for debug and test.
-- use the default token type mapping to convert lsp token type to haskell token type
fromLspTokenTypeStrict :: SemanticTokenOriginal SemanticTokenTypes -> SemanticTokenOriginal HsSemanticTokenType
fromLspTokenTypeStrict (SemanticTokenOriginal tokenType location name) =
        case fromLspTokenType tokenType of
        Just t  -> SemanticTokenOriginal t location name
        Nothing -> error "recoverSemanticTokens: unknown lsp token type"

-- | recoverLspSemanticTokens
-- for debug and test.
-- this function is used to recover the original tokens(with token in standard lsp token type zoon)
-- from the lsp semantic tokens(with token in lsp token type zoon)
recoverLspSemanticTokens :: VirtualFile -> SemanticTokens -> Either Text [SemanticTokenOriginal SemanticTokenTypes]
recoverLspSemanticTokens vsf (SemanticTokens _ xs) = do
  tokens <- dataActualToken xs
  return $ mapMaybe (tokenOrigin sourceCode) tokens
  where
    sourceCode = unpack $ virtualFileText vsf
    tokenOrigin :: [Char] -> SemanticTokenAbsolute -> Maybe (SemanticTokenOriginal SemanticTokenTypes)
    tokenOrigin sourceCode' (SemanticTokenAbsolute line startChar len tokenType _tokenModifiers) = do
      -- convert back to count from 1
      let range = mkRange line startChar len
      CodePointRange (CodePointPosition x y) (CodePointPosition _ y1) <- rangeToCodePointRange vsf range
      let line' = x
      let startChar' = y
      let len' = y1 - y
      let tLine = lines sourceCode' !? fromIntegral line'
      let name = maybe "no source" (take (fromIntegral len') . drop (fromIntegral startChar')) tLine
      return $ SemanticTokenOriginal tokenType (Loc (line' + 1) (startChar' + 1) len') name

    dataActualToken :: [UInt] -> Either Text [SemanticTokenAbsolute]
    dataActualToken dt =
      maybe decodeError (Right . absolutizeTokens) $
        mapM fromTuple (chunksOf 5 $ map fromIntegral dt)
      where
        decodeError = Left "recoverSemanticTokenRelative: wrong token data"
        fromTuple [a, b, c, d, _] = SemanticTokenRelative a b c <$> fromInt (fromIntegral d) <*> return []
        fromTuple _ = Nothing


    -- legends :: SemanticTokensLegend
    fromInt :: Int -> Maybe SemanticTokenTypes
    fromInt i = Set.toAscList knownValues !? i

-- Note [Semantic information from Multiple Sources]
-- ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
-- We group Name into 2 categories since the information source is different:
-- 1. Locally defined Name
-- Information source is current module's HieAst,
-- Either from ContextInfo(all except differing function and none-function)
-- or from Hie Type(Differing Function and Non-function Variable)
-- 2. Imported Name
-- Information source is `TyThing` for the `Name`, looked up in `HscEnv`(with all imported things loaded).
-- `TyThing` is information rich, since it is used to represent the things that a name can refer to in ghc.
-- The reason why we need special handling for imported name is that
-- Up to 9.8
-- 1. For Hie Type, IfaceTyCon in hie type does not contain enough information to distinguish class, type syn, type family etc..
-- 2. Most imported name is only annotated as [Use] in the ContextInfo from hie.
-- 3. `namespace` in `Name` is limited, we can only classify `VarName, FldName, DataName, TvNamem, TcClsName`.
-- 4. WiredIn `Name` have `TyThing` attached, but not many are WiredIn names.
