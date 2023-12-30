{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeFamilies       #-}
{-# OPTIONS_GHC -Wno-orphans #-}


module Ide.Plugin.SemanticTokens.Utils where

import qualified Data.Array                      as A
import           Data.ByteString                 (ByteString)
import           Data.ByteString.Char8           (unpack)
import qualified Data.Map                        as Map
import           Development.IDE.GHC.Compat
import           Ide.Plugin.SemanticTokens.Types
import           Prelude                         hiding (span)

deriving instance Show DeclType
deriving instance Show BindType
deriving instance Show RecFieldContext

instance Show ContextInfo where
    show x = case x of
        Use                -> "Use"
        MatchBind          -> "MatchBind"
        IEThing _          -> "IEThing IEType" -- imported
        TyDecl             -> "TyDecl"
        ValBind bt _ sp    -> "ValBind of " <> show bt <> show sp
        PatternBind {}     -> "PatternBind"
        ClassTyDecl _      -> "ClassTyDecl"
        Decl d _           -> "Decl of " <> show d
        TyVarBind _ _      -> "TyVarBind"
        RecField c _       -> "RecField of " <> show c
        EvidenceVarBind {} -> "EvidenceVarBind"
        EvidenceVarUse     -> "EvidenceVarUse"

showCompactRealSrc :: RealSrcSpan -> String
showCompactRealSrc x = show (srcSpanStartLine x) <> ":" <> show (srcSpanStartCol x) <> "-" <> show (srcSpanEndCol x)

-- type RefMap a = M.Map Identifier [(Span, IdentifierDetails a)]
showRefMap :: RefMap a -> String
showRefMap m = unlines
    [
       showIdentifier idn ++ ":"
       ++ "\n" ++ unlines [showSDocUnsafe (ppr span) ++ "\n" ++ showIdentifierDetails v | (span, v) <- spans]
    | (idn, spans) <- Map.toList m]

showIdentifierDetails :: IdentifierDetails a -> String
showIdentifierDetails x = show $  identInfo x

showIdentifier :: Identifier -> String
showIdentifier (Left x)  = showSDocUnsafe (ppr x)
showIdentifier (Right x) = nameStableString x

showLocatedNames :: [LIdP GhcRn] -> String
showLocatedNames xs = unlines
    [ showSDocUnsafe (ppr locName) ++ " " ++ show (getLoc locName)
    | locName <- xs]

showClearName :: Name -> String
showClearName name = occNameString (occName name) <> ":" <> showSDocUnsafe (ppr name) <> ":" <> showNameType name

showName :: Name -> String
showName name = showSDocUnsafe (ppr name) <> ":" <> showNameType name

showNameType :: Name -> String
showNameType name
    | isWiredInName name  = "WiredInName"
    | isSystemName name   = "SystemName"
    | isInternalName name = "InternalName"
    | isExternalName name = "ExternalName"
    | otherwise           = "UnknownName"

bytestringString :: ByteString -> String
bytestringString = map (toEnum . fromEnum) . unpack

spanNamesString :: [(Span, Name)] -> String
spanNamesString xs = unlines
    [ showSDocUnsafe (ppr span) ++ " " ++ showSDocUnsafe (ppr name)
    | (span, name) <- xs]

nameTypesString :: [(Name, Type)] -> String
nameTypesString xs = unlines
    [ showSDocUnsafe (ppr span) ++ " " ++ showSDocUnsafe (ppr name)
    | (span, name) <- xs]


nameMapString :: NameSemanticMap -> [Name] -> String
nameMapString nsm  names = unlines
    [ showSDocUnsafe (ppr name) ++ " " ++ show tokenType
    | name <- names
    , let tokenType = lookupNameEnv nsm name
    ]


showSpan :: RealSrcSpan -> String
showSpan x = show (srcSpanStartLine x) <> ":" <> show (srcSpanStartCol x) <> "-" <> show (srcSpanEndCol x)


-- idea from https://gitlab.haskell.org/ghc/haddock/-/blob/b0b0e0366457c9aefebcc94df74e5de4d00e17b7/haddock-api/src/Haddock/Backends/Hyperlinker/Utils.hs#L107
recoverFunMaskArray
  :: A.Array TypeIndex HieTypeFlat -- ^ flat types
  -> A.Array TypeIndex Bool-- ^ full AST
recoverFunMaskArray flattened = unflattened
    where
    -- The recursion in 'unflattened' is crucial - it's what gives us sharing
    -- between the IfaceType's produced
    unflattened :: A.Array TypeIndex Bool
    unflattened = fmap (\flatTy -> go (fmap (unflattened A.!) flatTy)) flattened

    -- Unfold an 'HieType' whose subterms have already been unfolded
    go :: HieType Bool -> Bool
    go (HTyVarTy _name)            = False
    go (HAppTy _f _x)              = False
    go (HLitTy _lit)               = False
    go (HForAllTy ((_n,_k),_af) b) = b
    go (HFunTy _ _ _)              = True
    go (HQualTy _constraint b)     = b
    go (HCastTy b)                 = b
    go HCoercionTy                 = False
    go (HTyConApp _ _)             = False



