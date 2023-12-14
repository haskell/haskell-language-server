-- stand alone deriving
{-# LANGUAGE StandaloneDeriving #-}

module Ide.Plugin.SemanticTokens.Utils where

import           Data.ByteString            (ByteString)
import           Data.ByteString.Char8      (unpack)
import qualified Data.Map                   as Map
import           Development.IDE.GHC.Compat

deriving instance Show DeclType
deriving instance Show BindType
deriving instance Show RecFieldContext

instance Show ContextInfo where
    show x = case x of
        Use                -> "Use"
        MatchBind          -> "MatchBind"
        IEThing _          -> "IEThing IEType" -- imported
        TyDecl             -> "TyDecl"
        ValBind bt _ _     -> "ValBind of " <> show bt
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

showNameType :: Name -> String
showNameType name
    | isInternalName name = "InternalName"
    | isExternalName name = "ExternalName"
    | isSystemName name   = "SystemName"
    | isWiredInName name  = "WiredInName"

bytestringString :: ByteString -> String
bytestringString = map (toEnum . fromEnum) . unpack
