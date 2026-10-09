{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies      #-}
{-# OPTIONS_GHC -Wno-orphans #-}


module Ide.Plugin.SemanticTokens.Utils where

import           Data.ByteString            (ByteString)
import           Data.ByteString.Char8      (unpack)
import qualified Data.Map.Strict            as Map
import           Development.IDE            (Position (..), Range (..))
import           Development.IDE.GHC.Compat
import           GHC.Iface.Ext.Types        (BindType (..), ContextInfo (..),
                                             DeclType (..), Identifier,
                                             IdentifierDetails (..),
                                             RecFieldContext (..), Span)
import           GHC.Iface.Ext.Utils        (RefMap)
import           Prelude                    hiding (length, span)

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


showSpan :: RealSrcSpan -> String
showSpan x = show (srcSpanStartLine x) <> ":" <> show (srcSpanStartCol x) <> "-" <> show (srcSpanEndCol x)


-- rangeToCodePointRange
mkRange :: (Integral a1, Integral a2) => a1 -> a2 -> a2 -> Range
mkRange startLine startCol len =
    Range (Position (fromIntegral startLine) (fromIntegral startCol)) (Position (fromIntegral startLine) (fromIntegral $ startCol + len))


rangeShortStr :: Range -> String
rangeShortStr (Range (Position startLine startColumn) (Position endLine endColumn)) =
    show startLine <> ":" <> show startColumn <> "-" <> show endLine <> ":" <> show endColumn

