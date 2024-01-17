{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE TypeFamilies       #-}
{-# LANGUAGE PatternGuards #-}
{-# LANGUAGE ViewPatterns #-}
{-# OPTIONS_GHC -Wno-orphans #-}


module Ide.Plugin.SemanticTokens.Utils where

import           Data.ByteString            (ByteString)
import           Data.ByteString.Char8      (unpack)
import qualified Data.Map                   as Map
import           Development.IDE            (Position (..), Range (..))
import           Development.IDE.GHC.Compat
import           Prelude                         hiding (length, span)
import Development.IDE.GHC.Compat.Util (mkFastString)
import Language.LSP.VFS (_file_text, VirtualFile)
import qualified Data.Text.Utf16.Rope as Rope
import Data.Text.Utf16.Rope (Rope, splitAtPosition)
import Data.Text (breakOnEnd, length, Text)
import Control.Monad (guard)
import qualified Data.Text as T
import Data.Bool (bool)

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


-- nameLength is in code points unit.
-- while Range might not in code points unit.
-- but we can still use it to get the length
-- since it is only used to exclude some names
-- currently, we only break `(ModuleA.b)` into `(ModuleA` and `.b)`
splitModuleNameAndOccName :: VirtualFile -> Range -> Identifier -> [(Range,Identifier)]
splitModuleNameAndOccName _ ran (Left m) = [(ran, Left m)]
splitModuleNameAndOccName vf ran@(Range (Position startLine startColumn) (Position _endLine endColumn)) (Right name)
    | nameLength name < fromIntegral (endColumn - startColumn), (Just (prefixLen, stripFlag)) <- peekPrefixModuleNameLength vf ran =
        [(Range (Position startLine (startColumn + bool 0 1 stripFlag))
                (Position startLine (startColumn + fromIntegral prefixLen)) , Left (mkModuleName "")), -- we do not need the module name, only tis range
        (Range (Position startLine (startColumn + fromIntegral prefixLen))
                 (Position startLine (endColumn + bool 0 (-1) stripFlag)), Right name)]
    | otherwise = [(ran, Right name)]

nameLength :: Name -> Int
nameLength = lengthFS . occNameFS . nameOccName

-- | peek at the prefix of a range,
-- if it is a qualified name, return the length of the module name.
-- module name everything before the last dot.
peekPrefixModuleNameLength :: VirtualFile -> Range -> Maybe (Int, Bool)
peekPrefixModuleNameLength rp ran = do
  token <- getTextByCodePointRangeFromVfs rp ran
  (c, _) <- T.uncons token
  let prefixLen = length $ fst $ breakOnEnd "." token
  guard $ prefixLen > 0
  return (prefixLen, c `elem` strippedChars)
  where strippedChars = ['`', '(']

-- | get the text from a range in a virtual file
getTextByCodePointRangeFromVfs :: VirtualFile -> Range -> Maybe Text
getTextByCodePointRangeFromVfs vf ra = do
    let rp = vf._file_text
    let (pos, len) = rangeToPositionLength ra
    (_, suffix) <- splitAtPosition (codePointPositionRopePosition pos) rp
    (prefix, _) <- Rope.splitAt len suffix
    let token = Rope.toText prefix
    return token
    where
    rangeToPositionLength :: (Integral l) => Range -> (Position, l)
    rangeToPositionLength (Range beginPos@(Position _ startColumn) (Position _ endColumn)) =
        (beginPos, fromIntegral $ endColumn - startColumn)
    codePointPositionRopePosition :: Position -> Rope.Position
    codePointPositionRopePosition (Position line column) = do
        let line' = fromIntegral line
        let column' = fromIntegral column
        Rope.Position line' column'
