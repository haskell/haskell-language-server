module Ide.Plugin.Export.Cursor
  ( ExportFlavor (..)
  , UnderCursor (..)
  , locateUnderCursor
  ) where

import           Control.Applicative        ((<|>))
import           Data.Foldable              (toList)
import           Data.List                  (find)
import           Data.Maybe
import           Development.IDE
import           Development.IDE.GHC.Compat

-- | How a top-level entity is rendered in an export list.
data ExportFlavor
  = ExportName     -- ^ bare @x@. An operator is parenthesized with no @type@ keyword. Values and type synonyms.
  | ExportPattern  -- ^ @pattern X@.
  | ExportFamily   -- ^ bare @T@. An operator becomes @type (:<)@. Type and data families.
  | ExportAll      -- ^ @T(..)@. An operator becomes @type (:<)(..)@. Data, newtype, and class.
  deriving Eq

data UnderCursor
  = Decl ExportFlavor RdrName
  | Constructor RdrName RdrName
  | Header
  deriving Eq

locateUnderCursor :: Position -> ParsedSource -> Maybe UnderCursor
locateUnderCursor pos ps = classifyHeader pos (unLoc ps) <|> classifyInDecl
  where
    classifyInDecl = do
      L _ decl <- find (\(L l _) -> pos `isInsideSrcSpan` locA l) (hsmodDecls (unLoc ps))
      classifyDecl pos decl

-- | Match column-free so cursor anywhere on the @module ... where@ line counts.
classifyHeader :: Position -> HsModule GhcPs -> Maybe UnderCursor
classifyHeader pos mod = inName <|> inExports
  where
    isIn :: HasSrcSpan a => Maybe a -> Maybe UnderCursor
    isIn el = el >>= \n -> if pos `isInsideSrcSpanLines` getLoc n then Just Header else Nothing
    inName = isIn $ hsmodName mod
    inExports = isIn $ hsmodExports mod

-- | Line-based span containment, column-agnostic.
isInsideSrcSpanLines :: Position -> SrcSpan -> Bool
Position l _ `isInsideSrcSpanLines` r = case srcSpanToRange r of
  Just (Range (Position sl _) (Position el _)) -> sl <= l && l <= el
  _                                            -> False

-- | The exportable entities a top-level declaration defines, each with its
-- export flavor and located name.
declEntities :: HsDecl GhcPs -> [(ExportFlavor, LIdP GhcPs)]
declEntities = \case
  ValD _ (PatSynBind _ PSB {psb_id = lname}) -> [(ExportPattern, lname)]
  ValD _ FunBind {fun_id = lname}            -> [(ExportName, lname)]
  TyClD _ DataDecl {tcdLName = lname}        -> [(ExportAll, lname)]
  TyClD _ ClassDecl {tcdLName = lname}       -> [(ExportAll, lname)]
  TyClD _ SynDecl {tcdLName = lname}         -> [(ExportName, lname)]
  TyClD _ FamDecl {tcdFam = fam}             -> [(ExportFamily, fdLName fam)]
  _                                          -> []

classifyDecl :: Position -> HsDecl GhcPs -> Maybe UnderCursor
classifyDecl pos decl =
      listToMaybe [Decl flavor (unLoc n) | (flavor, n) <- declEntities decl, onName n]
  <|> typeSigUnderCursor
  <|> constructorUnderDecl
  where
    onName (L l _) = pos `isInsideSrcSpan` locA l
    -- A signature is not a definition (so not in 'declEntities'), but its name
    -- is still a valid place to invoke the export action from.
    typeSigUnderCursor = case decl of
      SigD _ (TypeSig _ names _) -> Decl ExportName . unLoc <$> find onName names
      _                          -> Nothing
    constructorUnderDecl = case decl of
      TyClD _ DataDecl {tcdLName = lname, tcdDataDefn = HsDataDefn {dd_cons = cons}}
        -> Constructor (unLoc lname) <$> constructorUnderCursor pos cons
      _ -> Nothing

constructorUnderCursor :: Position -> DataDefnCons (LConDecl GhcPs) -> Maybe RdrName
constructorUnderCursor pos cons =
  listToMaybe . mapMaybe nameAt $ extract_cons cons
  where
    nameAt (L _ cd) =
      listToMaybe [n | L l n <- conDeclNames cd, pos `isInsideSrcSpan` locA l]

    conDeclNames = \case
      ConDeclH98 {con_name = lname} -> [lname]
      ConDeclGADT {con_names = lnames} -> toList lnames
      _ -> []
