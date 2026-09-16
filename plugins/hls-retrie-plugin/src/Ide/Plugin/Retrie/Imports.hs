{-# LANGUAGE CPP                   #-}
{-# LANGUAGE DeriveAnyClass        #-}
{-# LANGUAGE DerivingStrategies    #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE PatternSynonyms       #-}
{-# LANGUAGE RecordWildCards       #-}
{-# LANGUAGE TypeFamilies          #-}

-- | Import resolution for retrie rewrites.
module Ide.Plugin.Retrie.Imports
  ( extractImports
  , extractInlineImports
  , renderImports
  ) where


import           Data.Foldable                             (find)
import           Data.Generics                             (Data, everything,
                                                            mkQ)
import qualified Data.Map                                  as Map
import qualified Data.Text                                 as T
import           Development.IDE                           hiding
                                                           (pluginHandlers)
import           Development.IDE.GHC.Compat                (AvailInfo,
                                                            EpAnnImportDecl (EpAnnImportDecl),
                                                            Extension (PatternSynonyms),
                                                            GREInfo (IAmConLike),
                                                            GenLocated (L),
                                                            GenModule (moduleName),
                                                            GhcPs, GhcRn,
                                                            GlobalRdrElt,
                                                            GlobalRdrEltX (gre_info),
                                                            GlobalRdrEnv,
                                                            HsBindLR (..),
                                                            ImportDecl (..),
                                                            ImportDeclQualifiedStyle (..),
                                                            ImportListInterpretation (Exactly),
                                                            LHsExpr, LIE,
                                                            LImportDecl,
                                                            LookupGRE (LookupRdrName),
                                                            ModuleName, Name,
                                                            OccName,
                                                            Parent (NoParent, ParentIs),
                                                            RdrName (..),
                                                            SourceText (..),
                                                            TcGblEnv (..),
                                                            WhichGREs (SameNameSpace),
                                                            XImportDeclPass (XImportDeclPass),
                                                            availsToNameSet,
                                                            elemNameSet, fun_id,
                                                            gre_imp, gre_lcl,
                                                            gre_name, gre_par,
                                                            isBuiltInSyntax,
                                                            isInternalName,
                                                            lookupGRE_Name,
                                                            mkRdrUnqual,
                                                            moduleNameString,
                                                            nameOccName,
                                                            occNameString,
                                                            rdrNameOcc,
                                                            simpleImportDecl)
import qualified Development.IDE.GHC.Compat                as GHC
import qualified Development.IDE.GHC.Compat                as RdrName
import           Development.IDE.GHC.Compat.ExactPrint     (d0, dn,
                                                            noAnnSrcSpanDP0,
                                                            noAnnSrcSpanDP1,
                                                            setEntryDP)
import qualified GHC                                       as GHCGHC
import qualified GHC.Data.EnumSet                          as EnumSet
import           GHC.Parser.Annotation                     (AnnList (..),
                                                            AnnListBrackets (ListParens),
                                                            DeltaPos (SameLine),
                                                            EpAnn (EpAnn),
                                                            EpToken (EpTok),
                                                            LocatedLI,
                                                            NoAnn (noAnn),
                                                            emptyComments)
import           GHC.Types.Avail                           (AvailInfo (AvailTC))
import           GHC.Types.GREInfo                         (ConInfo (conLikeInfo),
                                                            ConLikeInfo (ConIsPatSyn))
import           GHC.Types.Name.Reader                     (is_decl)
import           Retrie.Rewrites

import           Data.List.NonEmpty                        (NonEmpty ((:|)))
import           Development.IDE.GHC.ExactPrint.Annotation
import           Development.IDE.GHC.ExactPrint.IE
import           Retrie.ExactPrint                         (AnnotatedImports,
                                                            unsafeMkA)

-- TODO(simon):
-- * Unfold name -> lookup definition of name and determine necessary imports
-- * Fold name -> add import for name (retrie already implements this)
-- * Rules/Type Synonyms -> ??


-- TODO(simon)
-- * look up the provenance of the name from the defining modules GlobalRdrEnv using lookupGRE_Name
--   it can be: Importable (plain, via parent, as pattern) or Local Only (see ImportSpec)
-- * For a qualified import, check the alias isn't already in-use for a different module
-- * For a pattern synonym, check PatternSynonyms is enabled in the target
-- * If it's LocalOnly then refuse

-- TODO(simon): Apply with 'addRewriteImports'

-- | All of the 'Name's in this subtree along with their spellings.
occurrences :: Data a => a -> [(Name, RdrName)]
#if MIN_VERSION_ghc(9,13,0)
occurrences = everything (++) (mkQ [] occ)
  where
    occ :: GHCGHC.LIdOccP GhcRn -> [(Name, RdrName)]
    occ (L _ (GHC.WithUserRdr rdr name)) = [(name, rdr)]
#else
occurrences mg = [ (name, rdr) | (sp, name) <- renamedOccs mg, Just rdr <- [Map.lookup sp parsedRdrs] ]
  where
    renamedOccs = everything (++) (mkQ [] locName)
    parsedRdrs  = Map.fromList (everything (++) (mkQ [] locRdr) (pm_parsed_source (tmrParsed tmrDef)))
#endif

-- | Look at each 'Name' in the provided ast. If it can be imported, then
-- build an import statement for it.
--
-- TODO(simon): Build the imports for a specific clause.
extractInlineImports
  :: TcModuleResult
  -> TcModuleResult
  -> GHCGHC.MatchGroup GhcRn (LHsExpr GhcRn)
  -> Either String [(ModuleName, ImportForm)]
extractInlineImports def tgt matches = sequence $ map importName names
  where
    names = filter (not . skip) (occurrences matches)

    skip (name, rdr) =
      isInternalName name ||
      -- built-in syntax ([], (:), tuples) is not importable, so ignore it
      isBuiltInSyntax name ||
      -- already in scope in the target module
      inScope targetEnv rdr name

    importName (name, rdr)
      -- TODO(simon): Record dots
      = case provenanceOf (tcg_rdr_env tc) (tcg_exports tc) mod name of
        Just Importable{..}
          | contested targetEnv rdr name ->
              -- TODO(simon): Add a test for this
              refuse $ "spelling " ++ showRdr rdr
                ++ " is already bound to a different name in the target"
          | Qual q _ <- rdr ->
              -- import qualified M as Q
              Right $ (pModule, ImportQualifiedAs q)
          | Unqual _ <- rdr
          , AsPattern <- pMember
          , not (PatternSynonyms `EnumSet.member` extensions) ->
              refuse $ "importing pattern synonym " ++ showRdr rdr
                ++ " needs PatternSynonyms in the target"
          | Unqual occ <- rdr -> do
              -- import M (n)
              Right $ (pModule, ImportMembers [(occ, pMember)])
          | otherwise ->
              refuse $ "unsupported spelling " ++ showRdr rdr
        Just (LocalOnly m) ->
          refuse $ showRdr rdr ++ " is not exported from " ++ moduleNameString m
        Nothing ->
          refuse $ "no provenance for " ++ (showRdr rdr)

    tc = tmrTypechecked def
    mod = moduleName (GHC.tcg_mod tc)
    targetEnv = GHC.tcg_rdr_env (tmrTypechecked tgt)
    refuse = Left
    showRdr = occNameString . rdrNameOcc
    extensions = GHC.extensionFlags (GHCGHC.ms_hspp_opts (GHCGHC.pm_mod_summary (tmrParsed tgt)))

-- | Does this spelling resolve to something else in the target module?
contested :: GlobalRdrEnv -> RdrName -> Name -> Bool
contested env rdr name = any ((/= name) . gre_name) (lookupGRERdr env rdr)

-- | True if this spelling is already bound to the expected 'Name'.
inScope :: GlobalRdrEnv -> RdrName -> Name -> Bool
inScope env rdr name = any ((== name) . gre_name) (lookupGRERdr env rdr)

-- FIXME(simon): wrong namespace
lookupGRERdr :: GlobalRdrEnv -> RdrName -> [GlobalRdrElt]
lookupGRERdr env rdr = RdrName.lookupGRE env (LookupRdrName rdr SameNameSpace)

data Provenance
  = Importable
      { pModule :: ModuleName
        -- ^ The module to import the name from: the import-facing
        -- module the defining module itself used (so a re-exported
        -- name imports as @Data.Char@, not its defining
        -- @GHC.Internal.*@ module).
      , pMember :: MemberForm
        -- ^ How the name is spelled in an import list.
      }
  | LocalOnly ModuleName
    -- ^ Defined in the defining module and not exported

-- | The import-facing module an 'ImportSpec' brought a name in from
-- (@Data.Char@, not the defining @GHC.Internal.*@ module).
greImportModule :: GHC.ImportSpec -> ModuleName
greImportModule = moduleName . RdrName.is_mod . is_decl

-- | Figure out how the defining module references this 'Name'.
provenanceOf :: GlobalRdrEnv -> [AvailInfo] -> ModuleName -> Name -> Maybe Provenance
provenanceOf gre exports mod name = do
  gre <- lookupGRE_Name gre name
  let bundled = Map.fromList [ (n, p) | AvailTC p ns <- exports, n <- ns, n /= p ]
  -- is the name defined in this module?
  if gre_lcl gre
    then Just $
      -- check whether the defining module exports this name
      if name `elemNameSet` (availsToNameSet exports)
        then Importable mod $ case Map.lookup name bundled of
          Just p  -> ViaParent (nameOccName p)
          Nothing -> memberForm gre
        else LocalOnly mod
    else
      -- the name is imported into the defining module
      case map greImportModule (gre_imp gre) of
        m : _ -> Just $ Importable m (memberForm gre)
        []    -> Nothing -- is this possible?

memberForm :: GlobalRdrElt -> MemberForm
memberForm gre = case (gre_par gre, gre_info gre) of
  (ParentIs p, _) -> ViaParent (nameOccName p)
  (NoParent, IAmConLike con)
    | ConIsPatSyn <- conLikeInfo con -> AsPattern
  _ -> Plain

-- | How an unqualified name is spelled in an import list.
data MemberForm
  = Plain
    -- ^ @import M (f)@.
  | ViaParent OccName
    -- ^ @import M (T (C))@: data constructors, record fields and
    -- bundled pattern synonyms import via their parent type.
  | AsPattern
    -- ^ @import M (pattern P)@: an unbundled pattern synonym.
  deriving (Eq, Show)

data ImportForm
  = ImportModule
    -- ^ @import M@.
  | ImportMembers [(OccName, MemberForm)]
    -- ^ @import M (f, T (C), pattern P)@.
  | ImportQualifiedAs ModuleName
    -- ^ @import qualified M as Q@.

extractImports :: TcModuleResult -> TcModuleResult -> [HsBindLR GhcRn GhcRn] -> RewriteSpec -> Either String AnnotatedImports
extractImports tmrDef tmrTarget topLevelBinds (Unfold thing)
  | Just FunBind{fun_matches} <- find (\case FunBind{fun_id = L _ n} -> T.unpack (printOutputable n) == thing ; _ -> False) topLevelBinds
  = renderImports <$> extractInlineImports tmrDef tmrTarget fun_matches
-- TODO handle imports for all rewrites
extractImports _ _ _ _ = Right mempty

--------------------------------------------------------------------------------
-- Import rendering

renderImports :: [(ModuleName, ImportForm)] -> AnnotatedImports
renderImports items = unsafeMkA (map renderImport items) 0

renderImport :: (ModuleName, ImportForm) -> LImportDecl GhcPs
renderImport (m, form) = L noAnnSrcSpanDP0 $ case form of
  ImportModule -> base { ideclExt = ext Nothing Nothing }
  ImportQualifiedAs q
    | q == m ->
        base { ideclQualified = QualifiedPre, ideclExt = ext (Just qualTok) Nothing }
    | otherwise ->
        base { ideclQualified = QualifiedPre
             , ideclAs        = Just (L noAnnSrcSpanDP1 q)
             , ideclExt       = ext (Just qualTok) (Just (EpTok (dn 1))) }
  ImportMembers ms ->
    base { ideclImportList = Just (Exactly, importList (zipWith member [0 ..] ms))
         , ideclExt        = ext Nothing Nothing }
  where
    base = (simpleImportDecl m) { ideclName = L noAnnSrcSpanDP1 m }
    qualTok = EpTok (dn 1)
    ext qual as' = XImportDeclPass
      (EpAnn d0 (EpAnnImportDecl (EpTok d0) Nothing Nothing Nothing qual Nothing as') emptyComments)
      NoSourceText False
    member :: Int -> (OccName, MemberForm) -> LIE GhcPs
    member i (occ, mf) = setEntryDP ie (SameLine (min i 1))
      where
        ie = case mf of
          Plain       -> ieVar (mkIEName (mkRdrUnqual occ))
          ViaParent p -> mkTypeWithIE (mkRdrUnqual p) (mkRdrUnqual occ :| [])
          AsPattern   -> ieVar (mkWrappedName WrapPattern (mkRdrUnqual occ))

-- | @(item, item)@ one space after the module name.
importList :: [LIE GhcPs] -> LocatedLI [LIE GhcPs]
importList items = L (EpAnn (dn 1) ann emptyComments) (withCommas items)
  where
    ann = noAnn { al_brackets = ListParens (EpTok d0) (EpTok d0) }
