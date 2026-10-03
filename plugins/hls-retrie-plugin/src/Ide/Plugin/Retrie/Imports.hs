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
#if !MIN_VERSION_ghc(9,13,0)
import           Data.Generics                             (everythingBut, extQ)
#endif
import qualified Data.Map                                  as Map
import qualified Data.Text                                 as T
import           Development.IDE                           hiding
                                                           (pluginHandlers)
import           Development.IDE.GHC.Compat                (AvailInfo,
                                                            EpAnnImportDecl (..),
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
                                                            isExact,
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
import           Development.IDE.GHC.Compat.ExactPrint     (d0, dn, setEntryDP)
#if MIN_VERSION_ghc(9,11,0)
import           Development.IDE.GHC.Compat.ExactPrint     (noAnnSrcSpanDP0,
                                                            noAnnSrcSpanDP1)
#else
import qualified Development.IDE.GHC.Compat.ExactPrint     as ExactPrint
#endif
import qualified GHC                                       as GHCGHC
import qualified GHC.Data.EnumSet                          as EnumSet
import           GHC.Parser.Annotation                     (AnnList (..),
                                                            DeltaPos (SameLine),
                                                            EpAnn (EpAnn),
                                                            EpaLocation,
                                                            emptyComments)
#if MIN_VERSION_ghc(9,11,0)
import           GHC.Parser.Annotation                     (AnnListBrackets (ListParens),
                                                            EpToken (EpTok),
                                                            LocatedLI,
                                                            NoAnn (noAnn))
#else
import           GHC.Parser.Annotation                     (AddEpAnn (..),
                                                            Anchor (..),
                                                            AnchorOperation (..),
                                                            AnnKeywordId (..),
                                                            LocatedL,
                                                            SrcSpanAnn' (..),
                                                            realSrcSpan)
import           GHC.Types.SrcLoc                          (generatedSrcSpan)
#endif
import           GHC.Types.Avail                           (AvailInfo (AvailTC))
import           GHC.Types.Name                            (nameModule_maybe)
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

-- | All of the 'Name's in this subtree along with their spellings, given
-- the parsed module the subtree was renamed from.
occurrences :: Data a => GHCGHC.ParsedSource -> a -> [(Name, RdrName)]
#if MIN_VERSION_ghc(9,13,0)
occurrences _ = everything (++) (mkQ [] occ)
  where
    occ :: GHCGHC.LIdOccP GhcRn -> [(Name, RdrName)]
    occ (L _ (GHC.WithUserRdr rdr name)) = [(name, rdr)]
#else
-- look up the original spelling in the parsed source by span
occurrences ps = everythingBut (++) (([], False) `mkQ` occ `extQ` ctxt)
  where
    occ :: GHCGHC.LIdP GhcRn -> ([(Name, RdrName)], Bool)
    occ (L l name) =
      ([ (name, rdr) | Just sp <- [realSpan l], Just rdr <- [Map.lookup sp spellings] ], False)
#if MIN_VERSION_ghc(9,9,0)
    ctxt :: GHC.HsMatchContextRn -> ([(Name, RdrName)], Bool)
#else
    ctxt :: GHCGHC.HsMatchContext GhcRn -> ([(Name, RdrName)], Bool)
#endif
    ctxt _ = ([], True)
    spellings = Map.fromList (everything (++) (mkQ [] spelling) ps)
    spelling :: GHCGHC.LIdP GhcPs -> [(GHC.RealSrcSpan, RdrName)]
    spelling (L l rdr) = [ (sp, rdr) | Just sp <- [realSpan l] ]
    realSpan l = GHC.srcSpanToRealSrcSpan (GHC.locA l)
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
    names = filter (not . skip) (occurrences (GHCGHC.pm_parsed_source (tmrParsed def)) matches)

    skip (name, rdr) =
      isInternalName name ||
      -- built-in syntax ([], (:), tuples) is not importable, so ignore it
      isBuiltInSyntax name ||
      -- already in scope in the target module
      inScope targetEnv rdr name ||
      -- renamer synthesized names such as 'GHC.Internal.Base.>>'
      isExact rdr

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
  (ParentIs p, _)          -> ViaParent (nameOccName p)
  -- a data constructor always has its type as parent
  (NoParent, IAmConLike _) -> AsPattern
  _                        -> Plain

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
  | Just FunBind{fun_matches} <- find (\case FunBind{fun_id = L _ n} -> qualifiedName n == thing ; _ -> False) topLevelBinds
  = renderImports <$> extractInlineImports tmrDef tmrTarget fun_matches
-- TODO handle imports for all rewrites
extractImports _ _ _ _ = Right mempty

qualifiedName :: Name -> String
qualifiedName n = T.unpack $ maybe "" ((<> ".") . printOutputable) (nameModule_maybe n) <> printOutputable n

--------------------------------------------------------------------------------
-- Import rendering

renderImports :: [(ModuleName, ImportForm)] -> AnnotatedImports
renderImports items = unsafeMkA (map renderImport items) 0

renderImport :: (ModuleName, ImportForm) -> LImportDecl GhcPs
renderImport (m, form) = L noAnnSrcSpanDP0 $ case form of
  ImportModule -> base { ideclExt = ext Nothing Nothing }
  ImportQualifiedAs q
    | q == m ->
        base { ideclQualified = QualifiedPre, ideclExt = ext (Just (dn 1)) Nothing }
    | otherwise ->
        base { ideclQualified = QualifiedPre
             , ideclAs        = Just (L noAnnSrcSpanDP1 q)
             , ideclExt       = ext (Just (dn 1)) (Just (dn 1)) }
  ImportMembers ms ->
    base { ideclImportList = Just (Exactly, importList (zipWith member [0 ..] ms))
         , ideclExt        = ext Nothing Nothing }
  where
    base = (simpleImportDecl m) { ideclName = L noAnnSrcSpanDP1 m }
    ext qual as' = XImportDeclPass (importAnn qual as') NoSourceText False
    member :: Int -> (OccName, MemberForm) -> LIE GhcPs
    member i (occ, mf) = setEntryDP ie (SameLine (min i 1))
      where
        ie = case mf of
          Plain       -> ieVar (mkIEName (mkRdrUnqual occ))
          ViaParent p -> mkTypeWithIE (mkRdrUnqual p) (mkRdrUnqual occ :| [])
          AsPattern   -> ieVar (mkWrappedName WrapPattern (mkRdrUnqual occ))

-- | The @import@ keyword, plus @qualified@ and @as@ at the given
-- deltas when present.
importAnn :: Maybe EpaLocation -> Maybe EpaLocation -> EpAnn EpAnnImportDecl
#if MIN_VERSION_ghc(9,11,0)
importAnn qual as' = EpAnn d0 ann emptyComments
  where
    ann = noAnn { importDeclAnnImport    = EpTok d0
                , importDeclAnnQualified = EpTok <$> qual
                , importDeclAnnAs        = EpTok <$> as' }
#else
importAnn qual as' = EpAnn (anchorDP (SameLine 0)) ann emptyComments
  where
    ann = EpAnnImportDecl d0 Nothing Nothing qual Nothing as'
#endif

-- | @(item, item)@ one space after the module name.
#if MIN_VERSION_ghc(9,11,0)
importList :: [LIE GhcPs] -> LocatedLI [LIE GhcPs]
importList items = L (EpAnn (dn 1) ann emptyComments) (withCommas items)
  where
    ann = noAnn { al_brackets = ListParens (EpTok d0) (EpTok d0) }
#else
importList :: [LIE GhcPs] -> LocatedL [LIE GhcPs]
importList items =
  L (SrcSpanAnn (EpAnn (anchorDP (SameLine 1)) ann emptyComments) generatedSrcSpan)
    (withCommas items)
  where
    ann = mempty { al_open  = Just (AddEpAnn AnnOpenP d0)
                 , al_close = Just (AddEpAnn AnnCloseP d0) }

-- | An anchor with no source position, printed at the given delta.
anchorDP :: DeltaPos -> Anchor
anchorDP dp = Anchor (realSrcSpan generatedSrcSpan) (MovedAnchor dp)

-- | Fresh annotations; exactprint 1.8 wants the span they stand in for.
noAnnSrcSpanDP0, noAnnSrcSpanDP1 :: Monoid ann => SrcSpanAnn' (EpAnn ann)
noAnnSrcSpanDP0 = ExactPrint.noAnnSrcSpanDP0 generatedSrcSpan
noAnnSrcSpanDP1 = ExactPrint.noAnnSrcSpanDP1 generatedSrcSpan
#endif
