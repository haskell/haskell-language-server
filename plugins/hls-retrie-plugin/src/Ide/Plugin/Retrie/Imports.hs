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
                                                            GenLocated (L),
                                                            GenModule (moduleName),
                                                            GhcPs, GhcRn,
                                                            GlobalRdrElt,
                                                            GlobalRdrEnv,
                                                            HsBindLR (..),
                                                            ImportDecl (..),
                                                            ImportDeclQualifiedStyle (..),
                                                            ImportListInterpretation (Exactly),
                                                            ImportSpec, LHsExpr,
                                                            LIE, LImportDecl,
                                                            ModuleName, Name,
                                                            OccName,
                                                            Parent (NoParent, ParentIs),
                                                            RdrName (..),
                                                            SourceText (..),
                                                            TcGblEnv (..),
                                                            XImportDeclPass (XImportDeclPass),
                                                            availsToNameSet,
                                                            elemNameSet,
                                                            extensionFlags,
                                                            fun_id, gre_imp,
                                                            gre_lcl, gre_name,
                                                            gre_par,
                                                            isBuiltInSyntax,
                                                            isExact,
                                                            isInternalName,
                                                            is_mod,
                                                            lookupGRE_Name,
                                                            mkRdrUnqual,
                                                            moduleNameString,
                                                            ms_hspp_opts,
                                                            nameOccName,
                                                            occNameString,
                                                            rdrNameOcc,
                                                            simpleImportDecl)
#if MIN_VERSION_ghc(9,13,0)
import           Development.IDE.GHC.Compat                (WithUserRdr (WithUserRdr))
#elif MIN_VERSION_ghc(9,9,0)
import           Development.IDE.GHC.Compat                (RealSrcSpan, locA,
                                                            srcSpanToRealSrcSpan)
#else
import           Development.IDE.GHC.Compat                (RealSrcSpan,
                                                            srcSpanToRealSrcSpan)
#endif
#if MIN_VERSION_ghc(9,7,0)
import           Development.IDE.GHC.Compat                (LookupGRE (LookupRdrName),
                                                            WhichGREs (SameNameSpace),
                                                            lookupGRE)
#else
import           Development.IDE.GHC.Compat                (lookupGRE_RdrName)
#endif
import           Development.IDE.GHC.Compat.ExactPrint     (d0, dn, setEntryDP)
#if MIN_VERSION_ghc(9,11,0)
import           Development.IDE.GHC.Compat.ExactPrint     (noAnnSrcSpanDP,
                                                            noAnnSrcSpanDP1)
#else
import qualified Development.IDE.GHC.Compat.ExactPrint     as ExactPrint
#endif
import           GHC                                       (MatchGroup,
                                                            ParsedModule (..),
                                                            ParsedSource)
#if MIN_VERSION_ghc(9,13,0)
import           GHC                                       (LIdOccP)
#elif MIN_VERSION_ghc(9,9,0)
import           Development.IDE.GHC.Compat                (HsMatchContextRn)
import           GHC                                       (LIdP)
#else
import           GHC                                       (HsMatchContext,
                                                            LIdP)
#endif
import qualified GHC.Data.EnumSet                          as EnumSet
import           GHC.Parser.Annotation                     (AnnList (..),
                                                            DeltaPos (DifferentLine, SameLine),
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
import           GHC.Types.Avail                           (AvailInfo (AvailTC),
                                                            availNames)
import           GHC.Types.Name                            (nameModule_maybe)
import           GHC.Types.Name.Reader                     (is_decl)
import           Retrie.Rewrites

import           Data.List.NonEmpty                        (NonEmpty ((:|)))
import           Development.IDE.GHC.ExactPrint.Annotation
import           Development.IDE.GHC.ExactPrint.IE
import           Retrie.ExactPrint                         (AnnotatedImports,
                                                            unsafeMkA)
#if MIN_VERSION_ghc(9,14,0)
import           Development.IDE.GHC.Compat                (GREInfo (IAmConLike),
                                                            gre_info)
import           GHC.Types.GREInfo                         (ConInfo (conLikeInfo),
                                                            ConLikeInfo (ConIsPatSyn))
#else
import           Development.IDE.GHC.Compat                (isDataOcc)
#endif


-- | All of the 'Name's in this subtree along with their spellings, given
-- the parsed module the subtree was renamed from.
occurrences :: Data a => ParsedSource -> a -> [(Name, RdrName)]
#if MIN_VERSION_ghc(9,13,0)
occurrences _ = everything (++) (mkQ [] occ)
  where
    occ :: LIdOccP GhcRn -> [(Name, RdrName)]
    occ (L _ (WithUserRdr rdr name)) = [(name, rdr)]
#else
-- look up the original spelling in the parsed source by span
occurrences ps = everythingBut (++) (([], False) `mkQ` occ `extQ` ctxt)
  where
    occ :: LIdP GhcRn -> ([(Name, RdrName)], Bool)
    occ (L l name) =
      ([ (name, rdr) | Just sp <- [realSpan l], Just rdr <- [Map.lookup sp spellings] ], False)
#if MIN_VERSION_ghc(9,9,0)
    ctxt :: HsMatchContextRn -> ([(Name, RdrName)], Bool)
#else
    ctxt :: HsMatchContext GhcRn -> ([(Name, RdrName)], Bool)
#endif
    ctxt _ = ([], True)
    spellings = Map.fromList (everything (++) (mkQ [] spelling) ps)
    spelling :: LIdP GhcPs -> [(RealSrcSpan, RdrName)]
    spelling (L l rdr) = [ (sp, rdr) | Just sp <- [realSpan l] ]
    realSpan l = srcSpanToRealSrcSpan (locA l)
#endif

-- | Look at each 'Name' in the provided ast. If it can be imported, then
-- build an import statement for it.
--
-- TODO(simon): Build the imports for a specific clause.
extractInlineImports
  :: TcModuleResult
  -> TcModuleResult
  -> MatchGroup GhcRn (LHsExpr GhcRn)
  -> Either String [(ModuleName, ImportForm)]
extractInlineImports def tgt matches = sequence $ map importName names
  where
    names = filter (not . skip) (occurrences (pm_parsed_source (tmrParsed def)) matches)

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
    mod = moduleName (tcg_mod tc)
    targetEnv = tcg_rdr_env (tmrTypechecked tgt)
    refuse = Left
    showRdr = occNameString . rdrNameOcc
    extensions = extensionFlags (ms_hspp_opts (pm_mod_summary (tmrParsed tgt)))

-- | Does this spelling resolve to something else in the target module?
contested :: GlobalRdrEnv -> RdrName -> Name -> Bool
contested env rdr name = any ((/= name) . gre_name) (lookupGRERdr env rdr)

-- | True if this spelling is already bound to the expected 'Name'.
inScope :: GlobalRdrEnv -> RdrName -> Name -> Bool
inScope env rdr name = any ((== name) . gre_name) (lookupGRERdr env rdr)

-- FIXME(simon): wrong namespace
lookupGRERdr :: GlobalRdrEnv -> RdrName -> [GlobalRdrElt]
#if MIN_VERSION_ghc(9,7,0)
lookupGRERdr env rdr = lookupGRE env (LookupRdrName rdr SameNameSpace)
#else
lookupGRERdr env rdr = lookupGRE_RdrName rdr env
#endif

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
greImportModule :: ImportSpec -> ModuleName
#if MIN_VERSION_ghc(9,7,0)
greImportModule = moduleName . is_mod . is_decl
#else
greImportModule = is_mod . is_decl
#endif

-- | Figure out how the defining module references this 'Name'.
provenanceOf :: GlobalRdrEnv -> [AvailInfo] -> ModuleName -> Name -> Maybe Provenance
provenanceOf gre exports mod name = do
  gre <- lookupGRE_Name gre name
  let bundled = Map.fromList
        [ (n, p) | a@(AvailTC p _) <- exports, n <- availNames a, n /= p ]
  -- is the name defined in this module?
  if gre_lcl gre
    then Just $
      -- check whether the defining module exports this name
      if name `elemNameSet` (availsToNameSet exports)
        then Importable mod $ case Map.lookup name bundled of
          Just p | conLike gre -> ViaParent (nameOccName p)
          _                    -> memberForm gre
        else LocalOnly mod
    else
      -- the name is imported into the defining module
      case map greImportModule (gre_imp gre) of
        m : _ -> Just $ Importable m (memberForm gre)
        []    -> Nothing -- is this possible?

memberForm :: GlobalRdrElt -> MemberForm
memberForm gre = case gre_par gre of
  ParentIs p | conLike gre -> ViaParent (nameOccName p)
  NoParent   | patSyn gre  -> AsPattern
  _                        -> Plain

conLike, patSyn :: GlobalRdrElt -> Bool
#if MIN_VERSION_ghc(9,14,0)
conLike gre
  | IAmConLike{} <- gre_info gre = True
  | otherwise                    = False
patSyn gre
  | IAmConLike con <- gre_info gre
  , ConIsPatSyn <- conLikeInfo con = True
  | otherwise                      = False
#else
conLike = isDataOcc . nameOccName . gre_name
patSyn = conLike
#endif

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
renderImport (m, form) = L (noAnnSrcSpanDP (DifferentLine 1 0)) $ case form of
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

-- | Fresh annotations; older exactprint wants the span they stand in for.
noAnnSrcSpanDP :: Monoid ann => DeltaPos -> SrcSpanAnn' (EpAnn ann)
noAnnSrcSpanDP = ExactPrint.noAnnSrcSpanDP generatedSrcSpan

noAnnSrcSpanDP1 :: Monoid ann => SrcSpanAnn' (EpAnn ann)
noAnnSrcSpanDP1 = ExactPrint.noAnnSrcSpanDP1 generatedSrcSpan
#endif
