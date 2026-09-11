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
  , toImportDecl
  , ImportSpec(..)
  ) where


import           Data.Aeson                 (FromJSON (..), ToJSON (..))
import           Data.List.Extra            (find)
import qualified Data.Text                  as T
import           Development.IDE            hiding (pluginHandlers)
import           Development.IDE.GHC.Compat (GenLocated (L), GhcRn,
                                             HsBindLR (FunBind),
                                             ImportDecl (..), LHsExpr,
                                             ModSummary (ModSummary, ms_mod),
                                             SourceText (..), fun_id, isQual,
                                             isQual_maybe, moduleNameString,
                                             nameModule_maybe, nameRdrName,
                                             noLocA, occNameString,
                                             pattern IsBoot, pattern NotBoot,
                                             rdrNameOcc)
import qualified Development.IDE.GHC.Compat as GHC
import qualified GHC                        as GHCGHC
import           GHC.Generics               (Generic)
import           Retrie.Rewrites
import           Retrie.SYB                 (listify)

import           GHC.Types.PkgQual


-- TODO(simon):
-- * Unfold name -> lookup definition of name and determine necessary imports
-- * Fold name -> add import for name (retrie already implements this)
-- * Rules/Type Synonyms -> ??

newtype IE name
  = IEVar name
  deriving stock (Eq, Show, Generic)
  deriving anyclass (FromJSON, ToJSON)

data ImportSpec = AddImport
  { ideclNameString    :: String,
    ideclSource        :: Bool,
    ideclQualifiedBool :: Bool,
    ideclAsString      :: Maybe String,
    ideclThing         :: Maybe (IE String)
  }
  deriving (Eq, Show, Generic, FromJSON, ToJSON)

toImportDecl :: ImportSpec -> GHC.ImportDecl GHC.GhcPs
toImportDecl AddImport {..} = GHC.ImportDecl {ideclSource = ideclSource', ..}
  where
    ideclSource' = if ideclSource then IsBoot else NotBoot
    toMod = noLocA . GHC.mkModuleName
    ideclName = toMod ideclNameString
    ideclSafe = False
    ideclImplicit = False
    ideclSourceSrc = NoSourceText
    ideclAs = toMod <$> ideclAsString
    ideclQualified = if ideclQualifiedBool then GHC.QualifiedPre else GHC.NotQualified

    ideclPkgQual = NoRawPkgQual

#if MIN_VERSION_ghc(9,13,0)
    ideclLevelSpec = GHCGHC.NotLevelled
#endif

    ideclImportList = Nothing
    ideclExt = GHCGHC.XImportDeclPass
      { ideclAnn =
#if MIN_VERSION_ghc(9,9,0)
        GHCGHC.noAnn
#else
        GHCGHC.EpAnnNotUsed
#endif
      , ideclSourceText = ideclSourceSrc
      , ideclImplicit = ideclImplicit
      }

extractInlineImports  :: ModSummary -> GHC.LIdP GhcRn -> GHCGHC.MatchGroup GhcRn (LHsExpr GhcRn) -> [ImportSpec]
extractInlineImports ModSummary{ms_mod} fun_id fun_matches =
  let
    p name = nameModule_maybe name /= Just ms_mod
    names = listify p fun_matches
  in
    [ AddImport {..}
    | let ideclSource = False,
        name <- names,
        let r = nameRdrName name,
        let ideclQualifiedBool = isQual r,
        let ideclAsString = moduleNameString . fst <$> isQual_maybe r,
        let ideclThing = Just (IEVar $ occNameString $ rdrNameOcc r),
        Just ideclNameString <-
        [moduleNameString . GHC.moduleName <$> nameModule_maybe name]
    ]

extractImports :: ModSummary -> [HsBindLR GhcRn GhcRn] -> RewriteSpec -> [ImportSpec]
extractImports ms topLevelBinds (Unfold thing)
  | Just FunBind{fun_id, fun_matches} <- find (\case FunBind{fun_id = L _ n} -> T.unpack (printOutputable n) == thing ; _ -> False) topLevelBinds
  = extractInlineImports ms fun_id fun_matches
-- TODO handle imports for all rewrites
extractImports _ _ _ = []
