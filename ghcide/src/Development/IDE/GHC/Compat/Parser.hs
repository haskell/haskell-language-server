{-# LANGUAGE CPP             #-}
{-# LANGUAGE PatternSynonyms #-}

-- | Parser compatibility module.
module Development.IDE.GHC.Compat.Parser (
    initParserOpts,
    initParserState,
    ApiAnns,
    PsSpan(..),
    pattern HsParsedModule,
    type GHC.HsParsedModule,
    Development.IDE.GHC.Compat.Parser.hpm_module,
    Development.IDE.GHC.Compat.Parser.hpm_src_files,
    Development.IDE.GHC.Compat.Parser.hpm_annotations,
    pattern ParsedModule,
    Development.IDE.GHC.Compat.Parser.pm_parsed_source,
    type GHC.ParsedModule,
    Development.IDE.GHC.Compat.Parser.pm_mod_summary,
    Development.IDE.GHC.Compat.Parser.pm_extra_src_files,
    Development.IDE.GHC.Compat.Parser.pm_annotations,
    mkApiAnns,
    -- * API Annotations
    Anno.AnnKeywordId(..),
    pattern EpaLineComment,
    pattern EpaBlockComment
    ) where

import           Development.IDE.GHC.Compat.Core
import           Development.IDE.GHC.Compat.Util
import qualified GHC.Parser.Annotation           as Anno
import qualified GHC.Parser.Lexer                as Lexer
import           GHC.Types.SrcLoc                (PsSpan (..))



import           GHC                             (EpaCommentTok (..),
                                                  pm_extra_src_files,
                                                  pm_mod_summary,
                                                  pm_parsed_source)
import qualified GHC
import           GHC.Hs                          (hpm_module, hpm_src_files)

-- See Note [Guidelines For Using CPP In GHCIDE Import Statements]


import qualified GHC.Driver.Config.Parser        as Config



initParserOpts :: DynFlags -> ParserOpts
initParserOpts =
  Config.initParserOpts

initParserState :: ParserOpts -> StringBuffer -> RealSrcLoc -> PState
initParserState =
  Lexer.initParserState

-- GHC 9.2 does not have ApiAnns anymore packaged in ParsedModule. Now the
-- annotations are found in the ast.
type ApiAnns = ()

#if MIN_VERSION_ghc(9,5,0)
pattern HsParsedModule :: Located (HsModule GhcPs) -> [FilePath] -> ApiAnns -> GHC.HsParsedModule
#else
pattern HsParsedModule :: Located HsModule -> [FilePath] -> ApiAnns -> GHC.HsParsedModule
#endif
pattern HsParsedModule
    { hpm_module
    , hpm_src_files
    , hpm_annotations
    } <- ( (,()) -> (GHC.HsParsedModule{..}, hpm_annotations))
    where
        HsParsedModule hpm_module hpm_src_files _hpm_annotations =
            GHC.HsParsedModule hpm_module hpm_src_files


pattern ParsedModule :: ModSummary -> ParsedSource -> [FilePath] -> ApiAnns -> GHC.ParsedModule
pattern ParsedModule
    { pm_mod_summary
    , pm_parsed_source
    , pm_extra_src_files
    , pm_annotations
    } <- ( (,()) -> (GHC.ParsedModule{..}, pm_annotations))
    where
        ParsedModule ms parsed extra_src_files _anns =
            GHC.ParsedModule
             { pm_mod_summary = ms
             , pm_parsed_source = parsed
             , pm_extra_src_files = extra_src_files
             }
{-# COMPLETE ParsedModule :: GHC.ParsedModule #-}

mkApiAnns :: PState -> ApiAnns
mkApiAnns = const ()

