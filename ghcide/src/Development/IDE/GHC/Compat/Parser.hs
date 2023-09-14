{-# LANGUAGE CPP             #-}
{-# LANGUAGE PatternSynonyms #-}
{-# HLINT ignore "Unused LANGUAGE pragma" #-}

-- | Parser compatibility module.
module Development.IDE.GHC.Compat.Parser (
    initParserOpts,
    initParserState,
#if !MIN_VERSION_ghc(9,2,0)
    -- in GHC == 9.2 the type doesn't exist
    -- In GHC == 9.0 it is a data-type
    -- and GHC < 9.0 it is type-def
    --
    -- Export data-type here, otherwise only the simple type.
    Anno.ApiAnns(..),
#else
    ApiAnns,
#endif
    PsSpan(..),
#if MIN_VERSION_ghc(9,2,0)
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
#else
    GHC.HsParsedModule(..),
    GHC.ParsedModule(..),
#endif
    mkApiAnns,
    -- * API Annotations
    Anno.AnnKeywordId(..),
#if !MIN_VERSION_ghc(9,2,0)
    Anno.AnnotationComment(..),
#endif
    pattern EpaLineComment,
    pattern EpaBlockComment
    ) where

import           Development.IDE.GHC.Compat.Core
import           Development.IDE.GHC.Compat.Util

-- See Note [Guidelines For Using CPP In GHCIDE Import Statements]

import qualified GHC.Parser.Annotation           as Anno
import qualified GHC.Parser.Lexer                as Lexer
import           GHC.Types.SrcLoc                (PsSpan (..))

#if !MIN_VERSION_ghc(9,2,0)
import qualified GHC.Driver.Types                as GHC
#endif

#if !MIN_VERSION_ghc(9,2,0)
import qualified Data.Map                        as Map
import qualified GHC
#endif

#if MIN_VERSION_ghc(9,2,0)
import           GHC                             (EpaCommentTok (..),
                                                  pm_extra_src_files,
                                                  pm_mod_summary,
                                                  pm_parsed_source)
import qualified GHC
import           GHC.Hs                          (hpm_module, hpm_src_files)
#endif

#if MIN_VERSION_ghc(9,2,0) && !MIN_VERSION_ghc(9,3,0)
import qualified GHC.Driver.Config               as Config
#endif

#if MIN_VERSION_ghc(9,3,0)
import qualified GHC.Driver.Config.Parser        as Config
#endif


#if !MIN_VERSION_ghc(9,2,0)
type ParserOpts = Lexer.ParserFlags
#endif

initParserOpts :: DynFlags -> ParserOpts
initParserOpts =
#if MIN_VERSION_ghc(9,2,0)
  Config.initParserOpts
#else
  Lexer.mkParserFlags
#endif

initParserState :: ParserOpts -> StringBuffer -> RealSrcLoc -> PState
initParserState =
#if MIN_VERSION_ghc(9,2,0)
  Lexer.initParserState
#else
  Lexer.mkPStatePure
#endif

#if MIN_VERSION_ghc(9,2,0)
-- GHC 9.2 does not have ApiAnns anymore packaged in ParsedModule. Now the
-- annotations are found in the ast.
type ApiAnns = ()
#else
type ApiAnns = Anno.ApiAnns
#endif

#if MIN_VERSION_ghc(9,2,0)
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
#endif


#if MIN_VERSION_ghc(9,2,0)
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
#endif

mkApiAnns :: PState -> ApiAnns
#if MIN_VERSION_ghc(9,2,0)
mkApiAnns = const ()
#else
mkApiAnns pst =
    -- Copied from GHC.Driver.Main
    Anno.ApiAnns {
            apiAnnItems = Map.fromListWith (++) $ annotations pst,
            apiAnnEofPos = eof_pos pst,
            apiAnnComments = Map.fromList (annotations_comments pst),
            apiAnnRogueComments = comment_q pst
        }
#endif

#if !MIN_VERSION_ghc(9,2,0)
pattern EpaLineComment a = Anno.AnnLineComment a
pattern EpaBlockComment a = Anno.AnnBlockComment a
#endif
