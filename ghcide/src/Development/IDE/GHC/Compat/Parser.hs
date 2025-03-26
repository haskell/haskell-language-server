{-# LANGUAGE CPP             #-}
{-# LANGUAGE PatternSynonyms #-}

-- | Parser compatibility module.
module Development.IDE.GHC.Compat.Parser (
    initParserOpts,
    initParserState,
    PsSpan(..),
    pattern HsParsedModule,
    type GHC.HsParsedModule,
    Development.IDE.GHC.Compat.Parser.hpm_module,
    Development.IDE.GHC.Compat.Parser.hpm_src_files,
    pattern ParsedModule,
    Development.IDE.GHC.Compat.Parser.pm_parsed_source,
    type GHC.ParsedModule,
    Development.IDE.GHC.Compat.Parser.pm_mod_summary,
    Development.IDE.GHC.Compat.Parser.pm_extra_src_files,
    -- * API Annotations
#if !MIN_VERSION_ghc(9,11,0)
    Anno.AnnKeywordId(..),
#endif
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
import qualified GHC.Driver.Config.Parser        as Config
import           GHC.Hs                          (hpm_module, hpm_src_files)



initParserOpts :: DynFlags -> ParserOpts
initParserOpts =
  Config.initParserOpts

initParserState :: ParserOpts -> StringBuffer -> RealSrcLoc -> PState
initParserState =
  Lexer.initParserState

#if MIN_VERSION_ghc(9,5,0)
pattern HsParsedModule :: Located (HsModule GhcPs) -> [FilePath] -> GHC.HsParsedModule
#else
pattern HsParsedModule :: Located HsModule -> [FilePath] -> GHC.HsParsedModule
#endif
pattern HsParsedModule
    { hpm_module
    , hpm_src_files
    } <- GHC.HsParsedModule{..}
    where
        HsParsedModule hpm_module hpm_src_files =
            GHC.HsParsedModule hpm_module hpm_src_files


pattern ParsedModule :: ModSummary -> ParsedSource -> [FilePath] -> GHC.ParsedModule
pattern ParsedModule
    { pm_mod_summary
    , pm_parsed_source
    , pm_extra_src_files
    } <- GHC.ParsedModule{..}
    where
        ParsedModule ms parsed extra_src_files =
            GHC.ParsedModule
             { pm_mod_summary = ms
             , pm_parsed_source = parsed
             , pm_extra_src_files = extra_src_files
             }
{-# COMPLETE ParsedModule :: GHC.ParsedModule #-}


