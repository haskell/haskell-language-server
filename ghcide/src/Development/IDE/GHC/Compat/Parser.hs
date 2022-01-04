{-# LANGUAGE CPP #-}

-- | Parser compaibility module.
module Development.IDE.GHC.Compat.Parser (
    initParserOpts,
    initParserState,
#if MIN_VERSION_ghc(9,0,0) && !MIN_VERSION_ghc(9,2,0)
    -- in GHC == 9.2 the type doesn't exist
    -- In GHC == 9.0 it is a data-type
    -- and GHC < 9.0 it is type-def
    --
    -- Export data-type here, otherwise only the simple type.
    Anno.ApiAnns(..),
#else
    ApiAnns,
#endif
#if MIN_VERSION_ghc(9,0,0)
    PsSpan(..),
#endif
    mkHsParsedModule,
    mkParsedModule,
    mkApiAnns,
    -- * API Annotations
    Anno.AnnKeywordId(..),
    Anno.AnnotationComment(..),
    ) where

#if MIN_VERSION_ghc(9,0,0)
import qualified GHC.Parser.Lexer                as Lexer
import           GHC.Types.SrcLoc                (PsSpan (..))
#if MIN_VERSION_ghc(9,2,0)
import qualified GHC.Driver.Config               as Config
import           GHC.Parser.Lexer                hiding (initParserState)
#else
import qualified GHC.Parser.Annotation           as Anno
#endif
#else
import qualified ApiAnnotation                   as Anno
import           Lexer
import qualified SrcLoc
#endif
import           Development.IDE.GHC.Compat.Core
import           Development.IDE.GHC.Compat.Util

#if !MIN_VERSION_ghc(9,2,0)
import qualified Data.Map                        as Map
#endif

#if !MIN_VERSION_ghc(9,0,0)
type ParserOpts = DynFlags
#elif !MIN_VERSION_ghc(9,2,0)
type ParserOpts = Lexer.ParserFlags
#endif

initParserOpts :: DynFlags -> ParserOpts
initParserOpts =
#if MIN_VERSION_ghc(9,2,0)
  Config.initParserOpts
#elif MIN_VERSION_ghc(9,0,0)
  Lexer.mkParserFlags
#else
  id
#endif

initParserState :: ParserOpts -> StringBuffer -> RealSrcLoc -> PState
initParserState =
#if MIN_VERSION_ghc(9,2,0)
  Lexer.initParserState
#elif MIN_VERSION_ghc(9,0,0)
  Lexer.mkPStatePure
#else
  Lexer.mkPState
#endif

#if MIN_VERSION_ghc(9,2,0)
type ApiAnns = ()
#else
type ApiAnns = Anno.ApiAnns
#endif


mkHsParsedModule :: ParsedSource -> [FilePath] -> ApiAnns -> HsParsedModule
mkHsParsedModule parsed fps hpm_annotations =
  HsParsedModule
    parsed
    fps
#if !MIN_VERSION_ghc(9,2,0)
    hpm_annotations
#endif


mkParsedModule :: ModSummary -> ParsedSource -> [FilePath] -> ApiAnns -> ParsedModule
mkParsedModule ms parsed extra_src_files _hpm_annotations =
  ParsedModule {
    pm_mod_summary = ms
  , pm_parsed_source = parsed
  , pm_extra_src_files = extra_src_files
#if !MIN_VERSION_ghc(9,2,0)
  , pm_annotations = _hpm_annotations
#endif
  }

mkApiAnns :: PState -> ApiAnns
#if MIN_VERSION_ghc(9,2,0)
mkApiAnns = const ()
#else
mkApiAnns pst =
#if MIN_VERSION_ghc(9,0,1)
    -- Copied from GHC.Driver.Main
    Anno.ApiAnns {
            apiAnnItems = Map.fromListWith (++) $ annotations pst,
            apiAnnEofPos = eof_pos pst,
            apiAnnComments = Map.fromList (annotations_comments pst),
            apiAnnRogueComments = comment_q pst
        }
#else
    (Map.fromListWith (++) $ annotations pst,
     Map.fromList ((SrcLoc.noSrcSpan,comment_q pst)
                  :annotations_comments pst))
#endif
#endif
