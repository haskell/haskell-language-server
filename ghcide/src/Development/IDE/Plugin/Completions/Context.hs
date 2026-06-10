{-# LANGUAGE CPP            #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RankNTypes     #-}

-- | Completion-context detection. Given a parsed module and a cursor position,
-- determine a context the cursor sits in so the completion logic can pick which
-- completions to offer.
module Development.IDE.Plugin.Completions.Context
  ( Context (..)
  , contextFilter
  , deduceContext
  , getContext
  ) where

import           Data.Generics                        (GenericQ, extQ, gmapQ,
                                                       mkQ)
import           Data.Maybe                           (maybeToList)
import qualified Data.Text                            as T
import           Development.IDE
import           Development.IDE.Core.PositionMapping
import           Development.IDE.GHC.Compat           hiding (getContext)
import           Language.LSP.Protocol.Types          (isSubrangeOf)

#if !MIN_VERSION_base(4,20,0)
import           Data.List                            (foldl')
#endif

#if MIN_VERSION_ghc(9,9,0)
import           GHC.Hs                               (HasLoc)
#endif

-- | The kind of context the cursor sits in used to pick which completions to
-- show.
data Context
  = TypeContext
  | ValueContext
  | -- | The module's name of an import.
    ImportModuleContext T.Text
  | -- | Import context (explicit or hiding) with module name.
    ImportListContext T.Text
  | -- | The export list of the current module.
    ExportContext
  | -- | Fallback. Show all known symbols.
    DefaultContext
  deriving (Show, Eq)

data ContextResult = NoContext | ContextResult !Range !Context

-- | Keep the innermost of two results.
tighten :: ContextResult -> ContextResult -> ContextResult
tighten NoContext b = b
tighten a NoContext = a
tighten ar@(ContextResult a _) br@(ContextResult b _)
  | b `isSubrangeOf` a = br
  | otherwise          = ar

foldTighten :: (a -> ContextResult) -> [a] -> ContextResult
foldTighten f = foldr (tighten . f) NoContext

-- | Filter completions for a context. The predicate reports whether a candidate
-- is a type-level name. An export list accepts both, so it is unfiltered. Import
-- contexts are dispatched by getCompletions before this runs and never reach
-- here.
contextFilter :: (a -> Bool) -> Context -> [a] -> [a]
contextFilter isTypeCompl ctx = case ctx of
  TypeContext           -> filter isTypeCompl
  ValueContext          -> filter (not . isTypeCompl)
  DefaultContext        -> id
  ExportContext         -> id
  ImportModuleContext{} -> dispatchedEarlier
  ImportListContext{}   -> dispatchedEarlier
  where
    dispatchedEarlier = id

-- | Look up the completion context at the given position, accounting for stale
-- data via the position mapping.
deduceContext :: Maybe (ParsedModule, PositionMapping) -> Position -> Context
deduceContext Nothing _ = DefaultContext
deduceContext (Just (pm, pmapping)) pos =
  let PositionMapping pDelta = pmapping
  in getContext pm (fromDelta pDelta pos)

-- | Determine the completion 'Context' at the cursor, returning the innermost
-- (most specific) declaration that contains it, or 'DefaultContext' if none do.
getContext :: ParsedModule -> PositionResult Position -> Context
getContext pm query =
  case foldTighten (getExportContext q) (maybeToList hsmodExports)
       `tighten` foldTighten (getImportContext q) hsmodImports
       `tighten` foldTighten (getDeclContext q) hsmodDecls of
    NoContext             -> DefaultContext
    ContextResult _ found -> found
  where
    q = case query of
      PositionExact p   -> Range p p
      PositionRange l u -> Range l u
    HsModule {hsmodExports, hsmodImports, hsmodDecls} = unLoc (pm_parsed_source pm)

getExportContext :: Range -> XRec GhcPs [LIE GhcPs] -> ContextResult
getExportContext = contextual ExportContext

getImportContext :: Range -> LImportDecl GhcPs -> ContextResult
getImportContext query limp@(L _ imp) =
  let modName = T.pack $ moduleNameString $ unLoc $ ideclName imp
      inline = case ideclImportList imp of
        Just (_, l) -> contextual (ImportListContext modName) query l
        Nothing     -> NoContext
   in inline `tighten` contextual (ImportModuleContext modName) query limp

getDeclContext :: Range -> LHsDecl GhcPs -> ContextResult
getDeclContext query =
  gather (mkQ (NoContext, False) (declQ query) `extQ` sigQ query `extQ` bindQ query `extQ` typeQ query)

-- * SYB query types

-- | Does this signature carry a type? Fixity, INLINE, MINIMAL and similar
-- pragmas do not.
typeSig :: Sig GhcPs -> Bool
typeSig TypeSig {}    = True
typeSig ClassOpSig {} = True
typeSig PatSynSig {}  = True
typeSig _             = False

-- | Classify a top-level declaration.
declQ :: Range -> LHsDecl GhcPs -> (ContextResult, Bool)
declQ query decl'@(L _ decl) =
  contInRange query (rangeOf decl') $ \declRange -> case decl of
    SigD _ sig | typeSig sig -> (ContextResult declRange TypeContext, True)
    ValD {}                  -> (ContextResult declRange ValueContext, False)
    _                        -> (NoContext, False)

-- | A signature reached by descent (local, class, or instance). Only the
-- type-bearing ones are a type context.
sigQ :: Range -> LSig GhcPs -> (ContextResult, Bool)
sigQ query lsig@(L _ sig)
  | typeSig sig = stopAt TypeContext query lsig
  | otherwise   = (NoContext, False)

bindQ :: Range -> LHsBind GhcPs -> (ContextResult, Bool)
bindQ query = descendInto ValueContext query

typeQ :: Range -> LHsType GhcPs -> (ContextResult, Bool)
typeQ query = stopAt TypeContext query

#if MIN_VERSION_ghc(9,9,0)
stopAt, descendInto :: HasLoc a => Context -> Range -> a -> (ContextResult, Bool)
contextual :: HasLoc a => Context -> Range -> a -> ContextResult
#else
stopAt, descendInto :: Context -> Range -> GenLocated (SrcSpanAnn' a) e -> (ContextResult, Bool)
contextual :: Context -> Range -> GenLocated (SrcSpanAnn' a) e -> ContextResult
#endif
-- | Match a node and stop descending (its whole range is the context).
stopAt context query s =
  contInRange query (rangeOf s) $ \range -> (ContextResult range context, True)
-- | Match a node and keep descending, so a tighter inner node can win.
descendInto context query s =
  contInRange query (rangeOf s) $ \range -> (ContextResult range context, False)
-- | The result of 'stopAt' without the descent flag, for non-SYB callers.
contextual context query s = fst (stopAt context query s)

-- | Run a continuation with the 'Range' of a source span, returning no context
-- if the span is missing or does not contain the query range.
contInRange :: Range -> Maybe Range -> (Range -> (ContextResult, Bool)) -> (ContextResult, Bool)
contInRange query range k = case range of
  Just range' | within query range' -> k range'
  _                                 -> (NoContext, True)

-- * Helpers

-- | The trailing edge is widened to the end of the source's final line, so
-- a cursor past the last token on a line still counts as inside the node
-- occupying that line.
within :: Range -> Range -> Bool
within query (Range start end) =
  query `isSubrangeOf` Range start (end { _character = maxBound })

#if MIN_VERSION_ghc(9,9,0)
rangeOf :: HasLoc a => a -> Maybe Range
rangeOf = srcSpanToRange . locA
#else
rangeOf :: GenLocated (SrcSpanAnn' a) e -> Maybe Range
rangeOf = srcSpanToRange . getLocA
#endif

-- | Variation of @Data.Generics.Schemes.everythingBut@ that combines with
-- 'tighten' and folds strictly.
gather :: GenericQ (ContextResult, Bool) -> GenericQ ContextResult
gather f = go
  where
    go :: GenericQ ContextResult
    go x = let (v, stop) = f x
           in if stop then v else foldl' tighten v (gmapQ go x)
