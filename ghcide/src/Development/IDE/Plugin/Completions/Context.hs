{-# LANGUAGE CPP            #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RankNTypes     #-}

-- | Completion-context detection. Given a parsed module and a cursor position,
-- determine a context the cursor sits in so the completion logic can pick which
-- completions to offer.
module Development.IDE.Plugin.Completions.Context
  ( Context (..)
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

-- | Used during context finding. @(<>)@ keeps the tightest (innermost) match.
data ContextResult = NoContext | ContextResult !Range !Context
instance Monoid ContextResult where mempty = NoContext
instance Semigroup ContextResult where (<>) = tighten

tighten :: ContextResult -> ContextResult -> ContextResult
tighten NoContext b = b
tighten a NoContext = a
tighten ar@(ContextResult a _) br@(ContextResult b _)
  | b `isSubrangeOf` a = br
  | otherwise          = ar

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
  case foldMap (getExportContext q) (maybeToList hsmodExports)
       <> foldMap (getImportContext q) hsmodImports
       <> foldMap (getDeclContext q) hsmodDecls of
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
   in inline <> contextual (ImportModuleContext modName) query limp

getDeclContext :: Range -> LHsDecl GhcPs -> ContextResult
getDeclContext query =
  gather (mkQ (mempty, False) (declQ query) `extQ` sigQ query `extQ` bindQ query `extQ` typeQ query)

-- * SYB query types

-- | A declaration node. Type signatures are types and value bindings are
-- values. Other declarations carry no context of their own. We descend into
-- them so nested signatures, bindings, and inline type annotations resolve.
declQ :: Range -> LHsDecl GhcPs -> (ContextResult, Bool)
declQ query decl'@(L _ decl) =
  contInRange query (rangeOf decl') $ \declRange -> case decl of
    SigD {} -> (ContextResult declRange TypeContext, True)
    ValD {} -> (ContextResult declRange ValueContext, False)
    _       -> (NoContext, False)

sigQ :: Range -> LSig GhcPs -> (ContextResult, Bool)
sigQ query = stopAt TypeContext query

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
-- if the span is missing or outside the query range.
contInRange :: Range -> Maybe Range -> (Range -> (ContextResult, Bool)) -> (ContextResult, Bool)
contInRange query range k = case range of
  Nothing                            -> (NoContext, True)
  Just range' | outside query range' -> (NoContext, True)
  Just range'                        -> k range'

-- * Helpers

-- | A query range is outside a source range if it ends before the source
-- starts, or it starts on a line after the source ends. We compare only lines
-- for the trailing boundary so a cursor past the last token on a line still
-- falls inside the node occupying that line.
outside :: Range -> Range -> Bool
outside (Range ps pe) (Range qs qe) = pe < qs || _line ps > _line qe

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
