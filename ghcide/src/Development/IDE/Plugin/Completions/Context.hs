{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE NoFieldSelectors   #-}
{-# LANGUAGE TypeFamilies       #-}

module Development.IDE.Plugin.Completions.Context
  ( Context (..)
  , ContextMap
  , GetContextMap (..)
  , getContext
  , getContextMap
  ) where

import           Control.DeepSeq                      (NFData (..), rwhnf)
import           Data.Hashable                        (Hashable)
import           Data.List                            (maximumBy, singleton)
import           Data.Maybe                           (maybeToList)
import           Data.Ord                             (Down (..), comparing)
import qualified Data.Text                            as T
import           Development.IDE
import           Development.IDE.Core.PositionMapping
import           Development.IDE.GHC.Compat           hiding (getContext)
import           Development.IDE.GHC.Compat.Util      (bagToList)
import           GHC.Generics                         (Generic)

-- | A context of a declaration in the program
-- e.g. is the declaration a type declaration or a value declaration
-- Used for determining which code completions to show
data Context
  = TypeContext
  | ValueContext
  | -- | import context with module name
    ImportContext T.Text
  | -- | import list context with module name
    ImportListContext T.Text
  | -- | import hiding context with module name
    ImportHidingContext T.Text
  | -- | List of exported identifiers from the current module
    ExportContext
  | -- | Top-level context
    TopContext
  | -- | Unsupported context, a placeholder context where we give up being smart
    -- and show all known symbols.
    DefaultContext
  deriving (Show, Eq)

instance Pretty Context where
  pretty = \case
    TypeContext -> "type context"
    ValueContext -> "value context"
    ImportContext mod -> "import context " <> pretty mod
    ImportListContext mod -> "import explicit context " <> pretty mod
    ImportHidingContext mod -> "import hiding context " <> pretty mod
    ExportContext -> "export context"
    TopContext -> "top context"
    DefaultContext -> "unknown context"

data GetContextMap = GetContextMap
  deriving (Eq, Show, Generic)
instance Hashable GetContextMap
instance NFData GetContextMap
type instance RuleResult GetContextMap = ContextMap

-- | A lazy chunked interval structure for context lookups.
--
-- Entries within each chunk are from a contiguous group of source items
-- (imports or declarations). The spine is purposefully lazy, to avoid finding
-- what context the cursor precisely is in.
data ContextChunk = Chunk
  { low   :: {-# UNPACK #-} !Position
  , high  :: {-# UNPACK #-} !Position
  , items :: [(Range, Context)]
  }

-- | Build a single 'Chunk' from a flat list of entries, or 'ChunkEnd' if empty.
singleChunk :: [(Range, Context)] -> ContextMap
singleChunk [] = ContextMap mempty
singleChunk items = ContextMap $ singleton $ Chunk
  (minimum (map (_start . fst) items))
  (maximum (map (_end   . fst) items))
  items

-- | Build lazy 'ContextChunks' by processing @n@ source items at a time.
-- The spine past the first chunk is a thunk until queried.
groupedChunks :: Int -> (a -> [(Range, Context)]) -> [a] -> ContextMap
groupedChunks n f xs = ContextMap $ go xs
  where
    go [] = []
    go xs =
      let (group, rest) = splitAt n xs
          items = concatMap f group
      in case items of
            [] -> go rest
            _  -> Chunk
              { low   = minimum (map (_start . fst) items)
              , high  = maximum (map (_end   . fst) items)
              , items = items
              } : go rest

newtype ContextMap = ContextMap [ContextChunk]
  deriving newtype (Monoid, Semigroup)
instance Show ContextMap where show _ = "<context map>"
instance NFData ContextMap where rnf = rwhnf

-- | Build a 'ContextMap' from a parsed module.
--
-- Walks module header, exports, imports, and top-level declarations
-- (one level into class bodies). Built once per file edit and cached
-- as a Shake rule.
getContextMap :: ParsedModule -> ContextMap
getContextMap pm =
  singleChunk exportEntry
    <> groupedChunks 10 importEntry hsmodImports
    <> groupedChunks 10 declEntry hsmodDecls
  where
    HsModule {hsmodExports, hsmodImports, hsmodDecls} =
      unLoc (pm_parsed_source pm)

    -- Export list -> ExportContext
    exportEntry :: [(Range, Context)]
    exportEntry = case hsmodExports of
      Just (L (locA -> ss) _) ->
        maybeToList $ (,ExportContext) <$> srcSpanToRange ss
      Nothing -> []

    importEntry :: LImportDecl GhcPs -> [(Range, Context)]
    importEntry (L (locA -> ss) impDecl) =
      let modName = T.pack $ moduleNameString $ unLoc $ ideclName impDecl
          outerCtx = (,ImportContext modName) <$> srcSpanToRange ss
          innerCtx = importListEntry modName (fmap (fmap reLoc) $ ideclImportList impDecl)
       in maybeToList outerCtx ++ innerCtx

    importListEntry :: T.Text -> Maybe (ImportListInterpretation, Located [LIE GhcPs]) -> [(Range, Context)]
    importListEntry modName (Just (EverythingBut, L ss _)) =
      maybeToList $ (,ImportHidingContext modName) <$> srcSpanToRange ss
    importListEntry modName (Just (Exactly, L ss _)) =
      maybeToList $ (,ImportListContext modName) <$> srcSpanToRange ss
    importListEntry _ _ = []

    declEntry :: LHsDecl GhcPs -> [(Range, Context)]
    declEntry (L (locA -> ss) decl) = case srcSpanToRange ss of
      Nothing -> []
      Just range -> case decl of
        SigD {}                -> [(range, TypeContext)]
        ValD _ bind            -> (range, ValueContext) : bindEntries bind
        TyClD _ cd@ClassDecl{} -> (range, TypeContext) : classEntries cd
        TyClD {}               -> [(range, TypeContext)]   -- DataDecl, SynDecl, FamilyDecl
        InstD _ instDecl       -> (range, ValueContext) : instEntries instDecl
        DerivD {}              -> [(range, TypeContext)]
        ForD {}                -> [(range, ValueContext)]
        SpliceD {}             -> [(range, TopContext)]
        _                      -> [(range, DefaultContext)]  -- DefD, WarningD, AnnD, RuleD, DocD, KindSigD

    sigsAndBindEntries :: [LSig GhcPs] -> LHsBinds GhcPs -> [(Range, Context)]
    sigsAndBindEntries sigs binds =
      [ (r, TypeContext)
      | L (locA -> ss) _ <- sigs
      , Just r <- [srcSpanToRange ss]
      ] ++
      [ entry
      | L (locA -> ss) bind <- bagToList binds
      , Just r <- [srcSpanToRange ss]
      , entry <- (r, ValueContext) : bindEntries bind
      ]

    classEntries :: TyClDecl GhcPs -> [(Range, Context)]
    classEntries ClassDecl { tcdSigs, tcdMeths } = sigsAndBindEntries tcdSigs tcdMeths
    classEntries _ = []

    instEntries :: InstDecl GhcPs -> [(Range, Context)]
    instEntries ClsInstD { cid_inst = ClsInstDecl { cid_sigs, cid_binds } } =
      sigsAndBindEntries cid_sigs cid_binds
    instEntries _ = []

    bindEntries :: HsBind GhcPs -> [(Range, Context)]
    bindEntries FunBind { fun_matches = MG { mg_alts = L _ alts } } =
      concatMap matchLocalEntries alts
    bindEntries PatBind { pat_rhs = GRHSs { grhssLocalBinds } } =
      localBindEntries grhssLocalBinds
    bindEntries _ = []

    matchLocalEntries :: LMatch GhcPs (LHsExpr GhcPs) -> [(Range, Context)]
    matchLocalEntries (L _ Match { m_grhss = GRHSs { grhssLocalBinds } }) =
      localBindEntries grhssLocalBinds

    localBindEntries :: HsLocalBinds GhcPs -> [(Range, Context)]
    localBindEntries (HsValBinds _ (ValBinds _ binds sigs)) =
      sigsAndBindEntries sigs binds
    localBindEntries _ = []

-- | Look up the completion context at a given position.
-- Returns the innermost (most specific) context that contains the position.
--
-- Only the 'ContextChunks' up to and including the chunk containing the
-- query position are forced; later chunks remain as unevaluated thunks.
getContext :: ContextMap -> PositionResult Position -> Context
getContext (ContextMap chunks) pos =
  case searchChunks chunks of
    [] -> TopContext
    xs -> snd $ maximumBy (comparing (\(Range s e, _) -> (s, Down e))) xs
  where
    (qLo, qHi) = case pos of
      PositionExact p   -> (p, p)
      PositionRange l u -> (l, u)

    dominates :: (Range, Context) -> Bool
    dominates (Range s e, _) = s <= qLo && qHi <= e

    searchChunks :: [ContextChunk] -> [(Range, Context)]
    searchChunks [] = []
    searchChunks (Chunk cLo cHi items : rest)
      | qLo > cHi = searchChunks rest  -- query is past this chunk
      | qHi < cLo = []                 -- query is before this chunk; stop (source order)
      | otherwise  = filter dominates items ++ searchChunks rest
