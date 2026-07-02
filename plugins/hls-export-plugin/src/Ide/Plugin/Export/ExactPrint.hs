{-# LANGUAGE CPP                 #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Ide.Plugin.Export.ExactPrint
  ( LExportList
  , mkExportIE
  , appendIE
  , removeMatchingIE
  , addCtorUnderParent
  , removeCtorUnderParent
  , printExportList
  , printIE
  , freshCtorEntry
  ) where

import           Control.Lens                              (_last, over)
import           Data.Bifunctor                            (first)
import           Data.List                                 (mapAccumL)
import           Data.List.NonEmpty                        (NonEmpty (..))
import           Data.Text                                 (Text)
import qualified Data.Text                                 as T
import           Development.IDE.GHC.Compat
import           Development.IDE.GHC.Orphans               ()
#if MIN_VERSION_ghc(9,11,0)
import           GHC                                       (DeltaPos (..),
                                                            TrailingAnn (..))
#elif MIN_VERSION_ghc(9,9,0)
import           GHC                                       (DeltaPos (..),
                                                            LocatedL,
                                                            NoAnn (..),
                                                            TrailingAnn (..),
                                                            noAnn)
#else
import           GHC                                       (DeltaPos (..),
                                                            LocatedL,
                                                            TrailingAnn (..),
                                                            addAnns,
                                                            emptyComments,
                                                            noAnn)
#endif

import           Language.Haskell.GHC.ExactPrint           (addComma,
                                                            exactPrint,
                                                            getEntryDP,
                                                            setEntryDP)

#if MIN_VERSION_ghc(9,11,0)
import           GHC                                       (EpToken (..),
                                                            LocatedLI)
#else
import           GHC                                       (AddEpAnn (..))
#endif
import           Data.Maybe                                (listToMaybe)
import           Development.IDE.GHC.ExactPrint.Annotation (ensureTrailingComma,
                                                            epl, isCommaAnn,
                                                            parenthesizeName,
                                                            removeTrailingCommaAnn,
                                                            trailingAnns,
                                                            withTrailingComma)
import           GHC                                       (LocatedN)
import           Ide.Plugin.Export.Cursor                  (ExportFlavor (..))
import           Ide.Plugin.Export.Utils

-- | Located @[LIE GhcPs]@, the shape of an export list. Aliases either
-- 'LocatedL' (pre-9.12) or 'LocatedLI'.
#if MIN_VERSION_ghc(9,11,0)
type LExportList = LocatedLI [LIE GhcPs]
#else
type LExportList = LocatedL [LIE GhcPs]
#endif

mkExportIE :: ExportFlavor -> RdrName -> LIE GhcPs
mkExportIE flavor rdr = case flavor of
  ExportName    -> ieVar (mkWrappedName WrapPlain rdr)
  ExportPattern -> ieVar (mkWrappedName WrapPattern rdr)
  ExportFamily  -> mkTypeAbsIE' (mkWrappedName keywordWrap rdr)
  ExportAll     -> mkTypeAllIE' (mkWrappedName keywordWrap rdr)
  where
    keywordWrap
      | isSymOcc (rdrNameOcc rdr) = WrapType
      | otherwise                 = WrapPlain

ieVar :: LIEWrappedName GhcPs -> LIE GhcPs
ieVar w =
  reLocA $ L noSrcSpan $ IEVar
#if MIN_VERSION_ghc(9,8,0)
    Nothing
#else
    noExtField
#endif
    w
#if MIN_VERSION_ghc(9,9,0)
    Nothing
#endif

mkTypeAbsIE' :: LIEWrappedName GhcPs -> LIE GhcPs
mkTypeAbsIE' w =
  reLocA $ L noSrcSpan $ IEThingAbs
#if MIN_VERSION_ghc(9,11,0)
    Nothing
#elif MIN_VERSION_ghc(9,8,0)
    (Nothing, noAnn)
#else
    noAnn
#endif
    w
#if MIN_VERSION_ghc(9,9,0)
    Nothing
#endif

mkTypeAllIE' :: LIEWrappedName GhcPs -> LIE GhcPs
mkTypeAllIE' w =
  reLocA $ L noSrcSpan $ IEThingAll
#if MIN_VERSION_ghc(9,11,0)
    (Nothing, (EpTok (epl 1), EpTok (epl 0), EpTok (epl 0)))
#elif MIN_VERSION_ghc(9,9,0)
    ( Nothing
    , [ AddEpAnn AnnOpenP  (epl 1)
      , AddEpAnn AnnDotdot (epl 0)
      , AddEpAnn AnnCloseP (epl 0)
      ]
    )
#elif MIN_VERSION_ghc(9,8,0)
    ( Nothing
    , addAnns mempty
        [ AddEpAnn AnnOpenP  (epl 1)
        , AddEpAnn AnnDotdot (epl 0)
        , AddEpAnn AnnCloseP (epl 0)
        ]
        emptyComments
    )
#else
    (addAnns mempty
       [ AddEpAnn AnnOpenP  (epl 1)
       , AddEpAnn AnnDotdot (epl 0)
       , AddEpAnn AnnCloseP (epl 0)
       ]
       emptyComments)
#endif
    w
#if MIN_VERSION_ghc(9,9,0)
    Nothing
#endif

-- | @T(C1, C2, ...)@. The non-empty list is the child constructors.
mkTypeWithIE :: RdrName -> NonEmpty RdrName -> LIE GhcPs
mkTypeWithIE parent ctors =
  reLocA $ L noSrcSpan $ IEThingWith
#if MIN_VERSION_ghc(9,11,0)
    (Nothing, (EpTok (epl 1), NoEpTok, NoEpTok, EpTok (epl 0)))
#elif MIN_VERSION_ghc(9,9,0)
    (Nothing, [AddEpAnn AnnOpenP (epl 1), AddEpAnn AnnCloseP (epl 0)])
#elif MIN_VERSION_ghc(9,8,0)
    ( Nothing
    , addAnns mempty
        [AddEpAnn AnnOpenP (epl 1), AddEpAnn AnnCloseP (epl 0)]
        emptyComments
    )
#else
    (addAnns mempty
       [AddEpAnn AnnOpenP (epl 1), AddEpAnn AnnCloseP (epl 0)]
       emptyComments)
#endif
    (mkIEName parent)
    NoIEWildcard
    children
#if MIN_VERSION_ghc(9,9,0)
    Nothing
#endif
  where
    children = mkIEName c : map (first addComma . mkIEName) cs
    c :| cs = ctors

-- | Map over an @IEThingWith@'s listed constructors, a no-op for any other item.
overThingWithChildren :: ([LIEWrappedName GhcPs] -> [LIEWrappedName GhcPs]) -> IE GhcPs -> IE GhcPs
#if MIN_VERSION_ghc(9,9,0)
overThingWithChildren f (IEThingWith x n w cs docs) = IEThingWith x n w (f cs) docs
#else
overThingWithChildren f (IEThingWith x n w cs)      = IEThingWith x n w (f cs)
#endif
overThingWithChildren _ ie                          = ie

data WrapKind = WrapPlain | WrapPattern | WrapType

mkIEName :: RdrName -> LIEWrappedName GhcPs
mkIEName = mkWrappedName WrapPlain

-- | Wrap an 'RdrName' as an export item. Operators are parenthesized and any
-- @pattern@ or @type@ keyword is followed by a single space.
mkWrappedName :: WrapKind -> RdrName -> LIEWrappedName GhcPs
mkWrappedName kind rdr =
  reLocA $ L noSrcSpan $ case kind of
    WrapPlain   -> IEName noExtField plainName
    WrapPattern -> IEPattern keywordTok spacedName
    WrapType    -> IEType keywordTok spacedName
  where
    plainName = parenthesizeOperator (reLocA (L noSrcSpan rdr))
    spacedName = setEntryDP plainName (SameLine 1)
    keywordTok =
#if MIN_VERSION_ghc(9,11,0)
      EpTok (epl 0)
#else
      epl 0
#endif

parenthesizeOperator :: LocatedN RdrName -> LocatedN RdrName
parenthesizeOperator ln
  | isSymOcc (rdrNameOcc (unLoc ln)) = parenthesizeName ln
  | otherwise = ln

appendIE :: LIE GhcPs -> LExportList -> LExportList
appendIE item (L l items) = L l (fixLast items ++ [newItem (not (null items))])
  where
    newItem hasSibling =
      setEntryDP (first removeTrailingCommaAnn item) (SameLine (if hasSibling then 1 else 0))
    -- Reuse the comma that already separates the list's items. On a multiline
    -- leading comma list that comma carries a 'DifferentLine' delta, so the new
    -- separator lands on its own line instead of collapsing onto the last item.
    fixLast = over _last (first addSep)
    addSep = maybe ensureTrailingComma withTrailingComma (separatorComma items)

-- | The trailing comma that separates existing items, if the list has any.
separatorComma :: [LIE GhcPs] -> Maybe TrailingAnn
separatorComma items =
  listToMaybe [c | L ann _ <- items, c <- trailingAnns ann, isCommaAnn c]

-- | Drop the first element matching @p@. A removed head hands its entry delta
-- to the next element so the list keeps its start, and the new last element
-- sheds its separator comma.
--
-- - @Nothing@ if nothing matches
-- - @Just []@ if the sole element was removed.
removeListItem
  :: (LocatedAn AnnListItem a -> Bool)
  -> [LocatedAn AnnListItem a]
  -> Maybe [LocatedAn AnnListItem a]
removeListItem p items = case break p items of
  (_, [])               -> Nothing
  (pre, removed : post) ->
    let survivors = case (pre, post) of
          ([], next : rest) -> setEntryDP next (getEntryDP removed) : rest
          _                 -> pre ++ post
     in Just (over _last (first removeTrailingCommaAnn) survivors)

removeMatchingIE :: (IE GhcPs -> Bool) -> LExportList -> Maybe LExportList
removeMatchingIE p (L l items) = L l <$> removeListItem (p . unLoc) items

-- | 'Nothing' iff @ctor@ is already exported (via @T(..)@ or @T(...,ctor,...)@).
addCtorUnderParent ::
  -- | parent
  RdrName ->
  -- | ctor
  RdrName ->
  LExportList ->
  Maybe LExportList
addCtorUnderParent parent ctor lst@(L l items) =
  case ctorExportEdit parent ctor items of
    AlreadyExported -> Nothing
    AppendParent    -> Just (appendIE newThing lst)
    UpgradeBare     -> Just (L l (map (transformParent (const (unLoc newThing))) items))
    AddChild        -> Just (L l (map (transformParent (addCtorChildren ctor)) items))
  where
    newThing = mkTypeWithIE parent (ctor :| [])
    transformParent f (L itemLoc ie)
      | parentNameIs (rdrNameFS parent) ie = L itemLoc (f ie)
      | otherwise = L itemLoc ie

-- | Append @ctor@ to an @IEThingWith@'s children, reusing the sibling separator
-- comma. No-op for other items.
addCtorChildren :: RdrName -> IE GhcPs -> IE GhcPs
addCtorChildren ctor = overThingWithChildren $ \cs ->
  let hasSibling = not (null cs)
      newChild = setEntryDP (mkIEName ctor) (SameLine (if hasSibling then 1 else 0))
   in (if hasSibling then map (first ensureTrailingComma) cs else cs) ++ [newChild]

-- | Remove @ctor@ from the export entries listing it under @parent@, or
-- 'Nothing' if none does. Removing the last child downgrades @T(ctor)@ to @T@.
removeCtorUnderParent ::
  -- | parent
  RdrName ->
  -- | ctor
  RdrName ->
  LExportList ->
  Maybe LExportList
removeCtorUnderParent parent ctor (L l items)
  | edited    = Just (L l items')
  | otherwise = Nothing
  where
    (edited, items') = mapAccumL dropCtor False items
    parentFS = rdrNameFS parent
    ctorFS = rdrNameFS ctor
    isCtor = (== ctorFS) . lieWrappedNameFS

    dropCtor changed item@(L itemLoc ie)
      | parentNameIs parentFS ie
      , Just children <- ieThingWithChildren ie
      , Just kept <- removeListItem isCtor children
      = (True, L itemLoc (rebuild ie kept))
      | otherwise = (changed, item)

    -- An empty child list means @ctor@ was the only child, so collapse @T(ctor)@
    -- to a bare @T@.
    rebuild ie []   = downgradeToAbs ie
    rebuild ie kept = overThingWithChildren (const kept) ie

    -- Reuse the head so @type@/operator wrapping survives, e.g. @type (:<)(C)@
    -- becomes @type (:<)@.
    downgradeToAbs ie = case ieThingWithHead ie of
      Just n  -> unLoc (mkTypeAbsIE' (setEntryDP n (SameLine 0)))
      Nothing -> ie

printExportList :: LExportList -> Text
printExportList l = T.pack (exactPrint (setEntryDP l (SameLine 0)))

-- | Exactprint a single item, without the surrounding list layout. Dropping
-- the trailing comma keeps a spliced item from carrying it into text that
-- already supplies its own.
printIE :: LIE GhcPs -> Text
printIE item = T.pack (exactPrint (setEntryDP (first removeTrailingCommaAnn item) (SameLine 0)))

-- | A fresh @T(ctor)@ export entry rendered as text, or 'Nothing' if @ctor@ is
-- already exported in the parsed list. Under CPP this adds a standalone entry so
-- the splice never reprints an existing @T(...)@ span, which can straddle a
-- directive.
freshCtorEntry :: RdrName -> RdrName -> [LIE GhcPs] -> Maybe Text
freshCtorEntry parent ctor items = case ctorExportEdit parent ctor items of
  AlreadyExported -> Nothing
  _               -> Just (printIE (mkTypeWithIE parent (ctor :| [])))

-- | How to add @ctor@ to an export list so its parent type @T@ exports it.
data CtorEdit
  = AlreadyExported  -- ^ @T(..)@ or @T(..., ctor, ...)@, nothing to do
  | AppendParent     -- ^ no entry for @T@ yet, add a fresh @T(ctor)@
  | UpgradeBare      -- ^ replace the bare @T@ entry with @T(ctor)@
  | AddChild         -- ^ add @ctor@ to the existing @T(...)@ entry

-- | Decide how @ctor@ should be added under @parent@, classifying the first
-- matching export item by its constructor-carrying shape.
ctorExportEdit :: RdrName -> RdrName -> [LIE GhcPs] -> CtorEdit
ctorExportEdit parent ctor = go
  where
    parentFS = rdrNameFS parent
    ctorFS = rdrNameFS ctor
    go [] = AppendParent
    go (L _ ie : rest)
      | parentNameIs parentFS ie = case ie of
          IEThingAll {} -> AlreadyExported
          IEThingAbs {} -> UpgradeBare
          _ | Just cs <- ieThingWithChildren ie ->
                if any ((== ctorFS) . lieWrappedNameFS) cs then AlreadyExported else AddChild
            | otherwise -> go rest
      | otherwise = go rest
