{-# LANGUAGE CPP                 #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Ide.Plugin.Export.ExactPrint
  ( LExportList
  , mkExportIE
  , appendIE
  , addCtorUnderParent
  , printExportList
  , toDeltaExportList
  ) where

import           Control.Lens                              (_last, over)
import           Data.Bifunctor                            (first)
import           Data.List.NonEmpty                        (NonEmpty (..))
import qualified Data.List.NonEmpty                        as NE
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
                                                            makeDeltaAst,
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
    children = case NE.toList ctors of
      []     -> [] -- impossible
      (c:cs) -> mkIEName c : map (\x -> first addComma (mkIEName x)) cs

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

-- | 'Nothing' iff @ctor@ is already exported (via @T(..)@ or @T(...,ctor,...)@).
addCtorUnderParent ::
  RdrName {- ^ parent -} ->
  RdrName {- ^ ctor -} ->
  LExportList ->
  Maybe LExportList
addCtorUnderParent parent ctor lst@(L l items) =
  case findParent items of
    ParentNotFound -> Just $ appendIE (mkTypeWithIE parent (ctor :| [])) lst
    FoundIEThingAll -> Nothing
    FoundIEThingWith CtorPresent -> Nothing
    FoundIEThingWith CtorAbsent -> Just (L l (map (transformParent extendThingWith) items))
    FoundIEThingAbs ->
      let upgraded = unLoc (mkTypeWithIE parent (ctor :| []))
      in Just (L l (map (transformParent (const upgraded)) items))
  where
    parentFS = rdrNameFS parent
    ctorFS = rdrNameFS ctor

    ctorPresence cs
      | any ((== ctorFS) . lieWrappedNameFS) cs = CtorPresent
      | otherwise = CtorAbsent

    findParent [] = ParentNotFound
    findParent (L _ ie : rest)
      | parentNameIs parentFS ie =
          case ie of
            IEThingAll{} -> FoundIEThingAll
            IEThingAbs{} -> FoundIEThingAbs
            _ | Just cs <- ieThingWithChildren ie -> FoundIEThingWith (ctorPresence cs)
              | otherwise                         -> findParent rest
      | otherwise = findParent rest

    transformParent f (L itemLoc ie)
      | parentNameIs parentFS ie = L itemLoc (f ie)
      | otherwise = L itemLoc ie

    extendThingWith :: IE GhcPs -> IE GhcPs
    extendThingWith = overThingWithChildren $ \cs ->
      let hasSibling = not (null cs)
          newChild = setEntryDP (mkIEName ctor) (SameLine (if hasSibling then 1 else 0))
      in (if hasSibling then map (first ensureTrailingComma) cs else cs) ++ [newChild]

printExportList :: LExportList -> Text
printExportList l = T.pack (exactPrint (setEntryDP l (SameLine 0)))

toDeltaExportList :: LExportList -> LExportList
toDeltaExportList = makeDeltaAst

data FindParentResult
  = ParentNotFound
  | FoundIEThingAll
  | FoundIEThingWith CtorPresence
  | FoundIEThingAbs

data CtorPresence = CtorAbsent | CtorPresent
  deriving Eq
