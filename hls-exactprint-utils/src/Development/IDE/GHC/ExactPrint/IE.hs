{-# LANGUAGE CPP #-}

-- | Builders for fresh import/export items with the annotations
-- exact-print needs to render them.
module Development.IDE.GHC.ExactPrint.IE
  ( WrapKind (..)
  , mkWrappedName
  , mkIEName
  , ieVar
  , mkTypeWithIE
  ) where

import           Data.Bifunctor                            (first)
import           Data.List.NonEmpty                        (NonEmpty (..))
import           Development.IDE.GHC.Compat
import           Development.IDE.GHC.ExactPrint.Annotation (epl,
                                                            parenthesizeName)
import           Development.IDE.GHC.Orphans               ()
import           GHC                                       (DeltaPos (..),
                                                            LocatedN)
import           Language.Haskell.GHC.ExactPrint           (addComma,
                                                            setEntryDP)
#if MIN_VERSION_ghc(9,11,0)
import           GHC                                       (EpToken (..))
#elif MIN_VERSION_ghc(9,9,0)
import           GHC                                       (AddEpAnn (..))
#else
import           GHC                                       (AddEpAnn (..),
                                                            addAnns,
                                                            emptyComments)
#endif

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

data WrapKind = WrapPlain | WrapPattern | WrapType

mkIEName :: RdrName -> LIEWrappedName GhcPs
mkIEName = mkWrappedName WrapPlain

-- | Wrap an 'RdrName' as an import/export item. Operators are parenthesized
-- and any @pattern@ or @type@ keyword is followed by a single space.
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
