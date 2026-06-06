{-# LANGUAGE CPP             #-}
{-# LANGUAGE RecordWildCards #-}

-- | Version-agnostic primitives for ghc-exactprint annotations, shared by the
-- refactor and export plugins.
module Development.IDE.GHC.ExactPrint.Annotation
  ( epl
  , isCommaAnn
  , trailingAnns
  , overTrailingAnns
  , removeTrailingCommaAnn
  , ensureTrailingComma
  , withTrailingComma
  , modifyAnns
  , addParens
  ) where

import           Data.Bifunctor                  (first)
import           Development.IDE.GHC.Compat
import           Development.IDE.GHC.Orphans     ()
#if MIN_VERSION_ghc(9,11,0)
import           GHC                             (DeltaPos (..), EpAnn (..),
                                                  EpToken (..),
                                                  EpaLocation,
                                                  EpaLocation' (..),
                                                  NameAdornment (..),
                                                  SrcSpanAnnA, TrailingAnn (..))
import           GHC.Types.SrcLoc                (UnhelpfulSpanReason (..))
#elif MIN_VERSION_ghc(9,9,0)
import           GHC                             (DeltaPos (..), EpAnn (..),
                                                  EpaLocation,
                                                  EpaLocation' (..),
                                                  NameAdornment (..),
                                                  SrcSpanAnnA, TrailingAnn (..))
#else
import           GHC                             (DeltaPos (..), EpAnn (..),
                                                  EpaLocation (..),
                                                  NameAdornment (NameParens),
                                                  SrcSpanAnn' (..), SrcSpanAnnA,
                                                  TrailingAnn (..))
#endif
import           Language.Haskell.GHC.ExactPrint (addComma)

-- | An entry delta of @n@ spaces on the same line.
epl :: Int -> EpaLocation
#if MIN_VERSION_ghc(9,11,0)
epl n = EpaDelta (UnhelpfulSpan UnhelpfulNoLocationInfo) (SameLine n) []
#else
epl n = EpaDelta (SameLine n) []
#endif

isCommaAnn :: TrailingAnn -> Bool
isCommaAnn AddCommaAnn{} = True
isCommaAnn _             = False

trailingAnns :: SrcSpanAnnA -> [TrailingAnn]
#if MIN_VERSION_ghc(9,9,0)
trailingAnns (EpAnn _ (AnnListItem as) _) = as
#else
trailingAnns sa = case ann sa of
  EpAnn _ (AnnListItem as) _ -> as
  _                          -> []
#endif

-- | Map over an item's trailing annotations, hiding the version-specific 'AnnListItem' shape.
overTrailingAnns :: ([TrailingAnn] -> [TrailingAnn]) -> SrcSpanAnnA -> SrcSpanAnnA
#if MIN_VERSION_ghc(9,9,0)
overTrailingAnns f (EpAnn anc (AnnListItem as) cs) = EpAnn anc (AnnListItem (f as)) cs
#else
overTrailingAnns _ it@(SrcSpanAnn EpAnnNotUsed _) = it
overTrailingAnns f (SrcSpanAnn (EpAnn anc (AnnListItem as) cs) l) =
  SrcSpanAnn (EpAnn anc (AnnListItem (f as)) cs) l
#endif

removeTrailingCommaAnn :: SrcSpanAnnA -> SrcSpanAnnA
removeTrailingCommaAnn = overTrailingAnns (filter (not . isCommaAnn))

ensureTrailingComma :: SrcSpanAnnA -> SrcSpanAnnA
ensureTrailingComma ann
  | any isCommaAnn (trailingAnns ann) = ann
  | otherwise = addComma ann

-- | Replace an item's trailing comma with @c@, preserving its delta.
withTrailingComma :: TrailingAnn -> SrcSpanAnnA -> SrcSpanAnnA
withTrailingComma c = overTrailingAnns (\as -> filter (not . isCommaAnn) as ++ [c])

modifyAnns :: LocatedAn a ast -> (a -> a) -> LocatedAn a ast
#if MIN_VERSION_ghc(9,9,0)
modifyAnns x f = first (fmap f) x
#else
modifyAnns x f = first ((fmap . fmap) f) x
#endif

addParens :: Bool -> NameAnn -> NameAnn
#if MIN_VERSION_ghc(9,11,0)
addParens True it@NameAnn{} =
  it{nann_adornment = NameParens (EpTok (epl 0)) (EpTok (epl 0)) }
addParens True it@NameAnnCommas{} =
  it{nann_adornment = NameParens (EpTok (epl 0)) (EpTok (epl 0)) }
addParens True it@NameAnnOnly{} =
  it{nann_adornment = NameParens (EpTok (epl 0)) (EpTok (epl 0)) }
addParens True it@NameAnnTrailing{} =
  NameAnn{nann_adornment = NameParens (EpTok (epl 0)) (EpTok (epl 0)), nann_name = epl 0, nann_trailing = nann_trailing it}
#else
addParens True it@NameAnn{} =
  it{nann_adornment = NameParens, nann_open=epl 0, nann_close=epl 0 }
addParens True it@NameAnnCommas{} =
  it{nann_adornment = NameParens, nann_open=epl 0, nann_close=epl 0 }
addParens True it@NameAnnOnly{} =
  it{nann_adornment = NameParens, nann_open=epl 0, nann_close=epl 0 }
addParens True NameAnnTrailing{..} =
  NameAnn{nann_adornment = NameParens, nann_open=epl 0, nann_close=epl 0, nann_name = epl 0, ..}
#endif
addParens _ it = it
