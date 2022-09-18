{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings     #-}

module Ide.Plugin.HaddockComments.Data
    ( genForDataDecl
    ) where

import           Control.Monad                         (unless, when)
import           Control.Monad.Trans.Class             (lift)
import           Data.Data                             (Data)
import           Data.Foldable                         (for_)
import           Data.List                             (isPrefixOf)
import qualified Data.Map.Strict                       as Map
import           Development.IDE                       (realSpan)
import           Development.IDE.GHC.Compat
import           Development.IDE.GHC.ExactPrint
import           Ide.Plugin.HaddockComments.Prelude
import           Language.Haskell.GHC.ExactPrint
import           Language.Haskell.GHC.ExactPrint.Types hiding (GhcPs)
import           Language.Haskell.GHC.ExactPrint.Utils (mkComment)

genForDataDecl :: GenComments
genForDataDecl =
    GenComments {
        title = "Generate fields comments",
        updateAnns = updateDataAnns
    }

updateDataAnns :: LHsDecl GhcPs -> TransformT Maybe ()
updateDataAnns (L declLoc (TyClD _ DataDecl {tcdDataDefn = HsDataDefn { dd_cons = cons }})) = do
    -- skip if all constructors and fields already have a haddock comment
    getAnnsT >>= (\anns -> unless (missingSomeHaddock anns cons) (lift Nothing))

    -- visit each constructor and field
    for_ (zip cons (Nothing: fmap Just cons)) $ \(con, prevCon) -> do
        let sameLineAsPrev = maybe False (\prevCon' -> notSeperatedByLineEnding prevCon' con) prevCon
        when sameLineAsPrev $ modifyAnnsT $ \anns ->
            let updateSepAnn :: Annotation -> Annotation
                updateSepAnn ann = ann {annsDP =
                    Map.toList . Map.adjust (const (DP (1, 0))) (G AnnVbar) . Map.fromList $ annsDP ann}
             in flip (maybe anns) prevCon $ \prevCon' -> Map.adjust updateSepAnn (mkAnnKey prevCon') anns
        let anchorCol = maybe 0 srcSpanStartCol . realSpan $ maybe declLoc getLoc prevCon
        modifyAnnsT $
            let updateCurrent :: Annotation -> Annotation
                updateCurrent ann = ann {
                        annPriorComments =
                            case annPriorComments ann of
                                (c, dp) : rem -> (emptyPriorHaddockComment, dp) : (c, DP (2,0)) : rem
                                _ -> [(emptyPriorHaddockComment, annEntryDelta ann)],
                        annEntryDelta = DP (1, dpCol)
                    }
                dpCol = if sameLineAsPrev then 0
                    else (maybe 2 srcSpanStartCol . realSpan $ getLoc con) - anchorCol
             in Map.adjust updateCurrent (mkAnnKey con)
        -- TODO: add haddock comments to fields
updateDataAnns _ = pure ()

    {-
        for each constructor:
            if it's on the same line as the previous one, move it to the next line
            add a leading comment, inherit the constructor's DP
            move the constructor to the next line, preserving its indentation
            for each field:
                add a leading comment, inherit the field's DP
                move the field to the next line, preserving the indentation

        special case:
            if there is an existing constructor/field on the same line, move the current constructor/field to
            the next line first, using the previous constructor/field's indentation

        algorithm to move an element to the next line, preserving its indentation:
            - if the row offset was 0, change col offset to col - col of the previous element + col of the anchor
            - otherwise, preserve the col offset
    -}

missingSomeHaddock :: Anns -> [LConDecl GhcPs] -> Bool
missingSomeHaddock anns = any $ \lcon@(L _ conDecl) -> case conDecl of
    ConDeclH98 { con_args = RecCon (L _ fields) } ->
        elem (Just False) $ hasHaddock anns lcon : fmap (hasHaddock anns) fields
    _                                       -> False -- GADT is not supported yet

notSeperatedByLineEnding :: Located a
                         -> Located a
                         -> Bool
notSeperatedByLineEnding (L (RealSrcSpan x _) _) (L (RealSrcSpan y _) _) =
    srcLocLine (realSrcSpanEnd x) == srcLocLine (realSrcSpanStart y)
notSeperatedByLineEnding _ _ = False

updateAnnsByConDecl :: LConDecl GhcPs -> Anns -> Maybe Anns
updateAnnsByConDecl lconDecl@(L (RealSrcSpan conLoc _) ConDeclH98 { con_args = RecCon (L _ fields) }) anns = do
    annConDecl <- anns Map.!? mkAnnKey lconDecl

    Nothing -- TODO
  where
    conStartLoc = realSrcSpanStart conLoc
updateAnnsByConDecl _ _ = Nothing

emptyPriorHaddockComment :: Comment
emptyPriorHaddockComment = mkComment "-- | " noSrcSpan

hasHaddock :: Data a => Anns -> Located a -> Maybe Bool
hasHaddock anns node = fmap annHasHaddock (anns Map.!? key)
  where
    key = mkAnnKey node
    annHasHaddock ann =
        any (isPriorHaddock . fst) (annPriorComments ann)
        || any (isFollowingHaddock . fst) (annFollowingComments ann)

    isPriorHaddock :: Comment -> Bool
    isPriorHaddock comment = any (`isPrefixOf` commentContents comment) ["-- |", "{-|", "{- |"]

    isFollowingHaddock :: Comment -> Bool
    isFollowingHaddock comment = any (`isPrefixOf` commentContents comment) ["-- ^", "{-^", "{- ^"]

