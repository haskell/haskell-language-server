{-# LANGUAGE CPP                   #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE LambdaCase            #-}
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
import           Data.Maybe                            (fromMaybe)
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
updateDataAnns decl@(L declLoc (TyClD _ DataDecl {tcdDataDefn = HsDataDefn { dd_cons = cons }})) = do
    -- skip if all constructors and fields already have a haddock comment
    getAnnsT >>= (\anns -> unless (missingSomeHaddock anns cons) (lift Nothing))

    -- visit each constructor and field
    addHaddockCommentsToList True declLoc cons
    for_ cons $ \case
        L conLoc ConDeclH98 { con_args = RecCon (L _ fields) } -> addHaddockCommentsToList False conLoc fields
        _ -> pure ()
    modifyAnnsT $ Map.adjust (\ann -> ann {annPriorComments = []}) (mkAnnKey decl)
updateDataAnns _ = pure ()

-- TODO Add explaination to this complex function.
addHaddockCommentsToList :: (Data a, Monad m) => Bool -> SrcSpan -> [Located a] -> TransformT m ()
addHaddockCommentsToList usePrevNodeAsAnchor outerLoc nodes =
    for_ (zip nodes (Nothing: fmap Just nodes)) $ \(node, prevNode) -> do
        addHaddockCommentToCurrentNode <- fmap (not . fromMaybe True . flip hasHaddock node) getAnnsT
        when addHaddockCommentToCurrentNode $ do
            let sameLineAsPrev = maybe False (\prevNode' -> notSeperatedByLineEnding prevNode' node) prevNode
            when sameLineAsPrev $ modifyAnnsT $ \anns ->
                let updateSepAnn :: Annotation -> Annotation
                    updateSepAnn ann = ann {annsDP =
                        Map.toList . Map.adjust (const (DP (1,0))) (G AnnVbar) . Map.fromList $ annsDP ann}
                 in flip (maybe anns) prevNode $ \prevNode' -> Map.adjust updateSepAnn (mkAnnKey prevNode') anns
            let anchorCol = maybe 0 srcSpanStartCol . realSpan . maybe outerLoc getLoc $
                    if usePrevNodeAsAnchor then prevNode else Nothing
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
                        else (maybe 2 srcSpanStartCol . realSpan $ getLoc node) - anchorCol
                 in Map.adjust updateCurrent (mkAnnKey node)

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

emptyPriorHaddockComment :: Comment
emptyPriorHaddockComment = mkComment "-- |"
#if MIN_VERSION_ghc(9,0,0)
    badRealSrcSpan
#else
    noSrcSpan
#endif

hasHaddock :: Data a => Anns -> Located a -> Maybe Bool
hasHaddock anns node = fmap annHasHaddock (anns Map.!? key)
  where
    key = mkAnnKey node
    annHasHaddock ann =
        any (matchCommentPrefix priorCommentPrefix . fst) (annPriorComments ann)
        || any (matchCommentPrefix followingCommentPrefix . fst) (annFollowingComments ann)
        || any (keywordIdIsHaddockComment . fst) (annsDP ann)

keywordIdIsHaddockComment :: KeywordId -> Bool
keywordIdIsHaddockComment (AnnComment comment) = any (`isPrefixOf` commentContents comment) (priorCommentPrefix ++ followingCommentPrefix)
keywordIdIsHaddockComment _ = False

priorCommentPrefix :: [String]
priorCommentPrefix = ["-- |", "{-|", "{- |"]

followingCommentPrefix :: [String]
followingCommentPrefix = ["-- ^", "{-^", "{- ^"]

matchCommentPrefix :: [String] -> Comment -> Bool
matchCommentPrefix prefix comment = any (`isPrefixOf` commentContents comment) prefix
