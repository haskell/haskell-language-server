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
import           Data.Maybe                            (fromMaybe, isJust)
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
        title = "Generate haddock comments",
        updateAnns = updateDataAnns
    }

updateDataAnns :: LHsDecl GhcPs -> TransformT Maybe ()
updateDataAnns decl@(L declLoc (TyClD _ DataDecl {tcdDataDefn = HsDataDefn { dd_cons = cons }})) = do
    -- skip if all constructors and fields already have a haddock comment
    getAnnsT >>= (\anns -> unless (missingSomeHaddock anns cons) (lift Nothing))

    -- visit each constructor and field
    addHaddockCommentsToList True declLoc (G AnnVbar) cons
    for_ cons $ \case
        L conLoc ConDeclH98 { con_args = RecCon (L _ fields) } -> addHaddockCommentsToList False conLoc (G AnnComma) fields
        _ -> pure ()
    modifyAnnsT $ Map.adjust (\ann -> ann {annPriorComments = []}) (mkAnnKey decl)
updateDataAnns _ = lift Nothing

-- | Add haddock comments to a list of nodes
addHaddockCommentsToList
    :: (Data a, Monad m)
    => Bool -- ^ If true, for each node, use previous node in the list as the anchor. Otherwise, use the outer node
    -> SrcSpan -- ^ The outer node
    -> KeywordId -- ^ The separator between adjacent nodes
    -> [Located a] -- ^ The list of nodes. Haddock comments will be added to each of them
    -> TransformT m ()
addHaddockCommentsToList usePrevNodeAsAnchor outerLoc separator nodes =
    -- If you want to understand this function, please first read this page carefully:
    --     https://hackage.haskell.org/package/ghc-exactprint-0.6.4/docs/Language-Haskell-GHC-ExactPrint-Delta.html
    -- The important part is that for DP(r,c), if r is zero, c is the offset start from the end of the previous node.
    -- However, if r is greater than zero, c is the offset start from the 'anchor'.
    -- Generally speaking, the 'anchor' is the node that "enclose" the current node. But it's not always the case.
    --     Sometimes 'anchor' is just the previous node. It depends on the the syntactic structure.
    --     For constructors, the anchor is the previous node (if there is any).
    --     For record fields, the anchor is always the constructor they belong to.
    for_ (zip nodes (Nothing: fmap Just nodes)) $ \(node, prevNode) -> do
        addHaddockCommentToCurrentNode <- fmap (not . fromMaybe True . flip hasHaddock node) getAnnsT
        -- We don't add new haddock comments to nodes with existing ones.
        when addHaddockCommentToCurrentNode $ do
            -- 'sameLineAsPrev' is a flag to determine the inline case, for example:
            --     data T = A { a :: Int, b :: String } | B { b :: Double }
            -- Note that it's a 'Maybe (Located a)', containing the previous node if the current node
            -- and the previous node are on the same line.
            --
            -- For the multiline case (which is the most common), we keep the original indentation of each constructor
            -- and field.
            --
            -- For the inline case, we use the first constructor/field as the base, and align all following items
            -- to them.
            let sameLineAsPrev = prevNode >>= (
                    \prevNode' -> if notSeparatedByLineEnding prevNode' node
                        then pure prevNode'
                        else Nothing
                    )
            -- For the inline case, we need to move the separator to the next line.
            -- For constructors, it's vertical bar; for fields, it's comma.
            -- The separator is passed in as function argument.
            when (isJust sameLineAsPrev) $ modifyAnnsT $ \anns ->
                let newSepCol :: Annotation -> Int
                    newSepCol ann =
                        if usePrevNodeAsAnchor then 0 else deltaColumn (annEntryDelta ann)
                    updateSepAnn :: Annotation -> Annotation
                    updateSepAnn ann = ann {annsDP =
                        Map.toList . Map.adjust (const $ DP (1, newSepCol ann)) separator . Map.fromList $ annsDP ann}
                 in flip (maybe anns) prevNode $ \prevNode' -> Map.adjust updateSepAnn (mkAnnKey prevNode') anns
            -- Calculate the real column of the anchor
            let anchorCol = maybe 0 srcSpanStartCol . realSpan . maybe outerLoc getLoc $
                    if usePrevNodeAsAnchor then prevNode else Nothing
            -- 'dpCol' is what we will use for the current node's entry delta's column
            dpCol <- flip fmap getAnnsT $ \anns ->
                case sameLineAsPrev of
                    Just prevNode' ->
                        -- If the previous node is the anchor, using 0 as column will make current code align with
                        -- the previous one.
                        -- Otherwise, use the column of entry delta of the previous node.
                        -- The map lookup should not fail. '2' is used as a fallback value to make sure the syntax
                        -- is correct after the changes.
                        if usePrevNodeAsAnchor then 0 else maybe 2 (deltaColumn . annEntryDelta)
                            $ anns Map.!? mkAnnKey prevNode'
                    -- We subtract the real column to get dp column.
                    Nothing -> (maybe 2 srcSpanStartCol . realSpan $ getLoc node) - anchorCol
            -- Modify the current node
            modifyAnnsT $
                let updateCurrent :: Annotation -> Annotation
                    updateCurrent ann = ann {
                            -- If there exist non-haddock comments, we simply inherit the first one's delta pos,
                            -- and move them two lines below, to separate them from our newly added haddock comments
                            -- Otherwise, inherit the node's entry delta pos.
                            annPriorComments = case annPriorComments ann of
                                (c, dp) : rem -> (emptyPriorHaddockComment, dp) : (c, DP (2,0)) : rem
                                _ -> [(emptyPriorHaddockComment, annEntryDelta ann)],
                            annEntryDelta = DP (1, dpCol)
                        }
                 in Map.adjust updateCurrent (mkAnnKey node)

-- | Determine if a list of constructor declarations is missing some haddock comments.
missingSomeHaddock :: Anns -> [LConDecl GhcPs] -> Bool
missingSomeHaddock anns = any $ \lcon@(L _ conDecl) -> case conDecl of
    ConDeclH98 { con_args = RecCon (L _ fields) } ->
        elem (Just False) $ hasHaddock anns lcon : fmap (hasHaddock anns) fields
    _                                       -> False -- GADT is not supported yet

-- | Returns 'True' if the end of the first node and the start of the second node are on the same line.
notSeparatedByLineEnding :: Located a
                         -> Located a
                         -> Bool
notSeparatedByLineEnding (L (RealSrcSpan x _) _) (L (RealSrcSpan y _) _) =
    srcLocLine (realSrcSpanEnd x) == srcLocLine (realSrcSpanStart y)
notSeparatedByLineEnding _ _ = False

-- | Empty haddock, suitable for being added to 'annPriorComments'
emptyPriorHaddockComment :: Comment
emptyPriorHaddockComment = mkComment "-- |"
#if MIN_VERSION_ghc(9,0,0)
    badRealSrcSpan
#else
    noSrcSpan
#endif

-- | Determines the given node has haddock comments attached to it.
hasHaddock :: Data a => Anns -> Located a -> Maybe Bool
hasHaddock anns node = fmap annHasHaddock (anns Map.!? key)
  where
    key = mkAnnKey node
    annHasHaddock ann =
        any (matchCommentPrefix priorCommentPrefix . fst) (annPriorComments ann)
        || any (matchCommentPrefix followingCommentPrefix . fst) (annFollowingComments ann)
        || any (keywordIdIsHaddockComment . fst) (annsDP ann)

-- | Checks if the given 'KeywordId' is a comment, and specifically, a haddock comment.
keywordIdIsHaddockComment :: KeywordId -> Bool
keywordIdIsHaddockComment (AnnComment comment) = any (`isPrefixOf` commentContents comment) (priorCommentPrefix ++ followingCommentPrefix)
keywordIdIsHaddockComment _ = False

priorCommentPrefix :: [String]
priorCommentPrefix = ["-- |", "{-|", "{- |"]

followingCommentPrefix :: [String]
followingCommentPrefix = ["-- ^", "{-^", "{- ^"]

matchCommentPrefix :: [String] -> Comment -> Bool
matchCommentPrefix prefix comment = any (`isPrefixOf` commentContents comment) prefix
