{-# LANGUAGE NamedFieldPuns    #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

module Ide.Plugin.SelectionRange (descriptor) where

import           Control.Monad.IO.Class               (liftIO)
import           Control.Monad.Trans.Maybe            (MaybeT (MaybeT),
                                                       runMaybeT)
import           Data.Foldable                        (find)
import qualified Data.Map.Strict                      as Map
import           Data.Maybe                           (fromMaybe, mapMaybe)
import           Development.IDE                      (GetHieAst (GetHieAst),
                                                       HieAstResult (HAR, hieAst),
                                                       IdeAction,
                                                       IdeState (shakeExtras),
                                                       Range (Range),
                                                       fromNormalizedFilePath,
                                                       realSrcSpanToRange,
                                                       runIdeAction,
                                                       toNormalizedFilePath',
                                                       uriToFilePath')
import           Development.IDE.Core.Actions         (useE)
import           Development.IDE.Core.PositionMapping (PositionMapping,
                                                       fromCurrentPosition,
                                                       toCurrentRange)
import           Development.IDE.GHC.Compat           (HieAST (Node), Span,
                                                       getAsts)
import           Development.IDE.GHC.Compat.Util      (mkFastString)
import           Ide.Types                            (PluginDescriptor (pluginHandlers),
                                                       PluginId,
                                                       defaultPluginDescriptor,
                                                       mkPluginHandler)
import           Language.LSP.Server                  (LspM)
import           Language.LSP.Types                   (List (List),
                                                       NormalizedFilePath,
                                                       Position, ResponseError,
                                                       SMethod (STextDocumentSelectionRange),
                                                       SelectionRange (..),
                                                       SelectionRangeParams (..),
                                                       TextDocumentIdentifier (TextDocumentIdentifier))
import           Prelude                              hiding (span)

descriptor :: PluginId -> PluginDescriptor IdeState
descriptor plId = (defaultPluginDescriptor plId)
    { pluginHandlers = mkPluginHandler STextDocumentSelectionRange selectionRangeHandler
    }

selectionRangeHandler :: IdeState -> PluginId -> SelectionRangeParams -> LspM c (Either ResponseError (List SelectionRange))
selectionRangeHandler ide _ SelectionRangeParams{..} = do
    let (TextDocumentIdentifier uri) = _textDocument
    let filePathMaybe = toNormalizedFilePath' <$> uriToFilePath' uri
    case filePathMaybe of
        Nothing -> pure . Right . List $ []
        Just filePath -> liftIO $ do
            let (List positions) = _positions
            selectionRanges <- runIdeAction "SelectionRange" (shakeExtras ide) $ getSelectionRanges filePath positions
            pure . Right . List $ selectionRanges

getSelectionRanges :: NormalizedFilePath -> [Position] -> IdeAction [SelectionRange]
getSelectionRanges file positions = fmap (fromMaybe []) <$> runMaybeT $ do
    (HAR{hieAst}, positionMapping) <- useE GetHieAst file
    positions' <- MaybeT . pure $ traverse (fromCurrentPosition positionMapping) positions
    ast <- MaybeT . pure $ getAsts hieAst Map.!? (mkFastString . fromNormalizedFilePath) file
    MaybeT . pure . traverse (toCurrentSelectionRange positionMapping) $
        findSelectionRangesByPositions (astPathsLeafToRoot ast) positions'

-- | Like 'toCurrentPosition', but works on 'SelectionRange'
toCurrentSelectionRange :: PositionMapping -> SelectionRange -> Maybe SelectionRange
toCurrentSelectionRange positionMapping SelectionRange{..} = do
    newRange <- toCurrentRange positionMapping _range
    pure $ SelectionRange {
        _range = newRange,
        _parent = _parent >>= toCurrentSelectionRange positionMapping
    }

-- | Build all paths from ast leaf to root
astPathsLeafToRoot :: HieAST a -> [SelectionRange]
astPathsLeafToRoot = mapMaybe spansToSelectionRange . go [[]]
  where
    go acc (Node _ span [])       = fmap (span:) acc
    go acc (Node _ span children) = concatMap (go (fmap (span:) acc)) children

spansToSelectionRange :: [Span] -> Maybe SelectionRange
spansToSelectionRange [] = Nothing
spansToSelectionRange (span:spans) = Just $
    SelectionRange {_range = realSrcSpanToRange span, _parent = spansToSelectionRange spans}

findSelectionRangesByPositions :: [SelectionRange] -> [Position] -> [SelectionRange]
findSelectionRangesByPositions selectionRanges = fmap findByPosition
  where
    findByPosition p = fromMaybe SelectionRange{_range = Range p p, _parent = Nothing} $
        find (isPositionInSelectionRange p) selectionRanges
    isPositionInSelectionRange p SelectionRange{_range} =
        let Range sp ep = _range in sp <= p && p <= ep
