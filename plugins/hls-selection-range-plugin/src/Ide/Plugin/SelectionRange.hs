{-# LANGUAGE NamedFieldPuns    #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

module Ide.Plugin.SelectionRange (descriptor) where

import           Control.Monad.IO.Class                  (liftIO)
import           Control.Monad.Reader                    (runReader)
import           Control.Monad.Trans.Maybe               (MaybeT (MaybeT),
                                                          runMaybeT)
import           Data.Coerce                             (coerce)
import           Data.Containers.ListUtils               (nubOrd)
import           Data.Foldable                           (find)
import qualified Data.Map.Strict                         as Map
import           Data.Maybe                              (fromMaybe, mapMaybe)
import           Development.IDE                         (GetHieAst (GetHieAst),
                                                          HieAstResult (HAR, hieAst, refMap),
                                                          IdeAction,
                                                          IdeState (shakeExtras),
                                                          Range (Range),
                                                          fromNormalizedFilePath,
                                                          realSrcSpanToRange,
                                                          runIdeAction,
                                                          toNormalizedFilePath',
                                                          uriToFilePath')
import           Development.IDE.Core.Actions            (useE)
import           Development.IDE.Core.PositionMapping    (PositionMapping,
                                                          fromCurrentPosition,
                                                          toCurrentRange)
import           Development.IDE.GHC.Compat              (HieAST (Node), Span,
                                                          getAsts)
import           Development.IDE.GHC.Compat.Util
import           Ide.Plugin.SelectionRange.ASTPreProcess (PreProcessEnv (PreProcessEnv),
                                                          preProcessAST)
import           Ide.Types                               (PluginDescriptor (pluginHandlers),
                                                          PluginId,
                                                          defaultPluginDescriptor,
                                                          mkPluginHandler)
import           Language.LSP.Server                     (LspM)
import           Language.LSP.Types                      (List (List),
                                                          NormalizedFilePath,
                                                          Position,
                                                          ResponseError,
                                                          SMethod (STextDocumentSelectionRange),
                                                          SelectionRange (..),
                                                          SelectionRangeParams (..),
                                                          TextDocumentIdentifier (TextDocumentIdentifier))
import           Prelude                                 hiding (span)

descriptor :: PluginId -> PluginDescriptor IdeState
descriptor plId = (defaultPluginDescriptor plId)
    { pluginHandlers = mkPluginHandler STextDocumentSelectionRange selectionRangeHandler
    }

selectionRangeHandler :: IdeState -> PluginId -> SelectionRangeParams -> LspM c (Either ResponseError (List SelectionRange))
selectionRangeHandler ide _ SelectionRangeParams{..} = do
    let (TextDocumentIdentifier uri) = _textDocument
    -- TODO improve error reporting (both here and in 'getSelectionRanges')
    let filePathMaybe = toNormalizedFilePath' <$> uriToFilePath' uri
    case filePathMaybe of
        Nothing -> pure . Right . List $ []
        Just filePath -> liftIO $ do
            let (List positions) = _positions
            selectionRanges <- runIdeAction "SelectionRange" (shakeExtras ide) $ getSelectionRanges filePath positions
            pure . Right . List $ selectionRanges

getSelectionRanges :: NormalizedFilePath -> [Position] -> IdeAction [SelectionRange]
getSelectionRanges file positions = fmap (fromMaybe []) <$> runMaybeT $ do
    (HAR{hieAst, refMap}, positionMapping) <- useE GetHieAst file
    -- 'positionMapping' should be applied to the input positions before using them
    positions' <- MaybeT . pure $ traverse (fromCurrentPosition positionMapping) positions

    ast <- MaybeT . pure $ getAsts hieAst Map.!? (coerce. mkFastString . fromNormalizedFilePath) file
    let ast' = runReader (preProcessAST ast) (PreProcessEnv refMap)
    let selectionRanges = findSelectionRangesByPositions (astPathsLeafToRoot ast') positions'

    -- 'positionMapping' should be applied to the output ranges before returning them
    MaybeT . pure . traverse (toCurrentSelectionRange positionMapping) $ selectionRanges

-- | Likes 'toCurrentPosition', but works on 'SelectionRange'
toCurrentSelectionRange :: PositionMapping -> SelectionRange -> Maybe SelectionRange
toCurrentSelectionRange positionMapping SelectionRange{..} = do
    newRange <- toCurrentRange positionMapping _range
    pure $ SelectionRange {
        _range = newRange,
        _parent = _parent >>= toCurrentSelectionRange positionMapping
    }

-- | Build all paths from ast leaf to root
astPathsLeafToRoot :: HieAST a -> [SelectionRange]
astPathsLeafToRoot = mapMaybe (spansToSelectionRange . nubOrd) . go [[]]
  where
    go :: [[Span]] -> HieAST a -> [[Span]]
    go acc (Node _ span [])       = fmap (span:) acc
    go acc (Node _ span children) = concatMap (go (fmap (span:) acc)) children

spansToSelectionRange :: [Span] -> Maybe SelectionRange
spansToSelectionRange [] = Nothing
spansToSelectionRange (span:spans) = Just $
    SelectionRange {_range = realSrcSpanToRange span, _parent = spansToSelectionRange spans}

-- | Filters the selection ranges containing at least one of the given positions.
findSelectionRangesByPositions :: [SelectionRange] -- ^ all possible selection ranges
                               -> [Position] -- ^ requested positions
                               -> [SelectionRange]
findSelectionRangesByPositions selectionRanges = fmap findByPosition
  where
    findByPosition :: Position -> SelectionRange
    findByPosition p = fromMaybe SelectionRange{_range = Range p p, _parent = Nothing} $
        find (isPositionInSelectionRange p) selectionRanges

    isPositionInSelectionRange :: Position -> SelectionRange -> Bool
    isPositionInSelectionRange p SelectionRange{_range} =
        let Range sp ep = _range in sp <= p && p <= ep
