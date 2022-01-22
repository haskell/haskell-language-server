{-# LANGUAGE NamedFieldPuns    #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

module Ide.Plugin.SelectionRange (descriptor) where

import           Control.Monad.Except                    (ExceptT (ExceptT),
                                                          runExceptT)
import           Control.Monad.IO.Class                  (liftIO)
import           Control.Monad.Reader                    (runReader)
import           Control.Monad.Trans.Maybe               (MaybeT (MaybeT),
                                                          maybeToExceptT)
import           Data.Coerce                             (coerce)
import           Data.Containers.ListUtils               (nubOrd)
import           Data.Either.Extra                       (maybeToEither)
import           Data.Foldable                           (find)
import qualified Data.Map.Strict                         as Map
import           Data.Maybe                              (fromMaybe, mapMaybe)
import qualified Data.Text                               as T
import           Development.IDE                         (GetHieAst (GetHieAst),
                                                          HieAstResult (HAR, hieAst, refMap),
                                                          IdeAction,
                                                          IdeState (shakeExtras),
                                                          Range (Range),
                                                          fromNormalizedFilePath,
                                                          ideLogger, logInfo,
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
import           Ide.PluginUtils                         (response)
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
                                                          TextDocumentIdentifier (TextDocumentIdentifier),
                                                          Uri)
import           Prelude                                 hiding (span)

descriptor :: PluginId -> PluginDescriptor IdeState
descriptor plId = (defaultPluginDescriptor plId)
    { pluginHandlers = mkPluginHandler STextDocumentSelectionRange selectionRangeHandler
    }

selectionRangeHandler :: IdeState -> PluginId -> SelectionRangeParams -> LspM c (Either ResponseError (List SelectionRange))
selectionRangeHandler ide _ SelectionRangeParams{..} = do
    liftIO $ logInfo logger $ "requesting selection range for file: " <> T.pack (show uri)
    response $ do
        filePath <- ExceptT . pure . maybeToEither "fail to convert uri to file path" $
                toNormalizedFilePath' <$> uriToFilePath' uri
        selectionRanges <- ExceptT . liftIO . runIdeAction "SelectionRange" (shakeExtras ide) . runExceptT $
            getSelectionRanges filePath positions
        pure . List $ selectionRanges
  where
    uri :: Uri
    TextDocumentIdentifier uri = _textDocument

    positions :: [Position]
    List positions = _positions

    logger = ideLogger ide

getSelectionRanges :: NormalizedFilePath -> [Position] -> ExceptT String IdeAction [SelectionRange]
getSelectionRanges file positions = do
    (HAR{hieAst, refMap}, positionMapping) <- maybeToExceptT "fail to get hie ast" $ useE GetHieAst file
    -- 'positionMapping' should be applied to the input positions before using them
    positions' <- maybeToExceptT "fail to apply position mapping to input positions" . MaybeT . pure $
        traverse (fromCurrentPosition positionMapping) positions

    ast <- maybeToExceptT "fail to get ast for current file" . MaybeT . pure $
        -- in GHC 9, the 'FastString' in 'HieASTs' is replaced by a newtype wrapper around 'LexicalFastString'
        -- so we use 'coerce' to make it work in both GHC 8 and 9
        getAsts hieAst Map.!? (coerce . mkFastString . fromNormalizedFilePath) file

    let ast' = runReader (preProcessAST ast) (PreProcessEnv refMap)
    let selectionRanges = findSelectionRangesByPositions (astPathsLeafToRoot ast') positions'

    -- 'positionMapping' should be applied to the output ranges before returning them
    maybeToExceptT "fail to apply position mapping to output positions" . MaybeT . pure $
         traverse (toCurrentSelectionRange positionMapping) selectionRanges

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

{-|
For each position, find the selection range that contains it, without taking each selection range's
parent into account. These selection ranges are un-divisible, representing the leaf nodes in original AST, so they
won't overlap.
-}
findSelectionRangesByPositions :: [SelectionRange] -- ^ all possible selection ranges
                               -> [Position] -- ^ requested positions
                               -> [SelectionRange]
findSelectionRangesByPositions selectionRanges = fmap findByPosition
    {-
        Performance Tips:
        Doing a linear search from the first selection range for each position is not optimal.
        If it becomes too slow for a large file and many positions, you may optimize the implementation.
        At least we don't need to search from the very beginning for each position, if the are sorted firstly.
        Or maybe we could apply some techniques like binary search?
    -}
  where
    findByPosition :: Position -> SelectionRange
    findByPosition p = fromMaybe SelectionRange{_range = Range p p, _parent = Nothing} $
        find (isPositionInSelectionRange p) selectionRanges

    isPositionInSelectionRange :: Position -> SelectionRange -> Bool
    isPositionInSelectionRange p SelectionRange{_range} =
        let Range sp ep = _range in sp <= p && p <= ep
