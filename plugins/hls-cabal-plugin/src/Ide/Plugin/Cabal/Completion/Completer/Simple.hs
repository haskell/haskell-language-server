{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}

module Ide.Plugin.Cabal.Completion.Completer.Simple where

import           Control.Lens                                ((?~))
import           Data.Function                               ((&))
import qualified Data.List                                   as List
import           Data.Map                                    (Map)
import qualified Data.Map                                    as Map
import           Data.Maybe                                  (fromMaybe,
                                                              mapMaybe)
import           Data.Ord                                    (Down (Down))
import qualified Data.Text                                   as T
import qualified Distribution.Fields                         as Syntax
import           Ide.Logger                                  (Priority (..),
                                                              logWith)
import           Ide.Plugin.Cabal.Completion.CabalFields
import           Ide.Plugin.Cabal.Completion.Completer.Types
import           Ide.Plugin.Cabal.Completion.Types           (CabalPrefixInfo (..),
                                                              Log)
import qualified Language.LSP.Protocol.Lens                  as JL
import qualified Language.LSP.Protocol.Types                 as Compls (CompletionItem (..))
import qualified Language.LSP.Protocol.Types                 as LSP
import qualified Text.Fuzzy.Parallel                         as Fuzzy

-- | Completer to be used when no completion suggestions
--  are implemented for the field
noopCompleter :: Completer
noopCompleter _ _ = pure []

-- | Completer to be used when no completion suggestions
--  are implemented for the field and a log message should be emitted.
errorNoopCompleter :: Log -> Completer
errorNoopCompleter l recorder _ = do
  logWith recorder Warning l
  pure []

-- | Completer to be used when a simple set of values
--  can be completed for a field.
constantCompleter :: [T.Text] -> Completer
constantCompleter completions _ cData = do
  let prefInfo = cabalPrefixInfo cData
      scored = runMatcher (matcher cData) (completionPrefix prefInfo) completions
      range = completionRange prefInfo
  pure $ map (mkSimpleCompletionItem range . Fuzzy.original) scored

-- | Completer to be used for import fields.
--
-- TODO: Does not exclude imports, defined after the current cursor position
-- which are not allowed according to the cabal specification
importCompleter :: Completer
importCompleter l cData = do
  cabalCommonsM <- getCabalCommonSections cData
  case cabalCommonsM of
    Just cabalCommons -> do
        let commonNames = mapMaybe (\case
              Syntax.Section (Syntax.Name _ "common") commonNames _ -> getOptionalSectionName commonNames
              _ -> Nothing)
              cabalCommons
        constantCompleter commonNames l cData
    Nothing -> noopCompleter l cData

-- | Completer to be used for the field @name:@ value.
--
-- This is almost always the name of the cabal file. However,
-- it is not forbidden by the specification to have a different name,
-- it is just forbidden on hackage.
nameCompleter :: Completer
nameCompleter _ cData = do
  let scored = runMatcher (matcher cData) (completionPrefix prefInfo) [completionFileName prefInfo]
      prefInfo = cabalPrefixInfo cData
      range = completionRange prefInfo
  pure $ map (mkSimpleCompletionItem range . Fuzzy.original) scored

-- | Completer to be used when a set of values with priority weights
-- attached to some values are to be completed for a field.
--
--  The higher the weight, the higher the priority to show
--  the value in the completion suggestion.
--
--  If the value does not occur in the weighted map its weight is defaulted to zero.
weightedConstantCompleter :: [T.Text] -> Map T.Text Double -> Completer
weightedConstantCompleter completions weights _ cData = do
  let scored =
        if perfectScore > 0
          then
            -- TODO: Would be nice to use to be able to use the matcher in `cData`
            fmap Fuzzy.original $
              Fuzzy.simpleFilter' Fuzzy.defChunkSize Fuzzy.defMaxResults prefix completions customMatch
          else topTenByWeight
      range = completionRange prefInfo
  pure $ map (mkSimpleCompletionItem range) scored
  where
    prefInfo = cabalPrefixInfo cData
    prefix = completionPrefix prefInfo
    -- The perfect score is the score of the word matched with itself
    -- this should never return Nothing since we match the word with itself
    perfectScore = fromMaybe (error "match is broken") $ Fuzzy.match prefix prefix
    -- \| Since the best score is cut off at the perfect score, we use a custom match
    -- which allows for the score to be larger than the perfect score.
    --
    -- This is necessary since the weight is multiplied with the originally matched
    -- score and thus the calculated score may be larger than the perfect score.
    customMatch :: (T.Text -> T.Text -> Maybe Int)
    customMatch toSearch searchSpace = do
      matched <- Fuzzy.match toSearch searchSpace
      let weight = fromMaybe 0 $ Map.lookup searchSpace weights
      let score =
            min
              perfectScore
              (round (fromIntegral matched * (1 + weight)))
      pure score
    -- \| Sorts the list in descending order based on the map of weights and then
    -- returns the top ten items in the list
    topTenByWeight :: [T.Text]
    topTenByWeight = take 10 $ map fst $ List.sortOn (Down . snd) $ Map.assocs weights

-- | Creates a CompletionItem with the given text as the label
-- where the completion item kind is keyword.
mkDefaultCompletionItem :: T.Text -> LSP.CompletionItem
mkDefaultCompletionItem label =
  LSP.CompletionItem
    { Compls._label = label,
      Compls._labelDetails = Nothing,
      Compls._kind = Just LSP.CompletionItemKind_Keyword,
      Compls._tags = Nothing,
      Compls._detail = Nothing,
      Compls._documentation = Nothing,
      Compls._deprecated = Nothing,
      Compls._preselect = Nothing,
      Compls._sortText = Nothing,
      Compls._filterText = Nothing,
      Compls._insertText = Nothing,
      Compls._insertTextFormat = Nothing,
      Compls._insertTextMode = Nothing,
      Compls._textEdit = Nothing,
      Compls._textEditText = Nothing,
      Compls._additionalTextEdits = Nothing,
      Compls._commitCharacters = Nothing,
      Compls._command = Nothing,
      Compls._data_ = Nothing
    }

-- | Returns a CompletionItem with the given starting position
--  and text to be inserted, where the displayed text is the same as the
--  inserted text.
mkSimpleCompletionItem :: LSP.Range -> T.Text -> LSP.CompletionItem
mkSimpleCompletionItem range txt =
  mkDefaultCompletionItem txt
    & JL.textEdit ?~ LSP.InL (LSP.TextEdit range txt)

-- | Returns a completionItem with the given starting position,
--  text to be inserted and text to be displayed in the completion suggestion.
mkCompletionItem :: LSP.Range -> T.Text -> T.Text -> LSP.CompletionItem
mkCompletionItem range insertTxt displayTxt =
  mkDefaultCompletionItem displayTxt
    & JL.textEdit ?~ LSP.InL (LSP.TextEdit range insertTxt)
