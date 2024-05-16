{-# LANGUAGE OverloadedStrings #-}

module Ide.Plugin.Cabal.Completion.Completions (contextToCompleter, getContext, getCabalPrefixInfo) where

import           Control.Lens                                  ((^.))
import           Control.Monad.IO.Class                        (MonadIO)
import           Control.Monad.Trans.Maybe
import           Data.Foldable                                 (asum)
import qualified Data.List                                     as List
import           Data.Map                                      (Map)
import qualified Data.Map                                      as Map
import qualified Data.Text                                     as T
import qualified Data.Text.Utf16.Lines                         as Rope (Position (..))
import           Data.Text.Utf16.Rope.Mixed                    (Rope)
import qualified Data.Text.Utf16.Rope.Mixed                    as Rope
import           Development.IDE                               as D
import qualified Development.IDE.Plugin.Completions.Types      as Ghcide
import           Ide.Plugin.Cabal.Completion.Completer.Simple
import           Ide.Plugin.Cabal.Completion.Completer.Snippet
import           Ide.Plugin.Cabal.Completion.Completer.Types   (Completer)
import           Ide.Plugin.Cabal.Completion.Data
import           Ide.Plugin.Cabal.Completion.Types
import qualified Language.LSP.Protocol.Lens                    as JL
import qualified System.FilePath                               as FP
import           System.FilePath                               (takeBaseName)

-- ----------------------------------------------------------------
-- Public API for Completions
-- ----------------------------------------------------------------

-- | Takes information about the completion context within the file
--  and finds the correct completer to be applied.
contextToCompleter :: Context -> Completer
-- if we are in the top level of the cabal file and not in a keyword context,
-- we can write any top level keywords or a stanza declaration
contextToCompleter (TopLevel, None) =
  snippetCompleter
    <> ( constantCompleter $
           Map.keys (cabalVersionKeyword <> cabalKeywords) ++ Map.keys stanzaKeywordMap
       )
-- if we are in a keyword context in the top level,
-- we look up that keyword in the top level context and can complete its possible values
contextToCompleter (TopLevel, KeyWord kw) =
  case Map.lookup kw (cabalVersionKeyword <> cabalKeywords) of
    Nothing -> errorNoopCompleter (LogUnknownKeyWordInContextError kw)
    Just l  -> l
-- if we are in a stanza and not in a keyword context,
-- we can write any of the stanza's keywords or a stanza declaration
contextToCompleter (Stanza s _, None) =
  case Map.lookup s stanzaKeywordMap of
    Nothing -> errorNoopCompleter (LogUnknownStanzaNameInContextError s)
    Just l  -> constantCompleter $ Map.keys l
-- if we are in a stanza's keyword's context we can complete possible values of that keyword
contextToCompleter (Stanza s _, KeyWord kw) =
  case Map.lookup s stanzaKeywordMap of
    Nothing -> errorNoopCompleter (LogUnknownStanzaNameInContextError s)
    Just m -> case Map.lookup kw m of
      Nothing -> errorNoopCompleter (LogUnknownKeyWordInContextError kw)
      Just l  -> l

-- | Takes prefix info about the previously written text
--  and a rope (representing a file), returns the corresponding context.
--
--  Can return Nothing if an error occurs.
--
--  TODO: first line can only have cabal-version: keyword
getContext :: (MonadIO m) => Recorder (WithPriority Log) -> CabalPrefixInfo -> Rope -> MaybeT m Context
getContext recorder prefInfo ls =
  case prevLinesM of
    Just prevLines -> do
      let lvlContext =
            if completionIndentation prefInfo == 0
              then TopLevel
              else currentLevel prevLines
      case lvlContext of
        TopLevel -> do
          kwContext <- MaybeT . pure $ getKeyWordContext prefInfo prevLines (cabalVersionKeyword <> cabalKeywords)
          pure (TopLevel, kwContext)
        Stanza s n ->
          case Map.lookup s stanzaKeywordMap of
            Nothing -> do
              pure (Stanza s n, None)
            Just m -> do
              kwContext <- MaybeT . pure $ getKeyWordContext prefInfo prevLines m
              pure (Stanza s n, kwContext)
    Nothing -> do
      logWith recorder Warning $ LogFileSplitError pos
      -- basically returns nothing
      fail "Abort computation"
  where
    pos = completionCursorPosition prefInfo
    prevLinesM = splitAtPosition pos ls

-- | Takes information about the current file's file path,
--  and the cursor position in the file; and builds a CabalPrefixInfo
--  with the prefix up to that cursor position.
--  Checks whether a suffix needs to be completed
--  and calculates the range in the document
--  where the completion action should be applied.
getCabalPrefixInfo :: FilePath -> Ghcide.PosPrefixInfo -> CabalPrefixInfo
getCabalPrefixInfo fp prefixInfo =
  CabalPrefixInfo
    { completionPrefix = completionPrefix',
      isStringNotation = mkIsStringNotation separator afterCursorText,
      completionCursorPosition = Ghcide.cursorPos prefixInfo,
      completionRange = Range completionStart completionEnd,
      completionWorkingDir = FP.takeDirectory fp,
      completionFileName = T.pack $ takeBaseName fp
    }
  where
    completionEnd = Ghcide.cursorPos prefixInfo
    completionStart =
      Position
        (_line completionEnd)
        (_character completionEnd - (fromIntegral $ T.length completionPrefix'))
    (beforeCursorText, afterCursorText) = T.splitAt cursorColumn $ Ghcide.fullLine prefixInfo
    completionPrefix' = T.takeWhileEnd (not . (`elem` stopConditionChars)) beforeCursorText
    separator =
      -- if there is an opening apostrophe before the cursor in the line somewhere,
      -- everything after that apostrophe is the completion prefix
      if odd $ T.count "\"" beforeCursorText
        then '\"'
        else ' '
    cursorColumn = fromIntegral $ Ghcide.cursorPos prefixInfo ^. JL.character
    stopConditionChars = separator : [',', ':']

    -- \| Takes the character occurring exactly before,
    --  and the text occurring after the item to be completed and
    --  returns whether the item is already surrounded by apostrophes.
    --
    --  Example: (@|@ indicates the cursor position)
    --
    --  @"./src|@ would call @'\"'@ @""@ and result in Just LeftSide
    --
    --  @"./src|"@ would call @'\"'@ @'\"'@ and result in Just Surrounded
    --
    mkIsStringNotation :: Char -> T.Text -> Maybe Apostrophe
    mkIsStringNotation '\"' restLine
      | Just ('\"', _) <- T.uncons restLine = Just Surrounded
      | otherwise = Just LeftSide
    mkIsStringNotation _ _ = Nothing

-- ----------------------------------------------------------------
-- Implementation Details
-- ----------------------------------------------------------------

-- | Takes prefix info about the previously written text,
--  a list of lines (representing a file) and a map of
--  keywords and returns a keyword context if the
--  previously written keyword matches one in the map.
--
--  From a cursor position, we traverse the cabal file upwards to
--  find the latest written keyword if there is any.
--  Values may be written on subsequent lines,
--  in order to allow for this we take the indentation of the current
--  word to be completed into account to find the correct keyword context.
getKeyWordContext :: CabalPrefixInfo -> [T.Text] -> Map KeyWordName a -> Maybe FieldContext
getKeyWordContext prefInfo ls keywords = do
  case lastNonEmptyLineM of
    Nothing -> Just None
    Just lastLine' -> do
      let (whiteSpaces, lastLine) = T.span (== ' ') lastLine'
      let keywordIndentation = T.length whiteSpaces
      let cursorIndentation = completionIndentation prefInfo
      -- in order to be in a keyword context the cursor needs
      -- to be indented more than the keyword
      if cursorIndentation > keywordIndentation
        then -- if the last thing written was a keyword without a value
        case List.find (`T.isPrefixOf` lastLine) (Map.keys keywords) of
          Nothing -> Just None
          Just kw -> Just $ KeyWord kw
        else Just None
  where
    lastNonEmptyLineM :: Maybe T.Text
    lastNonEmptyLineM = do
      (curLine, rest) <- List.uncons ls
      -- represents the current line while disregarding the
      -- currently written text we want to complete
      let cur = stripPartiallyWritten curLine
      List.find (not . T.null . T.stripEnd) $
        cur : rest

-- | Traverse the given lines (starting before current cursor position
--  up to the start of the file) to find the nearest stanza declaration,
--  if none is found we are in the top level context.
--
--  TODO: this could be merged with getKeyWordContext in order to increase
--  performance by reducing the number of times we have to traverse the cabal file.
currentLevel :: [T.Text] -> StanzaContext
currentLevel [] = TopLevel
currentLevel (cur : xs)
  | Just (s, n) <- stanza = Stanza s n
  | otherwise = currentLevel xs
  where
    stanza = asum $ map checkStanza (Map.keys stanzaKeywordMap)
    checkStanza :: StanzaType -> Maybe (StanzaType, Maybe StanzaName)
    checkStanza t =
      case T.stripPrefix t (T.strip cur) of
        Just n
          | T.null n -> Just (t, Nothing)
          | otherwise -> Just (t, Just $ T.strip n)
        Nothing -> Nothing

-- | Get all lines before the given cursor position in the given file
--  and reverse their order to traverse backwards starting from the given position.
splitAtPosition :: Position -> Rope -> Maybe [T.Text]
splitAtPosition pos ls = do
  split <- splitFile
  pure $ reverse $ Rope.lines $ fst split
  where
    splitFile = Rope.utf16SplitAtPosition ropePos ls
    ropePos =
      Rope.Position
        { Rope.posLine = fromIntegral $ pos ^. JL.line,
          Rope.posColumn = fromIntegral $ pos ^. JL.character
        }

-- | Takes a line of text and removes the last partially
-- written word to be completed.
stripPartiallyWritten :: T.Text -> T.Text
stripPartiallyWritten = T.dropWhileEnd (\y -> (y /= ' ') && (y /= ':'))

-- | Calculates how many spaces the currently completed item is indented.
completionIndentation :: CabalPrefixInfo -> Int
completionIndentation prefInfo = fromIntegral (pos ^. JL.character) - (T.length $ completionPrefix prefInfo)
  where
    pos = completionCursorPosition prefInfo
