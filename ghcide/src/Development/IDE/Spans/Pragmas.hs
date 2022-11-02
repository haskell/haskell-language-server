{-# LANGUAGE CPP                   #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE MultiWayIf            #-}

module Development.IDE.Spans.Pragmas
  ( NextPragmaInfo(..)
  , LineSplitTextEdits(..)
  , getNextPragmaInfo
  , insertNewPragma
  , getFirstPragma ) where

import           Data.Bits                       (Bits (setBit))
import           Data.Function                   ((&))
import qualified Data.List                       as List
import qualified Data.Maybe                      as Maybe
import           Data.Text                       (Text, pack)
import qualified Data.Text                       as Text
import           Development.IDE                 (srcSpanToRange, IdeState, NormalizedFilePath, runAction, useWithStale, GhcSession (..), getFileContents, hscEnv)
import           Development.IDE.GHC.Compat
import           Development.IDE.GHC.Compat.Util
import qualified Language.LSP.Types              as LSP
import Control.Monad.IO.Class (MonadIO (..))
import Control.Monad.Trans.Except (ExceptT)
import Ide.Types (PluginId(..))
import qualified Data.Text as T
import Ide.PluginUtils (handleMaybeM)

getNextPragmaInfo :: DynFlags -> Maybe Text -> NextPragmaInfo
getNextPragmaInfo dynFlags sourceText =
  if | Just sourceText <- sourceText
     , let sourceStringBuffer = stringToStringBuffer (Text.unpack sourceText)
     , POk _ parserState <- parsePreDecl dynFlags sourceStringBuffer
     -> case parserState of
         ParserStateNotDone{ nextPragma } -> nextPragma
         ParserStateDone{ nextPragma }    -> nextPragma
     | otherwise
     -> NextPragmaInfo 0 Nothing

-- NOTE(ozkutuk): `RecordPuns` extension is renamed to `NamedFieldPuns`
-- in GHC 9.4, but we still want to insert `NamedFieldPuns` in pre-9.4
-- GHC as well, hence the replacement.
-- https://gitlab.haskell.org/ghc/ghc/-/merge_requests/6156
showExtension :: Extension -> Text
showExtension NamedFieldPuns = "NamedFieldPuns"
showExtension ext = pack (show ext)

insertNewPragma :: NextPragmaInfo -> Extension -> LSP.TextEdit
insertNewPragma (NextPragmaInfo _ (Just (LineSplitTextEdits ins _))) newPragma = ins { LSP._newText = "{-# LANGUAGE " <> showExtension newPragma <> " #-}\n" } :: LSP.TextEdit
insertNewPragma (NextPragmaInfo nextPragmaLine _) newPragma =  LSP.TextEdit pragmaInsertRange $ "{-# LANGUAGE " <> showExtension newPragma <> " #-}\n"
    where
        pragmaInsertPosition = LSP.Position (fromIntegral nextPragmaLine) 0
        pragmaInsertRange = LSP.Range pragmaInsertPosition pragmaInsertPosition

getFirstPragma :: MonadIO m => PluginId -> IdeState -> NormalizedFilePath -> ExceptT String m NextPragmaInfo
getFirstPragma (PluginId pId) state nfp = handleMaybeM "Could not get NextPragmaInfo" $ do
  ghcSession <- liftIO $ runAction (T.unpack pId <> ".GhcSession") state $ useWithStale GhcSession nfp
  (_, fileContents) <- liftIO $ runAction (T.unpack pId <> ".GetFileContents") state $ getFileContents nfp
  case ghcSession of
    Just (hscEnv -> hsc_dflags -> sessionDynFlags, _) -> pure $ Just $ getNextPragmaInfo sessionDynFlags fileContents
    Nothing                                           -> pure Nothing

-- Pre-declaration comments parser -----------------------------------------------------

-- | Each mode represents the "strongest" thing we've seen so far.
-- From strongest to weakest:
-- ModePragma, ModeHaddock, ModeComment, ModeInitial
data Mode = ModePragma | ModeHaddock | ModeComment | ModeInitial deriving Show

data LineSplitTextEdits = LineSplitTextEdits {
  lineSplitInsertTextEdit :: !LSP.TextEdit,
  lineSplitDeleteTextEdit :: !LSP.TextEdit
} deriving Show

data NextPragmaInfo = NextPragmaInfo {
  nextPragmaLine     :: !Int,
  lineSplitTextEdits :: !(Maybe LineSplitTextEdits)
} deriving Show

data ParserState
  = ParserStateNotDone
    { nextPragma           :: !NextPragmaInfo
    , mode                 :: !Mode
    , lastBlockCommentLine :: !Int
    , lastPragmaLine       :: !Int
    , isLastTokenHash      :: !Bool
    }
  | ParserStateDone { nextPragma :: NextPragmaInfo }
  deriving Show

isPragma :: String -> Bool
isPragma = List.isPrefixOf "{-#"

isDownwardBlockHaddock :: String -> Bool
isDownwardBlockHaddock = List.isPrefixOf "{-|"

isDownwardLineHaddock :: String -> Bool
isDownwardLineHaddock = List.isPrefixOf "-- |"

-- need to merge tokens that are deleted/inserted into one TextEdit each
-- to work around some weird TextEdits applied in reversed order issue
updateLineSplitTextEdits :: LSP.Range -> String -> Maybe LineSplitTextEdits -> LineSplitTextEdits
updateLineSplitTextEdits tokenRange tokenString prevLineSplitTextEdits
  | Just prevLineSplitTextEdits <- prevLineSplitTextEdits
  , let LineSplitTextEdits
          { lineSplitInsertTextEdit = prevInsertTextEdit
          , lineSplitDeleteTextEdit = prevDeleteTextEdit } = prevLineSplitTextEdits
  , let LSP.TextEdit prevInsertRange prevInsertText = prevInsertTextEdit
  , let LSP.TextEdit prevDeleteRange _prevDeleteText = prevDeleteTextEdit
  , let LSP.Range prevInsertStartPos  prevInsertEndPos = prevInsertRange
  , let LSP.Position _prevInsertStartLine _prevInsertStartCol = prevInsertStartPos
  , let LSP.Position _prevInsertEndLine _prevInsertEndCol = prevInsertEndPos
  , let LSP.Range prevDeleteStartPos prevDeleteEndPos = prevDeleteRange
  , let LSP.Position _prevDeleteStartLine _prevDeleteStartCol = prevDeleteStartPos
  , let LSP.Position _prevDeleteEndLine prevDeleteEndCol = prevDeleteEndPos
  , let currInsertRange = prevInsertRange
  , let currInsertText =
          Text.init prevInsertText
          <> Text.replicate (fromIntegral $ startCol - prevDeleteEndCol) " "
          <> Text.pack (List.take newLineCol tokenString)
          <> "\n"
  , let currInsertTextEdit = LSP.TextEdit currInsertRange currInsertText
  , let currDeleteStartPos = prevDeleteStartPos
  , let currDeleteEndPos = LSP.Position endLine endCol
  , let currDeleteRange = LSP.Range currDeleteStartPos currDeleteEndPos
  , let currDeleteTextEdit = LSP.TextEdit currDeleteRange ""
  = LineSplitTextEdits currInsertTextEdit currDeleteTextEdit
  | otherwise
  , let LSP.Range startPos _ = tokenRange
  , let deleteTextEdit = LSP.TextEdit (LSP.Range startPos startPos{ LSP._character = startCol + fromIntegral newLineCol }) ""
  , let insertPosition = LSP.Position (startLine + 1) 0
  , let insertRange = LSP.Range insertPosition insertPosition
  , let insertText = Text.pack (List.take newLineCol tokenString) <> "\n"
  , let insertTextEdit = LSP.TextEdit insertRange insertText
  = LineSplitTextEdits insertTextEdit deleteTextEdit
  where
    LSP.Range (LSP.Position startLine startCol) (LSP.Position endLine endCol) = tokenRange

    newLineCol = Maybe.fromMaybe (length tokenString) (List.elemIndex '\n' tokenString)

-- ITvarsym "#" after a block comment is a parse error so we don't need to worry about it
updateParserState :: Token -> LSP.Range -> ParserState -> ParserState
updateParserState token range prevParserState
  | ParserStateNotDone
      { nextPragma = prevNextPragma@NextPragmaInfo{ lineSplitTextEdits = prevLineSplitTextEdits }
      , mode = prevMode
      , lastBlockCommentLine
      , lastPragmaLine
      } <- prevParserState
  , let defaultParserState = prevParserState { isLastTokenHash = False }
  , let LSP.Range (LSP.Position (fromIntegral -> startLine) _) (LSP.Position (fromIntegral -> endLine) _) = range
  = case prevMode of
      ModeInitial ->
        case token of
          ITvarsym "#" -> defaultParserState{ isLastTokenHash = True }
#if !MIN_VERSION_ghc(9,2,0)
          ITlineComment s
#else
          ITlineComment s _
#endif
            | isDownwardLineHaddock s -> defaultParserState{ mode = ModeHaddock }
            | otherwise ->
                defaultParserState
                  { nextPragma = NextPragmaInfo (endLine + 1) Nothing
                  , mode = ModeComment }
#if !MIN_VERSION_ghc(9,2,0)
          ITblockComment s
#else
          ITblockComment s _
#endif
            | isPragma s ->
                defaultParserState
                  { nextPragma = NextPragmaInfo (endLine + 1) Nothing
                  , mode = ModePragma
                  , lastPragmaLine = endLine }
            | isDownwardBlockHaddock s -> defaultParserState{ mode = ModeHaddock }
            | otherwise ->
                defaultParserState
                  { nextPragma = NextPragmaInfo (endLine + 1) Nothing
                  , mode = ModeComment
                  , lastBlockCommentLine = endLine }
          _ -> ParserStateDone prevNextPragma
      ModeComment ->
        case token of
          ITvarsym "#" -> defaultParserState{ isLastTokenHash = True }
#if !MIN_VERSION_ghc(9,2,0)
          ITlineComment s
#else
          ITlineComment s _
#endif
            | hasDeleteStartedOnSameLine startLine prevLineSplitTextEdits
            , let currLineSplitTextEdits = updateLineSplitTextEdits range s prevLineSplitTextEdits ->
                defaultParserState{ nextPragma = prevNextPragma{ lineSplitTextEdits = Just currLineSplitTextEdits } }
            | isDownwardLineHaddock s
            , lastBlockCommentLine == startLine
            , let currLineSplitTextEdits = updateLineSplitTextEdits range s Nothing ->
                defaultParserState
                  { nextPragma = prevNextPragma{ lineSplitTextEdits = Just currLineSplitTextEdits }
                  , mode = ModeHaddock }
            | otherwise ->
                defaultParserState { nextPragma = NextPragmaInfo (endLine + 1) Nothing }
#if !MIN_VERSION_ghc(9,2,0)
          ITblockComment s
#else
          ITblockComment s _
#endif
            | isPragma s ->
                defaultParserState
                  { nextPragma = NextPragmaInfo (endLine + 1) Nothing
                  , mode = ModePragma
                  , lastPragmaLine = endLine }
            | hasDeleteStartedOnSameLine startLine prevLineSplitTextEdits
            , let currLineSplitTextEdits = updateLineSplitTextEdits range s prevLineSplitTextEdits ->
                defaultParserState{ nextPragma = prevNextPragma{ lineSplitTextEdits = Just currLineSplitTextEdits } }
            | isDownwardBlockHaddock s
            , lastBlockCommentLine == startLine
            , let currLineSplitTextEdits = updateLineSplitTextEdits range s Nothing ->
                defaultParserState{
                  nextPragma = prevNextPragma{ lineSplitTextEdits = Just currLineSplitTextEdits },
                  mode = ModeHaddock }
            | otherwise ->
                defaultParserState{
                  nextPragma = NextPragmaInfo (endLine + 1) Nothing,
                  lastBlockCommentLine = endLine }
          _ -> ParserStateDone prevNextPragma
      ModeHaddock ->
        case token of
          ITvarsym "#" ->
            defaultParserState{ isLastTokenHash = True }
#if !MIN_VERSION_ghc(9,2,0)
          ITlineComment s
#else
          ITlineComment s _
#endif
            | hasDeleteStartedOnSameLine startLine prevLineSplitTextEdits
            , let currLineSplitTextEdits = updateLineSplitTextEdits range s prevLineSplitTextEdits ->
                defaultParserState{ nextPragma = prevNextPragma{ lineSplitTextEdits = Just currLineSplitTextEdits } }
            | otherwise ->
                defaultParserState
#if !MIN_VERSION_ghc(9,2,0)
          ITblockComment s
#else
          ITblockComment s _
#endif
            | isPragma s ->
                defaultParserState{
                  nextPragma = NextPragmaInfo (endLine + 1) Nothing,
                  mode = ModePragma,
                  lastPragmaLine = endLine }
            | hasDeleteStartedOnSameLine startLine prevLineSplitTextEdits
            , let currLineSplitTextEdits = updateLineSplitTextEdits range s prevLineSplitTextEdits ->
                defaultParserState{ nextPragma = prevNextPragma{ lineSplitTextEdits = Just currLineSplitTextEdits } }
            | otherwise -> defaultParserState{ lastBlockCommentLine = endLine }
          _ -> ParserStateDone prevNextPragma
      ModePragma ->
        case token of
          ITvarsym "#" -> defaultParserState{ isLastTokenHash = True }
#if !MIN_VERSION_ghc(9,2,0)
          ITlineComment s
#else
          ITlineComment s _
#endif
            | hasDeleteStartedOnSameLine startLine prevLineSplitTextEdits
            , let currLineSplitTextEdits = updateLineSplitTextEdits range s prevLineSplitTextEdits ->
                defaultParserState{ nextPragma = prevNextPragma{ lineSplitTextEdits = Just currLineSplitTextEdits } }
            | isDownwardLineHaddock s
            , lastPragmaLine == startLine
            , let currLineSplitTextEdits = updateLineSplitTextEdits range s Nothing ->
                defaultParserState{ nextPragma = prevNextPragma{ lineSplitTextEdits = Just currLineSplitTextEdits } }
            | otherwise ->
                defaultParserState
#if !MIN_VERSION_ghc(9,2,0)
          ITblockComment s
#else
          ITblockComment s _
#endif
            | isPragma s ->
                defaultParserState{ nextPragma = NextPragmaInfo (endLine + 1) Nothing, lastPragmaLine = endLine }
            | hasDeleteStartedOnSameLine startLine prevLineSplitTextEdits
            , let currLineSplitTextEdits = updateLineSplitTextEdits range s prevLineSplitTextEdits ->
                defaultParserState{ nextPragma = prevNextPragma{ lineSplitTextEdits = Just currLineSplitTextEdits } }
            | isDownwardBlockHaddock s
            , lastPragmaLine == startLine
            , let currLineSplitTextEdits = updateLineSplitTextEdits range s Nothing ->
                defaultParserState{ nextPragma = prevNextPragma{ lineSplitTextEdits = Just currLineSplitTextEdits } }
            | lastPragmaLine == startLine && startLine < endLine
            , let currLineSplitTextEdits = updateLineSplitTextEdits range s Nothing ->
                defaultParserState{ nextPragma = prevNextPragma{ lineSplitTextEdits = Just currLineSplitTextEdits } }
            | otherwise ->
                defaultParserState{ lastBlockCommentLine = endLine }
          _ -> ParserStateDone prevNextPragma
  | otherwise = prevParserState
  where
    hasDeleteStartedOnSameLine :: Int -> Maybe LineSplitTextEdits -> Bool
    hasDeleteStartedOnSameLine line lineSplitTextEdits
      | Just lineSplitTextEdits <- lineSplitTextEdits
      , let LineSplitTextEdits{ lineSplitDeleteTextEdit } = lineSplitTextEdits
      , let LSP.TextEdit deleteRange _ = lineSplitDeleteTextEdit
      , let LSP.Range _ deleteEndPosition = deleteRange
      , let LSP.Position deleteEndLine _ = deleteEndPosition
      = fromIntegral deleteEndLine == line
      | otherwise = False

lexUntilNextLineIncl :: P (Located Token)
lexUntilNextLineIncl = do
  PState{ last_loc } <- getPState
#if MIN_VERSION_ghc(9,0,0)
  let PsSpan{ psRealSpan = lastRealSrcSpan } = last_loc
#else
  let lastRealSrcSpan = last_loc
#endif
  let prevEndLine = lastRealSrcSpan & realSrcSpanEnd & srcLocLine
  locatedToken@(L srcSpan _token) <- lexer False pure
  if | RealSrcLoc currEndRealSrcLoc _ <- srcSpan & srcSpanEnd
     , let currEndLine = currEndRealSrcLoc & srcLocLine
     -> if prevEndLine < currEndLine then
          pure locatedToken
        else lexUntilNextLineIncl
     | otherwise -> pure locatedToken

dropWhileStringBuffer :: (Char -> Bool) -> StringBuffer -> StringBuffer
dropWhileStringBuffer predicate buffer
  | atEnd buffer = buffer
  | let (c, remainingBuffer) = nextChar buffer
  = if predicate c then
      dropWhileStringBuffer predicate remainingBuffer
    else
      buffer

isHorizontalSpace :: Char -> Bool
isHorizontalSpace c = c == ' ' || c == '\t'

data ShebangParserState = ShebangParserState {
  nextPragmaLine :: !Int,
  newlineCount   :: !Int,
  prevCharIsHash :: !Bool,
  buffer         :: !StringBuffer
}

-- lexer seems to ignore shebangs completely hence this function
parseShebangs :: ShebangParserState -> ShebangParserState
parseShebangs prev@ShebangParserState{ newlineCount = prevNewlineCount, prevCharIsHash, buffer = prevBuffer }
  | atEnd prevBuffer
  = prev
  | let (c, currBuffer) = nextChar (dropWhileStringBuffer isHorizontalSpace prevBuffer)
  = if c == '#' then
      parseShebangs prev{ prevCharIsHash = True, buffer = currBuffer }
    else if c == '!' && prevCharIsHash then
      parseShebangs prev{ nextPragmaLine = prevNewlineCount + 1, buffer = dropWhileStringBuffer (/= '\n') currBuffer }
    else if c == '\n' then
      parseShebangs prev{ newlineCount = prevNewlineCount + 1, buffer = currBuffer }
    else
      prev


-- | Parses blank lines, comments, haddock comments ("-- |"), lines that start
-- with "#!", lines that start with "#", pragma lines using the GHC API lexer.
-- When it doesn't find one of these things then it's assumed that we've found
-- a declaration, end-of-file, or a ghc parse error, and the parser stops.
-- Shebangs are parsed separately than the rest becaues the lexer ignores them.
--
-- The reason for custom parsing instead of using annotations, or turning on/off
-- extensions in the dynflags is because there are a number of extensions that
-- while removing parse errors, can also introduce them. Hence, there are
-- cases where the file cannot be parsed without error when we want to insert
-- extension (and other) pragmas. The compiler (8.10.7) doesn't include
-- annotations in its failure state. So if the compiler someday returns
-- annotation or equivalent information when it fails then we can replace this
-- with that.
--
-- The reason for using the compiler lexer is to reduce duplicated
-- implementation, particularly nested comments, but in retrospect this comes
-- with the disadvantage of the logic feeling more complex, and not being able
-- to handle whitespace directly.
--
-- The parser keeps track of state in order to place the next pragma line
-- according to some rules:
--
-- - Ignore lines starting with '#' except for shebangs.
-- - If pragmas exist place after last pragma
-- - else if haddock comments exist:
--     - If comments exist place after last comment
--     - else if shebangs exist place after last shebang
--     - else place at first line
-- - else if comments exist place after last comment
-- - else if shebangs exist place after last shebang
-- - else place at first line
--
-- Additionally the parser keeps track of information to be able to insert
-- pragmas inbetween lines.
--
-- For example the parser keeps track of information so that
--
-- > {- block comment -} -- | haddock
--
-- can become
--
-- > {- block comment -}
-- > {-# pragma #-}
-- > -- | haddock
--
-- This information does not respect the type of whitespace, because the lexer
-- strips whitespace and gives locations.
--
-- In this example the tabs are converted to spaces in the TextEdits:
--
-- > {- block comment -}<space><tab><tab><space>-- | haddock
--
parsePreDecl :: DynFlags -> StringBuffer -> ParseResult ParserState
parsePreDecl dynFlags buffer = unP (go initialParserState) pState
  where
    initialShebangParserState = ShebangParserState{
      nextPragmaLine = 0,
      newlineCount = 0,
      prevCharIsHash = False,
      buffer = buffer }
    ShebangParserState{ nextPragmaLine } = parseShebangs initialShebangParserState
    pState = mkLexerPState dynFlags buffer
    initialParserState = ParserStateNotDone (NextPragmaInfo nextPragmaLine Nothing) ModeInitial (-1) (-1) False

    go :: ParserState -> P ParserState
    go prevParserState =
      case prevParserState of
        ParserStateDone _ -> pure prevParserState
        ParserStateNotDone{..} -> do
          L srcSpan token <-
            if isLastTokenHash then
              lexUntilNextLineIncl
            else
              lexer False pure
          case srcSpanToRange srcSpan of
            Just range -> go (updateParserState token range prevParserState)
            Nothing    -> pure prevParserState

mkLexerPState :: DynFlags -> StringBuffer -> PState
mkLexerPState dynFlags stringBuffer =
  let
    startRealSrcLoc = mkRealSrcLoc "asdf" 1 1
    updateDynFlags = flip gopt_unset Opt_Haddock . flip gopt_set Opt_KeepRawTokenStream
    finalDynFlags = updateDynFlags dynFlags
    pState = initParserState (initParserOpts finalDynFlags) stringBuffer startRealSrcLoc
    PState{ options = pStateOptions } = pState
    finalExtBitsMap = setBit (pExtsBitmap pStateOptions) (fromEnum UsePosPragsBit)
    finalPStateOptions = pStateOptions{ pExtsBitmap = finalExtBitsMap }
    finalPState = pState{ options = finalPStateOptions }
  in
    finalPState
