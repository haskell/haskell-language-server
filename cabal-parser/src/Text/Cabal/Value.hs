{-# LANGUAGE OverloadedStrings #-}

module Text.Cabal.Value where

import           Data.Functor                (void)
import qualified Data.Text                   as T
import qualified Language.LSP.Protocol.Types as LSP (Position (..), Range (..))
import           Text.Cabal.Types
import           Text.Megaparsec
import           Text.Megaparsec.Char

{- Note [Value Parser Implementation Guidelines]
  Any value parser needs to fulfill the following functionality:
  * Takes a boolean which signifies whether the parsing takes place in a braces context,
  if we are in braces context we do not consider indentation.
  * Parses a single line which contains at least one value but may contain multiple.
  * Must stop when it encounters an unescaped '}', but must not parse it.
  * Fails when called on an empty line.
  * The parser must not attempt to parse a fixed indentation.
  * Is only ever invoked to parse values, keywords are taken care of beforehand
-}

{- | Default parser which can be used to parse values for a field.

  Here we assume that the rest of the line contains exactly one value
  and simply parse anything up until the newline into the value.
-}
defaultValueParser :: Bool -> Parser [ValueItem]
defaultValueParser inBraces = do
  hspace
  (val, loc) <- annotateSrcLoc $ some $ noneOf endChars
  choice [eof, void eol, void $ lookAhead $ string "}"]
  pure [Value (T.pack val) (Annotation Nothing loc)]
 where
  endChars = ['\r', '\n'] ++ if inBraces then ['{', '}'] else []

{- | Parser to parse module paths.

  The module paths may start with a comma,
  then an arbitrary number of white spaces
  then a module path which may be wrapped in apostrophes
  and consists of words separated by dots,
  after the module path may be a comma before which there
  could be an arbitrary number of white spaces.
-}
moduleParser :: Bool -> Parser [ValueItem]
moduleParser inBraces = do
  vals <- some $ try $ do
    void $ many " "
    (val, loc) <- annotateSrcLoc $ do
      c1 <- option "" $ string ","
      s1 <- many $ char ' '
      val' <- between (char '\"') (char '\"') moduleNameParser <|> moduleNameParser
      s2 <- many $ char ' '
      c2 <- option "" $ string ","
      let val = c1 <> T.pack s1 <> T.pack val' <> T.pack s2 <> c2
      pure val
    pure $ Value val (Annotation Nothing loc)
  hspace
  choice endParsers
  pure vals
 where
  moduleNameParser = some (alphaNumChar <|> char '.' <|> char '_')
  endParsers = [eof, void eol] ++ ([void $ lookAhead $ string "}" | inBraces])

{- | Parser to parse file paths.

  The file paths may start with a comma,
  then an arbitrary number of white spaces
  then a file path which may be wrapped in apostrophes,
  after the file path may be a comma before which there
  could be an arbitrary number of white spaces.
-}
filepathParser :: Bool -> Parser [ValueItem]
filepathParser inBraces = do
  vals <- some $ try $ do
    void $ many " "
    (val, loc) <- annotateSrcLoc $ do
      c1 <- option "" $ string ","
      s1 <- many $ char ' '
      val' <- wrappedFpParser <|> filePathParser
      s2 <- many $ char ' '
      c2 <- option "" $ string ","
      let val = c1 <> T.pack s1 <> T.pack val' <> T.pack s2 <> c2
      pure val
    pure $ Value val (Annotation Nothing loc)
  hspace
  choice endParsers
  pure vals
 where
  wrappedFpParser = between (char '\"') (char '\"') $ many printChar
  filePathParser = some $ noneOf $ [' ', ',', '\r', '\n'] ++ if inBraces then ['{', '}'] else []
  endParsers = [eof, void eol] ++ ([void $ lookAhead $ string "}" | inBraces])

parseSourcePosToRange :: SourcePos -> SourcePos -> LSP.Range
parseSourcePosToRange start end =
  LSP.Range
    { LSP._start = parseSourcePosToPosition start
    , LSP._end = parseSourcePosToPosition end
    }

parseSourcePosToPosition :: SourcePos -> LSP.Position
parseSourcePosToPosition (SourcePos _ srcLine srcCol) =
  LSP.Position
    { LSP._line = fromIntegral $ unPos srcLine - 1
    , LSP._character = fromIntegral $ unPos srcCol - 1
    }

{- | Calls a given parser and returns the value returned by the parser
  and the range from the start of the parsed value to the end of the parsed value.
-}
annotateSrcLoc :: Parser a -> Parser (a, LSP.Range)
annotateSrcLoc a = do
  start <- getSourcePos
  parsed <- a
  end <- getSourcePos
  pure (parsed, parseSourcePosToRange start end)

{- | Calls a given parser and returns the value returned by the parser
with the positions of the braces around the parsed value.
-}
annotateBraces :: Parser a -> Parser (a, Braces)
annotateBraces a = do
  throwAwayLines
  hspace
  opening <- getSourcePos
  parsed <- between (char '{') (char '}') $ do
    throwAwayLines
    parsed' <- a
    throwAwayLines
    hspace
    pure parsed'
  closing <- getSourcePos
  pure (parsed, Braces{openingBrace = parseSourcePosToPosition opening, closingBrace = parseSourcePosToPosition closing})

{- | Discards an arbitrary number of consecutive lines we
 want to ignore when parsing a cabal file.

 This can be empty lines or a comment.
-}
throwAwayLines :: Parser ()
throwAwayLines = do
  void $ many $ try $ do
    emptyLineParser
    void $ optional commentParser
    eol
  void $ optional $ try $ do
    emptyLineParser
    void $ optional commentParser
    eof
 where
  --  Discards white spaces
  emptyLineParser :: Parser ()
  emptyLineParser = label "Empty Line parser" $ do
    hspace

  --  Discards a comment
  commentParser :: Parser ()
  commentParser = label "comment parser " $ do
    void $ string "--"
    void $ many (anySingleBut '\n')
