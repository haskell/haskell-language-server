{-# LANGUAGE OverloadedStrings #-}

-- | This module allows you to parse the output of @cabal info@.
module Ide.Plugin.Cabal.CabalInfoParser (parseCabalInfo, cabalInfo) where

import           Data.Map.Strict   (Map)
import           Data.Text         (Text)
import           Data.Void         (Void)
import           Text.Megaparsec   (MonadParsec (..), Parsec, chunk, failure,
                                    many, parse, single, (<|>))

import           Control.Monad     (void, when)
import           Data.Either.Extra (mapLeft)
import qualified Data.Map.Strict   as Map
import qualified Data.Text         as T

type Parser = Parsec Void Text

parseCabalInfo :: Text -> Either CabalInfoParserError (Map Text (Map Text [Text]))
parseCabalInfo = mapLeft (T.pack . show) . parse cabalInfo ""

type CabalInfoParserError = Text

cabalInfo :: Parser (Map Text (Map Text [Text]))
cabalInfo = do
    entries <- many $ try cabalInfoEntry
    eof

    pure $ Map.fromList entries

cabalInfoEntry :: Parser (Text, Map Text [Text])
cabalInfoEntry = do
    void $ single '*'
    void spaces

    name <- takeWhileP (Just "package name") (/= ' ')

    void restOfLine

    pairs <- many $ try field

    void $ takeWhileP (Just "trailing whitespace") (`elem` (" \t\r\n" :: String))

    pure (name, Map.fromList pairs)

field :: Parser (Text, [Text])
field = do
    spacesBeforeKey <- spaces
    -- We assume that all fields are indented ==> fail if that ain't so.
    when (T.null spacesBeforeKey) $ failure Nothing mempty

    key <- takeWhileP (Just "field name") (/= ':')
    void $ single ':'
    spacesAfterKey <- spaces
    firstLine <- restOfLine

    -- The first line of the field may be empty.
    -- In this case, we have to look at the second line to determine
    -- the indentation depth.
    if T.null firstLine then do
        spacesBeforeFirstLine <- spaces
        firstLine' <- restOfLine
        let indent = T.length spacesBeforeFirstLine
        lines <- trailingIndentedLines indent
        pure (key, firstLine' : lines)
    -- If the first line is *not* empty, we can determine the indentation
    -- depth by calculating how many characters came before it.
    else do
        let indent = T.length spacesBeforeKey + T.length key + 1 + T.length spacesAfterKey
        lines <- trailingIndentedLines indent
        pure (key, firstLine : lines)

    where
        trailingIndentedLines :: Int -> Parser [Text]
        trailingIndentedLines indent = many $ try $ indentedLine indent

indentedLine :: Int -> Parser Text
indentedLine indent = do
    void $ chunk $ T.replicate indent " "
    restOfLine

spaces :: Parser Text
spaces = takeWhileP Nothing (== ' ')

-- | Parse until next @\n@, return text before that.
restOfLine :: Parser Text
restOfLine = do
    s <- takeWhileP (Just "rest of line") (/= '\n')
    eolOrEof
    pure s

eolOrEof :: Parser ()
eolOrEof = void (single '\n') <|> eof
