-- Copyright (c) 2019 The DAML Authors. All rights reserved.
-- SPDX-License-Identifier: Apache-2.0


-- | Types and functions for working with source code locations.
module Development.IDE.Types.Location
    ( Location(..)
    , noFilePath
    , noRange
    , Position(..)
    , showPosition
    , Range(..)
    , LSP.Uri(..)
    , LSP.NormalizedUri
    , LSP.toNormalizedUri
    , LSP.fromNormalizedUri
    , LSP.NormalizedFilePath
    , fromUri
    , emptyFilePath
    , emptyPathUri
    , toNormalizedFilePath'
    , LSP.fromNormalizedFilePath
    , filePathToUri'
    , uriToFilePath'
    , readSrcSpan
    ) where

import Control.Applicative
import Language.Haskell.LSP.Types (Location(..), Range(..), Position(..))
import Control.Monad
import Data.Hashable (Hashable(hash))
import Data.String
import FastString
import qualified Language.Haskell.LSP.Types as LSP
import SrcLoc as GHC
import Text.ParserCombinators.ReadP as ReadP
import Data.Maybe (fromMaybe)

toNormalizedFilePath' :: FilePath -> LSP.NormalizedFilePath
-- We want to keep empty paths instead of normalising them to "."
toNormalizedFilePath' "" = emptyFilePath
toNormalizedFilePath' fp = LSP.toNormalizedFilePath fp

emptyFilePath :: LSP.NormalizedFilePath
emptyFilePath = LSP.NormalizedFilePath emptyPathUri ""

-- | We use an empty string as a filepath when we don’t have a file.
-- However, haskell-lsp doesn’t support that in uriToFilePath and given
-- that it is not a valid filepath it does not make sense to upstream a fix.
-- So we have our own wrapper here that supports empty filepaths.
uriToFilePath' :: LSP.Uri -> Maybe FilePath
uriToFilePath' uri
    | uri == LSP.fromNormalizedUri emptyPathUri = Just ""
    | otherwise = LSP.uriToFilePath uri

emptyPathUri :: LSP.NormalizedUri
emptyPathUri =
    let s = "file://"
    in LSP.NormalizedUri (hash s) s

filePathToUri' :: LSP.NormalizedFilePath -> LSP.NormalizedUri
filePathToUri' = LSP.normalizedFilePathToUri

fromUri :: LSP.NormalizedUri -> LSP.NormalizedFilePath
fromUri = fromMaybe (toNormalizedFilePath' noFilePath) . LSP.uriToNormalizedFilePath

noFilePath :: FilePath
noFilePath = "<unknown>"

-- A dummy range to use when range is unknown
noRange :: Range
noRange =  Range (Position 0 0) (Position 1 0)

showPosition :: Position -> String
showPosition Position{..} = show (_line + 1) ++ ":" ++ show (_character + 1)

-- | Parser for the GHC output format
readSrcSpan :: ReadS RealSrcSpan
readSrcSpan = readP_to_S (singleLineSrcSpanP <|> multiLineSrcSpanP)
  where
    singleLineSrcSpanP, multiLineSrcSpanP :: ReadP RealSrcSpan
    singleLineSrcSpanP = do
      fp <- filePathP
      l  <- readS_to_P reads <* char ':'
      c0 <- readS_to_P reads
      c1 <- (char '-' *> readS_to_P reads) <|> pure c0
      let from = mkRealSrcLoc fp l c0
          to   = mkRealSrcLoc fp l c1
      return $ mkRealSrcSpan from to

    multiLineSrcSpanP = do
      fp <- filePathP
      s <- parensP (srcLocP fp)
      void $ char '-'
      e <- parensP (srcLocP fp)
      return $ mkRealSrcSpan s e

    parensP :: ReadP a -> ReadP a
    parensP = between (char '(') (char ')')

    filePathP :: ReadP FastString
    filePathP = fromString <$> (readFilePath <* char ':') <|> pure ""

    srcLocP :: FastString -> ReadP RealSrcLoc
    srcLocP fp = do
      l <- readS_to_P reads
      void $ char ','
      c <- readS_to_P reads
      return $ mkRealSrcLoc fp l c

    readFilePath :: ReadP FilePath
    readFilePath = some ReadP.get
