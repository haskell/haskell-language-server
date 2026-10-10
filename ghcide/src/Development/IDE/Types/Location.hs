-- Copyright (c) 2019 The DAML Authors. All rights reserved.
-- SPDX-License-Identifier: Apache-2.0

-- | Types and functions for working with source code locations.
--
-- The path type 'NormalizedFilePath' and its conversions are defined in
-- "Ide.Types.Location" (hls-plugin-api) and re-exported here; ghcide and the
-- plugins all share this single path domain.
module Development.IDE.Types.Location
    ( Location(..)
    , noRange
    , Position(..)
    , showPosition
    , Range(..)
    , LSP.Uri(..)
    , LSP.NormalizedUri
    , LSP.toNormalizedUri
    , LSP.fromNormalizedUri
    , Ide.Types.Location.NormalizedFilePath(..)
    , Ide.Types.Location.systemFsEncoding
    , Ide.Types.Location.encodeOsPath
    , Ide.Types.Location.decodeOsPath
    , Ide.Types.Location.toNormalizedFilePath'
    , Ide.Types.Location.fromNormalizedFilePath
    , Ide.Types.Location.uriToFilePath'
    , Ide.Types.Location.uriToNormalizedFilePath
    , Ide.Types.Location.filePathToUri'
    , Ide.Types.Location.fromUri
    , Ide.Types.Location.emptyFilePath
    , Ide.Types.Location.emptyPathUri
    , Ide.Types.Location.noFilePath
    , readSrcSpan
    ) where

import           Control.Applicative
import           Control.Monad
import           Data.String
import qualified Ide.Types.Location
import           Language.LSP.Protocol.Types  (Location (..), Position (..),
                                               Range (..))
import qualified Language.LSP.Protocol.Types  as LSP
import           Text.ParserCombinators.ReadP as ReadP

import           GHC.Data.FastString
import           GHC.Types.SrcLoc             as GHC

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
