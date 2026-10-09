{-# OPTIONS_GHC -Wwarn #-}

-- | GHC language options parser
module Ide.Plugin.Eval.Parse.Option (
    langOptions,
    parseSetFlags,
) where

import           Control.Arrow        (left)
import           Data.Void            (Void)
import           Text.Megaparsec
import           Text.Megaparsec.Char

{- |
>>> langOptions ":set   -XBinaryLiterals  -XOverloadedStrings "
Right ["BinaryLiterals","OverloadedStrings"]

>>> langOptions ":set"
Right []

>>> langOptions ""
Left "No match"
-}
langOptions :: String -> Either String [String]
langOptions =
  left errorBundlePretty
  . parse (space *> languageOpts <* eof) ""

parseSetFlags :: String -> Maybe String
parseSetFlags = parseMaybe
    (hspace *> chunk ":set"
        *> hspace1 *> takeRest
        :: Parsec Void String String
    )

-- >>> parseMaybe languageOpts ":set -XBinaryLiterals -XOverloadedStrings"
-- Just ["BinaryLiterals","OverloadedStrings"]
languageOpts :: Parsec Void String [String]
languageOpts = string ":set" *> space1
  *> many (string "-X" *> many letterChar <* space)
