{-# OPTIONS_GHC -Wwarn #-}

-- | GHC language options parser
module Ide.Plugin.Eval.Parse.Option (
    langOptions,
) where

import Control.Monad.Combinators (many)
import Ide.Plugin.Eval.Parse.Parser (
    Parser,
    letterChar,
    runParser,
    space,
    string,
 )

{- |
>>> langOptions ":set   -XBinaryLiterals  -XOverloadedStrings "
Right ["BinaryLiterals","OverloadedStrings"]

>>> langOptions ":set"
Right []

>>> langOptions ""
Left "No match"
-}
langOptions :: [Char] -> Either String [[Char]]
langOptions = runParser (many space *> languageOpts <* many space)

-- >>> runParser languageOpts ":set -XBinaryLiterals -XOverloadedStrings"
-- Right ["BinaryLiterals","OverloadedStrings"]
languageOpts :: Parser Char [[Char]]
languageOpts = string ":set" *> many (many space *> string "-X" *> (many letterChar))
