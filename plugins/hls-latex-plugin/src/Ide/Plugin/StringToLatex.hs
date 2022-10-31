{-# LANGUAGE OverloadedStrings #-}

module Ide.Plugin.StringToLatex where

import qualified Data.Text                   as Text
import           Text.LaTeX                  (raw, tsqrt)
import           Text.LaTeX.Base.Class       (braces, commS)
import           Text.LaTeX.Base.Syntax      (LaTeX (..), between)
import           Text.LaTeX.Packages.AMSMath (alpha, beta, frac, gamma, gammau,
                                              omega, space, tsin)

type MapSymbol = LaTeX -> LaTeX
type MapOpSymbol = LaTeX -> LaTeX -> LaTeX -> LaTeX

mapSymbol :: MapSymbol
mapSymbol l@(TeXRaw s) = case Text.unpack s of
  "alpha" -> alpha
  "beta"  -> beta
  "omega" -> omega
  "gamma" -> gamma
  "_"     -> commS "textunderscore"
  ('_':x) -> commS "textunderscore " <> mapSymbol (raw (Text.pack x))
  _       -> l
mapSymbol _ = mempty

fToLatex :: LaTeX -> LaTeX -> LaTeX
fToLatex l@(TeXRaw t) a  = case Text.unpack t of
  "sqrt"   -> tsqrt Nothing a
  "sin"    -> tsin <> space <> a
  "gammau" -> gammau
  _        -> l <> space <> a
fToLatex _ _ = mempty

opToLatex :: MapOpSymbol
opToLatex l o@(TeXRaw op) r  = case Text.unpack op of
  "/"  -> frac l r
  "^"  -> between (raw "^") l (braces r)
  "**" -> between (raw "^") l (braces r)
  "."  -> between (between (commS "cdot") space space) l r
  op   -> l <> o <> r
opToLatex _ _ _  = mempty
