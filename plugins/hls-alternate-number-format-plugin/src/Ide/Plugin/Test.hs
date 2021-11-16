{-# LANGUAGE BinaryLiterals     #-}
{-# LANGUAGE HexFloatLiterals   #-}
{-# LANGUAGE MagicHash          #-}
{-# LANGUAGE NegativeLiterals   #-}
{-# LANGUAGE NumDecimals        #-}
{-# LANGUAGE NumericUnderscores #-}
module Ide.Plugin.Test where
import           GHC.Base (Int#, (+#))
import           GHC.Exts (Char#)

test 0x3E8 = 0o1 :: Integer
test x     = 345



test2 :: Double -> Double
test2 0.5 = 0.75
test2 _   = 0x1.eb851eb851eb8p-4
