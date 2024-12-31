{-# LANGUAGE TemplateHaskell #-}
module MyLib (i) where
import Control.Concurrent (threadDelay)
import Language.Haskell.TH.Syntax

-- | This code generates a super simple integer, but waits for 10s during
-- compilation
i = $(do
  runIO (threadDelay 10000000)
  [e|5|])
