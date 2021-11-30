{- block comment -} 
{-# LANGUAGE TupleSections #-}
{-| multi
line
block
haddock
-}

module BlockCommentThenMultiLineBlockHaddock where
import GHC.SourceGen (multiIf)
import Diagrams (block)

a = (1,)
