-- comment before header
module Comments where

{-# standalone annotation #-}

-- standalone comment

-- | haddock comment
f = {- inline comment -} ({- inline comment inside refactored code -}1) -- ending comment

-- final comment
