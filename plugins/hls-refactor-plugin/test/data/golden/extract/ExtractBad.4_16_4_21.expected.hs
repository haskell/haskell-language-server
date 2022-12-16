{-# LANGUAGE RecordWildCards #-}

data Rec = Rec { x :: Int }

newDefinition = x + 1

foo Rec {..} = newDefinition

quux bar = let tmp = bar in let bar = 1 in bar