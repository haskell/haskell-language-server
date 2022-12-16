{-# LANGUAGE RecordWildCards #-}

data Rec = Rec { x :: Int }

foo Rec {..} = x + 1

newDefinition = let tmp = bar in let bar = 1 in bar

quux bar = newDefinition