-- we need to name the args ourselves first
nothingToDestruct :: [a] -> [a] -> [a]
nothingToDestruct = _


-- can't destruct all for non-top-level holes
notTop :: Bool -> Bool -> Bool
notTop a b = a && _

-- destruct all is ok
canDestructAll :: Bool -> Bool -> Bool
canDestructAll a b = _
