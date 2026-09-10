module Class where

class Identity x where
    identity :: x -> x
    identity x = x

function x = identity x
