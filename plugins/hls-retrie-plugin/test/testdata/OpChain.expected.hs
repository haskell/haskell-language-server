module OpChain where

e :: Int -> Int
e y = y * 2

-- the argument 1 + 2 * 3 parses left-nested as (1 + 2) * 3. the ast must be
-- re-associated before passing to retrie.
f :: Int
f = (1 + 2 * 3) * 2
