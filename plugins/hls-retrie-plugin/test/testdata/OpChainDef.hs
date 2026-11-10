module OpChainDef where

-- the body y + 1 * 2 parses left-nested as (y + 1) * 2. the template must be
-- re-associated before substitution, or the result under * loses its parens.
e :: Int -> Int
e y = y + 1 * 2

f :: Int
f = e 3 * 4
