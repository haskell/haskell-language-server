module Bar where

import Data.Set (Set, empty)

a = 42

-- These blank lines are here
-- to ensure that b is defined
-- on a line number larger than
-- the number of lines in Foo.hs.
b = 43

emptySet :: Set Integer
emptySet = empty
