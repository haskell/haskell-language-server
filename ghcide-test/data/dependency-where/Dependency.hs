module Dependency where

import           Data.Scientific (Scientific(base10Exponent))

b :: Scientific -> Int
b = base10Exponent
