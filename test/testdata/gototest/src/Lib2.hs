module Lib2 where

import Lib

g = do
    someFunc
    print x
  where z = 1+2
        y = z+z
        x = y*z

otherId :: DataType -> DataType
otherId dataType = dataType