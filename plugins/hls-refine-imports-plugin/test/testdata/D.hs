module D (module E, module D) where

import E hiding (e1)
import qualified E

e1 :: String 
e1 = E.e1 <> " but overrided"