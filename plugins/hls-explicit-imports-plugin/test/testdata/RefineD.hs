module RefineD (module RefineE, module RefineD) where

import RefineE hiding (e1)
import qualified RefineE

e1 :: String 
e1 = RefineE.e1 <> " but overrided"