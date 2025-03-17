module B(TA(..), TB(..)) where
import {-# SOURCE #-} A( TA(..) )

data TB = MkTB !Int

g :: TA -> TB
g (MkTA x) = MkTB x
