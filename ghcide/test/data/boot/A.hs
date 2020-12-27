module A where

import B( TB(..) )

newtype TA = MkTA Int

f :: TB -> TA
f (MkTB x) = MkTA x
