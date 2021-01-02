{-# LANGUAGE TemplateHaskell #-}
module A where

import B( TB(..) )

newtype TA = MkTA Int
    deriving Eq

f :: TB -> TA
f (MkTB x) = MkTA x
