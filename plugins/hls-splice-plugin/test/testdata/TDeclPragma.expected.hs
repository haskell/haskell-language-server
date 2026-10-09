{-# LANGUAGE TemplateHaskell #-}
module TDeclPragma where
import Language.Haskell.TH

myId :: Int -> Int
{-# INLINE myId #-}
myId x = x
