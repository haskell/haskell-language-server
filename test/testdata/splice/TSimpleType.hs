{-# LANGUAGE TemplateHaskell #-}
module TSimpleType where
import Language.Haskell.TH ( tupleT )

main :: IO $(tupleT 0)
main = return ()
