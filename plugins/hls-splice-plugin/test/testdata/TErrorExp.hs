{-# LANGUAGE TemplateHaskell #-}
module TErrorExp where
import Language.Haskell.TH ( tupE, litE, integerL )

main :: IO ()
main = return $(tupE [litE $ integerL 42, tupE []])
