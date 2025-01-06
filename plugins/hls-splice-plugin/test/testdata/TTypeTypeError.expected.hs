{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TemplateHaskell #-}
module TTypeTypeError where
import Language.Haskell.TH ( appT, numTyLit, litT, conT )
import Data.Proxy ( Proxy )

main :: IO Proxy 42)
main = return ()
