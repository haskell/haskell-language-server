{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TemplateHaskell #-}
module TTypeTypeError where
import Language.Haskell.TH ( appT, numTyLit, litT, conT )
import Data.Proxy ( Proxy )

main :: IO $(conT ''Proxy `appT` litT (numTyLit 42))
main = return ()
