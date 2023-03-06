{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TemplateHaskell #-}
module TTypeKindError where
import Language.Haskell.TH ( numTyLit, litT )
import Data.Proxy ( Proxy )

main :: 42
main = return ()
