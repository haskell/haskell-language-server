{-# LANGUAGE DataKinds #-}
{-# LANGUAGE QuasiQuotes #-}
module TQQTypeTypeError where
import Language.Haskell.TH ( appT, numTyLit, litT, conT )
import Data.Proxy ( Proxy(..) )
import QQ

main :: IO (Proxy "str")
main = return ()
