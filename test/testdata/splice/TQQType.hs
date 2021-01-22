{-# LANGUAGE DataKinds #-}
{-# LANGUAGE QuasiQuotes #-}
module TQQType where
import Language.Haskell.TH ( appT, numTyLit, litT, conT )
import Data.Proxy ( Proxy(..) )
import QQ

main :: IO (Proxy [str|str|])
main = return Proxy
