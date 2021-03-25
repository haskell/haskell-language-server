{-# LANGUAGE QuasiQuotes #-}
module TQQPat where
import QQ

f :: String -> IO ()
f [str|str|] = putStrLn "is str"
f _ = putStrLn " not str"
