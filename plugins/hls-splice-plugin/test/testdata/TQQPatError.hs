{-# LANGUAGE QuasiQuotes #-}
module TQQPatError where
import QQ

f :: () -> IO ()
f [str|str|] = putStrLn "is str"
f _ = putStrLn " not str"
