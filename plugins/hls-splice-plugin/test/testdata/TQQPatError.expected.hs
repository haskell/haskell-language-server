{-# LANGUAGE QuasiQuotes #-}
module TQQPatError where
import QQ

f :: () -> IO ()
f "str"= putStrLn "is str"
f _ = putStrLn " not str"
