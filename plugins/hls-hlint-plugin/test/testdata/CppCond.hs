{-# LANGUAGE CPP #-}
module CppCond where

#ifdef FLAG
f = (1)
#else
g = 2
#endif
