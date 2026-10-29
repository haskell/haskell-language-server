{-# LANGUAGE CPP #-}
module Untaken where
#ifdef VERBOSE
import Ops
#endif
total :: Int
#ifdef VERBOSE
total=1<+>2
#else
total=3
#endif
