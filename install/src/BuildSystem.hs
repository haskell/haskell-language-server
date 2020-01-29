{-# LANGUAGE CPP #-}

module BuildSystem where

buildSystem :: String
buildSystem =
#if RUN_FROM_STACK
  "stack"
#else
  "cabal"
#endif

isRunFromStack :: Bool
isRunFromStack = buildSystem == "stack"

isRunFromCabal :: Bool
isRunFromCabal = buildSystem == "cabal"
