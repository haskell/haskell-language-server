module References where

import OtherModule

foo = bar

bar = let x = bar 42 in const "hello"

baz = do
  x <- bar 23
  return $ bar 14

data Account =
  Checking
  | Savings

bobsAccount = Checking

bobHasChecking = case bobsAccount of
                     Checking -> True
                     Savings -> False

x = symbolDefinedInOtherModule

y = symbolDefinedInOtherOtherModule
