main = return ()

foo = bar

bar = let x = bar 42 in const "hello"

baz = do
  x <- bar 23
  return $ bar 14
