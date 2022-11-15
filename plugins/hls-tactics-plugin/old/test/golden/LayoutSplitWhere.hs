data A = A | B | C

some :: A -> IO ()
some a = do
    foo
    bar a
  where
      foo = putStrLn "Hi"

      bar :: A -> IO ()
      bar x = _

