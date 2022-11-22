data A = A | B | C

some :: A -> IO ()
some a = do
    foo
    bar a
  where
      foo = putStrLn "Hi"

      bar :: A -> IO ()
      bar A = _w0
      bar B = _w1
      bar C = _w2

