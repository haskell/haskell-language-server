test :: Bool -> IO ()
test b = do
  putStrLn "hello"
  case b of
    False -> _w0
    True -> _w1
  pure ()

