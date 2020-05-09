
main = putStrLn "hello"

lifting x = x + y
  where
    y = 4

liftToTop x = x + y
  where
    y = z + 4
      where
        z = 7


