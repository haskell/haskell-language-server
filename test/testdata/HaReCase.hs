
main = putStrLn "hello"

foo :: Int -> Int
foo x = if odd x
        then
          x + 3
        else
          x

