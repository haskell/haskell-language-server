module    Format where
foo   :: Int ->  Int
foo  3 = 2
foo    x  = x
bar   :: String ->   IO String
bar s =  do
      x <- return "hello"
      return "asdf"

data Baz = Baz { a :: Int, b :: String }

