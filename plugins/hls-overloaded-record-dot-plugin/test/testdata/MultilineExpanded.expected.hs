{-# LANGUAGE OverloadedRecordDot #-}

data Happy = Happy {name :: String, age :: Int, happy :: Bool}

data Building = Building {address :: String, owner :: Happy}

main :: IO ()
main = do
  putStrLn test

man :: Happy
man = Happy {name = "Happy", age = 1, happy = True}

home :: Building
home = Building {address = "No. 10 Privet Dr.", owner = man}

home2 :: Building
home2 = Building {address = "No. 6 Beach Ave.", owner = man}

home3 :: Building
home3 = Building {address = "No. 12 Central Blvd.", owner = man}

n:: Int
n = 3

test :: String
test = (case n of
   0  -> owner home
   1 -> home2.owner
   2 -> owner home3
   _ -> man).name

