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

test :: String
test = name home.owner
