{-# LANGUAGE OverloadedRecordDot #-}
data Happy = Happy {name :: String, age :: Int, happy :: Bool}

main :: IO ()
main = do
    putStrLn test

man :: Happy
man = Happy {name = "Happy", age = 1, happy = True}

test :: String
test = name man
