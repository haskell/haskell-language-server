module Main (main) where

main :: IO ()
main =
    let
      greeting :: String
      greeting = "Bonjour"
      greetings = unwords $ replicate times greeting;
      who = "Tom"
      times = 123;
     in
      putStrLn $ unwords [ greetings, who ]
