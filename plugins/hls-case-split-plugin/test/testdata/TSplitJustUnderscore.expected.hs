fun :: String -> Maybe Bool -> IO ()
fun (x:xs) (Just False) = putStrLn "example"
fun (x:xs) (Just True) = putStrLn "example"
fun _ _ = pure ()
