fun :: String -> Maybe Bool -> IO ()
fun (x:xs) (Just _) = putStrLn "example"
fun _ _ = pure ()
