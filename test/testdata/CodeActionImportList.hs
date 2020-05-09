-- | Main entry point to the program
main :: IO ()
main =
    when True
        $ hPutStrLn stdout
        $ fromMaybe "Good night, World!" (Just "Hello, World!")