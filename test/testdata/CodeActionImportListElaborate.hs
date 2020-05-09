{-# LANGUAGE NoImplicitPrelude #-}
import           System.IO (IO)
import           Data.List (find, head, last, tail, init, union, (\\), null, length, cons, uncons)
-- | Main entry point to the program
main :: IO ()
main =
    when True
        $ hPutStrLn stderr
        $ fromMaybe "Good night, World!" (Just "Hello, World!")