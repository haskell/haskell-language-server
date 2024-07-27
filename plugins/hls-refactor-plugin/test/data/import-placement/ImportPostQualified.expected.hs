{-# LANGUAGE ImportQualifiedPost #-}
{-# OPTIONS_GHC -Wprepositive-qualified-module #-}
import Control.Monad qualified as Control
main :: IO ()
main = Control.when True $ putStrLn "hello"
