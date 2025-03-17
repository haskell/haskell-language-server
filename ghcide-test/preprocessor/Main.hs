
module Main(main) where

import           System.Environment

main :: IO ()
main = do
    _:input:output:_ <- getArgs
    let f = map (\x -> if x == 'x' then 'y' else x)
    writeFile output . f =<< readFile input
