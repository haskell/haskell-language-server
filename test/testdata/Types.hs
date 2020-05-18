module Types where

import Control.Applicative

foo :: Maybe Int -> Int
foo (Just x) = x
foo Nothing = 0

bar :: Maybe Int -> Int
bar x = case x of
    Just y -> y + 1
    Nothing -> 0

maybeMonad :: Maybe Int -> Maybe Int
maybeMonad x = do
    y <- x
    let z = return (y + 10)
    b <- z
    return (b + y)

funcTest :: (a -> a) -> a -> a
funcTest f a = f a

compTest :: (b -> c) -> (a -> b) -> a -> c
compTest f g = let h = f . g in h

monadStuff :: (a -> b) -> IO a -> IO b
monadStuff f action = f <$> action

data Test
    = TestC Int
    | TestM String
    deriving (Show, Eq, Ord)