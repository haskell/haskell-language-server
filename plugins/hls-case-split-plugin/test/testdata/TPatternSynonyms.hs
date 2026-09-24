{-# LANGUAGE PatternSynonyms #-}
{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE EmptyCase #-}
module Pat where

data Bar = Bar | Baz

pattern Foo :: Bar
pattern Foo = Baz

x :: Bar -> p
x y = case y :: Bar of
