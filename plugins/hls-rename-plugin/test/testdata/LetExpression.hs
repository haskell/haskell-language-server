module Let where

import Foo

bar :: Int
bar = let foo = 5 in
    foo * foo

quux :: Int
quux = Foo.foo 4
