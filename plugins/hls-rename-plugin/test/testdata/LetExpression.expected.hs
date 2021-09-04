module Let where

import Foo

bar :: Int
bar = let foobar = 5 in
    foobar * foobar

quux :: Int
quux = Foo.foo 4
