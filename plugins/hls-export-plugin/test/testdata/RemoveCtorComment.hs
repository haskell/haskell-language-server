module RemoveCtorComment
    ( Foo
        ( Foo1 -- first
        , Foo2 -- second
        , Foo3 -- third
        )
    ) where

data Foo = Foo1 | Foo2 | Foo3
