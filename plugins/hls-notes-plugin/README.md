# Note plugin

The [Note convention](https://gitlab.haskell.org/ghc/ghc/-/wikis/commentary/coding-style#2-using-notes) is a nice way to hoist and share big chunks of documentation out of the body of functions. This is done by referencing a long form note from within the function. This plugin extends goto-definition to jump from the reference to the note.

# Example

Main.hs
```haskell
module Main where

main :: IO
main = do
    doSomething -- We need this here, see Note [Do Something] in Foo
```

Foo.hs
```
module Foo where

doSomething :: IO ()
doSomething = undefined

{-
Note [Do Something]
~~~~~~~~~~~~~~~~~~~
Some very important explanation
-}
```

Using "Go-to-definition on the Note reference in `Main.hs` will jump to the beginning of the note in `Foo.hs`.
