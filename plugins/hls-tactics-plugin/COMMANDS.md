# Wingman Metaprogram Command Reference

## application

arguments: none.  
non-deterministic.

> Apply any function in the hypothesis that returns the correct type.


### Example

Given:

```haskell
f :: a -> b

_ :: b
```

running  `application` will produce:

```haskell
f (_ :: a)
```

## apply

arguments: single reference.  
deterministic.

> Apply the given function from *local* scope.


### Example

Given:

```haskell
f :: a -> b

_ :: b
```

running  `apply f` will produce:

```haskell
f (_ :: a)
```

## assume

arguments: single reference.  
deterministic.

> Use the given term from the hypothesis, unifying it with the current goal


### Example

Given:

```haskell
some_a_val :: a

_ :: a
```

running  `assume some_a_val` will produce:

```haskell
some_a_val
```

## assumption

arguments: none.  
non-deterministic.

> Use any term in the hypothesis that can unify with the current goal.


### Example

Given:

```haskell
some_a_val :: a

_ :: a
```

running  `assumption` will produce:

```haskell
some_a_val
```

## auto

arguments: none.  
non-deterministic.

> Repeatedly attempt to split, destruct, apply functions, and recurse in an attempt to fill the hole.


### Example

Given:

```haskell
f :: a -> b
g :: b -> c

_ :: a -> c
```

running  `auto` will produce:

```haskell
g . f
```

## binary

arguments: none.  
deterministic.

> Produce a hole for a two-parameter function, as well as holes for its arguments. The argument holes have the same type but are otherwise unconstrained, and will be solved before the function.


### Example

> In the example below, the variable `a` is free, and will unify to the resulting extract from any subsequent tactic.

Given:

```haskell
_ :: Int
```

running  `binary` will produce:

```haskell
(_3 :: a -> a -> Int) (_1 :: a) (_2 :: a)
```

## cata

arguments: single reference.  
deterministic.

> Destruct the given term, recursing on every resulting binding.


### Example

> Assume we're called in the context of a function `f.`

Given:

```haskell
x :: (a, a)

_ 
```

running  `cata x` will produce:

```haskell
case x of
  (a1, a2) ->
    let a1_c = f a1
        a2_c = f a2
     in _
```

## collapse

arguments: none.  
deterministic.

> Collapse every term in scope with the same type as the goal.


### Example

Given:

```haskell
a1 :: a
a2 :: a
a3 :: a

_ :: a
```

running  `collapse` will produce:

```haskell
(_ :: a -> a -> a -> a) a1 a2 a3
```

## ctor

arguments: single reference.  
deterministic.

> Use the given data constructor.


### Example

Given:

```haskell
_ :: Maybe a
```

running  `ctor Just` will produce:

```haskell
Just (_ :: a)
```

## destruct

arguments: single reference.  
deterministic.

> Pattern match on the argument.


### Example

Given:

```haskell
a :: Bool

_ 
```

running  `destruct a` will produce:

```haskell
case a of
  False -> _
  True -> _
```

## destruct_all

arguments: none.  
deterministic.

> Pattern match on every function parameter, in original binding order.


### Example

> Assume `a` and `b` were bound via `f a b = _`.

Given:

```haskell
a :: Bool
b :: Maybe Int

_ 
```

running  `destruct_all` will produce:

```haskell
case a of
  False -> case b of
    Nothing -> _
    Just i -> _
  True -> case b of
    Nothing -> _
    Just i -> _
```

## homo

arguments: single reference.  
deterministic.

> Pattern match on the argument, and fill the resulting hole in with the same data constructor.


### Example

> Only applicable when the type constructor of the argument is the same as that of the hole.

Given:

```haskell
e :: Either a b

_ :: Either x y
```

running  `homo e` will produce:

```haskell
case e of
  Left a -> Left (_ :: x)
  Right b -> Right (_ :: y)
```

## idiom

arguments: tactic.  
deterministic.

> Lift a tactic into idiom brackets.


### Example

Given:

```haskell
f :: a -> b -> Int

_ :: Maybe Int
```

running  `idiom (apply f)` will produce:

```haskell
f <$> (_ :: Maybe a) <*> (_ :: Maybe b)
```

## intro

arguments: single binding.  
deterministic.

> Construct a lambda expression, binding an argument with the given name.


### Example

Given:

```haskell
_ :: a -> b -> c -> d
```

running  `intro aye` will produce:

```haskell
\aye -> (_ :: b -> c -> d)
```

## intros

arguments: variadic binding.  
deterministic.

> Construct a lambda expression, using the specific names if given, generating unique names otherwise. When no arguments are given, all of the function arguments will be bound; otherwise, this tactic will bind only enough to saturate the given names. Extra names are ignored.


### Example

Given:

```haskell
_ :: a -> b -> c -> d
```

running  `intros` will produce:

```haskell
\a b c -> (_ :: d)
```

### Example

Given:

```haskell
_ :: a -> b -> c -> d
```

running  `intros aye` will produce:

```haskell
\aye -> (_ :: b -> c -> d)
```

### Example

Given:

```haskell
_ :: a -> b -> c -> d
```

running  `intros x y z w` will produce:

```haskell
\x y z -> (_ :: d)
```

## let

arguments: variadic binding.  
deterministic.

> Create let-bindings for each binder given to this tactic.


### Example

Given:

```haskell
_ :: x
```

running  `let a b c` will produce:

```haskell
let a = _1 :: a
    b = _2 :: b
    c = _3 :: c
 in (_4 :: x)

```

## nested

arguments: single reference.  
non-deterministic.

> Nest the given function (in module scope) with itself arbitrarily many times. NOTE: The resulting function is necessarily unsaturated, so you will likely need `with_arg` to use this tactic in a saturated context.


### Example

Given:

```haskell
_ :: [(Int, Either Bool a)] -> [(Int, Either Bool b)]
```

running  `nested fmap` will produce:

```haskell
fmap (fmap (fmap _))
```

## obvious

arguments: none.  
non-deterministic.

> Produce a nullary data constructor for the current goal.


### Example

Given:

```haskell
_ :: [a]
```

running  `obvious` will produce:

```haskell
[]
```

## pointwise

arguments: tactic.  
deterministic.

> Restrict the hypothesis in the holes of the given tactic to align up with the top-level bindings. This will ensure, eg, that the first hole can see only terms that came from the first position in any terms destructed from the top-level bindings.


### Example

> In the context of `f (a1, b1) (a2, b2) = _`. The resulting first hole can see only 'a1' and 'a2', and the second, only 'b1' and 'b2'.

Given:

```haskell
_ 
```

running  `pointwise (use mappend)` will produce:

```haskell
mappend _ _
```

## recursion

arguments: none.  
deterministic.

> Fill the current hole with a call to the defining function.


### Example

> In the context of `foo (a :: Int) (b :: b) = _`:

Given:

```haskell
_ 
```

running  `recursion` will produce:

```haskell
foo (_ :: Int) (_ :: b)
```

## sorry

arguments: none.  
deterministic.

> "Solve" the goal by leaving a hole.


### Example

Given:

```haskell
_ :: b
```

running  `sorry` will produce:

```haskell
_ :: b
```

## split

arguments: none.  
non-deterministic.

> Produce a data constructor for the current goal.


### Example

Given:

```haskell
_ :: Either a b
```

running  `split` will produce:

```haskell
Right (_ :: b)
```

## try

arguments: tactic.  
non-deterministic.

> Simultaneously run and do not run a tactic. Subsequent tactics will bind on both states.


### Example

Given:

```haskell
f :: a -> b

_ :: b
```

running  `try (apply f)` will produce:

```haskell
-- BOTH of:

f (_ :: a)

-- and

_ :: b

```

## unary

arguments: none.  
deterministic.

> Produce a hole for a single-parameter function, as well as a hole for its argument. The argument holes are completely unconstrained, and will be solved before the function.


### Example

> In the example below, the variable `a` is free, and will unify to the resulting extract from any subsequent tactic.

Given:

```haskell
_ :: Int
```

running  `unary` will produce:

```haskell
(_2 :: a -> Int) (_1 :: a)
```

## use

arguments: single reference.  
deterministic.

> Apply the given function from *module* scope.


### Example

> `import Data.Char (isSpace)`

Given:

```haskell
_ :: Bool
```

running  `use isSpace` will produce:

```haskell
isSpace (_ :: Char)
```

## with_arg

arguments: none.  
deterministic.

> Fill the current goal with a function application. This can be useful when you'd like to fill in the argument before the function, or when you'd like to use a non-saturated function in a saturated context.


### Example

> Where `a` is a new unifiable type variable.

Given:

```haskell
_ :: r
```

running  `with_arg` will produce:

```haskell
(_2 :: a -> r) (_1 :: a)
```

