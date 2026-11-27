module FoldExpression where

maybeDefault x y =
  if x == Nothing
    then y
    else x

-- replace an expression with a function call
f p =
  let y = 1 in
  maybeDefault p y
