{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedRecordDot #-}
module Foo (Node(..)) where

data Node = Node
  {
    label :: ()
  }

instance Semigroup Node where
  n1 <> n2 = Node
    { label = n1.label <> n2.label
    }
