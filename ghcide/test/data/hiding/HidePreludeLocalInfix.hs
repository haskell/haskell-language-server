module HidePreludeLocalInfix where

infixed xs ys = xs ++ ys

data Vec a

(++) :: Vec a -> Vec a -> Vec a
(++) = undefined
