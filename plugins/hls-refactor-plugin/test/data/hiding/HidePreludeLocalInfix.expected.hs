module HidePreludeLocalInfix where
import Prelude hiding ((++))

infixed xs ys = xs ++ ys

data Vec a

(++) :: Vec a -> Vec a -> Vec a
(++) = undefined
