import Data.Foldable as F
import Prelude as Reexport

baz :: Foldable t => (a -> b -> b) -> b -> t a -> b
baz = Reexport.foldr

bar :: (Foldable t, Applicative f) => (a -> f b) -> t a -> f ()
bar = F.traverse_

bux :: Foldable t => (b -> a -> b) -> b -> t a -> b
bux = Reexport.foldl
