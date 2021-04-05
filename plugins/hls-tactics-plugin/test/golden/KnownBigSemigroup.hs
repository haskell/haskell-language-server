import Data.Monoid

data Big a = Big [Bool] (Sum Int) String (Endo a) Any

instance Semigroup (Big a) where
  (<>) = _

