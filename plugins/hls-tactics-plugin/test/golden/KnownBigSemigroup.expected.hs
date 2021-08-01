import Data.Monoid

data Big a = Big [Bool] (Sum Int) String (Endo a) Any

instance Semigroup (Big a) where
  (Big bs sum s en any) <> (Big bs' sum' str en' any')
    = Big
        (bs <> bs') (sum <> sum') (s <> str) (en <> en') (any <> any')

