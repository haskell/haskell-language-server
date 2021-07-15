import Data.Monoid

data Foo = Foo (Sum Int) (Sum Int)

mappend2 :: Foo -> Foo -> Foo
mappend2 (Foo sum sum') (Foo sum2 sum3)
  = Foo (mappend sum sum2) (mappend sum' sum3)

