import Data.Monoid

data Foo = Foo (Sum Int) (Sum Int)

mappend2 :: Foo -> Foo -> Foo
mappend2 = [wingman| intros f1 f2, destruct_all, ctor Foo; pointwise (use mappend); assumption|]

