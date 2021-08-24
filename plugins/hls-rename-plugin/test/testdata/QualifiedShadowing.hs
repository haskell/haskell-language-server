import qualified Foo as F

bar :: Int -> Int
bar x = F.foo x + foo x

foo :: Int -> Int
foo _ = 5
