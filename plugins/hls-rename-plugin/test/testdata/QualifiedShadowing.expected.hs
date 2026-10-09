import qualified Foo as F

bar :: Int -> Int
bar x = F.foobar x + foo x

foo :: Int -> Int
foo _ = 5
