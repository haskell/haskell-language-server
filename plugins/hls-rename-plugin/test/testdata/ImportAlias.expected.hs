import Foo ((!))
import Foo as G
import Missing.Module as M

bar :: Int -> Int
bar = G.foo

baz :: Int -> Int -> Int
baz = (!)
