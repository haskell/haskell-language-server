import Foo ((!))
import Foo as F
import Missing.Module as M

bar :: Int -> Int
bar = F.foo

baz :: Int -> Int -> Int
baz = (!)
