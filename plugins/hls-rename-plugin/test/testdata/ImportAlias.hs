import Foo ((!))
import Foo as 𝐹𝔽
import Missing.Module as M

bar :: Int -> Int
bar = 𝐹𝔽.foo

baz :: Int -> Int -> Int
baz = (!)
