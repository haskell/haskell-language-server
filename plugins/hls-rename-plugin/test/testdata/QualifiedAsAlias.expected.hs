import Foo ((!))
import qualified Foo as G

bar :: Int -> Int
bar = G.foo

baz :: Int -> Int -> Int
baz = (!)
