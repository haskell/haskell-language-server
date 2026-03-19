import Foo ((!))
import qualified Foo as F

bar :: Int -> Int
bar = F.foo

baz :: Int -> Int -> Int
baz = (!)
