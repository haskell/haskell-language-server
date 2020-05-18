-- | Testing that HaRe can find source files from a cabal file

import qualified Foo.Bar as B

main = putStrLn "foo"

baz = 3 + B.baz
