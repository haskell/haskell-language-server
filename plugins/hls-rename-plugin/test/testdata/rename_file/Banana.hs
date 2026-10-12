module Banana where
import MyFile (foo)
import qualified MyFile
import MyFile qualified
import MyFile qualified as N
bar = foo