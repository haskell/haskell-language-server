
import GHC.Generics

main = putStrLn "hello"

type Foo = Int

instance Show Foo where
  show x = undefined

instance Show (Int,String) where
  show  = undefined

data FFF a = FFF Int String a
           deriving (Generic,Functor,Traversable)
