{- HLINT ignore -}
module Testing ( module Testing ) where
import Data.Text (Text, pack)
data TypeConstructor = DataConstructor
  { fff :: Text
  , ggg :: Int }
aaa :: TypeConstructor
aaa = DataConstructor
  { fff = ""
  , ggg = 0
  }
bbb :: TypeConstructor
bbb = DataConstructor "" 0
ccc :: (Text, Int)
ccc = (fff bbb, ggg aaa)
ddd :: Num a => a -> a -> a
ddd vv ww = vv +! ww
a +! b = a - b
hhh (Just a) (><) = a >< a
iii a b = a `b` a
jjj s = pack $ s <> s
class MyClass a where
  method :: a -> Int
instance MyClass Int where
  method = succ
kkk :: MyClass a => Int -> a -> Int
kkk n c = n + method c

doBind :: Maybe ()
doBind = do unwrapped <- Just ()
            return unwrapped

listCompBind :: [Char]
listCompBind = [ succ c | c <- "abc" ]

multipleClause :: Bool -> Char
multipleClause True  = 't'
multipleClause False = 'f'

-- | Recognizable docs: kpqz
documented = True
