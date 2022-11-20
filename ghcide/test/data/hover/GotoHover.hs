{-# LANGUAGE OverloadedStrings, TemplateHaskell #-}
{- HLINT ignore -}
module GotoHover ( module GotoHover) where
import Data.Text (Text, pack)
import Foo (Bar, foo)


data TypeConstructor = DataConstructor
  { fff :: Text
  , ggg :: Int }
aaa :: TypeConstructor
aaa = DataConstructor
  { fff = "dfgy"
  , ggg = 832
  }
bbb :: TypeConstructor
bbb = DataConstructor "mjgp" 2994
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
listCompBind = [ succ c | c <- "ptfx" ]

multipleClause :: Bool -> Char
multipleClause True  =    't'
multipleClause False = 'f'

-- | Recognizable docs: kpqz
documented :: Monad m => Either Int (m a)
documented = Left 7518

listOfInt = [ 8391 :: Int, 6268 ]

outer :: Bool
outer = undefined inner where

  inner :: Char
  inner = undefined

imported :: Bar
imported = foo

aa2 :: Bool
aa2 = $(id [| True |])

hole :: Int
hole = _

hole2 :: a -> Maybe a
hole2 = _

-- A comment above a type defnition with a deriving clause
data Example = Example
  deriving (Eq)
