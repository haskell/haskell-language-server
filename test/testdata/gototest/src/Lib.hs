module Lib

    where

someFunc :: IO ()
someFunc = putStrLn "someFunc"

data DataType = DataType Int

dataTypeId :: DataType -> DataType
dataTypeId dataType = dataType

newtype NewType = NewType Int

newTypeId :: NewType -> NewType
newTypeId newType = newType

data Enu = First | Second

enuId :: Enu -> Enu
enuId enu = enu

toNum :: Enu -> Int
toNum First = 1
toNum Second = 2

type MyInt = Int

myIntId :: MyInt -> MyInt
myIntId myInt = myInt

type TypEnu = Enu

typEnuId :: TypEnu -> TypEnu
typEnuId enu = enu

data Parameter a = Parameter a

parameterId :: Parameter a -> Parameter a
parameterId pid = pid