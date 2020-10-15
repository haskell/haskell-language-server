eitherFmap :: (a -> b) -> Either e a -> Either e b
eitherFmap fa eab = _

global :: Bool
global = True

foo :: Int
foo  = _

dontSuggestLambdaCase :: Either a b -> Int
dontSuggestLambdaCase = _

data T = T !(Int, Int)

suggestCon :: T
suggestCon = _

data T2 = T21 Int | T22 Int Int

suggestCons :: T2
suggestCons = _

suggestInt :: Int
suggestInt = _
