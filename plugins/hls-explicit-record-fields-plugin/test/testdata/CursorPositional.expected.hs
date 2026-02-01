module CursorPositional where
data RecOuter = RecOuter{
  middle :: RecMiddle
}

data RecMiddle = RecMiddle {
  inner :: RecInner
}

data RecInner = RecInner{
  foo   :: Char
  , bar :: Int
}

ex :: RecOuter
ex = RecOuter (RecMiddle (RecInner { foo = 'c', bar = 42 }))
