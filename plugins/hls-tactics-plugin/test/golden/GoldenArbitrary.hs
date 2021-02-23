-- Emulate a quickcheck import; deriveArbitrary works on any type with the
-- right name and kind
data Gen a

data Obj
  = Square Int Int
  | Circle Int
  | Polygon [(Int, Int)]
  | Rotate2 Double Obj
  | Empty
  | Full
  | Complement Obj
  | UnionR Double [Obj]
  | DifferenceR Double Obj [Obj]
  | IntersectR Double [Obj]
  | Translate Double Double Obj
  | Scale Double Double Obj
  | Mirror Double Double Obj
  | Outset Double Obj
  | Shell Double Obj
  | WithRounding Double Obj


arbitrary :: Gen Obj
arbitrary = _

