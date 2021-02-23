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
arbitrary
  = let
      terminal
        = [(Square <$> arbitrary) <*> arbitrary, Circle <$> arbitrary,
           Polygon <$> arbitrary, pure Empty, pure Full]
    in
      sized
        $ (\ n
             -> case n <= 1 of
                  True -> oneof terminal
                  False
                    -> oneof
                         $ ([(Rotate2 <$> arbitrary) <*> scale (subtract 1) arbitrary,
                             Complement <$> scale (subtract 1) arbitrary,
                             (UnionR <$> arbitrary) <*> scale (subtract 1) arbitrary,
                             ((DifferenceR <$> arbitrary) <*> scale (flip div 2) arbitrary)
                               <*> scale (flip div 2) arbitrary,
                             (IntersectR <$> arbitrary) <*> scale (subtract 1) arbitrary,
                             ((Translate <$> arbitrary) <*> arbitrary)
                               <*> scale (subtract 1) arbitrary,
                             ((Scale <$> arbitrary) <*> arbitrary)
                               <*> scale (subtract 1) arbitrary,
                             ((Mirror <$> arbitrary) <*> arbitrary)
                               <*> scale (subtract 1) arbitrary,
                             (Outset <$> arbitrary) <*> scale (subtract 1) arbitrary,
                             (Shell <$> arbitrary) <*> scale (subtract 1) arbitrary,
                             (WithRounding <$> arbitrary) <*> scale (subtract 1) arbitrary]
                              <> terminal))

