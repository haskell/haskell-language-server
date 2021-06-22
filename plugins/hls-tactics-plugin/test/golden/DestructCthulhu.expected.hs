{-# LANGUAGE GADTs #-}

data FreePro r c a b where
  ID        :: FreePro r c x x
  Comp      :: FreePro r c x y -> FreePro r c y z -> FreePro r c x z
  Copy      :: FreePro r c x (x, x)
  Consume   :: FreePro r c x ()
  Swap      :: FreePro r c (a, b) (b, a)
  SwapE     :: FreePro r c (Either a b) (Either b a)
  Fst       :: FreePro r c (a, b) a
  Snd       :: FreePro r c (a, b) b
  InjectL   :: FreePro r c a (Either a b)
  InjectR   :: FreePro r c b (Either a b)
  Unify     :: FreePro r c (Either a a) a
  First     :: FreePro r c a b -> FreePro r c (a, m) (b, m)
  Second    :: FreePro r c a b -> FreePro r c (m, a) (m, b)
  Alongside :: FreePro r c a b -> FreePro r c a' b' -> FreePro r c (a, a') (b, b')
  Fanout    :: FreePro r c a b -> FreePro r c a b' -> FreePro r c a (b, b')
  Left'     :: FreePro r c a b -> FreePro r c (Either a x) (Either b x)
  Right'    :: FreePro r c a b -> FreePro r c (Either x a) (Either x b)
  EitherOf  :: FreePro r c a b -> FreePro r c a' b' -> FreePro r c (Either a a') (Either b b')
  Fanin     :: FreePro r c a b -> FreePro r c a' b -> FreePro r c (Either a a') b
  LiftC     :: c a b -> FreePro r c a b
  Zero      :: FreePro r c x y
  Plus      :: FreePro r c x y -> FreePro r c x y -> FreePro r c x y
  Unleft    :: FreePro r c (Either a d) (Either b d) -> FreePro r c a b
  Unright   :: FreePro r c (Either d a) (Either d b) -> FreePro r c a b


cthulhu :: FreePro r c a b -> FreePro r c a b
cthulhu ID = _
cthulhu (Comp fp' fp_rcyb) = _
cthulhu Copy = _
cthulhu Consume = _
cthulhu Swap = _
cthulhu SwapE = _
cthulhu Fst = _
cthulhu Snd = _
cthulhu InjectL = _
cthulhu InjectR = _
cthulhu Unify = _
cthulhu (First fp') = _
cthulhu (Second fp') = _
cthulhu (Alongside fp' fp_rca'b') = _
cthulhu (Fanout fp' fp_rcab') = _
cthulhu (Left' fp') = _
cthulhu (Right' fp') = _
cthulhu (EitherOf fp' fp_rca'b') = _
cthulhu (Fanin fp' fp_rca'b) = _
cthulhu (LiftC cab) = _
cthulhu Zero = _
cthulhu (Plus fp' fp_rcab) = _
cthulhu (Unleft fp') = _
cthulhu (Unright fp') = _
