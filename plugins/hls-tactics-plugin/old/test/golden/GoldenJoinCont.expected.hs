type Cont r a = ((a -> r) -> r)

joinCont :: Cont r (Cont r a) -> Cont r a
joinCont f far = f (\ g -> g far)
