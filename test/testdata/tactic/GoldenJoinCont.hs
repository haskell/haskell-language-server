type Cont r a = ((a -> r) -> r)

joinCont :: Show a => (a -> c) -> (b -> c) -> Either a b -> (c -> d) -> d
joinCont = _
