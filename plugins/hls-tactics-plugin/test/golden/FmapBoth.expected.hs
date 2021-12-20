fmapBoth :: (Functor f, Functor g) => (a -> b) -> (f a, g a) -> (f b, g b)
fmapBoth fab (fa, ga) = (fmap fab fa, fmap fab ga)

