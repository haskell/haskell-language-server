fmapList :: (a -> b) -> [a] -> [b]
fmapList _ [] = []
fmapList fab (a : as') = fab a : fmapList fab as'
