module TPrettyPrint where
import Control.Monad (forM)

pretty :: t a -> (a -> m b) -> m (t b)
pretty = forM