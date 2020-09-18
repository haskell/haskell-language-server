module A (template) where

import Language.Haskell.TH

template :: DecsQ
template = (\consA -> [DataD [] (mkName "A") [] Nothing [NormalC consA []] []]) <$> newName "A"
