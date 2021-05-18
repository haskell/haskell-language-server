data Synthesized b a = Synthesized
  { syn_trace :: b
  , syn_val   :: a
  }
  deriving (Eq, Show)


mapTrace :: (b -> b) -> Synthesized b a -> Synthesized b a
mapTrace fbb (Synthesized b a)
  = Synthesized {syn_trace = fbb b, syn_val = a}

