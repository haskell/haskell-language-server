data Semi = Semi [String] Int

instance Semigroup Semi where
  (Semi ss n) <> (Semi strs i) = Semi (ss <> strs) _w0

