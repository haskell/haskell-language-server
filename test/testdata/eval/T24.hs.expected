{-# LANGUAGE DataKinds            #-}
{-# LANGUAGE PolyKinds            #-}
{-# LANGUAGE TypeFamilies         #-}
{-# LANGUAGE TypeOperators        #-}
{-# LANGUAGE UndecidableInstances #-}
module T24 where
import GHC.TypeNats (type (-))
data Proxy a = Stop | Next (Proxy a)

type family LongP n a where
  LongP 0 a = a
  LongP n a = Next (LongP (n - 1) a)

-- >>> :kind! ((LongP 10 Stop) :: Proxy (Proxy (Proxy (Proxy (Proxy (Proxy (Proxy (Proxy (Proxy (Proxy (Proxy (Proxy (Proxy Double)))))))))))))
-- ((LongP 10 Stop) :: Proxy (Proxy (Proxy (Proxy (Proxy (Proxy (Proxy (Proxy (Proxy (Proxy (Proxy (Proxy (Proxy Double))))))))))))) :: Proxy
--                                                                                                                                        (Proxy
--                                                                                                                                           (Proxy
--                                                                                                                                              (Proxy
--                                                                                                                                                 (Proxy
--                                                                                                                                                    (Proxy
--                                                                                                                                                       (Proxy
--                                                                                                                                                          (Proxy
--                                                                                                                                                             (Proxy
--                                                                                                                                                                (Proxy
--                                                                                                                                                                   (Proxy
--                                                                                                                                                                      (Proxy
--                                                                                                                                                                         (Proxy
--                                                                                                                                                                            Double))))))))))))
-- = 'Next
--     ('Next
--        ('Next
--           ('Next ('Next ('Next ('Next ('Next ('Next ('Next 'Stop)))))))))
