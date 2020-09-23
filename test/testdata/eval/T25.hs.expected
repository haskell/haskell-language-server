{-# LANGUAGE DataKinds            #-}
{-# LANGUAGE PolyKinds            #-}
{-# LANGUAGE TypeFamilies         #-}
{-# LANGUAGE TypeOperators        #-}
{-# LANGUAGE UndecidableInstances #-}
module T25 where
import GHC.TypeNats (type (-))
data Proxy a = Stop | Next (Proxy a)

type family LongP n a where
  LongP 0 a = a
  LongP n a = Next (LongP (n - 1) a)

-- >>> :kind (Stop :: Proxy (Proxy (Proxy (Proxy (Proxy (Proxy (Proxy (Proxy (Proxy (Proxy (Proxy (Proxy (Proxy Double)))))))))))))
-- (Stop :: Proxy (Proxy (Proxy (Proxy (Proxy (Proxy (Proxy (Proxy (Proxy (Proxy (Proxy (Proxy (Proxy Double))))))))))))) :: Proxy
--                                                                                                                             (Proxy
--                                                                                                                                (Proxy
--                                                                                                                                   (Proxy
--                                                                                                                                      (Proxy
--                                                                                                                                         (Proxy
--                                                                                                                                            (Proxy
--                                                                                                                                               (Proxy
--                                                                                                                                                  (Proxy
--                                                                                                                                                     (Proxy
--                                                                                                                                                        (Proxy
--                                                                                                                                                           (Proxy
--                                                                                                                                                              (Proxy
--                                                                                                                                                                 Double))))))))))))
