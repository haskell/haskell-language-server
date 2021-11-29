module NoDoubleQualify where

import A as AAA

thing = AAA.a
thing2 = (AAA.op)
thing3 = 1 `AAA.op` 2
