module OnlyImportedNames where

import A

thing1 a = a

thing2 b = b

thing3 = f1 a c

thing4 = f2 b d

f1 a = a

f2 c b = let { d = "k"; e = a } in c d ++ c b

