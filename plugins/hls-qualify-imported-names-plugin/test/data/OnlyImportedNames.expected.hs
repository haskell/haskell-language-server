module OnlyImportedNames where

import A

thing1 a = a

thing2 b = b

thing3 = f1 A.a A.c

thing4 = f2 A.b A.d

f1 a = a

f2 c b = let { d = "k"; e = A.a } in c d ++ c b

