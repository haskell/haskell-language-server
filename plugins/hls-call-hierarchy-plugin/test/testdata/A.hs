module A where
import B
import C

foo1 = B.a + C.a
foo2 = foo1 + B.a + C.b
foo3 = foo1 + foo2 + C.c

bar x = case x of
  A -> B.a + B.b + B.c + foo1
  B -> C.a + C.b + C.c + foo2
