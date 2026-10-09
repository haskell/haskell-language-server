module C(module C) where
import A
import B
cux = foo `seq` qux
