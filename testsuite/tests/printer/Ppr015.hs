module ExprPragmas where

a = {-# SCC "name"   #-}  0x5

b = {-# SCC foo   #-} 006

-- Should it be possible to ppr the following annotation?
c = {-# GENERATED "foobar" 1 : 2  -  3 :   4 #-} 0.00
