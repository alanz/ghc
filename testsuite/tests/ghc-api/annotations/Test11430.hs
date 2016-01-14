module Test11430 where


foo a b = a + b

infixl 0x09 foo

{-# SPECIALISE [~ 001] x ::
        Integer -> Integer -> Integer,
        Integer -> Int -> Integer,
        Int -> Int -> Int #-}
{-# INLINABLE [1] x #-}
x :: (Num a, Integral b) => a -> b -> a
x = undefined

{-# SPECIALISE INLINE [0x999] y ::
        Integer -> Integer -> Integer,
        Integer -> Int -> Integer,
        Int -> Int -> Int #-}
{-# INLINABLE [1] y #-}
y :: (Num a, Integral b) => a -> b -> a
y = undefined

c = {-# GENERATED "foob\x61r" 0x1 : 0x2  -  0x3 :   0x4 #-} 0.00
