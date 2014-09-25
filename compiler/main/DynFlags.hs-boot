{-# LANGUAGE KindSignatures #-}
module DynFlags where

import Platform

data DynFlags (l :: *)

targetPlatform       :: DynFlags l -> Platform
pprUserLength        :: DynFlags l -> Int
pprCols              :: DynFlags l -> Int
unsafeGlobalDynFlags :: DynFlags l
useUnicode     :: DynFlags l -> Bool
useUnicodeSyntax     :: DynFlags l -> Bool
