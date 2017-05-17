{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE RoleAnnotations #-}
module HsLit where

type role HsLit nominal
data HsLit (x :: *)
