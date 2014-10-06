module AnnotationLet (foo) where

import qualified Data.List as DL

foo = let
        a = 1
        b = 2
      in a + b
