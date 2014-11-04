{-# LANGUAGE TupleSections #-}
module AnnotationTuple (foo) where

{
import qualified Data.List as DL
;
foo = let
        a = 1
        b = 2
      in a + b

;
bar = print $ map (1, "hello"   , 6.5,, [5, 5, 6, 7]) [Just (), Nothing]
;
baz = (1, "hello", 6.5,,) 'a' (Just ())
}
