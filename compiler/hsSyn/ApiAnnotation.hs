{-
%
% (c) The University of Glasgow 2006
% (c) The GRASP/AQUA Project, Glasgow University, 1992-1998
%

ApiAnnotation: types and functions to add a companion annotation
structure to the parsed source for use in source to source conversions
via the GHC API.

See https://ghc.haskell.org/trac/ghc/wiki/GhcAstAnnotations

-}
{-# LANGUAGE DeriveDataTypeable #-}
module ApiAnnotation (
        ApiAnnKey(..), mkApiAnnKey,
        ApiAnn(..)
    ) where

import Data.Data
import SrcLoc

data ApiAnnKey = AK SrcSpan Int
                  deriving (Eq,Ord,Show)

mkApiAnnKey :: (Typeable e) => (Located e) -> ApiAnnKey
mkApiAnnKey (L l _) = AK l 3

data ApiAnn = AnnHsLet    SrcSpan -- of the word "let"
                          SrcSpan -- of the word "in"

            | AnnHsDo     SrcSpan -- of the word "do"
            deriving (Eq,Data,Typeable,Show)


