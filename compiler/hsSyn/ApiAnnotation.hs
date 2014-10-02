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
    -- ApiAnn(..),

    getAnnotation,

    AnnHsLet(..),AnnHsDo(..)

    ) where

import Data.Data
import Data.Dynamic
import SrcLoc

import qualified Data.Map as Map

data ApiAnnKey = AK SrcSpan TypeRep
                  deriving (Eq,Ord,Show)

mkApiAnnKey :: (Typeable a) => SrcSpan -> a -> ApiAnnKey
mkApiAnnKey l a = AK l (typeOf (Just a))

data AnnHsLet = AnnHsLet { ahslet_let, ahslet_in ::  SrcSpan }
            deriving (Eq,Data,Typeable,Show)

data AnnHsDo =  AnnHsDo { ahsdo_do :: SrcSpan }
            deriving (Eq,Data,Typeable,Show)


-- ---------------------------------------------------------------------


getAnnotation :: (Typeable a)
              => Map.Map ApiAnnKey Dynamic -> SrcSpan -> Maybe a
getAnnotation anns span = res
  where res = case  Map.lookup (AK span (typeOf res)) anns of
                       Nothing -> Nothing
                       Just d -> fromDynamic d
