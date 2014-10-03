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
    ApiAnns,

    getAnnotation,

    AnnHsModule(..),AnnLIEs(..),AnnHsCommaList(..),
    AnnHsLet(..),AnnHsDo(..)

    ) where

import Data.Data
import Data.Dynamic
import SrcLoc

import qualified Data.Map as Map

{-

Theory of operation.

The HsSyn AST does not capture the locations of certain keywords and
punctuation, such as 'let', 'in', 'do', etc.

These locations are required by any tools wanting to parse a haskell
file, transform the AST in some way, and then regenerate the original
layout for the unchaged parts.

Rather than pollute the AST with information irrelevant to the actual
compilation process, these locations are captured in the lexer /
parser and returned as a separate structure ApiAnns structure in the
ParsedSource.

Each AST element that needs an annotation has an entry in this Map,
which as a key comprising the SrcSpan of the original element and the
TyeRep of the stored annotation, if it were wrapped in a Just.

This allows code using the annotation to access this as follows

    processHsLet :: ApiAnns -> LHsExpr -> CustomReturnType
    processHsLet anns (L l (HsExpr localBinds expr)) = r
      where
        Just ann = getAnnotation anns l :: Maybe AnnHsLet
        ...


-}

type ApiAnns = Map.Map ApiAnnKey Dynamic

data ApiAnnKey = AK SrcSpan TypeRep
                  deriving (Eq,Ord,Show)

mkApiAnnKey :: (Typeable a) => SrcSpan -> a -> ApiAnnKey
mkApiAnnKey l a = AK l (typeOf (Just a))

-- ---------------------------------------------------------------------

-- | Retrieve an annotation based on the SrcSpan of the annotated AST
-- element, and the known type of the annotation.
getAnnotation :: (Typeable a)
              => Map.Map ApiAnnKey Dynamic -> SrcSpan -> Maybe a
getAnnotation anns span = res
  where res = case  Map.lookup (AK span (typeOf res)) anns of
                       Nothing -> Nothing
                       Just d -> fromDynamic d

-- --------------------------------------------------------------------

-- Each annotation data type is named with an "Ann" prefix on the name
-- of the type being annotated.

-- This annotation will only be present if there is a module header
data AnnHsModule = AnnHsModule { ahsmodule_module, ahsmodule_where :: SrcSpan }
            deriving (Eq,Data,Typeable,Show)

data AnnLIEs = AnnLIEs { alie_oparen, alie_cparen :: SrcSpan }
            deriving (Eq,Data,Typeable,Show)

data AnnHsCommaList = AnnHsCommaList { ahscommalist_comma :: SrcSpan }
            deriving (Eq,Data,Typeable,Show)

data AnnHsLet = AnnHsLet { ahslet_let, ahslet_in ::  SrcSpan }
            deriving (Eq,Data,Typeable,Show)

data AnnHsDo =  AnnHsDo { ahsdo_do :: SrcSpan }
            deriving (Eq,Data,Typeable,Show)


