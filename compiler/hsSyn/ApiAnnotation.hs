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
{-# LANGUAGE ExistentialQuantification #-}
module ApiAnnotation (
    ApiAnnKey(..),
    ApiAnns,

    getAnnotation,
    getAnnotationComments,

    -- * Annotation types
    Ann(..)

    ) where

import Data.Data
import Outputable
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

Each uncaptured token has an entry in this Map, which as a key
comprising the SrcSpan of the original AST element and indicator of
what the original uncaptured token is.

This allows code using the annotation to access this as follows

    processHsLet :: ApiAnns -> LHsExpr -> CustomReturnType
    processHsLet anns (L l (HsExpr localBinds expr)) = r
      where
        Just letPos = getAnnotation anns l AnnLet
        Just inPos  = getAnnotation anns l AnnIn
        ...


-}
-- ---------------------------------------------------------------------

type ApiAnns a = (Map.Map ApiAnnKey SrcSpan, Map.Map SrcSpan [a])

data ApiAnnKey = AK SrcSpan Ann
                  deriving (Eq,Ord,Show)

-- ---------------------------------------------------------------------

-- | Retrieve an annotation based on the @SrcSpan@ of the annotated AST
-- element, and the known type of the annotation.
getAnnotation :: ApiAnns a -> SrcSpan -> Ann -> Maybe SrcSpan
getAnnotation (anns,_) span ann = Map.lookup (AK span ann) anns

-- |Retrieve the comments allocated to the current @SrcSpan@
getAnnotationComments :: ApiAnns a -> SrcSpan -> [a]
getAnnotationComments (_,anns) span =
  case Map.lookup span anns of
    Just cs -> cs
    Nothing -> []

-- --------------------------------------------------------------------

-- | Note: in general the names of these are taken from the
-- corresponding token, unless otherwise noted
data Ann = AnnAs
         | AnnAt
         | AnnBang
         | AnnBy
         | AnnCase
         | AnnClass
         | AnnClose -- ^ } or ] or ) or #) etc
         | AnnColon
         | AnnColon2
         | AnnComma
         | AnnDarrow
         | AnnData
         | AnnDcolon
         | AnnDefault
         | AnnDeriving
         | AnnDo
         | AnnDot
         | AnnDotdot
         | AnnElse
         | AnnEqual
         | AnnExport
         | AnnFamily
         | AnnForall
         | AnnForeign
         | AnnGroup
         | AnnHeader -- ^ for CType
         | AnnHiding
         | AnnIf
         | AnnImport
         | AnnIn
         | AnnInstance
         | AnnLam
         | AnnLarrow
         | AnnLarrowtail
         | AnnLet
         | AnnMdo
         | AnnMinus
         | AnnModule
         | AnnNewtype
         | AnnOf
         | AnnOpen   -- ^ { or [ or ( or (# etc
         | AnnPackageName
         | AnnPattern
         | AnnProc
         | AnnQualified
         | AnnRarrow
         | AnnRarrowtail
         | AnnRec
         | AnnRole
         | AnnSafe
         | AnnSemi
         | AnnThen
         | AnnTilde
         | AnnTildehsh
         | AnnType
         | AnnUsing
         | AnnVal  -- ^ e.g. INTEGER
         | AnnVal2 -- ^ e.g. INTEGER
         | AnnVal3 -- ^ e.g. INTEGER
         | AnnVal4 -- ^ e.g. INTEGER
         | AnnVal5 -- ^ e.g. INTEGER
         | AnnVbar
         | AnnWhere
         | Annlarrowtail
         | Annrarrowtail
            deriving (Eq,Ord,Data,Typeable,Show)

instance Outputable Ann where
  ppr x = text (show x)
