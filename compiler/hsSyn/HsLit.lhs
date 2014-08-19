%
% (c) The University of Glasgow 2006
% (c) The GRASP/AQUA Project, Glasgow University, 1992-1998
%
\section[HsLit]{Abstract syntax: source-language literals}

\begin{code}
{-# OPTIONS_GHC -fno-warn-tabs #-}
-- The above warning supression flag is a temporary kludge.
-- While working on this module you are encouraged to remove it and
-- detab the module (please do the detabbing in a separate patch). See
--     http://ghc.haskell.org/trac/ghc/wiki/Commentary/CodingStyle#TabsvsSpaces
-- for details

{-# LANGUAGE CPP, DeriveDataTypeable #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE UndecidableInstances #-} -- Note [pass sensitive types]

module HsLit where

#include "HsVersions.h"

import {-# SOURCE #-} HsExpr( SyntaxExpr, pprExpr )
import BasicTypes ( FractionalLit(..) )
import Type       ( Type )
import Outputable
import FastString
import Name
import NameSet
import RdrName
import Var

import Data.ByteString (ByteString)
import Data.Data hiding ( Fixity )
import BasicTypes       ( Fixity )
\end{code}


%************************************************************************
%*                                                                      *
\subsection{Annotating the syntax}
%*                                                                      *
%************************************************************************

\begin{code}

-- | used as place holder in PostTc and PostRn values
data PlaceHolder = PlaceHolder
  deriving (Data,Typeable)

-- | Types that are not defined until after type checking
type family PostTc it ty :: * -- Note [pass sensitive types]
type instance PostTc Id      ty = ty
type instance PostTc Name    ty = PlaceHolder
type instance PostTc RdrName ty = PlaceHolder

-- | Types that are not defined until after renaming
type family PostRn id ty :: * -- Note [pass sensitive types]
type instance PostRn Id      ty = ty
type instance PostRn Name    ty = ty
type instance PostRn RdrName ty = PlaceHolder

placeHolderKind :: PlaceHolder
placeHolderKind = PlaceHolder

placeHolderFixity :: PlaceHolder
placeHolderFixity = PlaceHolder

placeHolderType :: PlaceHolder
placeHolderType = PlaceHolder

placeHolderTypeTc :: Type
placeHolderTypeTc = panic "Evaluated the place holder for a PostTcType"

placeHolderNames :: PlaceHolder
placeHolderNames = PlaceHolder

placeHolderNamesTc :: NameSet
placeHolderNamesTc = emptyNameSet
\end{code}

Note [pass sensitive types]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Since the same AST types are re-used through parsing,renaming and type
checking there are naturally some places in the AST that do not have
any meaningful value prior to the pass they are assigned a value.

Historically these have been filled in with place holder values of the form

  panic "error message"

This has meant the AST is difficult to traverse using standed generic
programming techniques. The problem is addressed by introducing
pass-specific data types, implemented as a pair of open type families,
one for PostTc and one for PostRn. These are then explicitly populated
with a PlaceHolder value when they do not yet have meaning.

Since the required bootstrap compiler at this stage does not have
closed type families, an open type family had to be used, which
unfortunately forces the requirement for UndecidableInstances.

In terms of actual usage, we have the following

  PostTc id Kind
  PostTc id Type

  PostRn id Fixity
  PostRn id NameSet

TcId and Var are synonyms for Id


%************************************************************************
%*                                                                      *
\subsection[HsLit]{Literals}
%*                                                                      *
%************************************************************************


\begin{code}
data HsLit
  = HsChar          Char               -- Character
  | HsCharPrim      Char               -- Unboxed character
  | HsString        FastString         -- String
  | HsStringPrim    ByteString         -- Packed bytes
  | HsInt           Integer            -- Genuinely an Int; arises from
                                       --     TcGenDeriv, and from TRANSLATION
  | HsIntPrim       Integer            -- literal Int#
  | HsWordPrim      Integer            -- literal Word#
  | HsInt64Prim     Integer            -- literal Int64#
  | HsWord64Prim    Integer            -- literal Word64#
  | HsInteger       Integer  Type      -- Genuinely an integer; arises only from
                                       --   TRANSLATION (overloaded literals are
                                       --   done with HsOverLit)
  | HsRat           FractionalLit Type -- Genuinely a rational; arises only from
                                       --   TRANSLATION (overloaded literals are
                                       --   done with HsOverLit)
  | HsFloatPrim     FractionalLit      -- Unboxed Float
  | HsDoublePrim    FractionalLit      -- Unboxed Double
  deriving (Data, Typeable)

instance Eq HsLit where
  (HsChar x1)       == (HsChar x2)       = x1==x2
  (HsCharPrim x1)   == (HsCharPrim x2)   = x1==x2
  (HsString x1)     == (HsString x2)     = x1==x2
  (HsStringPrim x1) == (HsStringPrim x2) = x1==x2
  (HsInt x1)        == (HsInt x2)        = x1==x2
  (HsIntPrim x1)    == (HsIntPrim x2)    = x1==x2
  (HsWordPrim x1)   == (HsWordPrim x2)   = x1==x2
  (HsInt64Prim x1)  == (HsInt64Prim x2)  = x1==x2
  (HsWord64Prim x1) == (HsWord64Prim x2) = x1==x2
  (HsInteger x1 _)  == (HsInteger x2 _)  = x1==x2
  (HsRat x1 _)      == (HsRat x2 _)      = x1==x2
  (HsFloatPrim x1)  == (HsFloatPrim x2)  = x1==x2
  (HsDoublePrim x1) == (HsDoublePrim x2) = x1==x2
  _                 == _                 = False


data HsOverLit id       -- An overloaded literal
  = OverLit {
        ol_val :: OverLitVal,
        ol_rebindable :: Bool,          -- Note [ol_rebindable]
        ol_witness :: SyntaxExpr id,    -- Note [Overloaded literal witnesses]
        ol_type :: PostTc id Type }
  deriving (Typeable)
deriving instance (Data id, Data (PostTc id Type), Data (PostRn id NameSet),
                            Data (PostRn id Fixity))
   => Data (HsOverLit id)

data OverLitVal
  = HsIntegral   !Integer       -- Integer-looking literals;
  | HsFractional !FractionalLit -- Frac-looking literals
  | HsIsString   !FastString    -- String-looking literals
 deriving (Data, Typeable)


overLitType :: HsOverLit a -> PostTc a Type
overLitType = ol_type
\end{code}

Note [ol_rebindable]
~~~~~~~~~~~~~~~~~~~~
The ol_rebindable field is True if this literal is actually
using rebindable syntax.  Specifically:

  False iff ol_witness is the standard one
  True  iff ol_witness is non-standard

Equivalently it's True if
  a) RebindableSyntax is on
  b) the witness for fromInteger/fromRational/fromString
     that happens to be in scope isn't the standard one

Note [Overloaded literal witnesses]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
*Before* type checking, the SyntaxExpr in an HsOverLit is the
name of the coercion function, 'fromInteger' or 'fromRational'.
*After* type checking, it is a witness for the literal, such as
        (fromInteger 3) or lit_78
This witness should replace the literal.

This dual role is unusual, because we're replacing 'fromInteger' with
a call to fromInteger.  Reason: it allows commoning up of the fromInteger
calls, which wouldn't be possible if the desguarar made the application.

The PostTcType in each branch records the type the overload literal is
found to have.

\begin{code}
-- Comparison operations are needed when grouping literals
-- for compiling pattern-matching (module MatchLit)
instance Eq (HsOverLit id) where
  (OverLit {ol_val = val1}) == (OverLit {ol_val=val2}) = val1 == val2

instance Eq OverLitVal where
  (HsIntegral i1)   == (HsIntegral i2)   = i1 == i2
  (HsFractional f1) == (HsFractional f2) = f1 == f2
  (HsIsString s1)   == (HsIsString s2)   = s1 == s2
  _                 == _                 = False

instance Ord (HsOverLit id) where
  compare (OverLit {ol_val=val1}) (OverLit {ol_val=val2}) = val1 `compare` val2

instance Ord OverLitVal where
  compare (HsIntegral i1)   (HsIntegral i2)   = i1 `compare` i2
  compare (HsIntegral _)    (HsFractional _)  = LT
  compare (HsIntegral _)    (HsIsString _)    = LT
  compare (HsFractional f1) (HsFractional f2) = f1 `compare` f2
  compare (HsFractional _)  (HsIntegral _)    = GT
  compare (HsFractional _)  (HsIsString _)    = LT
  compare (HsIsString s1)   (HsIsString s2)   = s1 `compare` s2
  compare (HsIsString _)    (HsIntegral _)    = GT
  compare (HsIsString _)    (HsFractional _)  = GT
\end{code}

\begin{code}
instance Outputable HsLit where
        -- Use "show" because it puts in appropriate escapes
    ppr (HsChar c)       = pprHsChar c
    ppr (HsCharPrim c)   = pprHsChar c <> char '#'
    ppr (HsString s)     = pprHsString s
    ppr (HsStringPrim s) = pprHsBytes s <> char '#'
    ppr (HsInt i)        = integer i
    ppr (HsInteger i _)  = integer i
    ppr (HsRat f _)      = ppr f
    ppr (HsFloatPrim f)  = ppr f <> char '#'
    ppr (HsDoublePrim d) = ppr d <> text "##"
    ppr (HsIntPrim i)    = integer i  <> char '#'
    ppr (HsWordPrim w)   = integer w  <> text "##"
    ppr (HsInt64Prim i)  = integer i  <> text "L#"
    ppr (HsWord64Prim w) = integer w  <> text "L##"

-- in debug mode, print the expression that it's resolved to, too
instance OutputableBndr id => Outputable (HsOverLit id) where
  ppr (OverLit {ol_val=val, ol_witness=witness})
        = ppr val <+> (ifPprDebug (parens (pprExpr witness)))

instance Outputable OverLitVal where
  ppr (HsIntegral i)   = integer i
  ppr (HsFractional f) = ppr f
  ppr (HsIsString s)   = pprHsString s
\end{code}
