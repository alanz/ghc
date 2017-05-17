{-
(c) The University of Glasgow 2006
(c) The GRASP/AQUA Project, Glasgow University, 1992-1998

\section[HsLit]{Abstract syntax: source-language literals}
-}

{-# LANGUAGE CPP, DeriveDataTypeable #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE UndecidableInstances #-} -- Note [Pass sensitive types]
                                      -- in module PlaceHolder
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE TypeFamilies #-}

module HsLit where

#include "HsVersions.h"

import {-# SOURCE #-} HsExpr( HsExpr, pprExpr )
import BasicTypes ( IntegralLit(..),FractionalLit(..),negateIntegralLit,
                    negateFractionalLit,SourceText(..),pprWithSourceText )
import Type       ( Type )
import Outputable
import FastString
import PlaceHolder ( PostTc,PostRn,DataId,OutputableBndrId )

import Data.ByteString (ByteString)
import Data.Data hiding ( Fixity )

{-
************************************************************************
*                                                                      *
\subsection[HsLit]{Literals}
*                                                                      *
************************************************************************
-}

-- Note [Literal source text] in BasicTypes for SourceText fields in
-- the following
-- | Haskell Literal
data HsLit x
  = HsChar (XHsChar x) {- SourceText -} Char
      -- ^ Character
  | HsCharPrim (XHsCharPrim x) {- SourceText -} Char
      -- ^ Unboxed character
  | HsString (XHsString x) {- SourceText -} FastString
      -- ^ String
  | HsStringPrim (XHsStringPrim x) {- SourceText -} ByteString
      -- ^ Packed bytes
  | HsInt (XHsInt x)  IntegralLit
      -- ^ Genuinely an Int; arises from
      -- @TcGenDeriv@, and from TRANSLATION
  | HsIntPrim (XHsIntPrim x) {- SourceText -} Integer
      -- ^ literal @Int#@
  | HsWordPrim (XHsWordPrim x) {- SourceText -} Integer
      -- ^ literal @Word#@
  | HsInt64Prim (XHsInt64Prim x) {- SourceText -} Integer
      -- ^ literal @Int64#@
  | HsWord64Prim (XHsWord64Prim x) {- SourceText -} Integer
      -- ^ literal @Word64#@
  | HsInteger (XHsInteger x) {- SourceText -} Integer Type
      -- ^ Genuinely an integer; arises only
      -- from TRANSLATION (overloaded
      -- literals are done with HsOverLit)
  | HsRat (XHsRat x)  FractionalLit Type
      -- ^ Genuinely a rational; arises only from
      -- TRANSLATION (overloaded literals are
      -- done with HsOverLit)
  | HsFloatPrim (XHsFloatPrim x) FractionalLit
      -- ^ Unboxed Float
  | HsDoublePrim (XHsDoublePrim x)  FractionalLit
      -- ^ Unboxed Double

deriving instance (DataHsLitX x) => Data (HsLit x)

-- Start of trees that grow extensionality -----------------------------
-- Question: What happens to SourceText? should end up in annotation, I imagine

type family XHsChar x
type family XHsCharPrim x
type family XHsString x
type family XHsStringPrim x
type family XHsInt x
type family XHsIntPrim x
type family XHsWordPrim x
type family XHsInt64Prim x
type family XHsWord64Prim x
type family XHsInteger x
type family XHsRat x
type family XHsFloatPrim x
type family XHsDoublePrim x

type DataHsLitX x =
  ( Data x
  , Data (XHsChar x)
  , Data (XHsCharPrim x)
  , Data (XHsString x)
  , Data (XHsStringPrim x)
  , Data (XHsInt x)
  , Data (XHsIntPrim x)
  , Data (XHsWordPrim x)
  , Data (XHsInt64Prim x)
  , Data (XHsWord64Prim x)
  , Data (XHsInteger x)
  , Data (XHsRat x)
  , Data (XHsFloatPrim x)
  , Data (XHsDoublePrim x)
  )

data GHCX

type instance XHsChar       GHCX = SourceText
type instance XHsCharPrim   GHCX = SourceText
type instance XHsString     GHCX = SourceText
type instance XHsStringPrim GHCX = SourceText
type instance XHsInt        GHCX = ()
type instance XHsIntPrim    GHCX = SourceText
type instance XHsWordPrim   GHCX = SourceText
type instance XHsInt64Prim  GHCX = SourceText
type instance XHsWord64Prim GHCX = SourceText
type instance XHsInteger    GHCX = SourceText
type instance XHsRat        GHCX = ()
type instance XHsFloatPrim  GHCX = ()
type instance XHsDoublePrim GHCX = ()

-- End of trees that grow extensionality -------------------------------

instance Eq (HsLit x) where
  (HsChar _ x1)       == (HsChar _ x2)       = x1==x2
  (HsCharPrim _ x1)   == (HsCharPrim _ x2)   = x1==x2
  (HsString _ x1)     == (HsString _ x2)     = x1==x2
  (HsStringPrim _ x1) == (HsStringPrim _ x2) = x1==x2
  (HsInt _ x1)        == (HsInt _ x2)        = x1==x2
  (HsIntPrim _ x1)    == (HsIntPrim _ x2)    = x1==x2
  (HsWordPrim _ x1)   == (HsWordPrim _ x2)   = x1==x2
  (HsInt64Prim _ x1)  == (HsInt64Prim _ x2)  = x1==x2
  (HsWord64Prim _ x1) == (HsWord64Prim _ x2) = x1==x2
  (HsInteger _ x1 _)  == (HsInteger _ x2 _)  = x1==x2
  (HsRat _ x1 _)      == (HsRat _ x2 _)      = x1==x2
  (HsFloatPrim _ x1)  == (HsFloatPrim _ x2)  = x1==x2
  (HsDoublePrim _ x1) == (HsDoublePrim _ x2) = x1==x2
  _                   == _                   = False

-- | Haskell Overloaded Literal
data HsOverLit id
  = OverLit {
        ol_val :: OverLitVal,
        ol_rebindable :: PostRn id Bool, -- Note [ol_rebindable]
        ol_witness :: HsExpr id,     -- Note [Overloaded literal witnesses]
        ol_type :: PostTc id Type }
deriving instance (DataId id) => Data (HsOverLit id)

-- Note [Literal source text] in BasicTypes for SourceText fields in
-- the following
-- | Overloaded Literal Value
data OverLitVal
  = HsIntegral   !IntegralLit            -- ^ Integer-looking literals;
  | HsFractional !FractionalLit          -- ^ Frac-looking literals
  | HsIsString   !SourceText !FastString -- ^ String-looking literals
  deriving Data

negateOverLitVal :: OverLitVal -> OverLitVal
negateOverLitVal (HsIntegral i) = HsIntegral (negateIntegralLit i)
negateOverLitVal (HsFractional f) = HsFractional (negateFractionalLit f)
negateOverLitVal _ = panic "negateOverLitVal: argument is not a number"

overLitType :: HsOverLit a -> PostTc a Type
overLitType = ol_type

{-
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
*Before* type checking, the HsExpr in an HsOverLit is the
name of the coercion function, 'fromInteger' or 'fromRational'.
*After* type checking, it is a witness for the literal, such as
        (fromInteger 3) or lit_78
This witness should replace the literal.

This dual role is unusual, because we're replacing 'fromInteger' with
a call to fromInteger.  Reason: it allows commoning up of the fromInteger
calls, which wouldn't be possible if the desguarar made the application.

The PostTcType in each branch records the type the overload literal is
found to have.
-}

-- Comparison operations are needed when grouping literals
-- for compiling pattern-matching (module MatchLit)
instance Eq (HsOverLit id) where
  (OverLit {ol_val = val1}) == (OverLit {ol_val=val2}) = val1 == val2

instance Eq OverLitVal where
  (HsIntegral   i1)   == (HsIntegral   i2)   = i1 == i2
  (HsFractional f1)   == (HsFractional f2)   = f1 == f2
  (HsIsString _ s1)   == (HsIsString _ s2)   = s1 == s2
  _                   == _                   = False

instance Ord (HsOverLit id) where
  compare (OverLit {ol_val=val1}) (OverLit {ol_val=val2}) = val1 `compare` val2

instance Ord OverLitVal where
  compare (HsIntegral i1)     (HsIntegral i2)     = i1 `compare` i2
  compare (HsIntegral _)      (HsFractional _)    = LT
  compare (HsIntegral _)      (HsIsString _ _)    = LT
  compare (HsFractional f1)   (HsFractional f2)   = f1 `compare` f2
  compare (HsFractional _)    (HsIntegral   _)    = GT
  compare (HsFractional _)    (HsIsString _ _)    = LT
  compare (HsIsString _ s1)   (HsIsString _ s2)   = s1 `compare` s2
  compare (HsIsString _ _)    (HsIntegral   _)    = GT
  compare (HsIsString _ _)    (HsFractional _)    = GT

-- Instance specific to GHCX, need the SourceText
instance Outputable (HsLit GHCX) where
    ppr (HsChar st c)       = pprWithSourceText st (pprHsChar c)
    ppr (HsCharPrim st c)   = pp_st_suffix st primCharSuffix (pprPrimChar c)
    ppr (HsString st s)     = pprWithSourceText st (pprHsString s)
    ppr (HsStringPrim st s) = pprWithSourceText st (pprHsBytes s)
    ppr (HsInt _ i)         = pprWithSourceText (il_text i) (integer (il_value i))
    ppr (HsInteger st i _)  = pprWithSourceText st (integer i)
    ppr (HsRat _ f _)       = ppr f
    ppr (HsFloatPrim _ f)   = ppr f <> primFloatSuffix
    ppr (HsDoublePrim _ d)  = ppr d <> primDoubleSuffix
    ppr (HsIntPrim st i)    = pprWithSourceText st (pprPrimInt i)
    ppr (HsWordPrim st w)   = pprWithSourceText st (pprPrimWord w)
    ppr (HsInt64Prim st i)  = pp_st_suffix st primInt64Suffix  (pprPrimInt64 i)
    ppr (HsWord64Prim st w) = pp_st_suffix st primWord64Suffix (pprPrimWord64 w)

pp_st_suffix :: SourceText -> SDoc -> SDoc -> SDoc
pp_st_suffix NoSourceText         _ doc = doc
pp_st_suffix (SourceText st) suffix _   = text st <> suffix

-- in debug mode, print the expression that it's resolved to, too
instance (OutputableBndrId id) => Outputable (HsOverLit id) where
  ppr (OverLit {ol_val=val, ol_witness=witness})
        = ppr val <+> (ifPprDebug (parens (pprExpr witness)))

instance Outputable OverLitVal where
  ppr (HsIntegral i)     = pprWithSourceText (il_text i) (integer (il_value i))
  ppr (HsFractional f)   = ppr f
  ppr (HsIsString st s)  = pprWithSourceText st (pprHsString s)

-- | pmPprHsLit pretty prints literals and is used when pretty printing pattern
-- match warnings. All are printed the same (i.e., without hashes if they are
-- primitive and not wrapped in constructors if they are boxed). This happens
-- mainly for too reasons:
--  * We do not want to expose their internal representation
--  * The warnings become too messy
pmPprHsLit :: HsLit GHCX -> SDoc
pmPprHsLit (HsChar _ c)       = pprHsChar c
pmPprHsLit (HsCharPrim _ c)   = pprHsChar c
pmPprHsLit (HsString st s)    = pprWithSourceText st (pprHsString s)
pmPprHsLit (HsStringPrim _ s) = pprHsBytes s
pmPprHsLit (HsInt _ i)        = integer (il_value i)
pmPprHsLit (HsIntPrim _ i)    = integer i
pmPprHsLit (HsWordPrim _ w)   = integer w
pmPprHsLit (HsInt64Prim _ i)  = integer i
pmPprHsLit (HsWord64Prim _ w) = integer w
pmPprHsLit (HsInteger _ i _)  = integer i
pmPprHsLit (HsRat _ f _)      = ppr f
pmPprHsLit (HsFloatPrim _ f)  = ppr f
pmPprHsLit (HsDoublePrim _ d) = ppr d
