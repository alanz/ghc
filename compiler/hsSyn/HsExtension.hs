{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE StandaloneDeriving #-}

module HsExtension where

-- This module captures the type families to precisely identify the extension points for HsSyn

import GHC.Exts (Constraint)
import Data.Data hiding ( Fixity )
import PlaceHolder
import BasicTypes
import ConLike
import NameSet
import Name
import RdrName
import Var
import Type       ( Type )
import Outputable
import SrcLoc (Located)
import Coercion
import TcEvidence

{-
Note [Trees that grow]
~~~~~~~~~~~~~~~~~~~~~~

To be completed

-}
-- | Used as a data type index for the hsSyn AST

{-
The problem with the

    data GHC (c :: Pass)
    data Pass = Parsed | Renamed | Typechecked | TemplateHaskell
             deriving (Data)

approach is that we need to hardcode the pass in certain cases, such as in
ValBindsOut, and to have a Data instance for HsValBindsLR we need a Data
instance for 'GHC c'.

But we seem to only be able to have one of these at a time.

data GHC (c :: Pass)
-- deriving instance Data (GHC 'Parsed)
-- deriving instance Data (GHC 'Renamed) -- AZ:Should not be necessary?
deriving instance Data (GHC 'Typechecked)
-- deriving instance Data (GHC 'TemplateHaskell)

data Pass = Parsed | Renamed | Typechecked | TemplateHaskell
         deriving (Data)

-- Type synonyms as a shorthand for tagging
type GHCP  = GHC 'Parsed
type GHCR  = GHC 'Renamed
type GHCT  = GHC 'Typechecked
type GHCTH = GHC 'TemplateHaskell
-}

-- Running with these until the above issue is clarified
data GHCP
data GHCR
data GHCT
data GHCTH

type GHCTc = GHCT -- TcId
type GHCTV = GHCT -- Var

deriving instance Data GHCP
deriving instance Data GHCR
deriving instance Data GHCT
deriving instance Data GHCTH

deriving instance Eq GHCP
deriving instance Eq GHCR
deriving instance Eq GHCT
deriving instance Eq GHCTH

type family PostTc x ty -- Note [Pass sensitive types]
type instance PostTc GHCP ty = PlaceHolder
type instance PostTc GHCR ty = PlaceHolder
type instance PostTc GHCT ty = ty

-- | Types that are not defined until after renaming
type family PostRn x ty  -- Note [Pass sensitive types]
type instance PostRn GHCP ty = PlaceHolder
type instance PostRn GHCR ty = ty
type instance PostRn GHCT ty = ty

-- Maps the "normal" id type for a given pass
type family IdP p
type instance IdP GHCP = RdrName
type instance IdP GHCR = Name
type instance IdP GHCT = Id


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

type ClassX (c :: * -> Constraint) (x :: *) =
  ( c (XHsChar x)
  , c (XHsCharPrim x)
  , c (XHsString x)
  , c (XHsStringPrim x)
  , c (XHsInt x)
  , c (XHsIntPrim x)
  , c (XHsWordPrim x)
  , c (XHsInt64Prim x)
  , c (XHsWord64Prim x)
  , c (XHsInteger x)
  , c (XHsRat x)
  , c (XHsFloatPrim x)
  , c (XHsDoublePrim x)
  )



type instance XHsChar       GHCP = SourceText
type instance XHsCharPrim   GHCP = SourceText
type instance XHsString     GHCP = SourceText
type instance XHsStringPrim GHCP = SourceText
type instance XHsInt        GHCP = ()
type instance XHsIntPrim    GHCP = SourceText
type instance XHsWordPrim   GHCP = SourceText
type instance XHsInt64Prim  GHCP = SourceText
type instance XHsWord64Prim GHCP = SourceText
type instance XHsInteger    GHCP = SourceText
type instance XHsRat        GHCP = ()
type instance XHsFloatPrim  GHCP = ()
type instance XHsDoublePrim GHCP = ()

type instance XHsChar       GHCR = SourceText
type instance XHsCharPrim   GHCR = SourceText
type instance XHsString     GHCR = SourceText
type instance XHsStringPrim GHCR = SourceText
type instance XHsInt        GHCR = ()
type instance XHsIntPrim    GHCR = SourceText
type instance XHsWordPrim   GHCR = SourceText
type instance XHsInt64Prim  GHCR = SourceText
type instance XHsWord64Prim GHCR = SourceText
type instance XHsInteger    GHCR = SourceText
type instance XHsRat        GHCR = ()
type instance XHsFloatPrim  GHCR = ()
type instance XHsDoublePrim GHCR = ()

type instance XHsChar       GHCT = SourceText
type instance XHsCharPrim   GHCT = SourceText
type instance XHsString     GHCT = SourceText
type instance XHsStringPrim GHCT = SourceText
type instance XHsInt        GHCT = ()
type instance XHsIntPrim    GHCT = SourceText
type instance XHsWordPrim   GHCT = SourceText
type instance XHsInt64Prim  GHCT = SourceText
type instance XHsWord64Prim GHCT = SourceText
type instance XHsInteger    GHCT = SourceText
type instance XHsRat        GHCT = ()
type instance XHsFloatPrim  GHCT = ()
type instance XHsDoublePrim GHCT = ()

class HasSourceText a where
  -- Provide setters to mimic existing constructors
  noSourceText  :: a
  sourceText    :: String -> a

  setSourceText :: SourceText -> a
  getSourceText :: a -> SourceText

type SourceTextX x =
  ( HasSourceText (XHsChar x)
  , HasSourceText (XHsCharPrim x)
  , HasSourceText (XHsString x)
  , HasSourceText (XHsStringPrim x)
  , HasSourceText (XHsIntPrim x)
  , HasSourceText (XHsWordPrim x)
  , HasSourceText (XHsInt64Prim x)
  , HasSourceText (XHsWord64Prim x)
  , HasSourceText (XHsInteger x)
  )

instance HasSourceText SourceText where
  noSourceText    = NoSourceText
  sourceText s    = SourceText s

  setSourceText s = s
  getSourceText a = a


-- ----------------------------------------------------------------------
-- Defaults for each annotation, used to simplify creation in arbitrary contexts
class HasDefault a where
  def :: a

instance HasDefault () where
  def = ()

instance HasDefault SourceText where
  def = NoSourceText

type HasDefaultX x = ClassX HasDefault x

-- ----------------------------------------------------------------------
-- Conversion of annotations from one type index to another
class Convertable a b  | a -> b where
  convert :: a -> b

-- want to convert from
-- convert :: XHsDoublePrim a -> XHsDoublePrim b

instance Convertable a a where
  convert = id

type ConvertIdX a b =
  (XHsDoublePrim a ~ XHsDoublePrim b,
   XHsFloatPrim a ~ XHsFloatPrim b,
   XHsRat a ~ XHsRat b,
   XHsInteger a ~ XHsInteger b,
   XHsWord64Prim a ~ XHsWord64Prim b,
   XHsInt64Prim a ~ XHsInt64Prim b,
   XHsWordPrim a ~ XHsWordPrim b,
   XHsIntPrim a ~ XHsIntPrim b,
   XHsInt a ~ XHsInt b,
   XHsStringPrim a ~ XHsStringPrim b,
   XHsString a ~ XHsString b,
   XHsCharPrim a ~ XHsCharPrim b,
   XHsChar a ~ XHsChar b)


-- ----------------------------------------------------------------------

type DataId p =
  ( Data p
  , ClassX Data p
  , Data (NameOrRdrName (IdP p))

  , Data (IdP p)
  , Data (PostRn p (IdP p))
  , Data (PostRn p (Located Name))
  , Data (PostRn p Bool)
  , Data (PostRn p Fixity)
  , Data (PostRn p NameSet)
  , Data (PostRn p [Name])

  , Data (PostTc p (IdP p))
  , Data (PostTc p Coercion)
  , Data (PostTc p ConLike)
  , Data (PostTc p HsWrapper)
  , Data (PostTc p Type)
  , Data (PostTc p [ConLike])
  , Data (PostTc p [Type])
  )


-- |Constraint type to bundle up the requirement for 'OutputableBndr' on both
-- the @id@ and the 'NameOrRdrName' type for it
type OutputableBndrId id =
  ( OutputableBndr (NameOrRdrName (IdP id))
  , OutputableBndr (IdP id)
  )


