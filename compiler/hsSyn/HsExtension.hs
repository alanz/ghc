{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE StandaloneDeriving #-}

module HsExtension where

-- This module captures the type families to precisely identify the extension points for HsSyn

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

data GHC (c :: Pass)
-- deriving instance Data (GHC 'Parsed)
deriving instance Data (GHC 'Renamed) -- AZ:Should not be necessary?
-- deriving instance Data (GHC 'Typechecked)
-- deriving instance Data (GHC 'TemplateHaskell)

data Pass = Parsed | Renamed | Typechecked | TemplateHaskell
         deriving (Data)

-- Type synonyms as a shorthand for tagging
type GHCP  = GHC 'Parsed
type GHCR  = GHC 'Renamed
type GHCT  = GHC 'Typechecked
type GHCTH = GHC 'TemplateHaskell

type family PostTC x ty -- Note [Pass sensitive types]
type instance PostTC GHCP ty = PlaceHolder
type instance PostTC GHCR ty = PlaceHolder
type instance PostTC GHCT ty = ty

-- | Types that are not defined until after renaming
type family PostRN x ty  -- Note [Pass sensitive types]
type instance PostRN GHCP ty = PlaceHolder
type instance PostRN GHCR ty = ty
type instance PostRN GHCT ty = ty

-- Maps the "normal" id type for a given pass
type family IdP p
type instance IdP GHCP = RdrName
type instance IdP GHCR = Name
type instance IdP GHCT = Id

-- AZ: dummy for now, to allow HsTypes to build
-- type instance IdP Name    = Name
-- type instance IdP RdrName = RdrName
-- type instance IdP Var     = Var
-- type instance IdP Id      = Id

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

-- AZ:TODO: kind error trying to use this?
type ClassX c x =
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

-- TODO: Should this be part of DataId?
type DataHsLitX x =
  ( Data x
  -- , ClassX Data x
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

class HasDefault a where
  def :: a

instance HasDefault () where
  def = ()

instance HasDefault SourceText where
  def = NoSourceText

-- type HasDefaultX x = ClassX HasDefault x

-- AZ: Question: do we need this, as only being used for NoSourceText case atm,
-- which can be handled via noSourceText
type HasDefaultX x =
  ( HasDefault (XHsChar x)
  , HasDefault (XHsCharPrim x)
  , HasDefault (XHsString x)
  , HasDefault (XHsStringPrim x)
  , HasDefault (XHsInt x)
  , HasDefault (XHsIntPrim x)
  , HasDefault (XHsWordPrim x)
  , HasDefault (XHsInt64Prim x)
  , HasDefault (XHsWord64Prim x)
  , HasDefault (XHsInteger x)
  , HasDefault (XHsRat x)
  , HasDefault (XHsFloatPrim x)
  , HasDefault (XHsDoublePrim x)
  )

-- ----------------------------------------------------------------------

type DataP p =
  ( Data p
  , DataHsLitX p
  , Data (IdP p)
  , Data (PostTC p Type)
  , Data (PostTC p [Type])
  , Data (PostRN p Bool)
  , Data (PostRN p [Name])
  , Data (PostRN p (Located Name))
  , Data (PostRN p NameSet)
  , Data (PostRN p Fixity)
  , Data (PostRN p (IdP p))
  , Data (PostTC p (IdP p))
  , Data (PostTC p Coercion)
  , Data (PostTC p HsWrapper)
  , Data (PostTC p ConLike)
  , Data (PostTC p [ConLike])
  )


-- |Constraint type to bundle up the requirement for 'OutputableBndr' on both
-- the @id@ and the 'NameOrRdrName' type for it
type OutputableBndrId id =
  -- ( OutputableBndr id
  ( OutputableBndr (NameOrRdrName (IdP id))
  , OutputableBndr (IdP id)
  )


-- Temporary
instance Outputable GHCR where
instance OutputableBndr GHCR where

