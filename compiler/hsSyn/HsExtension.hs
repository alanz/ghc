{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeFamilies #-}
module HsExtension where

-- This module captures the type families to precisely identify the extension points for HsSyn

import Data.Data hiding ( Fixity )
import PlaceHolder
import BasicTypes ( SourceText(..) )

{-
Note [Trees that grow]
~~~~~~~~~~~~~~~~~~~~~~

To be completed

-}
-- | Used as a data type index for the ParseSource.
data GHCX -- see Note [Trees that grow] in xxxxx
deriving instance Data GHCX


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

class HasSourceText a where
  -- Provide setters to mimic existing constructors
  noSourceText  :: a
  sourceText    :: String -> a

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
  getSourceText a = a

class HasDefault a where
  def :: a

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

-- End of trees that grow extensionality -------------------------------
