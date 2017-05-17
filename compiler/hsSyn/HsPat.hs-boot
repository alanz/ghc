{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE UndecidableInstances #-} -- Note [Pass sensitive types]
                                      -- in module PlaceHolder
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE RoleAnnotations #-}
{-# LANGUAGE FlexibleInstances #-}

module HsPat where
import SrcLoc( Located )

import Data.Data hiding (Fixity)
import Outputable
import PlaceHolder      ( DataId, OutputableBndrId )
import {-# SOURCE #-} HsLit ( HsLit )
import HsExtension

type role Pat nominal nominal
data Pat (x :: *) (i :: *)
type LPat x i = Located (Pat x i)

instance (DataHsLitX x, DataId id) => Data (Pat x id)
instance (OutputableBndrId name, Outputable (HsLit x))
           => Outputable (Pat x name)
