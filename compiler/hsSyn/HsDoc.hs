{-# LANGUAGE CPP, DeriveDataTypeable #-}

module HsDoc (
  HsDocString(..),
  LHsDocString,
  ppr_mbDoc
  ) where

#include "HsVersions.h"

import Outputable
import SrcLoc
import FastString

import Data.Data

newtype HsDocString = HsDocString FastString
  deriving (Eq, Show, Data, Typeable)

type LHsDocString l = GenLocated l HsDocString

instance Outputable HsDocString where
  ppr (HsDocString fs) = ftext fs

ppr_mbDoc :: (Outputable l) => Maybe (LHsDocString l) -> SDoc
ppr_mbDoc (Just doc) = ppr doc
ppr_mbDoc Nothing    = empty

