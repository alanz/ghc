{-# LANGUAGE CPP, KindSignatures #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE UndecidableInstances #-} -- Note [Pass sensitive types]
                                      -- in module PlaceHolder
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE RoleAnnotations #-}

module HsExpr where

import SrcLoc     ( Located )
import Outputable ( SDoc, Outputable )
import {-# SOURCE #-} HsPat  ( LPat )
import BasicTypes ( SpliceExplicitFlag(..))
import PlaceHolder ( DataId, OutputableBndrId )
import Data.Data hiding ( Fixity )
import HsExtension

type role HsExpr nominal nominal
type role HsCmd nominal nominal
type role MatchGroup nominal representational
type role GRHSs nominal representational
type role HsSplice nominal nominal
type role SyntaxExpr nominal nominal
data HsExpr (x :: *) (i :: *)
data HsCmd  (x :: *) (i :: *)
data HsSplice (x :: *) (i :: *)
data MatchGroup (a :: *) (body :: *)
data GRHSs (a :: *) (body :: *)
data SyntaxExpr (x :: *) (i :: *)

instance (DataHsLitX x, DataId id) => Data (HsSplice x id)
instance (DataId id) => Data (HsExpr x id)
instance (DataId id) => Data (HsCmd x id)
instance (Data body,DataId id) => Data (MatchGroup id body)
instance (Data body,DataId id) => Data (GRHSs id body)
instance (DataHsLitX x, DataId id) => Data (SyntaxExpr x id)

instance (OutputableBndrId id) => Outputable (HsExpr x id)
instance (OutputableBndrId id) => Outputable (HsCmd x id)

type LHsExpr x a = Located (HsExpr x a)

pprLExpr :: (OutputableBndrId id) => LHsExpr x id -> SDoc

pprExpr :: (OutputableBndrId id) => HsExpr x id -> SDoc

pprSplice :: (OutputableBndrId id) => HsSplice x id -> SDoc

pprSpliceDecl ::  (OutputableBndrId id)
          => HsSplice x id -> SpliceExplicitFlag -> SDoc

pprPatBind :: (OutputableBndrId bndr,
               OutputableBndrId id,
               Outputable body)
           => LPat x bndr -> GRHSs id body -> SDoc

pprFunBind :: (OutputableBndrId idR, Outputable body)
           => MatchGroup idR body -> SDoc
