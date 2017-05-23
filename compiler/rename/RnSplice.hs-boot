module RnSplice where

import HsSyn
import TcRnMonad
import NameSet
import Kind


rnSpliceType :: HsSplice GHCP   -> PostTc GHCR Kind
             -> RnM (HsType GHCR, FreeVars)
rnSplicePat  :: HsSplice GHCP   -> RnM ( Either (Pat GHCP) (Pat GHCR)
                                          , FreeVars )
rnSpliceDecl :: SpliceDecl GHCP -> RnM (SpliceDecl GHCR, FreeVars)

rnTopSpliceDecls :: HsSplice GHCP -> RnM ([LHsDecl GHCP], FreeVars)
