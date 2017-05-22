{-
(c) The University of Glasgow 2006
(c) The GRASP/AQUA Project, Glasgow University, 1992-1998
-}

module TcInstDcls ( tcInstDecls1 ) where

import HsSyn
import TcRnTypes
import TcEnv( InstInfo )
import TcDeriv
import HsExtension ( GHCR )

-- We need this because of the mutual recursion
-- between TcTyClsDecls and TcInstDcls
tcInstDecls1 :: [LInstDecl GHCR] -> TcM (TcGblEnv, [InstInfo GHCR], [DerivInfo])
