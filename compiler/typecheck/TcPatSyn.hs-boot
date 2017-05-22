module TcPatSyn where

import HsSyn     ( PatSynBind, LHsBinds )
import TcRnTypes ( TcM, TcPatSynInfo )
import TcRnMonad ( TcGblEnv)
import Outputable ( Outputable )
import HsExtension ( GHCR, GHCT )

tcInferPatSynDecl :: PatSynBind GHCR GHCR
                  -> TcM (LHsBinds GHCT, TcGblEnv)

tcCheckPatSynDecl :: PatSynBind GHCR GHCR
                  -> TcPatSynInfo
                  -> TcM (LHsBinds GHCT, TcGblEnv)

tcPatSynBuilderBind :: PatSynBind GHCR GHCR -> TcM (LHsBinds GHCT)

nonBidirectionalErr :: Outputable name => name -> TcM a
