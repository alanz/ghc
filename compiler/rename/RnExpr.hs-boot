module RnExpr where
import HsSyn
import NameSet     ( FreeVars )
import TcRnTypes
import SrcLoc      ( Located )
import Outputable  ( Outputable )
import HsExtension (IdP, GHCP, GHCR )

rnLExpr :: LHsExpr GHCP
        -> RnM (LHsExpr GHCR, FreeVars)

rnStmts :: --forall thing body.
           Outputable (body GHCP) => HsStmtContext (IdP GHCR)
        -> (Located (body GHCP) -> RnM (Located (body GHCR), FreeVars))
        -> [LStmt GHCP (Located (body GHCP))]
        -> ([IdP GHCR] -> RnM (thing, FreeVars))
        -> RnM (([LStmt GHCR (Located (body GHCR))], thing), FreeVars)
