module DsExpr where
import HsSyn       ( HsExpr, LHsExpr, LHsLocalBinds, SyntaxExpr )
import Var         ( Id )
import DsMonad     ( DsM )
import CoreSyn     ( CoreExpr )
import HsExtension ( GHCT)

dsExpr  :: HsExpr  GHCT -> DsM CoreExpr
dsLExpr, dsLExprNoLP :: LHsExpr GHCT -> DsM CoreExpr
dsSyntaxExpr :: SyntaxExpr GHCT -> [CoreExpr] -> DsM CoreExpr
dsLocalBinds :: LHsLocalBinds GHCT -> CoreExpr -> DsM CoreExpr
