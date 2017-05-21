module Match where
import TcType   ( Type )
import DsMonad  ( DsM, EquationInfo, MatchResult )
import CoreSyn  ( CoreExpr )
import HsSyn    ( LPat, HsMatchContext, MatchGroup, LHsExpr )
import Name     ( Name )
import HsExtension ( IdP, GHCR, GHCT )

match   :: [IdP GHCT]
        -> Type
        -> [EquationInfo]
        -> DsM MatchResult

matchWrapper
        :: HsMatchContext (IdP GHCR)
        -> Maybe (LHsExpr GHCT)
        -> MatchGroup GHCT (LHsExpr GHCT)
        -> DsM ([IdP GHCT], CoreExpr)

matchSimply
        :: CoreExpr
        -> HsMatchContext (IdP GHCR)
        -> LPat GHCT
        -> CoreExpr
        -> CoreExpr
        -> DsM CoreExpr

matchSinglePat
        :: CoreExpr
        -> HsMatchContext (IdP GHCR)
        -> LPat GHCT
        -> Type
        -> MatchResult
        -> DsM MatchResult
