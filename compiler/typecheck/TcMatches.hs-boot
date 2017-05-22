module TcMatches where
import HsSyn    ( GRHSs, MatchGroup, LHsExpr )
import TcEvidence( HsWrapper )
import TcType   ( ExpRhoType, TcRhoType )
import TcRnTypes( TcM )
import SrcLoc   ( Located )
import HsExtension ( GHCR, GHCTc, IdP )

tcGRHSsPat    :: GRHSs GHCR (LHsExpr GHCR)
              -> TcRhoType
              -> TcM (GRHSs GHCTc (LHsExpr GHCTc))

tcMatchesFun :: Located (IdP GHCR)
             -> MatchGroup GHCR (LHsExpr GHCR)
             -> ExpRhoType
             -> TcM (HsWrapper, MatchGroup GHCTc (LHsExpr GHCTc))
