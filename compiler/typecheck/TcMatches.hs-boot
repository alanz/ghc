module TcMatches where
import Name
import HsSyn    ( GRHSs, MatchGroup, LHsExpr )
import TcEvidence( HsWrapper )
import TcType   ( ExpRhoType, TcRhoType )
import TcRnTypes( TcM )
import SrcLoc   ( Located )
import HsExtension ( GhcRn, GhcTcId )

tcGRHSsPat    :: GRHSs GhcRn (LHsExpr GhcRn)
              -> TcRhoType
              -> TcM (GRHSs GhcTcId (LHsExpr GhcTcId))

tcMatchesFun :: Located Name
             -> MatchGroup GhcRn (LHsExpr GhcRn)
             -> ExpRhoType
             -> TcM (HsWrapper, MatchGroup GhcTcId (LHsExpr GhcTcId))
