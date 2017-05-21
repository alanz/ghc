module TcExpr where
import HsSyn    ( HsExpr, LHsExpr, SyntaxExpr )
import Name     ( Name )
import TcType   ( TcRhoType, TcSigmaType, SyntaxOpType, ExpType, ExpRhoType )
import TcRnTypes( TcM, TcId, CtOrigin )
import HsExtension ( IdP, GHCR, GHCTc )

tcPolyExpr ::
          LHsExpr GHCR
       -> TcSigmaType
       -> TcM (LHsExpr GHCTc)

tcMonoExpr, tcMonoExprNC ::
          LHsExpr GHCR
       -> ExpRhoType
       -> TcM (LHsExpr GHCTc)

tcInferSigma, tcInferSigmaNC ::
          LHsExpr GHCR
       -> TcM (LHsExpr GHCTc, TcSigmaType)

tcInferRho ::
          LHsExpr GHCR
       -> TcM (LHsExpr GHCTc, TcRhoType)

tcSyntaxOp :: CtOrigin
           -> SyntaxExpr GHCR
           -> [SyntaxOpType]           -- ^ shape of syntax operator arguments
           -> ExpType                  -- ^ overall result type
           -> ([TcSigmaType] -> TcM a) -- ^ Type check any arguments
           -> TcM (a, SyntaxExpr GHCTc)

tcSyntaxOpGen :: CtOrigin
              -> SyntaxExpr GHCR
              -> [SyntaxOpType]
              -> SyntaxOpType
              -> ([TcSigmaType] -> TcM a)
              -> TcM (a, SyntaxExpr GHCTc)


tcCheckId :: IdP GHCR -> ExpRhoType -> TcM (HsExpr GHCTc)
