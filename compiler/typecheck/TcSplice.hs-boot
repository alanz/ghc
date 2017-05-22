{-# LANGUAGE CPP #-}
{-# LANGUAGE TypeFamilies #-}

module TcSplice where
import HsSyn    ( HsSplice, HsBracket, HsExpr, LHsExpr )
import HsExpr   ( PendingRnSplice )
import Name     ( Name )
import TcRnTypes( TcM, TcId )
import TcType   ( ExpRhoType )
import Annotations ( Annotation, CoreAnnTarget )
import HsExtension  ( GHCTc, GHCR, GHCP, IdP )

import HsSyn      ( LHsType, LPat, LHsDecl, ThModFinalizers )
import TcRnTypes  ( SpliceType )
import qualified Language.Haskell.TH as TH

tcSpliceExpr :: HsSplice GHCR
             -> ExpRhoType
             -> TcM (HsExpr GHCTc)

tcUntypedBracket :: HsBracket GHCR
                 -> [PendingRnSplice]
                 -> ExpRhoType
                 -> TcM (HsExpr GHCTc)
tcTypedBracket :: HsBracket GHCR
               -> ExpRhoType
               -> TcM (HsExpr GHCTc)

runAnnotation     :: CoreAnnTarget -> LHsExpr GHCR -> TcM Annotation

tcTopSpliceExpr :: SpliceType -> TcM (LHsExpr GHCTc) -> TcM (LHsExpr GHCTc)

runMetaE :: LHsExpr GHCTc -> TcM (LHsExpr GHCP)
runMetaP :: LHsExpr GHCTc -> TcM (LPat GHCP)
runMetaT :: LHsExpr GHCTc -> TcM (LHsType GHCP)
runMetaD :: LHsExpr GHCTc -> TcM [LHsDecl GHCP]

lookupThName_maybe :: TH.Name -> TcM (Maybe (IdP GHCR))
runQuasi :: TH.Q a -> TcM a
runRemoteModFinalizers :: ThModFinalizers -> TcM ()
finishTH :: TcM ()
