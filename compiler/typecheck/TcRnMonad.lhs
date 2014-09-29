%
% (c) The University of Glasgow 2006
%

Functions for working with the typechecker environment (setters, getters...).

\begin{code}
{-# LANGUAGE CPP, ExplicitForAll, FlexibleInstances #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module TcRnMonad(
        module TcRnMonad,
        module TcRnTypes,
        module IOEnv
  ) where

#include "HsVersions.h"

import TcRnTypes        -- Re-export all
import IOEnv            -- Re-export all
import TcEvidence

import HsSyn hiding (LIE)
import HscTypes
import Module
import RdrName
import Name
import Type

import TcType
import InstEnv
import FamInstEnv
import PrelNames

import Var
import Id
import VarSet
import VarEnv
import ErrUtils
import SrcLoc
import NameEnv
import NameSet
import Bag
import Outputable
import UniqSupply
import UniqFM
import DynFlags
import StaticFlags
import FastString
import Panic
import Util
import Annotations
import BasicTypes( TopLevelFlag )

import Control.Exception
import Data.IORef
import qualified Data.Set as Set
import Control.Monad

#ifdef GHCI
import qualified Data.Map as Map
#endif
\end{code}



%************************************************************************
%*                                                                      *
                        initTc
%*                                                                      *
%************************************************************************

\begin{code}

-- | Setup the initial typechecking environment
initTc :: (ApiAnnotation l) => HscEnv l
       -> HscSource
       -> Bool          -- True <=> retain renamed syntax trees
       -> Module
       -> TcM l r
       -> IO (Messages, Maybe r)
                -- Nothing => error thrown by the thing inside
                -- (error messages should have been printed already)

initTc hsc_env hsc_src keep_rn_syntax mod do_this
 = do { errs_var     <- newIORef (emptyBag, emptyBag) ;
        tvs_var      <- newIORef emptyVarSet ;
        keep_var     <- newIORef emptyNameSet ;
        used_rdr_var <- newIORef Set.empty ;
        th_var       <- newIORef False ;
        th_splice_var<- newIORef False ;
        infer_var    <- newIORef True ;
        lie_var      <- newIORef emptyWC ;
        dfun_n_var   <- newIORef emptyOccSet ;
        type_env_var <- case hsc_type_env_var hsc_env of {
                           Just (_mod, te_var) -> return te_var ;
                           Nothing             -> newIORef emptyNameEnv } ;

        dependent_files_var <- newIORef [] ;
#ifdef GHCI
        th_topdecls_var      <- newIORef [] ;
        th_topnames_var      <- newIORef emptyNameSet ;
        th_modfinalizers_var <- newIORef [] ;
        th_state_var         <- newIORef Map.empty ;
#endif /* GHCI */
        let {
             maybe_rn_syntax :: forall a. a -> Maybe a ;
             maybe_rn_syntax empty_val
                | keep_rn_syntax = Just empty_val
                | otherwise      = Nothing ;

             gbl_env = TcGblEnv {
#ifdef GHCI
                tcg_th_topdecls      = th_topdecls_var,
                tcg_th_topnames      = th_topnames_var,
                tcg_th_modfinalizers = th_modfinalizers_var,
                tcg_th_state         = th_state_var,
#endif /* GHCI */

                tcg_mod            = mod,
                tcg_src            = hsc_src,
                tcg_rdr_env        = emptyGlobalRdrEnv,
                tcg_fix_env        = emptyNameEnv,
                tcg_field_env      = RecFields emptyNameEnv emptyNameSet,
                tcg_default        = Nothing,
                tcg_type_env       = emptyNameEnv,
                tcg_type_env_var   = type_env_var,
                tcg_inst_env       = emptyInstEnv,
                tcg_fam_inst_env   = emptyFamInstEnv,
                tcg_ann_env        = emptyAnnEnv,
                tcg_th_used        = th_var,
                tcg_th_splice_used = th_splice_var,
                tcg_exports        = [],
                tcg_imports        = emptyImportAvails,
                tcg_used_rdrnames  = used_rdr_var,
                tcg_dus            = emptyDUs,

                tcg_rn_imports     = [],
                tcg_rn_exports     = maybe_rn_syntax [],
                tcg_rn_decls       = maybe_rn_syntax emptyRnGroup,

                tcg_binds          = emptyLHsBinds,
                tcg_imp_specs      = [],
                tcg_sigs           = emptyNameSet,
                tcg_ev_binds       = emptyBag,
                tcg_warns          = NoWarnings,
                tcg_anns           = [],
                tcg_tcs            = [],
                tcg_insts          = [],
                tcg_fam_insts      = [],
                tcg_rules          = [],
                tcg_fords          = [],
                tcg_vects          = [],
                tcg_patsyns        = [],
                tcg_dfun_n         = dfun_n_var,
                tcg_keep           = keep_var,
                tcg_doc_hdr        = Nothing,
                tcg_hpc            = False,
                tcg_main           = Nothing,
                tcg_safeInfer      = infer_var,
                tcg_dependent_files = dependent_files_var
             } ;
             lcl_env = TcLclEnv {
                tcl_errs       = errs_var,
                tcl_loc        = annFromSpan $ mkGeneralSrcSpan (fsLit "Top level"),
                tcl_ctxt       = [],
                tcl_rdr        = emptyLocalRdrEnv,
                tcl_th_ctxt    = topStage,
                tcl_th_bndrs   = emptyNameEnv,
                tcl_arrow_ctxt = NoArrowCtxt,
                tcl_env        = emptyNameEnv,
                tcl_bndrs      = [],
                tcl_tidy       = emptyTidyEnv,
                tcl_tyvars     = tvs_var,
                tcl_lie        = lie_var,
                tcl_untch      = noUntouchables
             } ;
        } ;

        -- OK, here's the business end!
        maybe_res <- initTcRnIf 'a' hsc_env gbl_env lcl_env $
                     do { r <- tryM do_this
                        ; case r of
                          Right res -> return (Just res)
                          Left _    -> return Nothing } ;

        -- Check for unsolved constraints
        lie <- readIORef lie_var ;
        if isEmptyWC lie
           then return ()
           else pprPanic "initTc: unsolved constraints"
                         (pprWantedsWithLocs lie) ;

        -- Collect any error messages
        msgs <- readIORef errs_var ;

        let { dflags = hsc_dflags hsc_env
            ; final_res | errorsFound dflags msgs = Nothing
                        | otherwise               = maybe_res } ;

        return (msgs, final_res)
    }


initTcInteractive :: (ApiAnnotation l)
                  => HscEnv l -> TcM l a -> IO (Messages, Maybe a)
-- Initialise the type checker monad for use in GHCi
initTcInteractive hsc_env thing_inside
  = initTc hsc_env HsSrcFile False
           (icInteractiveModule (hsc_IC hsc_env))
           thing_inside

initTcForLookup :: (ApiAnnotation l) => HscEnv l -> TcM l a -> IO a
-- The thing_inside is just going to look up something
-- in the environment, so we don't need much setup
initTcForLookup hsc_env thing_inside
    = do (msgs, m) <- initTc hsc_env HsSrcFile False
                             (icInteractiveModule (hsc_IC hsc_env))  -- Irrelevant really
                             thing_inside
         case m of
             Nothing -> throwIO $ mkSrcErr $ snd msgs
             Just x -> return x
\end{code}

%************************************************************************
%*                                                                      *
                Initialisation
%*                                                                      *
%************************************************************************


\begin{code}
initTcRnIf :: Char              -- Tag for unique supply
           -> HscEnv l
           -> gbl -> lcl
           -> TcRnIf l gbl lcl a
           -> IO a
initTcRnIf uniq_tag hsc_env gbl_env lcl_env thing_inside
   = do { us     <- mkSplitUniqSupply uniq_tag ;
        ; us_var <- newIORef us ;

        ; let { env = Env { env_top = hsc_env,
                            env_us  = us_var,
                            env_gbl = gbl_env,
                            env_lcl = lcl_env} }

        ; runIOEnv env thing_inside
        }
\end{code}

%************************************************************************
%*                                                                      *
                Simple accessors
%*                                                                      *
%************************************************************************

\begin{code}
discardResult :: TcM l a -> TcM l ()
discardResult a = a >> return ()

getTopEnv :: TcRnIf l gbl lcl (HscEnv l)
getTopEnv = do { env <- getEnv; return (env_top env) }

getGblEnv :: TcRnIf l gbl lcl gbl
getGblEnv = do { env <- getEnv; return (env_gbl env) }

updGblEnv :: (gbl -> gbl) -> TcRnIf l gbl lcl a -> TcRnIf l gbl lcl a
updGblEnv upd = updEnv (\ env@(Env { env_gbl = gbl }) ->
                          env { env_gbl = upd gbl })

setGblEnv :: gbl -> TcRnIf l gbl lcl a -> TcRnIf l gbl lcl a
setGblEnv gbl_env = updEnv (\ env -> env { env_gbl = gbl_env })

getLclEnv :: TcRnIf l gbl lcl lcl
getLclEnv = do { env <- getEnv; return (env_lcl env) }

updLclEnv :: (lcl -> lcl) -> TcRnIf l gbl lcl a -> TcRnIf l gbl lcl a
updLclEnv upd = updEnv (\ env@(Env { env_lcl = lcl }) ->
                          env { env_lcl = upd lcl })

setLclEnv :: lcl' -> TcRnIf l gbl lcl' a -> TcRnIf l gbl lcl a
setLclEnv lcl_env = updEnv (\ env -> env { env_lcl = lcl_env })

getEnvs :: TcRnIf l gbl lcl (gbl, lcl)
getEnvs = do { env <- getEnv; return (env_gbl env, env_lcl env) }

setEnvs :: (gbl', lcl') -> TcRnIf l gbl' lcl' a -> TcRnIf l gbl lcl a
setEnvs (gbl_env, lcl_env) = updEnv (\ env -> env { env_gbl = gbl_env, env_lcl = lcl_env })
\end{code}


Command-line flags

\begin{code}
xoptM :: ExtensionFlag -> TcRnIf l gbl lcl Bool
xoptM flag = do { dflags <- getDynFlags; return (xopt flag dflags) }

doptM :: DumpFlag -> TcRnIf l gbl lcl Bool
doptM flag = do { dflags <- getDynFlags; return (dopt flag dflags) }

goptM :: GeneralFlag -> TcRnIf l gbl lcl Bool
goptM flag = do { dflags <- getDynFlags; return (gopt flag dflags) }

woptM :: WarningFlag -> TcRnIf l gbl lcl Bool
woptM flag = do { dflags <- getDynFlags; return (wopt flag dflags) }

setXOptM :: ExtensionFlag -> TcRnIf l gbl lcl a -> TcRnIf l gbl lcl a
setXOptM flag = updEnv (\ env@(Env { env_top = top }) ->
                          env { env_top = top { hsc_dflags = xopt_set (hsc_dflags top) flag}} )

unsetGOptM :: GeneralFlag -> TcRnIf l gbl lcl a -> TcRnIf l gbl lcl a
unsetGOptM flag = updEnv (\ env@(Env { env_top = top }) ->
                            env { env_top = top { hsc_dflags = gopt_unset (hsc_dflags top) flag}} )

unsetWOptM :: WarningFlag -> TcRnIf l gbl lcl a -> TcRnIf l gbl lcl a
unsetWOptM flag = updEnv (\ env@(Env { env_top = top }) ->
                            env { env_top = top { hsc_dflags = wopt_unset (hsc_dflags top) flag}} )

-- | Do it flag is true
whenDOptM :: DumpFlag -> TcRnIf l gbl lcl () -> TcRnIf l gbl lcl ()
whenDOptM flag thing_inside = do b <- doptM flag
                                 when b thing_inside

whenGOptM :: GeneralFlag -> TcRnIf l gbl lcl () -> TcRnIf l gbl lcl ()
whenGOptM flag thing_inside = do b <- goptM flag
                                 when b thing_inside

whenWOptM :: WarningFlag -> TcRnIf l gbl lcl () -> TcRnIf l gbl lcl ()
whenWOptM flag thing_inside = do b <- woptM flag
                                 when b thing_inside

whenXOptM :: ExtensionFlag -> TcRnIf l gbl lcl () -> TcRnIf l gbl lcl ()
whenXOptM flag thing_inside = do b <- xoptM flag
                                 when b thing_inside

getGhcMode :: TcRnIf l gbl lcl GhcMode
getGhcMode = do { env <- getTopEnv; return (ghcMode (hsc_dflags env)) }
\end{code}

\begin{code}
withDoDynamicToo :: TcRnIf l gbl lcl a -> TcRnIf l gbl lcl a
withDoDynamicToo m = do env <- getEnv
                        let dflags = extractDynFlags env
                            dflags' = dynamicTooMkDynamicDynFlags dflags
                            env' = replaceDynFlags env dflags'
                        setEnv env' m
\end{code}

\begin{code}
getEpsVar :: TcRnIf l gbl lcl (TcRef ExternalPackageState)
getEpsVar = do { env <- getTopEnv; return (hsc_EPS env) }

getEps :: TcRnIf l gbl lcl ExternalPackageState
getEps = do { env <- getTopEnv; readMutVar (hsc_EPS env) }

-- | Update the external package state.  Returns the second result of the
-- modifier function.
--
-- This is an atomic operation and forces evaluation of the modified EPS in
-- order to avoid space leaks.
updateEps :: (ExternalPackageState -> (ExternalPackageState, a))
          -> TcRnIf l gbl lcl a
updateEps upd_fn = do
  traceIf (text "updating EPS")
  eps_var <- getEpsVar
  atomicUpdMutVar' eps_var upd_fn

-- | Update the external package state.
--
-- This is an atomic operation and forces evaluation of the modified EPS in
-- order to avoid space leaks.
updateEps_ :: (ExternalPackageState -> ExternalPackageState)
           -> TcRnIf l gbl lcl ()
updateEps_ upd_fn = do
  traceIf (text "updating EPS_")
  eps_var <- getEpsVar
  atomicUpdMutVar' eps_var (\eps -> (upd_fn eps, ()))

getHpt :: TcRnIf l gbl lcl HomePackageTable
getHpt = do { env <- getTopEnv; return (hsc_HPT env) }

getEpsAndHpt :: TcRnIf l gbl lcl (ExternalPackageState, HomePackageTable)
getEpsAndHpt = do { env <- getTopEnv; eps <- readMutVar (hsc_EPS env)
                  ; return (eps, hsc_HPT env) }
\end{code}

%************************************************************************
%*                                                                      *
                Unique supply
%*                                                                      *
%************************************************************************

\begin{code}
newUnique :: TcRnIf l gbl lcl Unique
newUnique
 = do { env <- getEnv ;
        let { u_var = env_us env } ;
        us <- readMutVar u_var ;
        case takeUniqFromSupply us of { (uniq, us') -> do {
        writeMutVar u_var us' ;
        return $! uniq }}}
   -- NOTE 1: we strictly split the supply, to avoid the possibility of leaving
   -- a chain of unevaluated supplies behind.
   -- NOTE 2: we use the uniq in the supply from the MutVar directly, and
   -- throw away one half of the new split supply.  This is safe because this
   -- is the only place we use that unique.  Using the other half of the split
   -- supply is safer, but slower.

newUniqueSupply :: TcRnIf l gbl lcl UniqSupply
newUniqueSupply
 = do { env <- getEnv ;
        let { u_var = env_us env } ;
        us <- readMutVar u_var ;
        case splitUniqSupply us of { (us1,us2) -> do {
        writeMutVar u_var us1 ;
        return us2 }}}

newLocalName :: (ApiAnnotation l) => Name -> TcM l Name
newLocalName name = newName (nameOccName name)

newName :: (ApiAnnotation l) => OccName -> TcM l Name
newName occ
  = do { uniq <- newUnique
       ; loc  <- getSrcSpanM
       ; return (mkInternalName uniq occ (annGetSpan loc)) }

newSysName :: OccName -> TcM l Name
newSysName occ
  = do { uniq <- newUnique
       ; return (mkSystemName uniq occ) }

newSysLocalIds :: FastString -> [TcType] -> TcRnIf l gbl lcl [TcId]
newSysLocalIds fs tys
  = do  { us <- newUniqueSupply
        ; return (zipWith (mkSysLocal fs) (uniqsFromSupply us) tys) }

instance MonadUnique (IOEnv (Env l gbl lcl)) where
        getUniqueM = newUnique
        getUniqueSupplyM = newUniqueSupply
\end{code}


%************************************************************************
%*                                                                      *
                Debugging
%*                                                                      *
%************************************************************************

\begin{code}
newTcRef :: a -> TcRnIf l gbl lcl (TcRef a)
newTcRef = newMutVar

readTcRef :: TcRef a -> TcRnIf l gbl lcl a
readTcRef = readMutVar

writeTcRef :: TcRef a -> a -> TcRnIf l gbl lcl ()
writeTcRef = writeMutVar

updTcRef :: TcRef a -> (a -> a) -> TcRnIf l gbl lcl ()
updTcRef = updMutVar
\end{code}

%************************************************************************
%*                                                                      *
                Debugging
%*                                                                      *
%************************************************************************

\begin{code}
traceTc :: (ApiAnnotation l) => String -> SDoc -> TcRn l ()
traceTc = traceTcN 1

traceTcN :: (ApiAnnotation l) => Int -> String -> SDoc -> TcRn l ()
traceTcN level herald doc
    = do dflags <- getDynFlags
         when (level <= traceLevel dflags) $
             traceOptTcRn Opt_D_dump_tc_trace $ hang (text herald) 2 doc

traceRn, traceSplice :: (ApiAnnotation l) => SDoc -> TcRn l ()
traceRn      = traceOptTcRn Opt_D_dump_rn_trace
traceSplice  = traceOptTcRn Opt_D_dump_splices

traceIf, traceHiDiffs :: SDoc -> TcRnIf l m n ()
traceIf      = traceOptIf Opt_D_dump_if_trace
traceHiDiffs = traceOptIf Opt_D_dump_hi_diffs


traceOptIf :: DumpFlag -> SDoc -> TcRnIf l m n ()  -- No RdrEnv available, so qualify everything
traceOptIf flag doc = whenDOptM flag $
                          do dflags <- getDynFlags
                             liftIO (printInfoForUser dflags alwaysQualify doc)

traceOptTcRn :: (ApiAnnotation l) => DumpFlag -> SDoc -> TcRn l ()
-- Output the message, with current location if opt_PprStyle_Debug
traceOptTcRn flag doc = whenDOptM flag $ do
                        { loc  <- getSrcSpanM
                        ; let real_doc
                                | opt_PprStyle_Debug = mkLocMessage SevInfo (annGetSpan loc) doc
                                | otherwise = doc   -- The full location is
                                                    -- usually way too much
                        ; dumpTcRn real_doc }

dumpTcRn :: SDoc -> TcRn l ()
dumpTcRn doc = do { rdr_env <- getGlobalRdrEnv
                  ; dflags <- getDynFlags
                  ; liftIO (printInfoForUser dflags (mkPrintUnqualified dflags rdr_env) doc) }

debugDumpTcRn :: SDoc -> TcRn l ()
debugDumpTcRn doc | opt_NoDebugOutput = return ()
                  | otherwise         = dumpTcRn doc

dumpOptTcRn :: DumpFlag -> SDoc -> TcRn l ()
dumpOptTcRn flag doc = whenDOptM flag (dumpTcRn doc)
\end{code}


%************************************************************************
%*                                                                      *
                Typechecker global environment
%*                                                                      *
%************************************************************************

\begin{code}
setModule :: Module -> TcRn l a -> TcRn l a
setModule mod thing_inside = updGblEnv (\env -> env { tcg_mod = mod }) thing_inside

getIsGHCi :: TcRn l Bool
getIsGHCi = do { mod <- getModule
               ; return (isInteractiveModule mod) }

getGHCiMonad :: TcRn l Name
getGHCiMonad = do { hsc <- getTopEnv; return (ic_monad $ hsc_IC hsc) }

getInteractivePrintName :: TcRn l Name
getInteractivePrintName = do { hsc <- getTopEnv; return (ic_int_print $ hsc_IC hsc) }

tcIsHsBoot :: TcRn l Bool
tcIsHsBoot = do { env <- getGblEnv; return (isHsBoot (tcg_src env)) }

getGlobalRdrEnv :: TcRn l GlobalRdrEnv
getGlobalRdrEnv = do { env <- getGblEnv; return (tcg_rdr_env env) }

getRdrEnvs :: TcRn l (GlobalRdrEnv, LocalRdrEnv)
getRdrEnvs = do { (gbl,lcl) <- getEnvs; return (tcg_rdr_env gbl, tcl_rdr lcl) }

getImports :: TcRn l ImportAvails
getImports = do { env <- getGblEnv; return (tcg_imports env) }

getFixityEnv :: TcRn l FixityEnv
getFixityEnv = do { env <- getGblEnv; return (tcg_fix_env env) }

extendFixityEnv :: [(Name,FixItem)] -> RnM l a -> RnM l a
extendFixityEnv new_bit
  = updGblEnv (\env@(TcGblEnv { tcg_fix_env = old_fix_env }) ->
                env {tcg_fix_env = extendNameEnvList old_fix_env new_bit})

getRecFieldEnv :: TcRn l RecFieldEnv
getRecFieldEnv = do { env <- getGblEnv; return (tcg_field_env env) }

getDeclaredDefaultTys :: TcRn l (Maybe [Type])
getDeclaredDefaultTys = do { env <- getGblEnv; return (tcg_default env) }

addDependentFiles :: [FilePath] -> TcRn l ()
addDependentFiles fs = do
  ref <- fmap tcg_dependent_files getGblEnv
  dep_files <- readTcRef ref
  writeTcRef ref (fs ++ dep_files)
\end{code}

%************************************************************************
%*                                                                      *
                Error management
%*                                                                      *
%************************************************************************

\begin{code}
getSrcSpanM :: TcRn l l
        -- Avoid clash with Name.getSrcLoc
getSrcSpanM = do { env <- getLclEnv; return (tcl_loc env) }

setSrcSpan :: (ApiAnnotation l) => l -> TcRn l a -> TcRn l a
setSrcSpan loc thing_inside = setSrcSpan' (annGetSpan loc) thing_inside
  where
    setSrcSpan' (RealSrcSpan _) thing_inside
        = updLclEnv (\env -> env { tcl_loc = loc }) thing_inside
    -- Don't overwrite useful info with useless:
    setSrcSpan' (UnhelpfulSpan _) thing_inside = thing_inside


addLocM :: (ApiAnnotation l) => (a -> TcM l b) -> GenLocated l a -> TcM l b
addLocM fn (L loc a) = setSrcSpan loc $ fn a

wrapLocM :: (ApiAnnotation l) => (a -> TcM l b) -> GenLocated l a -> TcM l (GenLocated l b)
wrapLocM fn (L loc a) = setSrcSpan loc $ do b <- fn a; return (L loc b)

wrapLocFstM :: (ApiAnnotation l)
            => (a -> TcM l (b,c)) -> GenLocated l a -> TcM l (GenLocated l b, c)
wrapLocFstM fn (L loc a) =
  setSrcSpan loc $ do
    (b,c) <- fn a
    return (L loc b, c)

wrapLocSndM :: (ApiAnnotation l)
            => (a -> TcM l (b,c)) -> GenLocated l a -> TcM l (b, GenLocated l c)
wrapLocSndM fn (L loc a) =
  setSrcSpan loc $ do
    (b,c) <- fn a
    return (b, L loc c)
\end{code}

Reporting errors

\begin{code}
getErrsVar :: TcRn l (TcRef Messages)
getErrsVar = do { env <- getLclEnv; return (tcl_errs env) }

setErrsVar :: TcRef Messages -> TcRn l a -> TcRn l a
setErrsVar v = updLclEnv (\ env -> env { tcl_errs =  v })

addErr :: (ApiAnnotation l) => MsgDoc -> TcRn l ()  -- Ignores the context stack
addErr msg = do { loc <- getSrcSpanM; addErrAt loc msg }

failWith :: (ApiAnnotation l) => MsgDoc -> TcRn l a
failWith msg = addErr msg >> failM

addErrAt :: (ApiAnnotation l) => l -> MsgDoc -> TcRn l ()
-- addErrAt is mainly (exclusively?) used by the renamer, where
-- tidying is not an issue, but it's all lazy so the extra
-- work doesn't matter
addErrAt loc msg = do { ctxt <- getErrCtxt
                      ; tidy_env <- tcInitTidyEnv
                      ; err_info <- mkErrInfo tidy_env ctxt
                      ; addLongErrAt loc msg err_info }

addErrs :: (ApiAnnotation l) => [(l,MsgDoc)] -> TcRn l ()
addErrs msgs = mapM_ add msgs
             where
               add (loc,msg) = addErrAt loc msg

checkErr :: (ApiAnnotation l) => Bool -> MsgDoc -> TcRn l ()
-- Add the error if the bool is False
checkErr ok msg = unless ok (addErr msg)

warnIf :: (ApiAnnotation l) => Bool -> MsgDoc -> TcRn l ()
warnIf True  msg = addWarn msg
warnIf False _   = return ()

addMessages :: Messages -> TcRn l ()
addMessages (m_warns, m_errs)
  = do { errs_var <- getErrsVar ;
         (warns, errs) <- readTcRef errs_var ;
         writeTcRef errs_var (warns `unionBags` m_warns,
                               errs  `unionBags` m_errs) }

discardWarnings :: TcRn l a -> TcRn l a
-- Ignore warnings inside the thing inside;
-- used to ignore-unused-variable warnings inside derived code
discardWarnings thing_inside
  = do  { errs_var <- getErrsVar
        ; (old_warns, _) <- readTcRef errs_var ;

        ; result <- thing_inside

        -- Revert warnings to old_warns
        ; (_new_warns, new_errs) <- readTcRef errs_var
        ; writeTcRef errs_var (old_warns, new_errs) 

        ; return result }
\end{code}


%************************************************************************
%*                                                                      *
        Shared error message stuff: renamer and typechecker
%*                                                                      *
%************************************************************************

\begin{code}
mkLongErrAt :: (ApiAnnotation l) => l -> MsgDoc -> MsgDoc -> TcRn l ErrMsg
mkLongErrAt loc msg extra
  = do { rdr_env <- getGlobalRdrEnv ;
         dflags <- getDynFlags ;
         return $ mkLongErrMsg dflags (annGetSpan loc) (mkPrintUnqualified dflags rdr_env) msg extra }

addLongErrAt :: (ApiAnnotation l) => l -> MsgDoc -> MsgDoc -> TcRn l ()
addLongErrAt loc msg extra = mkLongErrAt loc msg extra >>= reportError

reportErrors :: (ApiAnnotation l) => [ErrMsg] -> TcM l ()
reportErrors = mapM_ reportError

reportError :: (ApiAnnotation l) => ErrMsg -> TcRn l ()
reportError err
  = do { traceTc "Adding error:" (pprLocErrMsg err) ;
         errs_var <- getErrsVar ;
         (warns, errs) <- readTcRef errs_var ;
         writeTcRef errs_var (warns, errs `snocBag` err) }

reportWarning :: (ApiAnnotation l) => ErrMsg -> TcRn l ()
reportWarning warn
  = do { traceTc "Adding warning:" (pprLocErrMsg warn) ;
         errs_var <- getErrsVar ;
         (warns, errs) <- readTcRef errs_var ;
         writeTcRef errs_var (warns `snocBag` warn, errs) }

dumpDerivingInfo :: SDoc -> TcM l ()
dumpDerivingInfo doc
  = do { dflags <- getDynFlags
       ; when (dopt Opt_D_dump_deriv dflags) $ do
       { rdr_env <- getGlobalRdrEnv
       ; let unqual = mkPrintUnqualified dflags rdr_env
       ; liftIO (putMsgWith dflags unqual doc) } }
\end{code}


\begin{code}
try_m :: (ApiAnnotation l) => TcRn l r -> TcRn l (Either IOEnvFailure r)
-- Does try_m, with a debug-trace on failure
try_m thing
  = do { mb_r <- tryM thing ;
         case mb_r of
             Left exn -> do { traceTc "tryTc/recoverM recovering from" $
                                      text (showException exn)
                            ; return mb_r }
             Right _  -> return mb_r }

-----------------------
recoverM :: (ApiAnnotation l)
         => TcRn l r      -- Recovery action; do this if the main one fails
         -> TcRn l r      -- Main action: do this first
         -> TcRn l r
-- Errors in 'thing' are retained
recoverM recover thing
  = do { mb_res <- try_m thing ;
         case mb_res of
           Left _    -> recover
           Right res -> return res }


-----------------------
mapAndRecoverM :: (ApiAnnotation l) => (a -> TcRn l b) -> [a] -> TcRn l [b]
-- Drop elements of the input that fail, so the result
-- list can be shorter than the argument list
mapAndRecoverM _ []     = return []
mapAndRecoverM f (x:xs) = do { mb_r <- try_m (f x)
                             ; rs <- mapAndRecoverM f xs
                             ; return (case mb_r of
                                          Left _  -> rs
                                          Right r -> r:rs) }

-- | Succeeds if applying the argument to all members of the lists succeeds,
--   but nevertheless runs it on all arguments, to collect all errors.
mapAndReportM :: (ApiAnnotation l) => (a -> TcRn l b) -> [a] -> TcRn l [b]
mapAndReportM f xs = checkNoErrs (mapAndRecoverM f xs)

-----------------------
tryTc :: (ApiAnnotation l) => TcRn l a -> TcRn l (Messages, Maybe a)
-- (tryTc m) executes m, and returns
--      Just r,  if m succeeds (returning r)
--      Nothing, if m fails
-- It also returns all the errors and warnings accumulated by m
-- It always succeeds (never raises an exception)
tryTc m
 = do { errs_var <- newTcRef emptyMessages ;
        res  <- try_m (setErrsVar errs_var m) ;
        msgs <- readTcRef errs_var ;
        return (msgs, case res of
                            Left _  -> Nothing
                            Right val -> Just val)
        -- The exception is always the IOEnv built-in
        -- in exception; see IOEnv.failM
   }

-----------------------
tryTcErrs :: (ApiAnnotation l) => TcRn l a -> TcRn l (Messages, Maybe a)
-- Run the thing, returning
--      Just r,  if m succceeds with no error messages
--      Nothing, if m fails, or if it succeeds but has error messages
-- Either way, the messages are returned; even in the Just case
-- there might be warnings
tryTcErrs thing
  = do  { (msgs, res) <- tryTc thing
        ; dflags <- getDynFlags
        ; let errs_found = errorsFound dflags msgs
        ; return (msgs, case res of
                          Nothing -> Nothing
                          Just val | errs_found -> Nothing
                                   | otherwise  -> Just val)
        }

-----------------------
tryTcLIE :: (ApiAnnotation l) => TcM l a -> TcM l (Messages, Maybe a)
-- Just like tryTcErrs, except that it ensures that the LIE
-- for the thing is propagated only if there are no errors
-- Hence it's restricted to the type-check monad
tryTcLIE thing_inside
  = do  { ((msgs, mb_res), lie) <- captureConstraints (tryTcErrs thing_inside) ;
        ; case mb_res of
            Nothing  -> return (msgs, Nothing)
            Just val -> do { emitConstraints lie; return (msgs, Just val) }
        }

-----------------------
tryTcLIE_ :: (ApiAnnotation l) => TcM l r -> TcM l r -> TcM l r
-- (tryTcLIE_ r m) tries m;
--      if m succeeds with no error messages, it's the answer
--      otherwise tryTcLIE_ drops everything from m and tries r instead.
tryTcLIE_ recover main
  = do  { (msgs, mb_res) <- tryTcLIE main
        ; case mb_res of
             Just val -> do { addMessages msgs  -- There might be warnings
                             ; return val }
             Nothing  -> recover                -- Discard all msgs
        }

-----------------------
checkNoErrs :: (ApiAnnotation l) => TcM l r -> TcM l r
-- (checkNoErrs m) succeeds iff m succeeds and generates no errors
-- If m fails then (checkNoErrsTc m) fails.
-- If m succeeds, it checks whether m generated any errors messages
--      (it might have recovered internally)
--      If so, it fails too.
-- Regardless, any errors generated by m are propagated to the enclosing context.
checkNoErrs main
  = do  { (msgs, mb_res) <- tryTcLIE main
        ; addMessages msgs
        ; case mb_res of
            Nothing  -> failM
            Just val -> return val
        }

ifErrsM :: TcRn l r -> TcRn l r -> TcRn l r
--      ifErrsM bale_out normal
-- does 'bale_out' if there are errors in errors collection
-- otherwise does 'normal'
ifErrsM bale_out normal
 = do { errs_var <- getErrsVar ;
        msgs <- readTcRef errs_var ;
        dflags <- getDynFlags ;
        if errorsFound dflags msgs then
           bale_out
        else
           normal }

failIfErrsM :: TcRn l ()
-- Useful to avoid error cascades
failIfErrsM = ifErrsM failM (return ())

checkTH :: (ApiAnnotation l,Outputable a) => a -> String -> TcRn l ()
#ifdef GHCI
checkTH _ _ = return () -- OK
#else
checkTH e what = failTH e what  -- Raise an error in a stage-1 compiler
#endif

failTH :: (ApiAnnotation l) => Outputable a => a -> String -> TcRn l x
failTH e what  -- Raise an error in a stage-1 compiler
  = failWithTc (vcat [ hang (char 'A' <+> text what
                             <+> ptext (sLit "requires GHC with interpreter support:"))
                          2 (ppr e)
                     , ptext (sLit "Perhaps you are using a stage-1 compiler?") ])
\end{code}


%************************************************************************
%*                                                                      *
        Context management for the type checker
%*                                                                      *
%************************************************************************

\begin{code}
getErrCtxt :: TcM l [ErrCtxt l]
getErrCtxt = do { env <- getLclEnv; return (tcl_ctxt env) }

setErrCtxt :: [ErrCtxt l] -> TcM l a -> TcM l a
setErrCtxt ctxt = updLclEnv (\ env -> env { tcl_ctxt = ctxt })

addErrCtxt :: MsgDoc -> TcM l a -> TcM l a
addErrCtxt msg = addErrCtxtM (\env -> return (env, msg))

addErrCtxtM :: (TidyEnv -> TcM l (TidyEnv, MsgDoc)) -> TcM l a -> TcM l a
addErrCtxtM ctxt = updCtxt (\ ctxts -> (False, ctxt) : ctxts)

addLandmarkErrCtxt :: MsgDoc -> TcM l a -> TcM l a
addLandmarkErrCtxt msg = updCtxt (\ctxts -> (True, \env -> return (env,msg)) : ctxts)

-- Helper function for the above
updCtxt :: ([ErrCtxt l] -> [ErrCtxt l]) -> TcM l a -> TcM l a
updCtxt upd = updLclEnv (\ env@(TcLclEnv { tcl_ctxt = ctxt }) ->
                           env { tcl_ctxt = upd ctxt })

popErrCtxt :: TcM l a -> TcM l a
popErrCtxt = updCtxt (\ msgs -> case msgs of { [] -> []; (_ : ms) -> ms })

getCtLoc :: CtOrigin l -> TcM l (CtLoc l)
getCtLoc origin
  = do { env <- getLclEnv 
       ; return (CtLoc { ctl_origin = origin
                       , ctl_env = env
                       , ctl_depth = initialSubGoalDepth }) }

setCtLoc :: CtLoc l -> TcM l a -> TcM l a
-- Set the SrcSpan and error context from the CtLoc
setCtLoc (CtLoc { ctl_env = lcl }) thing_inside
  = updLclEnv (\env -> env { tcl_loc = tcl_loc lcl
                           , tcl_bndrs = tcl_bndrs lcl
                           , tcl_ctxt = tcl_ctxt lcl }) 
              thing_inside
\end{code}

%************************************************************************
%*                                                                      *
             Error message generation (type checker)
%*                                                                      *
%************************************************************************

    The addErrTc functions add an error message, but do not cause failure.
    The 'M' variants pass a TidyEnv that has already been used to
    tidy up the message; we then use it to tidy the context messages

\begin{code}
addErrTc :: (ApiAnnotation l) => MsgDoc -> TcM l ()
addErrTc err_msg = do { env0 <- tcInitTidyEnv
                      ; addErrTcM (env0, err_msg) }

addErrsTc :: (ApiAnnotation l) => [MsgDoc] -> TcM l ()
addErrsTc err_msgs = mapM_ addErrTc err_msgs

addErrTcM :: (ApiAnnotation l) => (TidyEnv, MsgDoc) -> TcM l ()
addErrTcM (tidy_env, err_msg)
  = do { ctxt <- getErrCtxt ;
         loc  <- getSrcSpanM ;
         add_err_tcm tidy_env err_msg loc ctxt }

-- Return the error message, instead of reporting it straight away
mkErrTcM :: (ApiAnnotation l) => (TidyEnv, MsgDoc) -> TcM l ErrMsg
mkErrTcM (tidy_env, err_msg)
  = do { ctxt <- getErrCtxt ;
         loc  <- getSrcSpanM ;
         err_info <- mkErrInfo tidy_env ctxt ;
         mkLongErrAt loc err_msg err_info }
\end{code}

The failWith functions add an error message and cause failure

\begin{code}
failWithTc :: (ApiAnnotation l)
           => MsgDoc -> TcM l a             -- Add an error message and fail
failWithTc err_msg
  = addErrTc err_msg >> failM

failWithTcM :: (ApiAnnotation l)
            => (TidyEnv, MsgDoc) -> TcM l a -- Add an error message and fail
failWithTcM local_and_msg
  = addErrTcM local_and_msg >> failM

checkTc :: (ApiAnnotation l)
        => Bool -> MsgDoc -> TcM l ()       -- Check that the boolean is true
checkTc True  _   = return ()
checkTc False err = failWithTc err
\end{code}

        Warnings have no 'M' variant, nor failure

\begin{code}
warnTc :: (ApiAnnotation l) => Bool -> MsgDoc -> TcM l ()
warnTc warn_if_true warn_msg
  | warn_if_true = addWarnTc warn_msg
  | otherwise    = return ()

addWarnTc :: (ApiAnnotation l) => MsgDoc -> TcM l ()
addWarnTc msg = do { env0 <- tcInitTidyEnv
                   ; addWarnTcM (env0, msg) }

addWarnTcM :: (ApiAnnotation l) => (TidyEnv, MsgDoc) -> TcM l ()
addWarnTcM (env0, msg)
 = do { ctxt <- getErrCtxt ;
        err_info <- mkErrInfo env0 ctxt ;
        add_warn msg err_info }

addWarn :: (ApiAnnotation l) => MsgDoc -> TcRn l ()
addWarn msg = add_warn msg Outputable.empty

addWarnAt :: (ApiAnnotation l) => SrcSpan -> MsgDoc -> TcRn l ()
addWarnAt loc msg = add_warn_at loc msg Outputable.empty

add_warn :: (ApiAnnotation l) => MsgDoc -> MsgDoc -> TcRn l ()
add_warn msg extra_info
  = do { loc <- getSrcSpanM
       ; add_warn_at (annGetSpan loc) msg extra_info }

add_warn_at :: (ApiAnnotation l) => SrcSpan -> MsgDoc -> MsgDoc -> TcRn l ()
add_warn_at loc msg extra_info
  = do { rdr_env <- getGlobalRdrEnv ;
         dflags <- getDynFlags ;
         let { warn = mkLongWarnMsg dflags loc (mkPrintUnqualified dflags rdr_env)
                                    msg extra_info } ;
         reportWarning warn }

tcInitTidyEnv :: TcM l TidyEnv
tcInitTidyEnv
  = do  { lcl_env <- getLclEnv
        ; return (tcl_tidy lcl_env) }
\end{code}

-----------------------------------
        Other helper functions

\begin{code}
add_err_tcm :: (ApiAnnotation l) => TidyEnv -> MsgDoc -> l
            -> [ErrCtxt l]
            -> TcM l ()
add_err_tcm tidy_env err_msg loc ctxt
 = do { err_info <- mkErrInfo tidy_env ctxt ;
        addLongErrAt loc err_msg err_info }

mkErrInfo :: TidyEnv -> [ErrCtxt l] -> TcM l SDoc
-- Tidy the error info, trimming excessive contexts
mkErrInfo env ctxts
--  | opt_PprStyle_Debug     -- In -dppr-debug style the output
--  = return empty           -- just becomes too voluminous
 | otherwise
 = go 0 env ctxts
 where
   go :: Int -> TidyEnv -> [ErrCtxt l] -> TcM l SDoc
   go _ _   [] = return Outputable.empty
   go n env ((is_landmark, ctxt) : ctxts)
     | is_landmark || n < mAX_CONTEXTS -- Too verbose || opt_PprStyle_Debug
     = do { (env', msg) <- ctxt env
          ; let n' = if is_landmark then n else n+1
          ; rest <- go n' env' ctxts
          ; return (msg $$ rest) }
     | otherwise
     = go n env ctxts

mAX_CONTEXTS :: Int     -- No more than this number of non-landmark contexts
mAX_CONTEXTS = 3
\end{code}

debugTc is useful for monadic debugging code

\begin{code}
debugTc :: TcM l () -> TcM l ()
debugTc thing
 | debugIsOn = thing
 | otherwise = return ()
\end{code}

%************************************************************************
%*                                                                      *
             Type constraints
%*                                                                      *
%************************************************************************

\begin{code}
newTcEvBinds :: TcM l EvBindsVar
newTcEvBinds = do { ref <- newTcRef emptyEvBindMap
                  ; uniq <- newUnique
                  ; return (EvBindsVar ref uniq) }

addTcEvBind :: EvBindsVar -> EvVar -> EvTerm -> TcM l ()
-- Add a binding to the TcEvBinds by side effect
addTcEvBind (EvBindsVar ev_ref _) var t
  = do { bnds <- readTcRef ev_ref
       ; writeTcRef ev_ref (extendEvBinds bnds var t) }

getTcEvBinds :: EvBindsVar -> TcM l (Bag EvBind)
getTcEvBinds (EvBindsVar ev_ref _) 
  = do { bnds <- readTcRef ev_ref
       ; return (evBindMapBinds bnds) }

chooseUniqueOccTc :: (OccSet -> OccName) -> TcM l OccName
chooseUniqueOccTc fn =
  do { env <- getGblEnv
     ; let dfun_n_var = tcg_dfun_n env
     ; set <- readTcRef dfun_n_var
     ; let occ = fn set
     ; writeTcRef dfun_n_var (extendOccSet set occ)
     ; return occ }

getConstraintVar :: TcM l (TcRef (WantedConstraints l))
getConstraintVar = do { env <- getLclEnv; return (tcl_lie env) }

setConstraintVar :: TcRef (WantedConstraints l) -> TcM l a -> TcM l a
setConstraintVar lie_var = updLclEnv (\ env -> env { tcl_lie = lie_var })

emitConstraints :: WantedConstraints l -> TcM l ()
emitConstraints ct
  = do { lie_var <- getConstraintVar ;
         updTcRef lie_var (`andWC` ct) }

emitFlat :: Ct l -> TcM l ()
emitFlat ct
  = do { lie_var <- getConstraintVar ;
         updTcRef lie_var (`addFlats` unitBag ct) }

emitFlats :: Cts l -> TcM l ()
emitFlats cts
  = do { lie_var <- getConstraintVar ;
         updTcRef lie_var (`addFlats` cts) }
    
emitImplication :: Implication l -> TcM l ()
emitImplication ct
  = do { lie_var <- getConstraintVar ;
         updTcRef lie_var (`addImplics` unitBag ct) }

emitImplications :: Bag (Implication l) -> TcM l ()
emitImplications ct
  = do { lie_var <- getConstraintVar ;
         updTcRef lie_var (`addImplics` ct) }

emitInsoluble :: (ApiAnnotation l) => Ct l -> TcM l ()
emitInsoluble ct
  = do { lie_var <- getConstraintVar ;
         updTcRef lie_var (`addInsols` unitBag ct) ;
         v <- readTcRef lie_var ;
         traceTc "emitInsoluble" (ppr v) }

captureConstraints :: TcM l a -> TcM l (a, WantedConstraints
 l)
-- (captureConstraints m) runs m, and returns the type constraints it generates
captureConstraints thing_inside
  = do { lie_var <- newTcRef emptyWC ;
         res <- updLclEnv (\ env -> env { tcl_lie = lie_var })
                          thing_inside ;
         lie <- readTcRef lie_var ;
         return (res, lie) }

captureUntouchables :: TcM l a -> TcM l (a, Untouchables)
captureUntouchables thing_inside
  = do { env <- getLclEnv
       ; let untch' = pushUntouchables (tcl_untch env)
       ; res <- setLclEnv (env { tcl_untch = untch' })
                thing_inside
       ; return (res, untch') }

getUntouchables :: TcM l Untouchables
getUntouchables = do { env <- getLclEnv
                     ; return (tcl_untch env) }

setUntouchables :: Untouchables -> TcM l a -> TcM l a
setUntouchables untch thing_inside 
  = updLclEnv (\env -> env { tcl_untch = untch }) thing_inside

isTouchableTcM :: TcTyVar -> TcM l Bool
isTouchableTcM tv
  = do { env <- getLclEnv
       ; return (isTouchableMetaTyVar (tcl_untch env) tv) }

getLclTypeEnv :: TcM l TcTypeEnv
getLclTypeEnv = do { env <- getLclEnv; return (tcl_env env) }

setLclTypeEnv :: TcLclEnv l -> TcM l a -> TcM l a
-- Set the local type envt, but do *not* disturb other fields,
-- notably the lie_var
setLclTypeEnv lcl_env thing_inside
  = updLclEnv upd thing_inside
  where
    upd env = env { tcl_env = tcl_env lcl_env,
                    tcl_tyvars = tcl_tyvars lcl_env }

traceTcConstraints :: (ApiAnnotation l) => String -> TcM l ()
traceTcConstraints msg
  = do { lie_var <- getConstraintVar
       ; lie     <- readTcRef lie_var
       ; traceTc (msg ++ ": LIE:") (ppr lie)
       }
\end{code}


%************************************************************************
%*                                                                      *
             Template Haskell context
%*                                                                      *
%************************************************************************

\begin{code}
recordThUse :: TcM l ()
recordThUse = do { env <- getGblEnv; writeTcRef (tcg_th_used env) True }

recordThSpliceUse :: TcM l ()
recordThSpliceUse = do { env <- getGblEnv; writeTcRef (tcg_th_splice_used env) True }

keepAlive :: (ApiAnnotation l)
          => Name -> TcRn l ()   -- Record the name in the keep-alive set
keepAlive name
  = do { env <- getGblEnv
       ; traceRn (ptext (sLit "keep alive") <+> ppr name)
       ; updTcRef (tcg_keep env) (`addOneToNameSet` name) }

getStage :: TcM l (ThStage l)
getStage = do { env <- getLclEnv; return (tcl_th_ctxt env) }

getStageAndBindLevel :: Name
                     -> TcRn l (Maybe (TopLevelFlag, ThLevel, ThStage l))
getStageAndBindLevel name
  = do { env <- getLclEnv;
       ; case lookupNameEnv (tcl_th_bndrs env) name of
           Nothing                  -> return Nothing
           Just (top_lvl, bind_lvl) -> return (Just (top_lvl, bind_lvl, tcl_th_ctxt env)) }

setStage :: ThStage l -> TcM l a -> TcRn l a
setStage s = updLclEnv (\ env -> env { tcl_th_ctxt = s })
\end{code}


%************************************************************************
%*                                                                      *
             Safe Haskell context
%*                                                                      *
%************************************************************************

\begin{code}
-- | Mark that safe inference has failed
recordUnsafeInfer :: TcM l ()
recordUnsafeInfer = getGblEnv >>= \env -> writeTcRef (tcg_safeInfer env) False

-- | Figure out the final correct safe haskell mode
finalSafeMode :: DynFlags -> TcGblEnv l -> IO SafeHaskellMode
finalSafeMode dflags tcg_env = do
    safeInf <- readIORef (tcg_safeInfer tcg_env)
    return $ case safeHaskell dflags of
        Sf_None | safeInferOn dflags && safeInf -> Sf_Safe
                | otherwise                     -> Sf_None
        s -> s
\end{code}


%************************************************************************
%*                                                                      *
             Stuff for the renamer's local env
%*                                                                      *
%************************************************************************

\begin{code}
getLocalRdrEnv :: RnM l LocalRdrEnv
getLocalRdrEnv = do { env <- getLclEnv; return (tcl_rdr env) }

setLocalRdrEnv :: LocalRdrEnv -> RnM l a -> RnM l a
setLocalRdrEnv rdr_env thing_inside
  = updLclEnv (\env -> env {tcl_rdr = rdr_env}) thing_inside
\end{code}


%************************************************************************
%*                                                                      *
             Stuff for interface decls
%*                                                                      *
%************************************************************************

\begin{code}
mkIfLclEnv :: Module -> SDoc -> IfLclEnv
mkIfLclEnv mod loc = IfLclEnv { if_mod     = mod,
                                if_loc     = loc,
                                if_tv_env  = emptyUFM,
                                if_id_env  = emptyUFM }

initIfaceTcRn :: IfG l a -> TcRn l a
initIfaceTcRn thing_inside
  = do  { tcg_env <- getGblEnv
        ; let { if_env = IfGblEnv { if_rec_types = Just (tcg_mod tcg_env, get_type_env) }
              ; get_type_env = readTcRef (tcg_type_env_var tcg_env) }
        ; setEnvs (if_env, ()) thing_inside }

initIfaceCheck :: HscEnv l -> IfG l a -> IO a
-- Used when checking the up-to-date-ness of the old Iface
-- Initialise the environment with no useful info at all
initIfaceCheck hsc_env do_this
 = do let rec_types = case hsc_type_env_var hsc_env of
                         Just (mod,var) -> Just (mod, readTcRef var)
                         Nothing        -> Nothing
          gbl_env = IfGblEnv { if_rec_types = rec_types }
      initTcRnIf 'i' hsc_env gbl_env () do_this

initIfaceTc :: ModIface
            -> (TcRef TypeEnv -> IfL l a) -> TcRnIf l gbl lcl a
-- Used when type-checking checking an up-to-date interface file
-- No type envt from the current module, but we do know the module dependencies
initIfaceTc iface do_this
 = do   { tc_env_var <- newTcRef emptyTypeEnv
        ; let { gbl_env = IfGblEnv { if_rec_types = Just (mod, readTcRef tc_env_var) } ;
              ; if_lenv = mkIfLclEnv mod doc
           }
        ; setEnvs (gbl_env, if_lenv) (do_this tc_env_var)
    }
  where
    mod = mi_module iface
    doc = ptext (sLit "The interface for") <+> quotes (ppr mod)

initIfaceLcl :: Module -> SDoc -> IfL l a -> IfM l lcl a
initIfaceLcl mod loc_doc thing_inside
  = setLclEnv (mkIfLclEnv mod loc_doc) thing_inside

getIfModule :: IfL l Module
getIfModule = do { env <- getLclEnv; return (if_mod env) }

--------------------
failIfM :: MsgDoc -> IfL l a
-- The Iface monad doesn't have a place to accumulate errors, so we
-- just fall over fast if one happens; it "shouldnt happen".
-- We use IfL here so that we can get context info out of the local env
failIfM msg
  = do  { env <- getLclEnv
        ; let full_msg = (if_loc env <> colon) $$ nest 2 msg
        ; dflags <- getDynFlags
        ; liftIO (log_action dflags dflags SevFatal noSrcSpan (defaultErrStyle dflags) full_msg)
        ; failM }

--------------------
forkM_maybe :: SDoc -> IfL l a -> IfL l (Maybe a)
-- Run thing_inside in an interleaved thread.
-- It shares everything with the parent thread, so this is DANGEROUS.
--
-- It returns Nothing if the computation fails
--
-- It's used for lazily type-checking interface
-- signatures, which is pretty benign

forkM_maybe doc thing_inside
 -- NB: Don't share the mutable env_us with the interleaved thread since env_us
 --     does not get updated atomically (e.g. in newUnique and newUniqueSupply).
 = do { child_us <- newUniqueSupply
      ; child_env_us <- newMutVar child_us
        -- see Note [Masking exceptions in forkM_maybe]
      ; unsafeInterleaveM $ uninterruptibleMaskM_ $ updEnv (\env -> env { env_us = child_env_us }) $
        do { traceIf (text "Starting fork {" <+> doc)
           ; mb_res <- tryM $
                       updLclEnv (\env -> env { if_loc = if_loc env $$ doc }) $
                       thing_inside
           ; case mb_res of
                Right r  -> do  { traceIf (text "} ending fork" <+> doc)
                                ; return (Just r) }
                Left exn -> do {

                    -- Bleat about errors in the forked thread, if -ddump-if-trace is on
                    -- Otherwise we silently discard errors. Errors can legitimately
                    -- happen when compiling interface signatures (see tcInterfaceSigs)
                      whenDOptM Opt_D_dump_if_trace $ do
                          dflags <- getDynFlags
                          let msg = hang (text "forkM failed:" <+> doc)
                                       2 (text (show exn))
                          liftIO $ log_action dflags dflags SevFatal noSrcSpan (defaultErrStyle dflags) msg

                    ; traceIf (text "} ending fork (badly)" <+> doc)
                    ; return Nothing }
        }}

forkM :: SDoc -> IfL l a -> IfL l a
forkM doc thing_inside
 = do   { mb_res <- forkM_maybe doc thing_inside
        ; return (case mb_res of
                        Nothing -> pgmError "Cannot continue after interface file error"
                                   -- pprPanic "forkM" doc
                        Just r  -> r) }
\end{code}

Note [Masking exceptions in forkM_maybe]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

When using GHC-as-API it must be possible to interrupt snippets of code
executed using runStmt (#1381). Since commit 02c4ab04 this is almost possible
by throwing an asynchronous interrupt to the GHC thread. However, there is a
subtle problem: runStmt first typechecks the code before running it, and the
exception might interrupt the type checker rather than the code. Moreover, the
typechecker might be inside an unsafeInterleaveIO (through forkM_maybe), and
more importantly might be inside an exception handler inside that
unsafeInterleaveIO. If that is the case, the exception handler will rethrow the
asynchronous exception as a synchronous exception, and the exception will end
up as the value of the unsafeInterleaveIO thunk (see #8006 for a detailed
discussion).  We don't currently know a general solution to this problem, but
we can use uninterruptibleMask_ to avoid the situation. 
