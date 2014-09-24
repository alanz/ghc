
%
% (c) The University of Glasgow, 1992-2006
%

Here we collect a variety of helper functions that construct or
analyse HsSyn.  All these functions deal with generic HsSyn; functions
which deal with the instantiated versions are located elsewhere:

   Parameterised by     Module
   ----------------     -------------
   RdrName              parser/RdrHsSyn
   Name                 rename/RnHsSyn
   Id                   typecheck/TcHsSyn

\begin{code}
{-# LANGUAGE CPP #-}
{-# OPTIONS_GHC -fno-warn-tabs #-}
-- The above warning supression flag is a temporary kludge.
-- While working on this module you are encouraged to remove it and
-- detab the module (please do the detabbing in a separate patch). See
--     http://ghc.haskell.org/trac/ghc/wiki/Commentary/CodingStyle#TabsvsSpaces
-- for details
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

module HsUtils(
  -- Terms
  mkHsPar, mkHsApp, mkHsConApp, mkSimpleHsAlt,
  mkSimpleMatch, unguardedGRHSs, unguardedRHS,
  mkMatchGroup, mkMatchGroupName, mkMatch, mkHsLam, mkHsIf,
  mkHsWrap, mkLHsWrap, mkHsWrapCo, mkHsWrapCoR, mkLHsWrapCo,
  coToHsWrapper, mkHsDictLet, mkHsLams,
  mkHsOpApp, mkHsDo, mkHsComp, mkHsWrapPat, mkHsWrapPatCo,
  mkLHsPar, mkHsCmdCast,

  nlHsTyApp, nlHsVar, nlHsLit, nlHsApp, nlHsApps, nlHsIntLit, nlHsVarApps,
  nlHsDo, nlHsOpApp, nlHsLam, nlHsPar, nlHsIf, nlHsCase, nlList,
  mkLHsTupleExpr, mkLHsVarTuple, missingTupArg,
  toHsType, toHsKind,

  -- Bindings
  mkFunBind, mkVarBind, mkHsVarBind, mk_easy_FunBind, mkTopFunBind, mkPatSynBind,

  -- Literals
  mkHsIntegral, mkHsFractional, mkHsIsString, mkHsString,

  -- Patterns
  mkNPat, mkNPlusKPat, nlVarPat, nlLitPat, nlConVarPat, nlConPat,
  nlConPatName, nlInfixConPat, nlNullaryConPat, nlWildConPat, nlWildPat,
  nlWildPatName, nlWildPatId, nlTuplePat, mkParPat,

  -- Types
  mkHsAppTy, userHsTyVarBndrs,
  nlHsAppTy, nlHsTyVar, nlHsFunTy, nlHsTyConApp,

  -- Stmts
  mkTransformStmt, mkTransformByStmt, mkBodyStmt, mkBindStmt, mkLastStmt,
  emptyTransStmt, mkGroupUsingStmt, mkGroupByUsingStmt,
  emptyRecStmt, emptyRecStmtName, emptyRecStmtId, mkRecStmt,

  -- Template Haskell
  mkHsSpliceTy, mkHsSpliceE, mkHsSpliceTE, mkHsSplice,
  mkHsQuasiQuote, unqualQuasiQuote,

  -- Flags
  noRebindableInfo,

  -- Collecting binders
  collectLocalBinders, collectHsValBinders, collectHsBindListBinders,
  collectHsBindsBinders, collectHsBindBinders, collectMethodBinders,
  collectPatBinders, collectPatsBinders,
  collectLStmtsBinders, collectStmtsBinders,
  collectLStmtBinders, collectStmtBinders,

  hsLTyClDeclBinders, hsTyClDeclsBinders,
  hsForeignDeclsBinders, hsGroupBinders, hsDataFamInstBinders,

  -- Collecting implicit binders
  lStmtsImplicits, hsValBindsImplicits, lPatImplicits
  ) where

#include "HsVersions.h"

import HsDecls
import HsBinds
import HsExpr
import HsPat
import HsTypes
import HsLit
import PlaceHolder

import TcEvidence
import RdrName
import Var
import TypeRep
import TcType
import Kind
import DataCon
import Name
import NameSet
import BasicTypes
import SrcLoc
import FastString
import Util
import Bag
import Outputable

import Data.Either
import Data.Function
import Data.List
\end{code}


%************************************************************************
%*                                                                      *
        Some useful helpers for constructing syntax
%*                                                                      *
%************************************************************************

These functions attempt to construct a not-completely-useless SrcSpan
from their components, compared with the nl* functions below which
just attach noSrcSpan to everything.

\begin{code}
mkHsPar :: LHsExpr l id -> LHsExpr l id
mkHsPar e@(L l _) = L l (HsPar e)

mkSimpleMatch :: [LPat SrcSpan id] -> GenLocated SrcSpan (body id)
              -> LMatch SrcSpan id (GenLocated SrcSpan (body id))
mkSimpleMatch pats rhs
  = L loc $
    Match pats Nothing (unguardedGRHSs rhs)
  where
    loc = case pats of
                []      -> getLoc rhs
                (pat:_) -> combineSrcSpans (getLoc pat) (getLoc rhs)

unguardedGRHSs :: GenLocated l (body id) -> GRHSs l id (GenLocated l (body id))
unguardedGRHSs rhs = GRHSs (unguardedRHS rhs) emptyLocalBinds

unguardedRHS :: GenLocated l (body id) -> [LGRHS l id (GenLocated l (body id))]
unguardedRHS rhs@(L loc _) = [L loc (GRHS [] rhs)]

mkMatchGroup :: Origin -> [LMatch l RdrName (GenLocated l (body l RdrName))]
             -> MatchGroup l RdrName (GenLocated l (body l RdrName))
mkMatchGroup origin matches = MG { mg_alts = matches, mg_arg_tys = []
                                 , mg_res_ty = placeHolderType
                                 , mg_origin = origin }

mkMatchGroupName :: Origin -> [LMatch l Name (GenLocated l (body l Name))]
             -> MatchGroup l Name (GenLocated l (body l Name))
mkMatchGroupName origin matches = MG { mg_alts = matches, mg_arg_tys = []
                                     , mg_res_ty = placeHolderType
                                     , mg_origin = origin }

mkHsAppTy :: LHsType SrcSpan name -> LHsType SrcSpan name
          -> LHsType SrcSpan name
mkHsAppTy t1 t2 = addCLoc t1 t2 (HsAppTy t1 t2)

mkHsApp :: LHsExpr SrcSpan name -> LHsExpr SrcSpan name -> LHsExpr SrcSpan name
mkHsApp e1 e2 = addCLoc e1 e2 (HsApp e1 e2)

mkHsLam :: [LPat SrcSpan RdrName] -> LHsExpr SrcSpan RdrName
        -> LHsExpr SrcSpan RdrName
mkHsLam pats body = mkHsPar (L (getLoc body) (HsLam matches))
        where
          matches = mkMatchGroup Generated [mkSimpleMatch pats body]

mkHsLams :: [TyVar] -> [EvVar] -> LHsExpr l Id -> LHsExpr l Id
mkHsLams tyvars dicts expr = mkLHsWrap (mkWpTyLams tyvars
                                       <.> mkWpLams dicts) expr

mkHsConApp :: DataCon -> [Type] -> [HsExpr SrcSpan Id] -> LHsExpr SrcSpan Id
-- Used for constructing dictionary terms etc, so no locations
mkHsConApp data_con tys args
  = foldl mk_app (nlHsTyApp (dataConWrapId data_con) tys) args
  where
    mk_app f a = noLoc (HsApp f (noLoc a))

mkSimpleHsAlt :: LPat SrcSpan id -> (GenLocated SrcSpan (body SrcSpan id))
              -> LMatch SrcSpan id (GenLocated SrcSpan (body SrcSpan id))
-- A simple lambda with a single pattern, no binds, no guards; pre-typechecking
mkSimpleHsAlt pat expr
  = mkSimpleMatch [pat] expr

nlHsTyApp :: name -> [Type] -> LHsExpr SrcSpan name
nlHsTyApp fun_id tys = noLoc (HsWrap (mkWpTyApps tys) (HsVar fun_id))

--------- Adding parens ---------
mkLHsPar :: LHsExpr l name -> LHsExpr l name
-- Wrap in parens if hsExprNeedsParens says it needs them
-- So   'f x'  becomes '(f x)', but '3' stays as '3'
mkLHsPar le@(L loc e) | hsExprNeedsParens e = L loc (HsPar le)
                      | otherwise           = le

mkParPat :: LPat l name -> LPat l name
mkParPat lp@(L loc p) | hsPatNeedsParens p = L loc (ParPat lp)
                      | otherwise          = lp


-------------------------------
-- These are the bits of syntax that contain rebindable names
-- See RnEnv.lookupSyntaxName

mkHsIntegral   :: Integer -> PostTc RdrName Type -> HsOverLit l RdrName
mkHsFractional :: FractionalLit -> PostTc RdrName Type -> HsOverLit l RdrName
mkHsIsString   :: FastString -> PostTc RdrName Type -> HsOverLit l RdrName
mkHsDo         :: HsStmtContext Name -> [ExprLStmt l RdrName] -> HsExpr l RdrName
mkHsComp       :: HsStmtContext Name -> [ExprLStmt l RdrName] -> LHsExpr l RdrName
               -> HsExpr l RdrName

mkNPat      :: HsOverLit l id -> Maybe (SyntaxExpr l id) -> Pat l id
mkNPlusKPat :: GenLocated l id -> HsOverLit l id -> Pat l id

mkLastStmt :: GenLocated l (bodyR l idR) -> StmtLR l idL idR (GenLocated l (bodyR l idR))
mkBodyStmt :: GenLocated l (bodyR l RdrName)
           -> StmtLR l idL RdrName (GenLocated l (bodyR l RdrName))
mkBindStmt :: LPat l idL -> GenLocated l (bodyR l idR) -> StmtLR l idL idR (GenLocated l (bodyR l idR))

emptyRecStmt     :: StmtLR l idL  RdrName bodyR
emptyRecStmtName :: StmtLR l Name Name    bodyR
emptyRecStmtId   :: StmtLR l Id   Id      bodyR
mkRecStmt    :: [LStmtLR l idL RdrName bodyR] -> StmtLR l idL RdrName bodyR


mkHsIntegral   i       = OverLit (HsIntegral   i)  noRebindableInfo noSyntaxExpr
mkHsFractional f       = OverLit (HsFractional f)  noRebindableInfo noSyntaxExpr
mkHsIsString   s       = OverLit (HsIsString   s)  noRebindableInfo noSyntaxExpr

noRebindableInfo :: PlaceHolder
noRebindableInfo = PlaceHolder -- Just another placeholder;

mkHsDo ctxt stmts = HsDo ctxt stmts placeHolderType
mkHsComp ctxt stmts expr = mkHsDo ctxt (stmts ++ [last_stmt])
  where
    last_stmt = L (getLoc expr) $ mkLastStmt expr

mkHsIf :: LHsExpr l id -> LHsExpr l id -> LHsExpr l id -> HsExpr l id
mkHsIf c a b = HsIf (Just noSyntaxExpr) c a b

mkNPat lit neg     = NPat lit neg noSyntaxExpr
mkNPlusKPat id lit = NPlusKPat id lit noSyntaxExpr noSyntaxExpr

mkTransformStmt    :: (SrcAnnotation l) => [ExprLStmt l idL] -> LHsExpr l idR
                   -> StmtLR l idL idR (LHsExpr l idL)
mkTransformByStmt  :: (SrcAnnotation l) => [ExprLStmt l idL] -> LHsExpr l idR
                   -> LHsExpr l idR
                   -> StmtLR l idL idR (LHsExpr l idL)
mkGroupUsingStmt   :: (SrcAnnotation l) => [ExprLStmt l idL]
                   -> LHsExpr l idR
                   -> StmtLR l idL idR (LHsExpr l idL)
mkGroupByUsingStmt :: (SrcAnnotation l) => [ExprLStmt l idL] -> LHsExpr l idR
                   -> LHsExpr l idR
                   -> StmtLR l idL idR (LHsExpr l idL)

emptyTransStmt :: (SrcAnnotation l) => StmtLR l idL idR (LHsExpr l idR)
emptyTransStmt = TransStmt { trS_form = panic "emptyTransStmt: form"
                           , trS_stmts = [], trS_bndrs = []
                           , trS_by = Nothing, trS_using = annNoLoc noSyntaxExpr
                           , trS_ret = noSyntaxExpr, trS_bind = noSyntaxExpr
                           , trS_fmap = noSyntaxExpr }
mkTransformStmt    ss u   = emptyTransStmt { trS_form = ThenForm,  trS_stmts = ss, trS_using = u }
mkTransformByStmt  ss u b = emptyTransStmt { trS_form = ThenForm,  trS_stmts = ss, trS_using = u, trS_by = Just b }
mkGroupUsingStmt   ss u   = emptyTransStmt { trS_form = GroupForm, trS_stmts = ss, trS_using = u }
mkGroupByUsingStmt ss b u = emptyTransStmt { trS_form = GroupForm, trS_stmts = ss, trS_using = u, trS_by = Just b }

mkLastStmt body     = LastStmt body noSyntaxExpr
mkBodyStmt body     = BodyStmt body noSyntaxExpr noSyntaxExpr placeHolderType
mkBindStmt pat body = BindStmt pat body noSyntaxExpr noSyntaxExpr


emptyRecStmt' :: forall idL idR body l.
                       PostTc idR Type -> StmtLR l idL idR body
emptyRecStmt' tyVal =
   RecStmt
     { recS_stmts = [], recS_later_ids = []
     , recS_rec_ids = []
     , recS_ret_fn = noSyntaxExpr
     , recS_mfix_fn = noSyntaxExpr
     , recS_bind_fn = noSyntaxExpr, recS_later_rets = []
     , recS_rec_rets = [], recS_ret_ty = tyVal }

emptyRecStmt     = emptyRecStmt' placeHolderType
emptyRecStmtName = emptyRecStmt' placeHolderType
emptyRecStmtId   = emptyRecStmt' placeHolderTypeTc
mkRecStmt stmts  = emptyRecStmt { recS_stmts = stmts }

-------------------------------
--- A useful function for building @OpApps@.  The operator is always a
-- variable, and we don't know the fixity yet.
mkHsOpApp :: (SrcAnnotation l) => LHsExpr l id -> id -> LHsExpr l id -> HsExpr l id
mkHsOpApp e1 op e2 = OpApp e1 (annNoLoc (HsVar op)) (error "mkOpApp:fixity") e2

mkHsSplice :: LHsExpr l RdrName -> HsSplice l RdrName
mkHsSplice e = HsSplice unqualSplice e

mkHsSpliceE :: LHsExpr l RdrName -> HsExpr l RdrName
mkHsSpliceE e = HsSpliceE False (mkHsSplice e)

mkHsSpliceTE :: LHsExpr l RdrName -> HsExpr l RdrName
mkHsSpliceTE e = HsSpliceE True (mkHsSplice e)

mkHsSpliceTy :: LHsExpr l RdrName -> HsType l RdrName
mkHsSpliceTy e = HsSpliceTy (mkHsSplice e) placeHolderKind

unqualSplice :: RdrName
unqualSplice = mkRdrUnqual (mkVarOccFS (fsLit "splice"))
                -- A name (uniquified later) to
                -- identify the splice

mkHsQuasiQuote :: RdrName -> SrcSpan -> FastString -> HsQuasiQuote RdrName
mkHsQuasiQuote quoter span quote = HsQuasiQuote quoter span quote

unqualQuasiQuote :: RdrName
unqualQuasiQuote = mkRdrUnqual (mkVarOccFS (fsLit "quasiquote"))
                -- A name (uniquified later) to
                -- identify the quasi-quote

mkHsString :: String -> HsLit
mkHsString s = HsString (mkFastString s)

-------------
userHsTyVarBndrs :: l -> [name] -> [GenLocated l (HsTyVarBndr l name)]
-- Caller sets location
userHsTyVarBndrs loc bndrs = [ L loc (UserTyVar v) | v <- bndrs ]
\end{code}


%************************************************************************
%*                                                                      *
        Constructing syntax with no location info
%*                                                                      *
%************************************************************************

\begin{code}
nlHsVar :: (SrcAnnotation l) => id -> LHsExpr l id
nlHsVar n = annNoLoc (HsVar n)

nlHsLit :: (SrcAnnotation l) => HsLit -> LHsExpr l id
nlHsLit n = annNoLoc (HsLit n)

nlVarPat :: (SrcAnnotation l) => id -> LPat l id
nlVarPat n = annNoLoc (VarPat n)

nlLitPat :: (SrcAnnotation l) => HsLit -> LPat l id
nlLitPat l = annNoLoc (LitPat l)

nlHsApp :: (SrcAnnotation l) => LHsExpr l id -> LHsExpr l id -> LHsExpr l id
nlHsApp f x = annNoLoc (HsApp f x)

nlHsIntLit :: (SrcAnnotation l) => Integer -> LHsExpr l id
nlHsIntLit n = annNoLoc (HsLit (HsInt n))

nlHsApps :: (SrcAnnotation l) => id -> [LHsExpr l id] -> LHsExpr l id
nlHsApps f xs = foldl nlHsApp (nlHsVar f) xs

nlHsVarApps :: (SrcAnnotation l) => id -> [id] -> LHsExpr l id
nlHsVarApps f xs = annNoLoc (foldl mk (HsVar f) (map HsVar xs))
                 where
                   mk f a = HsApp (annNoLoc f) (annNoLoc a)

nlConVarPat :: (SrcAnnotation l) => RdrName -> [RdrName] -> LPat l RdrName
nlConVarPat con vars = nlConPat con (map nlVarPat vars)

nlInfixConPat :: (SrcAnnotation l) => id -> LPat l id -> LPat l id -> LPat l id
nlInfixConPat con l r = annNoLoc (ConPatIn (annNoLoc con) (InfixCon l r))

nlConPat :: (SrcAnnotation l) => RdrName -> [LPat l RdrName] -> LPat l RdrName
nlConPat con pats = annNoLoc (ConPatIn (annNoLoc con) (PrefixCon pats))

nlConPatName :: (SrcAnnotation l) => Name -> [LPat l Name] -> LPat l Name
nlConPatName con pats = annNoLoc (ConPatIn (annNoLoc con) (PrefixCon pats))

nlNullaryConPat :: (SrcAnnotation l) => id -> LPat l id
nlNullaryConPat con = annNoLoc (ConPatIn (annNoLoc con) (PrefixCon []))

nlWildConPat :: (SrcAnnotation l) => DataCon -> LPat l RdrName
nlWildConPat con = annNoLoc (ConPatIn (annNoLoc (getRdrName con))
                         (PrefixCon (nOfThem (dataConSourceArity con)
                                             nlWildPat)))

nlWildPat :: (SrcAnnotation l) => LPat l RdrName
nlWildPat  = annNoLoc (WildPat placeHolderType )  -- Pre-typechecking

nlWildPatName :: (SrcAnnotation l) => LPat l Name
nlWildPatName  = annNoLoc (WildPat placeHolderType )  -- Pre-typechecking

nlWildPatId :: (SrcAnnotation l) => LPat l Id
nlWildPatId  = annNoLoc (WildPat placeHolderTypeTc )  -- Post-typechecking

nlHsDo :: (SrcAnnotation l) =>
          HsStmtContext Name -> [LStmt l RdrName (LHsExpr l RdrName)]
       -> LHsExpr l RdrName
nlHsDo ctxt stmts = annNoLoc (mkHsDo ctxt stmts)

nlHsOpApp :: (SrcAnnotation l)
          => LHsExpr l id -> id -> LHsExpr l id -> LHsExpr l id
nlHsOpApp e1 op e2 = annNoLoc (mkHsOpApp e1 op e2)

nlHsLam  :: (SrcAnnotation l)
         => LMatch l RdrName (LHsExpr l RdrName) -> LHsExpr l RdrName
nlHsPar  :: (SrcAnnotation l)
         => LHsExpr l id -> LHsExpr l id
nlHsIf   :: (SrcAnnotation l)
         => LHsExpr l id -> LHsExpr l id -> LHsExpr l id -> LHsExpr l id
nlHsCase :: (SrcAnnotation l)
         => LHsExpr l RdrName -> [LMatch l RdrName (LHsExpr l RdrName)]
         -> LHsExpr l RdrName
nlList   :: (SrcAnnotation l) => [LHsExpr l RdrName] -> LHsExpr l RdrName

nlHsLam match          = annNoLoc (HsLam (mkMatchGroup Generated [match]))
nlHsPar e              = annNoLoc (HsPar e)
nlHsIf cond true false = annNoLoc (mkHsIf cond true false)
nlHsCase expr matches  = annNoLoc (HsCase expr (mkMatchGroup Generated matches))
nlList exprs           = annNoLoc (ExplicitList placeHolderType Nothing exprs)

nlHsAppTy :: (SrcAnnotation l)
          => LHsType l name -> LHsType l name -> LHsType l name
nlHsTyVar :: (SrcAnnotation l)
          => name                             -> LHsType l name
nlHsFunTy :: (SrcAnnotation l)
          => LHsType l name -> LHsType l name -> LHsType l name

nlHsAppTy f t           = annNoLoc (HsAppTy f t)
nlHsTyVar x             = annNoLoc (HsTyVar x)
nlHsFunTy a b           = annNoLoc (HsFunTy a b)

nlHsTyConApp :: (SrcAnnotation l) => name -> [LHsType l name] -> LHsType l name
nlHsTyConApp tycon tys  = foldl nlHsAppTy (nlHsTyVar tycon) tys
\end{code}

Tuples.  All these functions are *pre-typechecker* because they lack
types on the tuple.

\begin{code}
mkLHsTupleExpr :: (SrcAnnotation l) => [LHsExpr l a] -> LHsExpr l a
-- Makes a pre-typechecker boxed tuple, deals with 1 case
mkLHsTupleExpr [e] = e
mkLHsTupleExpr es  = annNoLoc $ ExplicitTuple (map Present es) Boxed

mkLHsVarTuple :: (SrcAnnotation l) => [a] -> LHsExpr l a
mkLHsVarTuple ids  = mkLHsTupleExpr (map nlHsVar ids)

nlTuplePat :: (SrcAnnotation l) => [LPat l id] -> Boxity -> LPat l id
nlTuplePat pats box = annNoLoc (TuplePat pats box [])

missingTupArg :: HsTupArg l RdrName
missingTupArg = Missing placeHolderType
\end{code}


%************************************************************************
%*                                                                      *
        Converting a Type to an HsType RdrName
%*                                                                      *
%************************************************************************

This is needed to implement GeneralizedNewtypeDeriving.

\begin{code}
toHsType :: (SrcAnnotation l) => Type -> LHsType l RdrName
toHsType ty
  | [] <- tvs_only
  , [] <- theta
  = to_hs_type tau
  | otherwise
  = annNoLoc $
    mkExplicitHsForAllTy (map mk_hs_tvb tvs_only)
                         (annNoLoc $ map toHsType theta)
                         (to_hs_type tau)

  where
    (tvs, theta, tau) = tcSplitSigmaTy ty
    tvs_only = filter isTypeVar tvs

    to_hs_type (TyVarTy tv) = nlHsTyVar (getRdrName tv)
    to_hs_type (AppTy t1 t2) = nlHsAppTy (toHsType t1) (toHsType t2)
    to_hs_type (TyConApp tc args) = nlHsTyConApp (getRdrName tc) (map toHsType args')
       where
         args' = filterOut isKind args
         -- Source-language types have _implicit_ kind arguments,
         -- so we must remove them here (Trac #8563)
    to_hs_type (FunTy arg res) = ASSERT( not (isConstraintKind (typeKind arg)) )
                                 nlHsFunTy (toHsType arg) (toHsType res)
    to_hs_type t@(ForAllTy {}) = pprPanic "toHsType" (ppr t)
    to_hs_type (LitTy (NumTyLit n)) = annNoLoc $ HsTyLit (HsNumTy n)
    to_hs_type (LitTy (StrTyLit s)) = annNoLoc $ HsTyLit (HsStrTy s)

    mk_hs_tvb tv = annNoLoc $ KindedTyVar (getRdrName tv) (toHsKind (tyVarKind tv))

toHsKind :: (SrcAnnotation l) => Kind -> LHsKind l RdrName
toHsKind = toHsType

\end{code}

\begin{code}
--------- HsWrappers: type args, dict args, casts ---------
mkLHsWrap :: HsWrapper -> LHsExpr l id -> LHsExpr l id
mkLHsWrap co_fn (L loc e) = L loc (mkHsWrap co_fn e)

mkHsWrap :: HsWrapper -> HsExpr l id -> HsExpr l id
mkHsWrap co_fn e | isIdHsWrapper co_fn = e
                 | otherwise           = HsWrap co_fn e

mkHsWrapCo :: TcCoercion   -- A Nominal coercion  a ~N b
           -> HsExpr l id -> HsExpr l id
mkHsWrapCo co e = mkHsWrap (coToHsWrapper co) e

mkHsWrapCoR :: TcCoercion   -- A Representational coercion  a ~R b
            -> HsExpr l id -> HsExpr l id
mkHsWrapCoR co e = mkHsWrap (coToHsWrapperR co) e

mkLHsWrapCo :: TcCoercion -> LHsExpr l id -> LHsExpr l id
mkLHsWrapCo co (L loc e) = L loc (mkHsWrapCo co e)

mkHsCmdCast :: TcCoercion -> HsCmd l id -> HsCmd l id
mkHsCmdCast co cmd | isTcReflCo co = cmd
                   | otherwise     = HsCmdCast co cmd

coToHsWrapper :: TcCoercion -> HsWrapper   -- A Nominal coercion
coToHsWrapper co | isTcReflCo co = idHsWrapper
                 | otherwise     = mkWpCast (mkTcSubCo co)

coToHsWrapperR :: TcCoercion -> HsWrapper   -- A Representational coercion
coToHsWrapperR co | isTcReflCo co = idHsWrapper
                  | otherwise     = mkWpCast co

mkHsWrapPat :: HsWrapper -> Pat l id -> Type -> Pat l id
mkHsWrapPat co_fn p ty | isIdHsWrapper co_fn = p
                       | otherwise           = CoPat co_fn p ty

mkHsWrapPatCo :: TcCoercion -> Pat l id -> Type -> Pat l id
mkHsWrapPatCo co pat ty | isTcReflCo co = pat
                        | otherwise     = CoPat (mkWpCast co) pat ty

mkHsDictLet :: TcEvBinds -> LHsExpr l Id -> LHsExpr l Id
mkHsDictLet ev_binds expr = mkLHsWrap (mkWpLet ev_binds) expr
\end{code}
l
%************************************************************************
%*                                                                      *
                Bindings; with a location at the top
%*                                                                      *
%************************************************************************

\begin{code}
mkFunBind :: GenLocated l RdrName -> [LMatch l RdrName (LHsExpr l RdrName)]
          -> HsBind l RdrName
-- Not infix, with place holders for coercion and free vars
mkFunBind fn ms = FunBind { fun_id = fn, fun_infix = False
                          , fun_matches = mkMatchGroup Generated ms
                          , fun_co_fn = idHsWrapper
                          , bind_fvs = placeHolderNames
                          , fun_tick = Nothing }

mkTopFunBind :: Origin -> GenLocated l Name -> [LMatch l Name (LHsExpr l Name)]
             -> HsBind l Name
-- In Name-land, with empty bind_fvs
mkTopFunBind origin fn ms = FunBind { fun_id = fn, fun_infix = False
                                    , fun_matches = mkMatchGroupName origin ms
                                    , fun_co_fn = idHsWrapper
                                    , bind_fvs = emptyNameSet -- NB: closed
                                                              --     binding
                                    , fun_tick = Nothing }

mkHsVarBind :: (SrcAnnotation l)
            => l -> RdrName -> LHsExpr l RdrName -> LHsBind l RdrName
mkHsVarBind loc var rhs = mk_easy_FunBind loc var [] rhs

mkVarBind :: (SrcAnnotation l) => id -> LHsExpr l id -> LHsBind l id
mkVarBind var rhs@(L lr _) = L lr $
                    VarBind { var_id = var, var_rhs = rhs, var_inline = False }

mkPatSynBind :: GenLocated l RdrName -> HsPatSynDetails (GenLocated l RdrName)
             -> LPat l RdrName -> HsPatSynDir l RdrName -> HsBind l RdrName
mkPatSynBind name details lpat dir = PatSynBind psb
  where
    psb = PSB{ psb_id = name
             , psb_args = details
             , psb_def = lpat
             , psb_dir = dir
             , psb_fvs = placeHolderNames }

------------
mk_easy_FunBind :: (SrcAnnotation l) => l -> RdrName -> [LPat l RdrName]
                -> LHsExpr l RdrName -> LHsBind l RdrName
mk_easy_FunBind loc fun pats expr
  = L loc $ mkFunBind (L loc fun) [mkMatch pats expr emptyLocalBinds]

------------
mkMatch :: (SrcAnnotation l) => [LPat l id] -> LHsExpr l id -> HsLocalBinds l id
        -> LMatch l id (LHsExpr l id)
mkMatch pats expr binds
  = annNoLoc (Match (map paren pats) Nothing
                 (GRHSs (unguardedRHS expr) binds))
  where
    paren lp@(L l p) | hsPatNeedsParens p = L l (ParPat lp)
                     | otherwise          = lp
\end{code}


%************************************************************************
%*                                                                      *
        Collecting binders
%*                                                                      *
%************************************************************************

Get all the binders in some HsBindGroups, IN THE ORDER OF APPEARANCE. eg.

...
where
  (x, y) = ...
  f i j  = ...
  [a, b] = ...

it should return [x, y, f, a, b] (remember, order important).

Note [Collect binders only after renaming]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
These functions should only be used on HsSyn *after* the renamer,
to return a [Name] or [Id].  Before renaming the record punning
and wild-card mechanism makes it hard to know what is bound.
So these functions should not be applied to (HsSyn RdrName)

\begin{code}
----------------- Bindings --------------------------
collectLocalBinders :: HsLocalBindsLR l idL idR -> [idL]
collectLocalBinders (HsValBinds val_binds) = collectHsValBinders val_binds
collectLocalBinders (HsIPBinds _)   = []
collectLocalBinders EmptyLocalBinds = []

collectHsValBinders :: HsValBindsLR l idL idR -> [idL]
collectHsValBinders (ValBindsIn  binds _) = collectHsBindsBinders binds
collectHsValBinders (ValBindsOut binds _) = foldr collect_one [] binds
  where
   collect_one (_,binds) acc = collect_binds binds acc

collectHsBindBinders :: HsBindLR l idL idR -> [idL]
collectHsBindBinders b = collect_bind b []

collect_bind :: HsBindLR l idL idR -> [idL] -> [idL]
collect_bind (PatBind { pat_lhs = p })    acc = collect_lpat p acc
collect_bind (FunBind { fun_id = L _ f }) acc = f : acc
collect_bind (VarBind { var_id = f })     acc = f : acc
collect_bind (AbsBinds { abs_exports = dbinds, abs_binds = _binds }) acc
  = map abe_poly dbinds ++ acc
        -- ++ foldr collect_bind acc binds
        -- I don't think we want the binders from the nested binds
        -- The only time we collect binders from a typechecked
        -- binding (hence see AbsBinds) is in zonking in TcHsSyn
collect_bind (PatSynBind (PSB { psb_id = L _ ps })) acc = ps : acc

collectHsBindsBinders :: LHsBindsLR l idL idR -> [idL]
collectHsBindsBinders binds = collect_binds binds []

collectHsBindListBinders :: [LHsBindLR l idL idR] -> [idL]
collectHsBindListBinders = foldr (collect_bind . unLoc) []

collect_binds :: LHsBindsLR l idL idR -> [idL] -> [idL]
collect_binds binds acc = foldrBag (collect_bind . unLoc) acc binds

collectMethodBinders :: LHsBindsLR l RdrName idR -> [GenLocated l RdrName]
-- Used exclusively for the bindings of an instance decl which are all FunBinds
collectMethodBinders binds = foldrBag (get . unLoc) [] binds
  where
    get (FunBind { fun_id = f }) fs = f : fs
    get _                        fs = fs
       -- Someone else complains about non-FunBinds

----------------- Statements --------------------------
collectLStmtsBinders :: [LStmtLR l idL idR body] -> [idL]
collectLStmtsBinders = concatMap collectLStmtBinders

collectStmtsBinders :: [StmtLR l idL idR body] -> [idL]
collectStmtsBinders = concatMap collectStmtBinders

collectLStmtBinders :: LStmtLR l idL idR body -> [idL]
collectLStmtBinders = collectStmtBinders . unLoc

collectStmtBinders :: StmtLR l idL idR body -> [idL]
  -- Id Binders for a Stmt... [but what about pattern-sig type vars]?
collectStmtBinders (BindStmt pat _ _ _) = collectPatBinders pat
collectStmtBinders (LetStmt binds)      = collectLocalBinders binds
collectStmtBinders (BodyStmt {})        = []
collectStmtBinders (LastStmt {})        = []
collectStmtBinders (ParStmt xs _ _)     = collectLStmtsBinders
                                        $ [s | ParStmtBlock ss _ _ <- xs, s <- ss]
collectStmtBinders (TransStmt { trS_stmts = stmts }) = collectLStmtsBinders stmts
collectStmtBinders (RecStmt { recS_stmts = ss })     = collectLStmtsBinders ss


----------------- Patterns --------------------------
collectPatBinders :: LPat l a -> [a]
collectPatBinders pat = collect_lpat pat []

collectPatsBinders :: [LPat l a] -> [a]
collectPatsBinders pats = foldr collect_lpat [] pats

-------------
collect_lpat :: LPat l name -> [name] -> [name]
collect_lpat (L _ pat) bndrs
  = go pat
  where
    go (VarPat var)               = var : bndrs
    go (WildPat _)                = bndrs
    go (LazyPat pat)              = collect_lpat pat bndrs
    go (BangPat pat)              = collect_lpat pat bndrs
    go (AsPat (L _ a) pat)        = a : collect_lpat pat bndrs
    go (ViewPat _ pat _)          = collect_lpat pat bndrs
    go (ParPat  pat)              = collect_lpat pat bndrs

    go (ListPat pats _ _)         = foldr collect_lpat bndrs pats
    go (PArrPat pats _)           = foldr collect_lpat bndrs pats
    go (TuplePat pats _ _)        = foldr collect_lpat bndrs pats

    go (ConPatIn _ ps)            = foldr collect_lpat bndrs (hsConPatArgs ps)
    go (ConPatOut {pat_args=ps})  = foldr collect_lpat bndrs (hsConPatArgs ps)
        -- See Note [Dictionary binders in ConPatOut]
    go (LitPat _)                 = bndrs
    go (NPat _ _ _)               = bndrs
    go (NPlusKPat (L _ n) _ _ _)  = n : bndrs

    go (SigPatIn pat _)           = collect_lpat pat bndrs
    go (SigPatOut pat _)          = collect_lpat pat bndrs
    go (SplicePat _)              = bndrs
    go (QuasiQuotePat _)          = bndrs
    go (CoPat _ pat _)            = go pat
\end{code}

Note [Dictionary binders in ConPatOut] See also same Note in DsArrows
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Do *not* gather (a) dictionary and (b) dictionary bindings as binders
of a ConPatOut pattern.  For most calls it doesn't matter, because
it's pre-typechecker and there are no ConPatOuts.  But it does matter
more in the desugarer; for example, DsUtils.mkSelectorBinds uses
collectPatBinders.  In a lazy pattern, for example f ~(C x y) = ...,
we want to generate bindings for x,y but not for dictionaries bound by
C.  (The type checker ensures they would not be used.)

Desugaring of arrow case expressions needs these bindings (see DsArrows
and arrowcase1), but SPJ (Jan 2007) says it's safer for it to use its
own pat-binder-collector:

Here's the problem.  Consider

data T a where
   C :: Num a => a -> Int -> T a

f ~(C (n+1) m) = (n,m)

Here, the pattern (C (n+1)) binds a hidden dictionary (d::Num a),
and *also* uses that dictionary to match the (n+1) pattern.  Yet, the
variables bound by the lazy pattern are n,m, *not* the dictionary d.
So in mkSelectorBinds in DsUtils, we want just m,n as the variables bound.

\begin{code}
hsGroupBinders :: HsGroup l Name -> [Name]
hsGroupBinders (HsGroup { hs_valds = val_decls, hs_tyclds = tycl_decls,
                          hs_instds = inst_decls, hs_fords = foreign_decls })
-- Collect the binders of a Group
  =  collectHsValBinders val_decls
  ++ hsTyClDeclsBinders tycl_decls inst_decls
  ++ hsForeignDeclsBinders foreign_decls

hsForeignDeclsBinders :: [LForeignDecl l Name] -> [Name]
hsForeignDeclsBinders foreign_decls
  = [n | L _ (ForeignImport (L _ n) _ _ _) <- foreign_decls]

hsTyClDeclsBinders :: [TyClGroup l Name] -> [GenLocated l (InstDecl l Name)]
                   -> [Name]
-- We need to look at instance declarations too,
-- because their associated types may bind data constructors
hsTyClDeclsBinders tycl_decls inst_decls
  = map unLoc (concatMap (concatMap hsLTyClDeclBinders . group_tyclds) tycl_decls ++
               concatMap (hsInstDeclBinders . unLoc) inst_decls)

-------------------
hsLTyClDeclBinders :: Eq name => GenLocated l (TyClDecl l name)
                   -> [GenLocated l name]
-- ^ Returns all the /binding/ names of the decl.
-- The first one is guaranteed to be the name of the decl. For record fields
-- mentioned in multiple constructors, the SrcLoc will be from the first
-- occurrence.  We use the equality to filter out duplicate field names.
--
-- Each returned (Located name) is wrapped in a @SrcSpan@ of the whole
-- /declaration/, not just the name itself (which is how it appears in
-- the syntax tree).  This SrcSpan (for the entire declaration) is used
-- as the SrcSpan for the Name that is finally produced, and hence for
-- error messages.  (See Trac #8607.)

hsLTyClDeclBinders (L loc (FamDecl { tcdFam = FamilyDecl { fdLName = L _ name } }))
  = [L loc name]
hsLTyClDeclBinders (L loc (ForeignType { tcdLName = L _ name })) = [L loc name]
hsLTyClDeclBinders (L loc (SynDecl     { tcdLName = L _ name })) = [L loc name]
hsLTyClDeclBinders (L loc (ClassDecl   { tcdLName = L _ cls_name
                                       , tcdSigs = sigs, tcdATs = ats }))
  = L loc cls_name :
    [ L fam_loc fam_name | L fam_loc (FamilyDecl { fdLName = L _ fam_name }) <- ats ] ++
    [ L mem_loc mem_name | L mem_loc (TypeSig ns _) <- sigs, L _ mem_name <- ns ]
hsLTyClDeclBinders (L loc (DataDecl    { tcdLName = L _ name, tcdDataDefn = defn }))
  = L loc name : hsDataDefnBinders defn

-------------------
hsInstDeclBinders :: Eq name => InstDecl l name -> [GenLocated l name]
hsInstDeclBinders (ClsInstD { cid_inst = ClsInstDecl { cid_datafam_insts = dfis } })
  = concatMap (hsDataFamInstBinders . unLoc) dfis
hsInstDeclBinders (DataFamInstD { dfid_inst = fi }) = hsDataFamInstBinders fi
hsInstDeclBinders (TyFamInstD {}) = []

-------------------
-- the SrcLoc returned are for the whole declarations, not just the names
hsDataFamInstBinders :: Eq name => DataFamInstDecl l name -> [GenLocated l name]
hsDataFamInstBinders (DataFamInstDecl { dfid_defn = defn })
  = hsDataDefnBinders defn
  -- There can't be repeated symbols because only data instances have binders

-------------------
-- the SrcLoc returned are for the whole declarations, not just the names
hsDataDefnBinders :: Eq name => HsDataDefn l name -> [GenLocated l name]
hsDataDefnBinders (HsDataDefn { dd_cons = cons }) = hsConDeclsBinders cons
  -- See Note [Binders in family instances]

-------------------
hsConDeclsBinders :: forall name l. (Eq name) => [LConDecl l name]
                  -> [GenLocated l name]
  -- See hsLTyClDeclBinders for what this does
  -- The function is boringly complicated because of the records
  -- And since we only have equality, we have to be a little careful
hsConDeclsBinders cons = go id cons
  where go :: ([GenLocated l name] -> [GenLocated l name]) -> [LConDecl l name]
           -> [GenLocated l name]
        go _ [] = []
        go remSeen (r:rs) =
          -- don't re-mangle the location of field names, because we don't
          -- have a record of the full location of the field declaration anyway
          case r of
             -- remove only the first occurrence of any seen field in order to
             -- avoid circumventing detection of duplicate fields (#9156)
             L loc (ConDecl { con_name = L _ name , con_details = RecCon flds }) ->
               (L loc name) : r' ++ go remSeen' rs
                  where r' = remSeen (map cd_fld_name flds)
                        remSeen' = foldr (.) remSeen [deleteBy ((==) `on` unLoc) v | v <- r']
             L loc (ConDecl { con_name = L _ name }) ->
                (L loc name) : go remSeen rs

\end{code}

Note [Binders in family instances]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
In a type or data family instance declaration, the type
constructor is an *occurrence* not a binding site
    type instance T Int = Int -> Int   -- No binders
    data instance S Bool = S1 | S2     -- Binders are S1,S2


%************************************************************************
%*                                                                      *
        Collecting binders the user did not write
%*                                                                      *
%************************************************************************

The job of this family of functions is to run through binding sites and find the set of all Names
that were defined "implicitly", without being explicitly written by the user.

The main purpose is to find names introduced by record wildcards so that we can avoid
warning the user when they don't use those names (#4404)

\begin{code}
lStmtsImplicits :: [LStmtLR l Name idR (GenLocated l (body l idR))] -> NameSet
lStmtsImplicits = hs_lstmts
  where
    hs_lstmts :: [LStmtLR l Name idR (GenLocated l (body l idR))] -> NameSet
    hs_lstmts = foldr (\stmt rest -> unionNameSets (hs_stmt (unLoc stmt)) rest) emptyNameSet

    hs_stmt (BindStmt pat _ _ _) = lPatImplicits pat
    hs_stmt (LetStmt binds)      = hs_local_binds binds
    hs_stmt (BodyStmt {})        = emptyNameSet
    hs_stmt (LastStmt {})        = emptyNameSet
    hs_stmt (ParStmt xs _ _)     = hs_lstmts [s | ParStmtBlock ss _ _ <- xs, s <- ss]
    hs_stmt (TransStmt { trS_stmts = stmts }) = hs_lstmts stmts
    hs_stmt (RecStmt { recS_stmts = ss })     = hs_lstmts ss

    hs_local_binds (HsValBinds val_binds) = hsValBindsImplicits val_binds
    hs_local_binds (HsIPBinds _)         = emptyNameSet
    hs_local_binds EmptyLocalBinds       = emptyNameSet

hsValBindsImplicits :: HsValBindsLR l Name idR -> NameSet
hsValBindsImplicits (ValBindsOut binds _)
  = foldr (unionNameSets . lhsBindsImplicits . snd) emptyNameSet binds
hsValBindsImplicits (ValBindsIn binds _)
  = lhsBindsImplicits binds

lhsBindsImplicits :: LHsBindsLR l Name idR -> NameSet
lhsBindsImplicits = foldBag unionNameSets (lhs_bind . unLoc) emptyNameSet
  where
    lhs_bind (PatBind { pat_lhs = lpat }) = lPatImplicits lpat
    lhs_bind _ = emptyNameSet

lPatImplicits :: LPat l Name -> NameSet
lPatImplicits = hs_lpat
  where
    hs_lpat (L _ pat) = hs_pat pat

    hs_lpats = foldr (\pat rest -> hs_lpat pat `unionNameSets` rest) emptyNameSet

    hs_pat (LazyPat pat)       = hs_lpat pat
    hs_pat (BangPat pat)       = hs_lpat pat
    hs_pat (AsPat _ pat)       = hs_lpat pat
    hs_pat (ViewPat _ pat _)   = hs_lpat pat
    hs_pat (ParPat  pat)       = hs_lpat pat
    hs_pat (ListPat pats _ _)  = hs_lpats pats
    hs_pat (PArrPat pats _)    = hs_lpats pats
    hs_pat (TuplePat pats _ _) = hs_lpats pats

    hs_pat (SigPatIn pat _)  = hs_lpat pat
    hs_pat (SigPatOut pat _) = hs_lpat pat
    hs_pat (CoPat _ pat _)   = hs_pat pat

    hs_pat (ConPatIn _ ps)           = details ps
    hs_pat (ConPatOut {pat_args=ps}) = details ps

    hs_pat _ = emptyNameSet

    details (PrefixCon ps)   = hs_lpats ps
    details (RecCon fs)      = hs_lpats explicit `unionNameSets` mkNameSet (collectPatsBinders implicit)
      where (explicit, implicit) = partitionEithers [if pat_explicit then Left pat else Right pat
                                                    | (i, fld) <- [0..] `zip` rec_flds fs
                                                    , let pat = hsRecFieldArg fld
                                                          pat_explicit = maybe True (i<) (rec_dotdot fs)]
    details (InfixCon p1 p2) = hs_lpat p1 `unionNameSets` hs_lpat p2
\end{code}
