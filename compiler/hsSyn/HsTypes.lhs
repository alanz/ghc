%
% (c) The University of Glasgow 2006
% (c) The GRASP/AQUA Project, Glasgow University, 1992-1998
%

HsTypes: Abstract syntax: user-defined types

\begin{code}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE UndecidableInstances #-} -- Note [Pass sensitive types]
                                      -- in module PlaceHolder
{-# LANGUAGE ConstraintKinds #-}

module HsTypes (
        HsType(..), LHsType, HsKind, LHsKind,
        HsTyVarBndr(..), LHsTyVarBndr, 
        LHsTyVarBndrs(..),
        HsWithBndrs(..),
        HsTupleSort(..), HsExplicitFlag(..),
        HsContext, LHsContext,
        HsQuasiQuote(..),
        HsTyWrapper(..),
        HsTyLit(..),
        HsIPName(..), hsIPNameFS,

        LBangType, BangType, HsBang(..), 
        getBangType, getBangStrictness, 

        ConDeclField(..), pprConDeclFields,
        
        mkHsQTvs, hsQTvBndrs, isHsKindedTyVar, hsTvbAllKinded,
        mkExplicitHsForAllTy, mkImplicitHsForAllTy, mkQualifiedHsForAllTy,
        hsExplicitTvs,
        hsTyVarName, mkHsWithBndrs, hsLKiTyVarNames,
        hsLTyVarName, hsLTyVarNames, hsLTyVarLocName, hsLTyVarLocNames,
        splitLHsInstDeclTy_maybe,
        splitHsClassTy_maybe, splitLHsClassTy_maybe,
        splitHsFunType,
        splitHsAppTys, hsTyGetAppHead_maybe, mkHsAppTys, mkHsOpTy,

        -- Printing
        pprParendHsType, pprHsForAll, pprHsContext, pprHsContextNoArrow, 
    ) where

import {-# SOURCE #-} HsExpr ( HsSplice, pprUntypedSplice )

import PlaceHolder ( PostTc,PostRn,DataId,PlaceHolder(..) )

import Name( Name )
import RdrName( RdrName )
import DataCon( HsBang(..) )
import TysPrim( funTyConName )
import Type
import HsDoc
import BasicTypes
import SrcLoc
import StaticFlags
import Outputable
import FastString

import Data.Data hiding ( Fixity )
\end{code}


%************************************************************************
%*                                                                      *
        Quasi quotes; used in types and elsewhere
%*                                                                      *
%************************************************************************

\begin{code}
data HsQuasiQuote id = HsQuasiQuote 
                           id           -- The quasi-quoter
                           SrcSpan      -- The span of the enclosed string
                           FastString   -- The enclosed string
  deriving (Data, Typeable)

instance OutputableBndr id => Outputable (HsQuasiQuote id) where
    ppr = ppr_qq

ppr_qq :: OutputableBndr id => HsQuasiQuote id -> SDoc
ppr_qq (HsQuasiQuote quoter _ quote) =
    char '[' <> ppr quoter <> ptext (sLit "|") <>
    ppr quote <> ptext (sLit "|]")
\end{code}


%************************************************************************
%*                                                                      *
\subsection{Bang annotations}
%*                                                                      *
%************************************************************************

\begin{code}
type LBangType l name = GenLocated l (BangType l name)
type BangType l name  = HsType l name       -- Bangs are in the HsType data type

getBangType :: LHsType l a -> LHsType l a
getBangType (L _ (HsBangTy _ ty)) = ty
getBangType ty                    = ty

getBangStrictness :: LHsType l a -> HsBang
getBangStrictness (L _ (HsBangTy s _)) = s
getBangStrictness _                    = HsNoBang
\end{code}


%************************************************************************
%*                                                                      *
\subsection{Data types}
%*                                                                      *
%************************************************************************

This is the syntax for types as seen in type signatures.

Note [HsBSig binder lists]
~~~~~~~~~~~~~~~~~~~~~~~~~~
Consider a binder (or pattern) decoarated with a type or kind, 
   \ (x :: a -> a). blah
   forall (a :: k -> *) (b :: k). blah
Then we use a LHsBndrSig on the binder, so that the
renamer can decorate it with the variables bound
by the pattern ('a' in the first example, 'k' in the second),
assuming that neither of them is in scope already
See also Note [Kind and type-variable binders] in RnTypes

\begin{code}
type LHsContext l name = GenLocated l (HsContext l name)

type HsContext l name = [LHsType l name]

type LHsType l name = GenLocated l (HsType l name)
type HsKind l name = HsType l name
type LHsKind l name = GenLocated l (HsKind l name)

type LHsTyVarBndr l name = GenLocated l (HsTyVarBndr l name)

data LHsTyVarBndrs l name
  = HsQTvs { hsq_kvs :: [Name]                  -- Kind variables
           , hsq_tvs :: [LHsTyVarBndr l name]   -- Type variables
             -- See Note [HsForAllTy tyvar binders]
    }
  deriving( Typeable)
deriving instance (DataId name, Data l) => Data (LHsTyVarBndrs l name)

mkHsQTvs :: [LHsTyVarBndr SrcSpan RdrName] -> LHsTyVarBndrs SrcSpan RdrName
-- Just at RdrName because in the Name variant we should know just
-- what the kind-variable binders are; and we don't
-- We put an empty list (rather than a panic) for the kind vars so
-- that the pretty printer works ok on them.
mkHsQTvs tvs = HsQTvs { hsq_kvs = [], hsq_tvs = tvs }

emptyHsQTvs :: LHsTyVarBndrs l name   -- Use only when you know there are no kind binders
emptyHsQTvs =  HsQTvs { hsq_kvs = [], hsq_tvs = [] }

hsQTvBndrs :: LHsTyVarBndrs l name -> [LHsTyVarBndr l name]
hsQTvBndrs = hsq_tvs

data HsWithBndrs name thing
  = HsWB { hswb_cts :: thing             -- Main payload (type or list of types)
         , hswb_kvs :: PostRn name [Name] -- Kind vars
         , hswb_tvs :: PostRn name [Name] -- Type vars
    }
  deriving (Typeable)
deriving instance (Data name, Data thing, Data (PostRn name [Name]))
  => Data (HsWithBndrs name thing)

mkHsWithBndrs :: thing -> HsWithBndrs RdrName thing
mkHsWithBndrs x = HsWB { hswb_cts = x, hswb_kvs = PlaceHolder
                                     , hswb_tvs = PlaceHolder }


-- | These names are used early on to store the names of implicit
-- parameters.  They completely disappear after type-checking.
newtype HsIPName = HsIPName FastString-- ?x
  deriving( Eq, Data, Typeable )

hsIPNameFS :: HsIPName -> FastString
hsIPNameFS (HsIPName n) = n

instance Outputable HsIPName where
    ppr (HsIPName n) = char '?' <> ftext n -- Ordinary implicit parameters

instance OutputableBndr HsIPName where
    pprBndr _ n   = ppr n         -- Simple for now
    pprInfixOcc  n = ppr n
    pprPrefixOcc n = ppr n

data HsTyVarBndr l name
  = UserTyVar        -- no explicit kinding
         name

  | KindedTyVar
         name
         (LHsKind l name)  -- The user-supplied kind signature
  deriving (Typeable)
deriving instance (DataId name, Data l) => Data (HsTyVarBndr l name)

-- | Does this 'HsTyVarBndr' come with an explicit kind annotation?
isHsKindedTyVar :: HsTyVarBndr l name -> Bool
isHsKindedTyVar (UserTyVar {})   = False
isHsKindedTyVar (KindedTyVar {}) = True

-- | Do all type variables in this 'LHsTyVarBndr' come with kind annotations?
hsTvbAllKinded :: LHsTyVarBndrs l name -> Bool
hsTvbAllKinded = all (isHsKindedTyVar . unLoc) . hsQTvBndrs

data HsType l name
  = HsForAllTy  HsExplicitFlag          -- Renamer leaves this flag unchanged, to record the way
                                        -- the user wrote it originally, so that the printer can
                                        -- print it as the user wrote it
                (LHsTyVarBndrs l name)
                (LHsContext l name)
                (LHsType l name)

  | HsTyVar             name            -- Type variable, type constructor, or data constructor
                                        -- see Note [Promotions (HsTyVar)]

  | HsAppTy             (LHsType l name)
                        (LHsType l name)

  | HsFunTy             (LHsType l name)   -- function type
                        (LHsType l name)

  | HsListTy            (LHsType l name)  -- Element type

  | HsPArrTy            (LHsType l name)  -- Elem. type of parallel array: [:t:]

  | HsTupleTy           HsTupleSort
                        [LHsType l name]  -- Element types (length gives arity)

  | HsOpTy              (LHsType l name) (LHsTyOp l name) (LHsType l name)

  | HsParTy             (LHsType l name)   -- See Note [Parens in HsSyn] in HsExpr
        -- Parenthesis preserved for the precedence re-arrangement in RnTypes
        -- It's important that a * (b + c) doesn't get rearranged to (a*b) + c!

  | HsIParamTy          HsIPName         -- (?x :: ty)
                        (LHsType l name)   -- Implicit parameters as they occur in contexts

  | HsEqTy              (LHsType l name)   -- ty1 ~ ty2
                        (LHsType l name)   -- Always allowed even without TypeOperators, and has special kinding rule

  | HsKindSig           (LHsType l name)  -- (ty :: kind)
                        (LHsKind l name)  -- A type with a kind signature

  | HsQuasiQuoteTy      (HsQuasiQuote name)

  | HsSpliceTy          (HsSplice name) 
                        (PostTc name Kind)

  | HsDocTy             (LHsType l name) LHsDocString -- A documented type

  | HsBangTy    HsBang (LHsType l name)   -- Bang-style type annotations 
  | HsRecTy [ConDeclField l name]         -- Only in data type declarations

  | HsCoreTy Type       -- An escape hatch for tunnelling a *closed* 
                        -- Core Type through HsSyn.  

  | HsExplicitListTy       -- A promoted explicit list
        (PostTc name Kind) -- See Note [Promoted lists and tuples]
        [LHsType l name]
                         
  | HsExplicitTupleTy      -- A promoted explicit tuple
        [PostTc name Kind] -- See Note [Promoted lists and tuples]
        [LHsType l name]

  | HsTyLit HsTyLit      -- A promoted numeric literal.

  | HsWrapTy HsTyWrapper (HsType l name) -- only in typechecker output
  deriving (Typeable)
deriving instance (DataId name, Data l) => Data (HsType l name)


data HsTyLit
  = HsNumTy Integer
  | HsStrTy FastString
    deriving (Data, Typeable)

data HsTyWrapper
  = WpKiApps [Kind]  -- kind instantiation: [] k1 k2 .. kn
  deriving (Data, Typeable)

type LHsTyOp l name = HsTyOp (GenLocated l name)
type HsTyOp name = (HsTyWrapper, name)

mkHsOpTy :: LHsType l name -> GenLocated l name -> LHsType l name -> HsType l name
mkHsOpTy ty1 op ty2 = HsOpTy ty1 (WpKiApps [], op) ty2
\end{code}

Note [HsForAllTy tyvar binders]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
After parsing:
  * Implicit => empty
    Explicit => the variables the user wrote

After renaming
  * Implicit => the *type* variables free in the type
    Explicit => the variables the user wrote (renamed)

Qualified currently behaves exactly as Implicit,
but it is deprecated to use it for implicit quantification.
In this case, GHC 7.10 gives a warning; see
Note [Context quantification] and Trac #4426.
In GHC 7.12, Qualified will no longer bind variables
and this will become an error.

The kind variables bound in the hsq_kvs field come both
  a) from the kind signatures on the kind vars (eg k1)
  b) from the scope of the forall (eg k2)
Example:   f :: forall (a::k1) b. T a (b::k2)


Note [Unit tuples]
~~~~~~~~~~~~~~~~~~
Consider the type
    type instance F Int = ()
We want to parse that "()" 
    as HsTupleTy HsBoxedOrConstraintTuple [], 
NOT as HsTyVar unitTyCon

Why? Because F might have kind (* -> Constraint), so we when parsing we
don't know if that tuple is going to be a constraint tuple or an ordinary
unit tuple.  The HsTupleSort flag is specifically designed to deal with
that, but it has to work for unit tuples too.

Note [Promotions (HsTyVar)]
~~~~~~~~~~~~~~~~~~~~~~~~~~~
HsTyVar: A name in a type or kind.
  Here are the allowed namespaces for the name.
    In a type:
      Var: not allowed
      Data: promoted data constructor
      Tv: type variable
      TcCls before renamer: type constructor, class constructor, or promoted data constructor
      TcCls after renamer: type constructor or class constructor
    In a kind:
      Var, Data: not allowed
      Tv: kind variable
      TcCls: kind constructor or promoted type constructor


Note [Promoted lists and tuples]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Notice the difference between
   HsListTy    HsExplicitListTy
   HsTupleTy   HsExplicitListTupleTy

E.g.    f :: [Int]                      HsListTy                

        g3  :: T '[]                   All these use  
        g2  :: T '[True]                  HsExplicitListTy        
        g1  :: T '[True,False]          
        g1a :: T [True,False]             (can omit ' where unambiguous)

  kind of T :: [Bool] -> *        This kind uses HsListTy!

E.g.    h :: (Int,Bool)                 HsTupleTy; f is a pair               
        k :: S '(True,False)            HsExplicitTypleTy; S is indexed by   
                                           a type-level pair of booleans 
        kind of S :: (Bool,Bool) -> *   This kind uses HsExplicitTupleTy

Note [Distinguishing tuple kinds]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

Apart from promotion, tuples can have one of three different kinds:

        x :: (Int, Bool)                -- Regular boxed tuples
        f :: Int# -> (# Int#, Int# #)   -- Unboxed tuples
        g :: (Eq a, Ord a) => a         -- Constraint tuples

For convenience, internally we use a single constructor for all of these,
namely HsTupleTy, but keep track of the tuple kind (in the first argument to
HsTupleTy, a HsTupleSort). We can tell if a tuple is unboxed while parsing,
because of the #. However, with -XConstraintKinds we can only distinguish
between constraint and boxed tuples during type checking, in general. Hence the
four constructors of HsTupleSort:
        
        HsUnboxedTuple                  -> Produced by the parser
        HsBoxedTuple                    -> Certainly a boxed tuple
        HsConstraintTuple               -> Certainly a constraint tuple
        HsBoxedOrConstraintTuple        -> Could be a boxed or a constraint 
                                        tuple. Produced by the parser only,
                                        disappears after type checking

\begin{code}
data HsTupleSort = HsUnboxedTuple
                 | HsBoxedTuple
                 | HsConstraintTuple
                 | HsBoxedOrConstraintTuple
                 deriving (Data, Typeable)

data HsExplicitFlag = Qualified | Implicit | Explicit deriving (Data, Typeable)

data ConDeclField l name  -- Record fields have Haddoc docs on them
  = ConDeclField { cd_fld_name :: GenLocated l name,
                   cd_fld_type :: LBangType l name,
                   cd_fld_doc  :: Maybe LHsDocString }
  deriving (Typeable)
deriving instance (DataId name, Data l) => Data (ConDeclField l name)

-----------------------
-- Combine adjacent for-alls. 
-- The following awkward situation can happen otherwise:
--      f :: forall a. ((Num a) => Int)
-- might generate HsForAll (Just [a]) [] (HsForAll Nothing [Num a] t)
-- Then a isn't discovered as ambiguous, and we abstract the AbsBinds wrt []
-- but the export list abstracts f wrt [a].  Disaster.
--
-- A valid type must have one for-all at the top of the type, or of the fn arg types

mkImplicitHsForAllTy  ::                           LHsContext SrcSpan RdrName -> LHsType SrcSpan RdrName -> HsType SrcSpan RdrName
mkExplicitHsForAllTy  :: [LHsTyVarBndr SrcSpan RdrName] -> LHsContext SrcSpan RdrName -> LHsType SrcSpan RdrName -> HsType SrcSpan RdrName
mkQualifiedHsForAllTy ::                           LHsContext SrcSpan RdrName -> LHsType SrcSpan RdrName -> HsType SrcSpan RdrName
mkImplicitHsForAllTy      ctxt ty = mkHsForAllTy Implicit  []  ctxt ty
mkExplicitHsForAllTy  tvs ctxt ty = mkHsForAllTy Explicit  tvs ctxt ty
mkQualifiedHsForAllTy     ctxt ty = mkHsForAllTy Qualified []  ctxt ty

mkHsForAllTy :: HsExplicitFlag -> [LHsTyVarBndr SrcSpan RdrName] -> LHsContext SrcSpan RdrName -> LHsType SrcSpan RdrName -> HsType SrcSpan RdrName
-- Smart constructor for HsForAllTy
mkHsForAllTy exp tvs (L _ []) ty = mk_forall_ty exp tvs ty
mkHsForAllTy exp tvs ctxt     ty = HsForAllTy exp (mkHsQTvs tvs) ctxt ty

-- mk_forall_ty makes a pure for-all type (no context)
mk_forall_ty :: HsExplicitFlag -> [LHsTyVarBndr SrcSpan RdrName] -> LHsType SrcSpan RdrName -> HsType SrcSpan RdrName
mk_forall_ty exp  tvs  (L _ (HsParTy ty))                    = mk_forall_ty exp tvs ty
mk_forall_ty exp1 tvs1 (L _ (HsForAllTy exp2 qtvs2 ctxt ty)) = mkHsForAllTy (exp1 `plus` exp2) (tvs1 ++ hsq_tvs qtvs2) ctxt ty
mk_forall_ty exp  tvs  ty                                    = HsForAllTy exp (mkHsQTvs tvs) (noLoc []) ty
        -- Even if tvs is empty, we still make a HsForAll!
        -- In the Implicit case, this signals the place to do implicit quantification
        -- In the Explicit case, it prevents implicit quantification    
        --      (see the sigtype production in Parser.y.pp)
        --      so that (forall. ty) isn't implicitly quantified

plus :: HsExplicitFlag -> HsExplicitFlag -> HsExplicitFlag
Qualified `plus` Qualified = Qualified
Explicit  `plus` _         = Explicit
_         `plus` Explicit  = Explicit
_         `plus` _         = Implicit

hsExplicitTvs :: LHsType SrcSpan Name -> [Name]
-- The explicitly-given forall'd type variables of a HsType
hsExplicitTvs (L _ (HsForAllTy Explicit tvs _ _)) = hsLKiTyVarNames tvs
hsExplicitTvs _                                   = []

---------------------
hsTyVarName :: HsTyVarBndr l name -> name
hsTyVarName (UserTyVar n)     = n
hsTyVarName (KindedTyVar n _) = n

hsLTyVarName :: LHsTyVarBndr l name -> name
hsLTyVarName = hsTyVarName . unLoc

hsLTyVarNames :: LHsTyVarBndrs l name -> [name]
-- Type variables only
hsLTyVarNames qtvs = map hsLTyVarName (hsQTvBndrs qtvs)

hsLKiTyVarNames :: LHsTyVarBndrs l Name -> [Name]
-- Kind and type variables
hsLKiTyVarNames (HsQTvs { hsq_kvs = kvs, hsq_tvs = tvs })
  = kvs ++ map hsLTyVarName tvs

hsLTyVarLocName :: LHsTyVarBndr l name -> GenLocated l name
hsLTyVarLocName = fmap hsTyVarName

hsLTyVarLocNames :: LHsTyVarBndrs l name -> [GenLocated l name]
hsLTyVarLocNames qtvs = map hsLTyVarLocName (hsQTvBndrs qtvs)
\end{code}


\begin{code}
splitHsAppTys :: LHsType l n -> [LHsType l n] -> (LHsType l n, [LHsType l n])
splitHsAppTys (L _ (HsAppTy f a)) as = splitHsAppTys f (a:as)
splitHsAppTys (L _ (HsParTy f))   as = splitHsAppTys f as
splitHsAppTys f                   as = (f,as)

-- retrieve the name of the "head" of a nested type application
-- somewhat like splitHsAppTys, but a little more thorough
-- used to examine the result of a GADT-like datacon, so it doesn't handle
-- *all* cases (like lists, tuples, (~), etc.)
hsTyGetAppHead_maybe :: LHsType l n -> Maybe (n, [LHsType l n])
hsTyGetAppHead_maybe = go []
  where
    go tys (L _ (HsTyVar n))             = Just (n, tys)
    go tys (L _ (HsAppTy l r))           = go (r : tys) l
    go tys (L _ (HsOpTy l (_, L _ n) r)) = Just (n, l : r : tys)
    go tys (L _ (HsParTy t))             = go tys t
    go tys (L _ (HsKindSig t _))         = go tys t
    go _   _                             = Nothing

mkHsAppTys :: (OutputableBndr n, Outputable l,SrcAnnotation l) => LHsType l n -> [LHsType l n] -> HsType l n
mkHsAppTys fun_ty [] = pprPanic "mkHsAppTys" (ppr fun_ty)
mkHsAppTys fun_ty (arg_ty:arg_tys)
  = foldl mk_app (HsAppTy fun_ty arg_ty) arg_tys
  where
    mk_app fun arg = HsAppTy (annNoLoc fun) arg    
       -- Add noLocs for inner nodes of the application; 
       -- they are never used 

splitLHsInstDeclTy_maybe
    :: LHsType l name
    -> Maybe (LHsTyVarBndrs l name, HsContext l name, GenLocated l name, [LHsType l name])
        -- Split up an instance decl type, returning the pieces
splitLHsInstDeclTy_maybe inst_ty = do
    let (tvs, cxt, ty) = splitLHsForAllTy inst_ty
    (cls, tys) <- splitLHsClassTy_maybe ty
    return (tvs, cxt, cls, tys)

splitLHsForAllTy
    :: LHsType l name
    -> (LHsTyVarBndrs l name, HsContext l name, LHsType l name)
splitLHsForAllTy poly_ty
  = case unLoc poly_ty of
        HsParTy ty              -> splitLHsForAllTy ty
        HsForAllTy _ tvs cxt ty -> (tvs, unLoc cxt, ty)
        _                       -> (emptyHsQTvs, [], poly_ty)
        -- The type vars should have been computed by now, even if they were implicit

splitHsClassTy_maybe :: (SrcAnnotation l) => HsType l name -> Maybe (name, [LHsType l name])
splitHsClassTy_maybe ty = fmap (\(L _ n, tys) -> (n, tys)) $ splitLHsClassTy_maybe (annNoLoc ty)

splitLHsClassTy_maybe :: LHsType l name -> Maybe (GenLocated l name, [LHsType l name])
--- Watch out.. in ...deriving( Show )... we use this on 
--- the list of partially applied predicates in the deriving,
--- so there can be zero args.

-- In TcDeriv we also use this to figure out what data type is being
-- mentioned in a deriving (Generic (Foo bar baz)) declaration (i.e. "Foo").
splitLHsClassTy_maybe ty
  = checkl ty []
  where
    checkl (L l ty) args = case ty of
        HsTyVar t          -> Just (L l t, args)
        HsAppTy l r        -> checkl l (r:args)
        HsOpTy l (_, tc) r -> checkl (fmap HsTyVar tc) (l:r:args)
        HsParTy t          -> checkl t args
        HsKindSig ty _     -> checkl ty args
        _                  -> Nothing

-- splitHsFunType decomposes a type (t1 -> t2 ... -> tn)
-- Breaks up any parens in the result type: 
--      splitHsFunType (a -> (b -> c)) = ([a,b], c)
-- Also deals with (->) t1 t2; that is why it only works on LHsType Name
--   (see Trac #9096)
splitHsFunType :: LHsType l Name -> ([LHsType l Name], LHsType l Name)
splitHsFunType (L _ (HsParTy ty)) 
  = splitHsFunType ty

splitHsFunType (L _ (HsFunTy x y))
  | (args, res) <- splitHsFunType y
  = (x:args, res)

splitHsFunType orig_ty@(L _ (HsAppTy t1 t2)) 
  = go t1 [t2]
  where  -- Look for (->) t1 t2, possibly with parenthesisation
    go (L _ (HsTyVar fn))    tys | fn == funTyConName
                                 , [t1,t2] <- tys
                                 , (args, res) <- splitHsFunType t2
                                 = (t1:args, res)
    go (L _ (HsAppTy t1 t2)) tys = go t1 (t2:tys)
    go (L _ (HsParTy ty))    tys = go ty tys
    go _                     _   = ([], orig_ty)  -- Failure to match

splitHsFunType other = ([], other)
\end{code}


%************************************************************************
%*                                                                      *
\subsection{Pretty printing}
%*                                                                      *
%************************************************************************

\begin{code}
instance (OutputableBndr name,Outputable l) => Outputable (HsType l name) where
    ppr ty = pprHsType ty

instance Outputable HsTyLit where
    ppr = ppr_tylit

instance (OutputableBndr name, Outputable l) => Outputable (LHsTyVarBndrs l name) where
    ppr (HsQTvs { hsq_kvs = kvs, hsq_tvs = tvs }) 
      = sep [ ifPprDebug $ braces (interppSP kvs), interppSP tvs ]

instance (OutputableBndr name, Outputable l) => Outputable (HsTyVarBndr l name) where
    ppr (UserTyVar n)     = ppr n
    ppr (KindedTyVar n k) = parens $ hsep [ppr n, dcolon, ppr k]

instance (Outputable thing) => Outputable (HsWithBndrs name thing) where
    ppr (HsWB { hswb_cts = ty }) = ppr ty

pprHsForAll :: (OutputableBndr name, Outputable l)
  => HsExplicitFlag -> LHsTyVarBndrs l name ->  LHsContext l name -> SDoc
pprHsForAll exp qtvs cxt 
  | show_forall = forall_part <+> pprHsContext (unLoc cxt)
  | otherwise   = pprHsContext (unLoc cxt)
  where
    show_forall =  opt_PprStyle_Debug
                || (not (null (hsQTvBndrs qtvs)) && is_explicit)
    is_explicit = case exp of {Explicit -> True; Implicit -> False; Qualified -> False}
    forall_part = forAllLit <+> ppr qtvs <> dot

pprHsContext :: (OutputableBndr name, Outputable l) => HsContext l name -> SDoc
pprHsContext []  = empty
pprHsContext cxt = pprHsContextNoArrow cxt <+> darrow

pprHsContextNoArrow :: (OutputableBndr name, Outputable l) => HsContext l name -> SDoc
pprHsContextNoArrow []         = empty
pprHsContextNoArrow [L _ pred] = ppr_mono_ty FunPrec pred
pprHsContextNoArrow cxt        = parens (interpp'SP cxt)

pprConDeclFields :: (OutputableBndr name, Outputable l) => [ConDeclField l name] -> SDoc
pprConDeclFields fields = braces (sep (punctuate comma (map ppr_fld fields)))
  where
    ppr_fld (ConDeclField { cd_fld_name = n, cd_fld_type = ty, 
                            cd_fld_doc = doc })
        = ppr n <+> dcolon <+> ppr ty <+> ppr_mbDoc doc
\end{code}

Note [Printing KindedTyVars]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Trac #3830 reminded me that we should really only print the kind
signature on a KindedTyVar if the kind signature was put there by the
programmer.  During kind inference GHC now adds a PostTcKind to UserTyVars,
rather than converting to KindedTyVars as before.

(As it happens, the message in #3830 comes out a different way now,
and the problem doesn't show up; but having the flag on a KindedTyVar
seems like the Right Thing anyway.)

\begin{code}
-- Printing works more-or-less as for Types

pprHsType, pprParendHsType :: (OutputableBndr name, Outputable l) => HsType l name -> SDoc

pprHsType ty       = getPprStyle $ \sty -> ppr_mono_ty TopPrec (prepare sty ty)
pprParendHsType ty = ppr_mono_ty TyConPrec ty

-- Before printing a type
-- (a) Remove outermost HsParTy parens
-- (b) Drop top-level for-all type variables in user style
--     since they are implicit in Haskell
prepare :: PprStyle -> HsType l name -> HsType l name
prepare sty (HsParTy ty)          = prepare sty (unLoc ty)
prepare _   ty                    = ty

ppr_mono_lty :: (OutputableBndr name, Outputable l) => TyPrec -> LHsType l name -> SDoc
ppr_mono_lty ctxt_prec ty = ppr_mono_ty ctxt_prec (unLoc ty)

ppr_mono_ty :: (OutputableBndr name, Outputable l) => TyPrec -> HsType l name -> SDoc
ppr_mono_ty ctxt_prec (HsForAllTy exp tvs ctxt ty)
  = maybeParen ctxt_prec FunPrec $
    sep [pprHsForAll exp tvs ctxt, ppr_mono_lty TopPrec ty]

ppr_mono_ty _    (HsBangTy b ty)     = ppr b <> ppr_mono_lty TyConPrec ty
ppr_mono_ty _    (HsQuasiQuoteTy qq) = ppr qq
ppr_mono_ty _    (HsRecTy flds)      = pprConDeclFields flds
ppr_mono_ty _    (HsTyVar name)      = pprPrefixOcc name
ppr_mono_ty prec (HsFunTy ty1 ty2)   = ppr_fun_ty prec ty1 ty2
ppr_mono_ty _    (HsTupleTy con tys) = tupleParens std_con (interpp'SP tys)
  where std_con = case con of
                    HsUnboxedTuple -> UnboxedTuple
                    _              -> BoxedTuple
ppr_mono_ty _    (HsKindSig ty kind) = parens (ppr_mono_lty TopPrec ty <+> dcolon <+> ppr kind)
ppr_mono_ty _    (HsListTy ty)       = brackets (ppr_mono_lty TopPrec ty)
ppr_mono_ty _    (HsPArrTy ty)       = paBrackets (ppr_mono_lty TopPrec ty)
ppr_mono_ty prec (HsIParamTy n ty)   = maybeParen prec FunPrec (ppr n <+> dcolon <+> ppr_mono_lty TopPrec ty)
ppr_mono_ty _    (HsSpliceTy s _)    = pprUntypedSplice s
ppr_mono_ty _    (HsCoreTy ty)       = ppr ty
ppr_mono_ty _    (HsExplicitListTy _ tys) = quote $ brackets (interpp'SP tys)
ppr_mono_ty _    (HsExplicitTupleTy _ tys) = quote $ parens (interpp'SP tys)
ppr_mono_ty _    (HsTyLit t)         = ppr_tylit t

ppr_mono_ty ctxt_prec (HsWrapTy (WpKiApps _kis) ty)
  = ppr_mono_ty ctxt_prec ty
-- We are not printing kind applications. If we wanted to do so, we should do
-- something like this:
{-
  = go ctxt_prec kis ty
  where
    go ctxt_prec [] ty = ppr_mono_ty ctxt_prec ty
    go ctxt_prec (ki:kis) ty
      = maybeParen ctxt_prec TyConPrec $
        hsep [ go FunPrec kis ty
             , ptext (sLit "@") <> pprParendKind ki ]
-}

ppr_mono_ty ctxt_prec (HsEqTy ty1 ty2)
  = maybeParen ctxt_prec TyOpPrec $
    ppr_mono_lty TyOpPrec ty1 <+> char '~' <+> ppr_mono_lty TyOpPrec ty2

ppr_mono_ty ctxt_prec (HsAppTy fun_ty arg_ty)
  = maybeParen ctxt_prec TyConPrec $
    hsep [ppr_mono_lty FunPrec fun_ty, ppr_mono_lty TyConPrec arg_ty]

ppr_mono_ty ctxt_prec (HsOpTy ty1 (_wrapper, L _ op) ty2)
  = maybeParen ctxt_prec TyOpPrec $
    sep [ ppr_mono_lty TyOpPrec ty1
        , sep [pprInfixOcc op, ppr_mono_lty TyOpPrec ty2 ] ]
    -- Don't print the wrapper (= kind applications)
    -- c.f. HsWrapTy

ppr_mono_ty _         (HsParTy ty)
  = parens (ppr_mono_lty TopPrec ty)
  -- Put the parens in where the user did
  -- But we still use the precedence stuff to add parens because
  --    toHsType doesn't put in any HsParTys, so we may still need them

ppr_mono_ty ctxt_prec (HsDocTy ty doc) 
  = maybeParen ctxt_prec TyOpPrec $
    ppr_mono_lty TyOpPrec ty <+> ppr (unLoc doc)
  -- we pretty print Haddock comments on types as if they were
  -- postfix operators

--------------------------
ppr_fun_ty :: (OutputableBndr name, Outputable l) => TyPrec -> LHsType l name -> LHsType l name -> SDoc
ppr_fun_ty ctxt_prec ty1 ty2
  = let p1 = ppr_mono_lty FunPrec ty1
        p2 = ppr_mono_lty TopPrec ty2
    in
    maybeParen ctxt_prec FunPrec $
    sep [p1, ptext (sLit "->") <+> p2]

--------------------------
ppr_tylit :: HsTyLit -> SDoc
ppr_tylit (HsNumTy i) = integer i
ppr_tylit (HsStrTy s) = text (show s)
\end{code}


