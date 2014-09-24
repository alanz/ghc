%
% (c) The University of Glasgow 2006
% (c) The GRASP/AQUA Project, Glasgow University, 1992-1998
%
\section[HsBinds]{Abstract syntax: top-level bindings and signatures}

Datatype for: @BindGroup@, @Bind@, @Sig@, @Bind@.

\begin{code}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE UndecidableInstances #-} -- Note [Pass sensitive types]
                                      -- in module PlaceHolder
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE CPP #-}

module HsBinds where

import {-# SOURCE #-} HsExpr ( pprExpr, LHsExpr,
                               MatchGroup, pprFunBind,
                               GRHSs, pprPatBind )
import {-# SOURCE #-} HsPat  ( LPat )

-- import PlaceHolder ( PostTc,PostRn,DataId )
import PlaceHolder ( PostTc,PostRn )
import HsTypes
import PprCore ()
import CoreSyn
import TcEvidence
import Type
import Name
import NameSet
import BasicTypes
import Outputable
import SrcLoc
import Var
import Bag
import FastString
import BooleanFormula (BooleanFormula)

import Data.Data hiding ( Fixity )
import Data.List
import Data.Ord
#if __GLASGOW_HASKELL__ < 709
import Data.Foldable ( Foldable(..) )
import Data.Traversable ( Traversable(..) )
import Data.Monoid ( mappend )
import Control.Applicative hiding (empty)
#else
import Control.Applicative ((<$>))
#endif
\end{code}

%************************************************************************
%*                                                                      *
\subsection{Bindings: @BindGroup@}
%*                                                                      *
%************************************************************************

Global bindings (where clauses)

\begin{code}
-- During renaming, we need bindings where the left-hand sides
-- have been renamed but the the right-hand sides have not.
-- the ...LR datatypes are parametrized by two id types,
-- one for the left and one for the right.
-- Other than during renaming, these will be the same.

type HsLocalBinds id = HsLocalBindsLR id id

-- | Bindings in a 'let' expression
-- or a 'where' clause
data HsLocalBindsLR l idL idR
  = HsValBinds (HsValBindsLR l idL idR)
  | HsIPBinds  (HsIPBinds l idR)
  | EmptyLocalBinds
  deriving (Typeable)
-- deriving instance (DataId idL, DataId idR, Data l)
--   => Data (HsLocalBindsLR l idL idR)

type HsValBinds l id = HsValBindsLR l id id

-- | Value bindings (not implicit parameters)
data HsValBindsLR l idL idR
  = -- | Before renaming RHS; idR is always RdrName
    -- Not dependency analysed
    -- Recursive by default
    ValBindsIn
        (LHsBindsLR l idL idR) [LSig l idR]

    -- | After renaming RHS; idR can be Name or Id
    --  Dependency analysed,
    -- later bindings in the list may depend on earlier
    -- ones.
  | ValBindsOut
        [(RecFlag, LHsBinds l idL)]
        [LSig l Name]
  deriving (Typeable)
-- deriving instance (DataId idL, DataId idR, Data l)
--   => Data (HsValBindsLR l idL idR)

type LHsBind  l id = LHsBindLR  l id id
type LHsBinds l id = LHsBindsLR l id id
type HsBind   l id = HsBindLR   l id id

type LHsBindsLR l idL idR = Bag (LHsBindLR l idL idR)
type LHsBindLR  l idL idR = GenLocated l (HsBindLR l idL idR)

data HsBindLR l idL idR
  = -- | FunBind is used for both functions   @f x = e@
    -- and variables                          @f = \x -> e@
    --
    -- Reason 1: Special case for type inference: see 'TcBinds.tcMonoBinds'.
    --
    -- Reason 2: Instance decls can only have FunBinds, which is convenient.
    --           If you change this, you'll need to change e.g. rnMethodBinds
    --
    -- But note that the form                 @f :: a->a = ...@
    -- parses as a pattern binding, just like
    --                                        @(f :: a -> a) = ... @
    FunBind {

        fun_id :: GenLocated l idL,

        fun_infix :: Bool,      -- ^ True => infix declaration

        fun_matches :: MatchGroup l idR (LHsExpr l idR), -- ^ The payload

        fun_co_fn :: HsWrapper, -- ^ Coercion from the type of the MatchGroup to the type of
                                -- the Id.  Example:
                                -- @
                                --      f :: Int -> forall a. a -> a
                                --      f x y = y
                                -- @
                                -- Then the MatchGroup will have type (Int -> a' -> a')
                                -- (with a free type variable a').  The coercion will take
                                -- a CoreExpr of this type and convert it to a CoreExpr of
                                -- type         Int -> forall a'. a' -> a'
                                -- Notice that the coercion captures the free a'.

        bind_fvs :: PostRn idL NameSet, -- ^ After the renamer, this contains
                                --  the locally-bound
                                -- free variables of this defn.
                                -- See Note [Bind free vars]


        fun_tick :: Maybe (Tickish Id)  -- ^ Tick to put on the rhs, if any
    }

  -- | The pattern is never a simple variable;
  -- That case is done by FunBind
  | PatBind {
        pat_lhs    :: LPat l idL,
        pat_rhs    :: GRHSs l idR (LHsExpr l idR),
        pat_rhs_ty :: PostTc idR Type,      -- ^ Type of the GRHSs
        bind_fvs   :: PostRn idL NameSet, -- ^ See Note [Bind free vars]
        pat_ticks  :: (Maybe (Tickish Id), [Maybe (Tickish Id)])
               -- ^ Tick to put on the rhs, if any, and ticks to put on
               -- the bound variables.
    }

  -- | Dictionary binding and suchlike.
  -- All VarBinds are introduced by the type checker
  | VarBind {   
        var_id     :: idL,           
        var_rhs    :: LHsExpr l idR, -- ^ Located only for consistency
        var_inline :: Bool           -- ^ True <=> inline this binding regardless
                                     -- (used for implication constraints only)
    }

  | AbsBinds {                      -- Binds abstraction; TRANSLATION
        abs_tvs     :: [TyVar],
        abs_ev_vars :: [EvVar],  -- ^ Includes equality constraints

       -- | AbsBinds only gets used when idL = idR after renaming,
       -- but these need to be idL's for the collect... code in HsUtil
       -- to have the right type
        abs_exports :: [ABExport l idL],

        abs_ev_binds :: TcEvBinds,     -- ^ Evidence bindings
        abs_binds    :: LHsBinds l idL -- ^ Typechecked user bindings
    }

  | PatSynBind (PatSynBind l idL idR)

  deriving (Typeable)
-- deriving instance (DataId idL, DataId idR, Data l)
--   => Data (HsBindLR l idL idR)

        -- Consider (AbsBinds tvs ds [(ftvs, poly_f, mono_f) binds]
        --
        -- Creates bindings for (polymorphic, overloaded) poly_f
        -- in terms of monomorphic, non-overloaded mono_f
        --
        -- Invariants:
        --      1. 'binds' binds mono_f
        --      2. ftvs is a subset of tvs
        --      3. ftvs includes all tyvars free in ds
        --
        -- See Note [AbsBinds]

data ABExport l id
  = ABE { abe_poly  :: id           -- ^ Any INLINE pragmas is attached to this Id
        , abe_mono  :: id
        , abe_wrap  :: HsWrapper    -- ^ See Note [AbsBinds wrappers]
             -- Shape: (forall abs_tvs. abs_ev_vars => abe_mono) ~ abe_poly
        , abe_prags :: TcSpecPrags l -- ^ SPECIALISE pragmas
  } deriving (Data, Typeable)

data PatSynBind l idL idR
  = PSB { psb_id   :: GenLocated l idL,        -- ^ Name of the pattern synonym
          psb_fvs  :: PostRn idR NameSet,      -- ^ See Note [Bind free vars]
          psb_args :: HsPatSynDetails (GenLocated l idR), -- ^ Formal parameter names
          psb_def  :: LPat l idR,                    -- ^ Right-hand side
          psb_dir  :: HsPatSynDir l idR              -- ^ Directionality
  } deriving (Typeable)
-- deriving instance (DataId idL, DataId idR, Data l )
--   => Data (PatSynBind l idL idR)

\end{code}

Note [AbsBinds]
~~~~~~~~~~~~~~~
The AbsBinds constructor is used in the output of the type checker, to record
*typechecked* and *generalised* bindings.  Consider a module M, with this
top-level binding
    M.reverse []     = []
    M.reverse (x:xs) = M.reverse xs ++ [x]

In Hindley-Milner, a recursive binding is typechecked with the *recursive* uses
being *monomorphic*.  So after typechecking *and* desugaring we will get something
like this
 
    M.reverse :: forall a. [a] -> [a]
      = /\a. letrec 
                reverse :: [a] -> [a] = \xs -> case xs of
                                                []     -> []
                                                (x:xs) -> reverse xs ++ [x]
             in reverse

Notice that 'M.reverse' is polymorphic as expected, but there is a local
definition for plain 'reverse' which is *monomorphic*.  The type variable
'a' scopes over the entire letrec.

That's after desugaring.  What about after type checking but before desugaring?  
That's where AbsBinds comes in.  It looks like this:

   AbsBinds { abs_tvs     = [a]
            , abs_exports = [ABE { abe_poly = M.reverse :: forall a. [a] -> [a],
                                 , abe_mono = reverse :: a -> a}]
            , abs_binds = { reverse :: [a] -> [a] 
                               = \xs -> case xs of
                                            []     -> []
                                            (x:xs) -> reverse xs ++ [x] } }

Here,
  * abs_tvs says what type variables are abstracted over the binding group, 
    just 'a' in this case.
  * abs_binds is the *monomorphic* bindings of the group
  * abs_exports describes how to get the polymorphic Id 'M.reverse' from the 
    monomorphic one 'reverse'

Notice that the *original* function (the polymorphic one you thought
you were defining) appears in the abe_poly field of the
abs_exports. The bindings in abs_binds are for fresh, local, Ids with
a *monomorphic* Id.

If there is a group of mutually recursive functions without type
signatures, we get one AbsBinds with the monomorphic versions of the
bindings in abs_binds, and one element of abe_exports for each
variable bound in the mutually recursive group.  This is true even for
pattern bindings.  Example:
        (f,g) = (\x -> x, f)
After type checking we get
   AbsBinds { abs_tvs     = [a]
            , abs_exports = [ ABE { abe_poly = M.f :: forall a. a -> a
                                  , abe_mono = f :: a -> a }
                            , ABE { abe_poly = M.g :: forall a. a -> a
                                  , abe_mono = g :: a -> a }]
            , abs_binds = { (f,g) = (\x -> x, f) }

Note [AbsBinds wrappers]
~~~~~~~~~~~~~~~~~~~~~~~~
Consider
   (f,g) = (\x.x, \y.y)
This ultimately desugars to something like this:
   tup :: forall a b. (a->a, b->b)
   tup = /\a b. (\x:a.x, \y:b.y)
   f :: forall a. a -> a
   f = /\a. case tup a Any of
               (fm::a->a,gm:Any->Any) -> fm
   ...similarly for g...

The abe_wrap field deals with impedence-matching between
    (/\a b. case tup a b of { (f,g) -> f })
and the thing we really want, which may have fewer type
variables.  The action happens in TcBinds.mkExport.

Note [Bind free vars]
~~~~~~~~~~~~~~~~~~~~~
The bind_fvs field of FunBind and PatBind records the free variables
of the definition.  It is used for two purposes

a) Dependency analysis prior to type checking
    (see TcBinds.tc_group)

b) Deciding whether we can do generalisation of the binding
    (see TcBinds.decideGeneralisationPlan)

Specifically,

  * bind_fvs includes all free vars that are defined in this module
    (including top-level things and lexically scoped type variables)

  * bind_fvs excludes imported vars; this is just to keep the set smaller

  * Before renaming, and after typechecking, the field is unused;
    it's just an error thunk

\begin{code}
instance (OutputableBndr idL, OutputableBndr idR, SrcAnnotation l)
   => Outputable (HsLocalBindsLR l idL idR) where
  ppr (HsValBinds bs) = ppr bs
  ppr (HsIPBinds bs)  = ppr bs
  ppr EmptyLocalBinds = empty

instance (OutputableBndr idL, OutputableBndr idR, SrcAnnotation l)
   => Outputable (HsValBindsLR l idL idR) where
  ppr (ValBindsIn binds sigs)
   = pprDeclList (pprLHsBindsForUser binds sigs)

  ppr (ValBindsOut sccs sigs)
    = getPprStyle $ \ sty ->
      if debugStyle sty then    -- Print with sccs showing
        vcat (map ppr sigs) $$ vcat (map ppr_scc sccs)
     else
        pprDeclList (pprLHsBindsForUser (unionManyBags (map snd sccs)) sigs)
   where
     ppr_scc (rec_flag, binds) = pp_rec rec_flag <+> pprLHsBinds binds
     pp_rec Recursive    = ptext (sLit "rec")
     pp_rec NonRecursive = ptext (sLit "nonrec")

pprLHsBinds :: (OutputableBndr idL, OutputableBndr idR, SrcAnnotation l)
  => LHsBindsLR l idL idR -> SDoc
pprLHsBinds binds
  | isEmptyLHsBinds binds = empty
  | otherwise = pprDeclList (map ppr (bagToList binds))

pprLHsBindsForUser :: (OutputableBndr idL, OutputableBndr idR,
                       OutputableBndr id2, SrcAnnotation l)
                   => LHsBindsLR l idL idR -> [LSig l id2] -> [SDoc]
--  pprLHsBindsForUser is different to pprLHsBinds because
--  a) No braces: 'let' and 'where' include a list of HsBindGroups
--     and we don't want several groups of bindings each
--     with braces around
--  b) Sort by location before printing
--  c) Include signatures
pprLHsBindsForUser binds sigs
  = map snd (sort_by_loc decls)
  where

    decls :: [(SrcSpan, SDoc)]
    decls = [(annGetLoc s, ppr sig)  | s@(L _ sig) <- sigs] ++
            [(annGetLoc b, ppr bind) | b@(L _ bind) <- bagToList binds]

    sort_by_loc decls = sortBy (comparing fst) decls

pprDeclList :: [SDoc] -> SDoc   -- Braces with a space
-- Print a bunch of declarations
-- One could choose  { d1; d2; ... }, using 'sep'
-- or      d1
--         d2
--         ..
--    using vcat
-- At the moment we chose the latter
-- Also we do the 'pprDeeperList' thing.
pprDeclList ds = pprDeeperList vcat ds

------------
emptyLocalBinds :: HsLocalBindsLR l a b
emptyLocalBinds = EmptyLocalBinds

isEmptyLocalBinds :: HsLocalBindsLR l a b -> Bool
isEmptyLocalBinds (HsValBinds ds) = isEmptyValBinds ds
isEmptyLocalBinds (HsIPBinds ds)  = isEmptyIPBinds ds
isEmptyLocalBinds EmptyLocalBinds = True

isEmptyValBinds :: HsValBindsLR l a b -> Bool
isEmptyValBinds (ValBindsIn ds sigs)  = isEmptyLHsBinds ds && null sigs
isEmptyValBinds (ValBindsOut ds sigs) = null ds && null sigs

emptyValBindsIn, emptyValBindsOut :: HsValBindsLR l a b
emptyValBindsIn  = ValBindsIn emptyBag []
emptyValBindsOut = ValBindsOut []      []

emptyLHsBinds :: LHsBindsLR l idL idR
emptyLHsBinds = emptyBag

isEmptyLHsBinds :: LHsBindsLR l idL idR -> Bool
isEmptyLHsBinds = isEmptyBag

------------
plusHsValBinds :: HsValBinds l a -> HsValBinds l a -> HsValBinds l a
plusHsValBinds (ValBindsIn ds1 sigs1) (ValBindsIn ds2 sigs2)
  = ValBindsIn (ds1 `unionBags` ds2) (sigs1 ++ sigs2)
plusHsValBinds (ValBindsOut ds1 sigs1) (ValBindsOut ds2 sigs2)
  = ValBindsOut (ds1 ++ ds2) (sigs1 ++ sigs2)
plusHsValBinds _ _
  = panic "HsBinds.plusHsValBinds"

getTypeSigNames :: HsValBinds l a -> NameSet
-- Get the names that have a user type sig
getTypeSigNames (ValBindsOut _ sigs)
  = mkNameSet [unLoc n | L _ (TypeSig names _) <- sigs, n <- names]
getTypeSigNames _
  = panic "HsBinds.getTypeSigNames"
\end{code}

What AbsBinds means
~~~~~~~~~~~~~~~~~~~
         AbsBinds tvs
                  [d1,d2]
                  [(tvs1, f1p, f1m),
                   (tvs2, f2p, f2m)]
                  BIND
means

        f1p = /\ tvs -> \ [d1,d2] -> letrec DBINDS and BIND
                                     in fm

        gp = ...same again, with gm instead of fm

This is a pretty bad translation, because it duplicates all the bindings.
So the desugarer tries to do a better job:

        fp = /\ [a,b] -> \ [d1,d2] -> case tp [a,b] [d1,d2] of
                                        (fm,gm) -> fm
        ..ditto for gp..

        tp = /\ [a,b] -> \ [d1,d2] -> letrec DBINDS and BIND
                                      in (fm,gm)

\begin{code}
instance (OutputableBndr idL, OutputableBndr idR, SrcAnnotation l)
   => Outputable (HsBindLR l idL idR) where
    ppr mbind = ppr_monobind mbind

ppr_monobind :: (OutputableBndr idL, OutputableBndr idR, SrcAnnotation l)
   => HsBindLR l idL idR -> SDoc

ppr_monobind (PatBind { pat_lhs = pat, pat_rhs = grhss })
  = pprPatBind pat grhss
ppr_monobind (VarBind { var_id = var, var_rhs = rhs })
  = sep [pprBndr CaseBind var, nest 2 $ equals <+> pprExpr (unLoc rhs)]
ppr_monobind (FunBind { fun_id = fun, fun_infix = inf,
                        fun_co_fn = wrap,
                        fun_matches = matches,
                        fun_tick = tick })
  = pprTicks empty (case tick of
                        Nothing -> empty
                        Just t  -> text "-- tick id = " <> ppr t)
    $$  ifPprDebug (pprBndr LetBind (unLoc fun))
    $$  pprFunBind (unLoc fun) inf matches
    $$  ifPprDebug (ppr wrap)
ppr_monobind (PatSynBind psb) = ppr psb
ppr_monobind (AbsBinds { abs_tvs = tyvars, abs_ev_vars = dictvars
                       , abs_exports = exports, abs_binds = val_binds
                       , abs_ev_binds = ev_binds })
  = hang (ptext (sLit "AbsBinds") <+> brackets (interpp'SP tyvars)
                                  <+> brackets (interpp'SP dictvars))
       2 $ braces $ vcat
    [ ptext (sLit "Exports:") <+> brackets (sep (punctuate comma (map ppr exports)))
    , ptext (sLit "Exported types:") <+> vcat [pprBndr LetBind (abe_poly ex) | ex <- exports]
    , ptext (sLit "Binds:") <+> pprLHsBinds val_binds
    , ifPprDebug (ptext (sLit "Evidence:") <+> ppr ev_binds) ]

instance (OutputableBndr id, Outputable l) => Outputable (ABExport l id) where
  ppr (ABE { abe_wrap = wrap, abe_poly = gbl, abe_mono = lcl, abe_prags = prags })
    = vcat [ ppr gbl <+> ptext (sLit "<=") <+> ppr lcl
           , nest 2 (pprTcSpecPrags prags)
           , nest 2 (ppr wrap)]

instance (OutputableBndr idL, OutputableBndr idR, SrcAnnotation l)
   => Outputable (PatSynBind l idL idR) where
  ppr (PSB{ psb_id = L _ psyn, psb_args = details, psb_def = pat, psb_dir = dir })
      = ppr_lhs <+> ppr_rhs
    where
      ppr_lhs = ptext (sLit "pattern") <+> ppr_details
      ppr_simple syntax = syntax <+> ppr pat

      (is_infix, ppr_details) = case details of
          InfixPatSyn v1 v2 -> (True, hsep [ppr v1, pprInfixOcc psyn, ppr v2])
          PrefixPatSyn vs   -> (False, hsep (pprPrefixOcc psyn : map ppr vs))

      ppr_rhs = case dir of
          Unidirectional           -> ppr_simple (ptext (sLit "<-"))
          ImplicitBidirectional    -> ppr_simple equals
          ExplicitBidirectional mg -> ppr_simple (ptext (sLit "<-")) <+> ptext (sLit "where") $$
                                      (nest 2 $ pprFunBind psyn is_infix mg)
\end{code}


\begin{code}
pprTicks :: SDoc -> SDoc -> SDoc
-- Print stuff about ticks only when -dppr-debug is on, to avoid
-- them appearing in error messages (from the desugarer); see Trac # 3263
-- Also print ticks in dumpStyle, so that -ddump-hpc actually does
-- something useful.
pprTicks pp_no_debug pp_when_debug
  = getPprStyle (\ sty -> if debugStyle sty || dumpStyle sty
                             then pp_when_debug
                             else pp_no_debug)
\end{code}

%************************************************************************
%*                                                                      *
                Implicit parameter bindings
%*                                                                      *
%************************************************************************

\begin{code}
data HsIPBinds l id
  = IPBinds
        [LIPBind l id]
        TcEvBinds       -- Only in typechecker output; binds
                        -- uses of the implicit parameters
  deriving (Typeable)
-- deriving instance (DataId id, Data l) => Data (HsIPBinds l id)

isEmptyIPBinds :: HsIPBinds l id -> Bool
isEmptyIPBinds (IPBinds is ds) = null is && isEmptyTcEvBinds ds

type LIPBind l id = GenLocated l (IPBind l id)

-- | Implicit parameter bindings.
{- These bindings start off as (Left "x") in the parser and stay
that way until after type-checking when they are replaced with
(Right d), where "d" is the name of the dictionary holding the
evidene for the implicit parameter. -}
data IPBind l id
  = IPBind (Either HsIPName id) (LHsExpr l id)
  deriving (Typeable)
-- deriving instance (DataId name, Data l) => Data (IPBind l name)

instance (OutputableBndr id, SrcAnnotation l) => Outputable (HsIPBinds l id) where
  ppr (IPBinds bs ds) = pprDeeperList vcat (map ppr bs)
                        $$ ifPprDebug (ppr ds)

instance (OutputableBndr id, SrcAnnotation l) => Outputable (IPBind l id) where
  ppr (IPBind lr rhs) = name <+> equals <+> pprExpr (unLoc rhs)
    where name = case lr of
                   Left ip  -> pprBndr LetBind ip
                   Right id -> pprBndr LetBind id
\end{code}


%************************************************************************
%*                                                                      *
\subsection{@Sig@: type signatures and value-modifying user pragmas}
%*                                                                      *
%************************************************************************

It is convenient to lump ``value-modifying'' user-pragmas (e.g.,
``specialise this function to these four types...'') in with type
signatures.  Then all the machinery to move them into place, etc.,
serves for both.

\begin{code}
type LSig l name = GenLocated l (Sig l name)

-- | Signatures and pragmas
data Sig l name
  =   -- | An ordinary type signature
      -- @f :: Num a => a -> a@
    TypeSig [GenLocated l name] (LHsType l name)

      -- | A pattern synonym type signature
      -- @pattern (Eq b) => P a b :: (Num a) => T a
  | PatSynSig (GenLocated l name)
              (HsPatSynDetails (LHsType l name))
              (LHsType l name)    -- Type
              (LHsContext l name) -- Provided context
              (LHsContext l name) -- Required contex

        -- | A type signature for a default method inside a class
        --
        -- > default eq :: (Representable0 a, GEq (Rep0 a)) => a -> a -> Bool
        --
  | GenericSig [GenLocated l name] (LHsType l name)

        -- | A type signature in generated code, notably the code
        -- generated for record selectors.  We simply record
        -- the desired Id itself, replete with its name, type
        -- and IdDetails.  Otherwise it's just like a type
        -- signature: there should be an accompanying binding
  | IdSig Id

        -- | An ordinary fixity declaration
        --
        -- >     infixl *** 8
        --
  | FixSig (FixitySig l name)

        -- | An inline pragma
        --
        -- > {#- INLINE f #-}
        --
  | InlineSig   (GenLocated l name) -- Function name
                InlinePragma        -- Never defaultInlinePragma

        -- | A specialisation pragma
        --
        -- > {-# SPECIALISE f :: Int -> Int #-}
        --
  | SpecSig     (GenLocated l name) -- Specialise a function or datatype  ...
                (LHsType l name)    -- ... to these types
                InlinePragma    -- The pragma on SPECIALISE_INLINE form.
                                -- If it's just defaultInlinePragma, then we said
                                --    SPECIALISE, not SPECIALISE_INLINE

        -- | A specialisation pragma for instance declarations only
        --
        -- > {-# SPECIALISE instance Eq [Int] #-}
        --
        -- (Class tys); should be a specialisation of the
        -- current instance declaration
  | SpecInstSig (LHsType l name)

        -- | A minimal complete definition pragma
        --
        -- > {-# MINIMAL a | (b, c | (d | e)) #-}
  | MinimalSig (BooleanFormula (GenLocated l name))

  deriving (Typeable)
-- deriving instance (DataId name, Data l) => Data (Sig l name)


type LFixitySig l name = GenLocated l (FixitySig l name)
data FixitySig l name = FixitySig (GenLocated l name) Fixity
  deriving (Data, Typeable)

-- | TsSpecPrags conveys pragmas from the type checker to the desugarer
data TcSpecPrags l
  = IsDefaultMethod     -- ^ Super-specialised: a default method should
                        -- be macro-expanded at every call site
  | SpecPrags [LTcSpecPrag l]
  deriving (Data, Typeable)

type LTcSpecPrag l = GenLocated l TcSpecPrag

data TcSpecPrag
  = SpecPrag
        Id              
        HsWrapper       
        InlinePragma    
  -- ^ The Id to be specialised, an wrapper that specialises the
  -- polymorphic function, and inlining spec for the specialised function
  deriving (Data, Typeable)

noSpecPrags :: TcSpecPrags l
noSpecPrags = SpecPrags []

hasSpecPrags :: TcSpecPrags l -> Bool
hasSpecPrags (SpecPrags ps) = not (null ps)
hasSpecPrags IsDefaultMethod = False

isDefaultMethod :: TcSpecPrags l -> Bool
isDefaultMethod IsDefaultMethod = True
isDefaultMethod (SpecPrags {})  = False


isFixityLSig :: LSig l name -> Bool
isFixityLSig (L _ (FixSig {})) = True
isFixityLSig _                 = False

isVanillaLSig :: LSig l name -> Bool       -- User type signatures
-- A badly-named function, but it's part of the GHCi (used
-- by Haddock) so I don't want to change it gratuitously.
isVanillaLSig (L _(TypeSig {})) = True
isVanillaLSig _                 = False

isTypeLSig :: LSig l name -> Bool  -- Type signatures
isTypeLSig (L _(TypeSig {}))    = True
isTypeLSig (L _(GenericSig {})) = True
isTypeLSig (L _(IdSig {}))      = True
isTypeLSig _                    = False

isSpecLSig :: LSig l name -> Bool
isSpecLSig (L _(SpecSig {})) = True
isSpecLSig _                 = False

isSpecInstLSig :: LSig l name -> Bool
isSpecInstLSig (L _ (SpecInstSig {})) = True
isSpecInstLSig _                      = False

isPragLSig :: LSig l name -> Bool
-- Identifies pragmas
isPragLSig (L _ (SpecSig {}))   = True
isPragLSig (L _ (InlineSig {})) = True
isPragLSig _                    = False

isInlineLSig :: LSig l name -> Bool
-- Identifies inline pragmas
isInlineLSig (L _ (InlineSig {})) = True
isInlineLSig _                    = False

isMinimalLSig :: LSig l name -> Bool
isMinimalLSig (L _ (MinimalSig {})) = True
isMinimalLSig _                    = False

hsSigDoc :: Sig l name -> SDoc
hsSigDoc (TypeSig {})           = ptext (sLit "type signature")
hsSigDoc (PatSynSig {})         = ptext (sLit "pattern synonym signature")
hsSigDoc (GenericSig {})        = ptext (sLit "default type signature")
hsSigDoc (IdSig {})             = ptext (sLit "id signature")
hsSigDoc (SpecSig {})           = ptext (sLit "SPECIALISE pragma")
hsSigDoc (InlineSig _ prag)     = ppr (inlinePragmaSpec prag) <+> ptext (sLit "pragma")
hsSigDoc (SpecInstSig {})       = ptext (sLit "SPECIALISE instance pragma")
hsSigDoc (FixSig {})            = ptext (sLit "fixity declaration")
hsSigDoc (MinimalSig {})        = ptext (sLit "MINIMAL pragma")
\end{code}

Check if signatures overlap; this is used when checking for duplicate
signatures. Since some of the signatures contain a list of names, testing for
equality is not enough -- we have to check if they overlap.

\begin{code}
instance (OutputableBndr name, SrcAnnotation l) => Outputable (Sig l name) where
    ppr sig = ppr_sig sig

ppr_sig :: (OutputableBndr name, SrcAnnotation l) => Sig l name -> SDoc
ppr_sig (TypeSig vars ty)         = pprVarSig (map unLoc vars) (ppr ty)
ppr_sig (GenericSig vars ty)      = ptext (sLit "default") <+> pprVarSig (map unLoc vars) (ppr ty)
ppr_sig (IdSig id)                = pprVarSig [id] (ppr (varType id))
ppr_sig (FixSig fix_sig)          = ppr fix_sig
ppr_sig (SpecSig var ty inl)      = pragBrackets (pprSpec (unLoc var) (ppr ty) inl)
ppr_sig (InlineSig var inl)       = pragBrackets (ppr inl <+> pprPrefixOcc (unLoc var))
ppr_sig (SpecInstSig ty)          = pragBrackets (ptext (sLit "SPECIALIZE instance") <+> ppr ty)
ppr_sig (MinimalSig bf)           = pragBrackets (pprMinimalSig bf)
ppr_sig (PatSynSig name arg_tys ty prov req)
  = pprPatSynSig (unLoc name) False args (ppr ty) (pprCtx prov) (pprCtx req)
  where
    args = fmap ppr arg_tys

    pprCtx lctx = case unLoc lctx of
        [] -> Nothing
        ctx -> Just (pprHsContextNoArrow ctx)

pprPatSynSig :: (OutputableBndr a)
             => a -> Bool -> HsPatSynDetails SDoc -> SDoc -> Maybe SDoc -> Maybe SDoc -> SDoc
pprPatSynSig ident is_bidir args rhs_ty prov_theta req_theta
  = sep [ ptext (sLit "pattern")
        , thetaOpt prov_theta, name_and_args
        , colon
        , thetaOpt req_theta, rhs_ty
        ]
  where
    name_and_args = case args of
        PrefixPatSyn arg_tys ->
            pprPrefixOcc ident <+> sep arg_tys
        InfixPatSyn left_ty right_ty ->
            left_ty <+> pprInfixOcc ident <+> right_ty

    -- TODO: support explicit foralls
    thetaOpt = maybe empty (<+> darrow)

    colon = if is_bidir then dcolon else dcolon -- TODO

instance (OutputableBndr name, Outputable l) => Outputable (FixitySig l name) where
  ppr (FixitySig name fixity) = sep [ppr fixity, pprInfixOcc (unLoc name)]

pragBrackets :: SDoc -> SDoc
pragBrackets doc = ptext (sLit "{-#") <+> doc <+> ptext (sLit "#-}")

pprVarSig :: (OutputableBndr id) => [id] -> SDoc -> SDoc
pprVarSig vars pp_ty = sep [pprvars <+> dcolon, nest 2 pp_ty]
  where
    pprvars = hsep $ punctuate comma (map pprPrefixOcc vars)

pprSpec :: (OutputableBndr id) => id -> SDoc -> InlinePragma -> SDoc
pprSpec var pp_ty inl = ptext (sLit "SPECIALIZE") <+> pp_inl <+> pprVarSig [var] pp_ty
  where
    pp_inl | isDefaultInlinePragma inl = empty
           | otherwise = ppr inl

pprTcSpecPrags :: TcSpecPrags l -> SDoc
pprTcSpecPrags IsDefaultMethod = ptext (sLit "<default method>")
pprTcSpecPrags (SpecPrags ps)  = vcat (map (ppr . unLoc) ps)

instance Outputable TcSpecPrag where
  ppr (SpecPrag var _ inl) = pprSpec var (ptext (sLit "<type>")) inl

pprMinimalSig :: (OutputableBndr name, Outputable l)
  => BooleanFormula (GenLocated l name) -> SDoc
pprMinimalSig bf = ptext (sLit "MINIMAL") <+> ppr (fmap unLoc bf)
\end{code}

%************************************************************************
%*                                                                      *
\subsection[PatSynBind]{A pattern synonym definition}
%*                                                                      *
%************************************************************************

\begin{code}
data HsPatSynDetails a
  = InfixPatSyn a a
  | PrefixPatSyn [a]
  deriving (Data, Typeable)

instance Functor HsPatSynDetails where
    fmap f (InfixPatSyn left right) = InfixPatSyn (f left) (f right)
    fmap f (PrefixPatSyn args) = PrefixPatSyn (fmap f args)

instance Foldable HsPatSynDetails where
    foldMap f (InfixPatSyn left right) = f left `mappend` f right
    foldMap f (PrefixPatSyn args) = foldMap f args

instance Traversable HsPatSynDetails where
    traverse f (InfixPatSyn left right) = InfixPatSyn <$> f left <*> f right
    traverse f (PrefixPatSyn args) = PrefixPatSyn <$> traverse f args

data HsPatSynDir l id
  = Unidirectional
  | ImplicitBidirectional
  | ExplicitBidirectional (MatchGroup l id (LHsExpr l id))
  deriving (Typeable)
-- deriving instance (DataId id, Data l) => Data (HsPatSynDir l id)
\end{code}
