%
% (c) The University of Glasgow 2006
% (c) The GRASP/AQUA Project, Glasgow University, 1992-1998
%

\begin{code}
{-# LANGUAGE CPP #-}
{-# LANGUAGE DeriveDataTypeable, DeriveFunctor, DeriveFoldable,
             DeriveTraversable #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE UndecidableInstances #-} -- Note [Pass sensitive types]
                                      -- in module PlaceHolder
{-# LANGUAGE ConstraintKinds #-}

-- | Abstract syntax of global declarations.
--
-- Definitions for: @SynDecl@ and @ConDecl@, @ClassDecl@,
-- @InstDecl@, @DefaultDecl@ and @ForeignDecl@.
module HsDecls (
  -- * Toplevel declarations
  HsDecl(..), LHsDecl, HsDataDefn(..),
  -- ** Class or type declarations
  TyClDecl(..), LTyClDecl,
  TyClGroup(..), tyClGroupConcat, mkTyClGroup,
  isClassDecl, isDataDecl, isSynDecl, tcdName,
  isFamilyDecl, isTypeFamilyDecl, isDataFamilyDecl,
  isOpenTypeFamilyInfo, isClosedTypeFamilyInfo,
  tyFamInstDeclName, tyFamInstDeclLName,
  countTyClDecls, pprTyClDeclFlavour,
  tyClDeclLName, tyClDeclTyVars,
  hsDeclHasCusk, famDeclHasCusk,
  FamilyDecl(..), LFamilyDecl,

  -- ** Instance declarations
  InstDecl(..), LInstDecl, NewOrData(..), FamilyInfo(..),
  TyFamInstDecl(..), LTyFamInstDecl, instDeclDataFamInsts,
  DataFamInstDecl(..), LDataFamInstDecl, pprDataFamInstFlavour,
  TyFamEqn(..), TyFamInstEqn, LTyFamInstEqn, TyFamDefltEqn, LTyFamDefltEqn,
  LClsInstDecl, ClsInstDecl(..),

  -- ** Standalone deriving declarations
  DerivDecl(..), LDerivDecl,
  -- ** @RULE@ declarations
  RuleDecl(..), LRuleDecl, RuleBndr(..),
  collectRuleBndrSigTys,
  -- ** @VECTORISE@ declarations
  VectDecl(..), LVectDecl,
  lvectDeclName, lvectInstDecl,
  -- ** @default@ declarations
  DefaultDecl(..), LDefaultDecl,
  -- ** Template haskell declaration splice
  SpliceExplicitFlag(..),
  SpliceDecl(..), LSpliceDecl,
  -- ** Foreign function interface declarations
  ForeignDecl(..), LForeignDecl, ForeignImport(..), ForeignExport(..),
  noForeignImportCoercionYet, noForeignExportCoercionYet,
  CImportSpec(..),
  -- ** Data-constructor declarations
  ConDecl(..), LConDecl, ResType(..),
  HsConDeclDetails, hsConDeclArgTys,
  -- ** Document comments
  DocDecl(..), LDocDecl, docDeclDoc,
  -- ** Deprecations
  WarnDecl(..),  LWarnDecl,
  -- ** Annotations
  AnnDecl(..), LAnnDecl,
  AnnProvenance(..), annProvenanceName_maybe,
  -- ** Role annotations
  RoleAnnotDecl(..), LRoleAnnotDecl, roleAnnotDeclName,

  -- * Grouping
  HsGroup(..),  emptyRdrGroup, emptyRnGroup, appendGroups

    ) where

-- friends:
import {-# SOURCE #-}   HsExpr( LHsExpr, HsExpr, HsSplice, pprExpr, pprUntypedSplice )
        -- Because Expr imports Decls via HsBracket

import HsBinds
import HsPat
import HsTypes
import HsDoc
import TyCon
import Name
import BasicTypes
import Coercion
import ForeignCall
import PlaceHolder ( PostTc,PostRn,PlaceHolder(..),DataId )
import NameSet

-- others:
import InstEnv
import Class
import Outputable
import Util
import SrcLoc
import FastString

import Bag
import Data.Data        hiding (TyCon,Fixity)
#if __GLASGOW_HASKELL__ < 709
import Data.Foldable ( Foldable )
import Data.Traversable ( Traversable )
#endif
import Data.Maybe
\end{code}

%************************************************************************
%*                                                                      *
\subsection[HsDecl]{Declarations}
%*                                                                      *
%************************************************************************

\begin{code}
type LHsDecl l id = GenLocated l (HsDecl l id)

-- | A Haskell Declaration
data HsDecl l id
  = TyClD       (TyClDecl l id)     -- ^ A type or class declaration.
  | InstD       (InstDecl  l id)    -- ^ An instance declaration.
  | DerivD      (DerivDecl l id)
  | ValD        (HsBind l id)
  | SigD        (Sig l id)
  | DefD        (DefaultDecl l id)
  | ForD        (ForeignDecl l id)
  | WarningD    (WarnDecl id)
  | AnnD        (AnnDecl l id)
  | RuleD       (RuleDecl l id)
  | VectD       (VectDecl l id)
  | SpliceD     (SpliceDecl l id)
  | DocD        (DocDecl)
  | QuasiQuoteD (HsQuasiQuote id)
  | RoleAnnotD  (RoleAnnotDecl l id)
  deriving (Typeable)
deriving instance (DataId id, Data l) => Data (HsDecl l id)


-- NB: all top-level fixity decls are contained EITHER
-- EITHER SigDs
-- OR     in the ClassDecls in TyClDs
--
-- The former covers
--      a) data constructors
--      b) class methods (but they can be also done in the
--              signatures of class decls)
--      c) imported functions (that have an IfacSig)
--      d) top level decls
--
-- The latter is for class methods only

-- | A 'HsDecl' is categorised into a 'HsGroup' before being
-- fed to the renamer.
data HsGroup l id
  = HsGroup {
        hs_valds  :: HsValBinds l id,
        hs_splcds :: [LSpliceDecl l id],

        hs_tyclds :: [TyClGroup l id],
                -- A list of mutually-recursive groups
                -- No family-instances here; they are in hs_instds
                -- Parser generates a singleton list;
                -- renamer does dependency analysis

        hs_instds  :: [LInstDecl l id],
                -- Both class and family instance declarations in here

        hs_derivds :: [LDerivDecl l id],

        hs_fixds  :: [LFixitySig l id],
                -- Snaffled out of both top-level fixity signatures,
                -- and those in class declarations

        hs_defds  :: [LDefaultDecl l id],
        hs_fords  :: [LForeignDecl l id],
        hs_warnds :: [LWarnDecl l id],
        hs_annds  :: [LAnnDecl l id],
        hs_ruleds :: [LRuleDecl l id],
        hs_vects  :: [LVectDecl l id],

        hs_docs   :: [LDocDecl l]
  } deriving (Typeable)
deriving instance (DataId id, Data l) => Data (HsGroup l id)

emptyGroup, emptyRdrGroup, emptyRnGroup :: HsGroup l a
emptyRdrGroup = emptyGroup { hs_valds = emptyValBindsIn }
emptyRnGroup  = emptyGroup { hs_valds = emptyValBindsOut }

emptyGroup = HsGroup { hs_tyclds = [], hs_instds = [],
                       hs_derivds = [],
                       hs_fixds = [], hs_defds = [], hs_annds = [],
                       hs_fords = [], hs_warnds = [], hs_ruleds = [], hs_vects = [],
                       hs_valds = error "emptyGroup hs_valds: Can't happen",
                       hs_splcds = [],
                       hs_docs = [] }

appendGroups :: HsGroup l a -> HsGroup l a -> HsGroup l a
appendGroups
    HsGroup {
        hs_valds  = val_groups1,
        hs_splcds = spliceds1,
        hs_tyclds = tyclds1,
        hs_instds = instds1,
        hs_derivds = derivds1,
        hs_fixds  = fixds1,
        hs_defds  = defds1,
        hs_annds  = annds1,
        hs_fords  = fords1,
        hs_warnds = warnds1,
        hs_ruleds = rulds1,
        hs_vects = vects1,
  hs_docs   = docs1 }
    HsGroup {
        hs_valds  = val_groups2,
        hs_splcds = spliceds2,
        hs_tyclds = tyclds2,
        hs_instds = instds2,
        hs_derivds = derivds2,
        hs_fixds  = fixds2,
        hs_defds  = defds2,
        hs_annds  = annds2,
        hs_fords  = fords2,
        hs_warnds = warnds2,
        hs_ruleds = rulds2,
        hs_vects  = vects2,
        hs_docs   = docs2 }
  =
    HsGroup {
        hs_valds  = val_groups1 `plusHsValBinds` val_groups2,
        hs_splcds = spliceds1 ++ spliceds2,
        hs_tyclds = tyclds1 ++ tyclds2,
        hs_instds = instds1 ++ instds2,
        hs_derivds = derivds1 ++ derivds2,
        hs_fixds  = fixds1 ++ fixds2,
        hs_annds  = annds1 ++ annds2,
        hs_defds  = defds1 ++ defds2,
        hs_fords  = fords1 ++ fords2,
        hs_warnds = warnds1 ++ warnds2,
        hs_ruleds = rulds1 ++ rulds2,
        hs_vects  = vects1 ++ vects2,
        hs_docs   = docs1  ++ docs2 }
\end{code}

\begin{code}
instance (OutputableBndr name, ApiAnnotation l) => Outputable (HsDecl l name) where
    ppr (TyClD dcl)             = ppr dcl
    ppr (ValD binds)            = ppr binds
    ppr (DefD def)              = ppr def
    ppr (InstD inst)            = ppr inst
    ppr (DerivD deriv)          = ppr deriv
    ppr (ForD fd)               = ppr fd
    ppr (SigD sd)               = ppr sd
    ppr (RuleD rd)              = ppr rd
    ppr (VectD vect)            = ppr vect
    ppr (WarningD wd)           = ppr wd
    ppr (AnnD ad)               = ppr ad
    ppr (SpliceD dd)            = ppr dd
    ppr (DocD doc)              = ppr doc
    ppr (QuasiQuoteD qq)        = ppr qq
    ppr (RoleAnnotD ra)         = ppr ra

instance (OutputableBndr name, ApiAnnotation l) => Outputable (HsGroup l name) where
    ppr (HsGroup { hs_valds  = val_decls,
                   hs_tyclds = tycl_decls,
                   hs_instds = inst_decls,
                   hs_derivds = deriv_decls,
                   hs_fixds  = fix_decls,
                   hs_warnds = deprec_decls,
                   hs_annds  = ann_decls,
                   hs_fords  = foreign_decls,
                   hs_defds  = default_decls,
                   hs_ruleds = rule_decls,
                   hs_vects  = vect_decls })
        = vcat_mb empty
            [ppr_ds fix_decls, ppr_ds default_decls,
             ppr_ds deprec_decls, ppr_ds ann_decls,
             ppr_ds rule_decls,
             ppr_ds vect_decls,
             if isEmptyValBinds val_decls
                then Nothing
                else Just (ppr val_decls),
             ppr_ds (tyClGroupConcat tycl_decls),
             ppr_ds inst_decls,
             ppr_ds deriv_decls,
             ppr_ds foreign_decls]
        where
          ppr_ds :: Outputable a => [a] -> Maybe SDoc
          ppr_ds [] = Nothing
          ppr_ds ds = Just (vcat (map ppr ds))

          vcat_mb :: SDoc -> [Maybe SDoc] -> SDoc
          -- Concatenate vertically with white-space between non-blanks
          vcat_mb _    []             = empty
          vcat_mb gap (Nothing : ds) = vcat_mb gap ds
          vcat_mb gap (Just d  : ds) = gap $$ d $$ vcat_mb blankLine ds

data SpliceExplicitFlag = ExplicitSplice | -- <=> $(f x y)
                          ImplicitSplice   -- <=> f x y,  i.e. a naked top level expression
    deriving (Data, Typeable)

type LSpliceDecl l name = GenLocated l (SpliceDecl l name)
data SpliceDecl l id
  = SpliceDecl                  -- Top level splice
        (GenLocated l (HsSplice l id))
        SpliceExplicitFlag
    deriving (Typeable)
deriving instance (DataId id, Data l) => Data (SpliceDecl l id)

instance (OutputableBndr name, ApiAnnotation l) => Outputable (SpliceDecl l name) where
   ppr (SpliceDecl (L _ e) _) = pprUntypedSplice e
\end{code}


%************************************************************************
%*                                                                      *
\subsection[SynDecl]{@data@, @newtype@ or @type@ (synonym) type declaration}
%*                                                                      *
%************************************************************************

                --------------------------------
                        THE NAMING STORY
                --------------------------------

Here is the story about the implicit names that go with type, class,
and instance decls.  It's a bit tricky, so pay attention!

"Implicit" (or "system") binders
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  Each data type decl defines
        a worker name for each constructor
        to-T and from-T convertors
  Each class decl defines
        a tycon for the class
        a data constructor for that tycon
        the worker for that constructor
        a selector for each superclass

All have occurrence names that are derived uniquely from their parent
declaration.

None of these get separate definitions in an interface file; they are
fully defined by the data or class decl.  But they may *occur* in
interface files, of course.  Any such occurrence must haul in the
relevant type or class decl.

Plan of attack:
 - Ensure they "point to" the parent data/class decl
   when loading that decl from an interface file
   (See RnHiFiles.getSysBinders)

 - When typechecking the decl, we build the implicit TyCons and Ids.
   When doing so we look them up in the name cache (RnEnv.lookupSysName),
   to ensure correct module and provenance is set

These are the two places that we have to conjure up the magic derived
names.  (The actual magic is in OccName.mkWorkerOcc, etc.)

Default methods
~~~~~~~~~~~~~~~
 - Occurrence name is derived uniquely from the method name
   E.g. $dmmax

 - If there is a default method name at all, it's recorded in
   the ClassOpSig (in HsBinds), in the DefMeth field.
   (DefMeth is defined in Class.lhs)

Source-code class decls and interface-code class decls are treated subtly
differently, which has given me a great deal of confusion over the years.
Here's the deal.  (We distinguish the two cases because source-code decls
have (Just binds) in the tcdMeths field, whereas interface decls have Nothing.

In *source-code* class declarations:

 - When parsing, every ClassOpSig gets a DefMeth with a suitable RdrName
   This is done by RdrHsSyn.mkClassOpSigDM

 - The renamer renames it to a Name

 - During typechecking, we generate a binding for each $dm for
   which there's a programmer-supplied default method:
        class Foo a where
          op1 :: <type>
          op2 :: <type>
          op1 = ...
   We generate a binding for $dmop1 but not for $dmop2.
   The Class for Foo has a NoDefMeth for op2 and a DefMeth for op1.
   The Name for $dmop2 is simply discarded.

In *interface-file* class declarations:
  - When parsing, we see if there's an explicit programmer-supplied default method
    because there's an '=' sign to indicate it:
        class Foo a where
          op1 = :: <type>       -- NB the '='
          op2   :: <type>
    We use this info to generate a DefMeth with a suitable RdrName for op1,
    and a NoDefMeth for op2
  - The interface file has a separate definition for $dmop1, with unfolding etc.
  - The renamer renames it to a Name.
  - The renamer treats $dmop1 as a free variable of the declaration, so that
    the binding for $dmop1 will be sucked in.  (See RnHsSyn.tyClDeclFVs)
    This doesn't happen for source code class decls, because they *bind* the default method.

Dictionary functions
~~~~~~~~~~~~~~~~~~~~
Each instance declaration gives rise to one dictionary function binding.

The type checker makes up new source-code instance declarations
(e.g. from 'deriving' or generic default methods --- see
TcInstDcls.tcInstDecls1).  So we can't generate the names for
dictionary functions in advance (we don't know how many we need).

On the other hand for interface-file instance declarations, the decl
specifies the name of the dictionary function, and it has a binding elsewhere
in the interface file:
        instance {Eq Int} = dEqInt
        dEqInt :: {Eq Int} <pragma info>

So again we treat source code and interface file code slightly differently.

Source code:
  - Source code instance decls have a Nothing in the (Maybe name) field
    (see data InstDecl below)

  - The typechecker makes up a Local name for the dict fun for any source-code
    instance decl, whether it comes from a source-code instance decl, or whether
    the instance decl is derived from some other construct (e.g. 'deriving').

  - The occurrence name it chooses is derived from the instance decl (just for
    documentation really) --- e.g. dNumInt.  Two dict funs may share a common
    occurrence name, but will have different uniques.  E.g.
        instance Foo [Int]  where ...
        instance Foo [Bool] where ...
    These might both be dFooList

  - The CoreTidy phase externalises the name, and ensures the occurrence name is
    unique (this isn't special to dict funs).  So we'd get dFooList and dFooList1.

  - We can take this relaxed approach (changing the occurrence name later)
    because dict fun Ids are not captured in a TyCon or Class (unlike default
    methods, say).  Instead, they are kept separately in the InstEnv.  This
    makes it easy to adjust them after compiling a module.  (Once we've finished
    compiling that module, they don't change any more.)


Interface file code:
  - The instance decl gives the dict fun name, so the InstDecl has a (Just name)
    in the (Maybe name) field.

  - RnHsSyn.instDeclFVs treats the dict fun name as free in the decl, so that we
    suck in the dfun binding


\begin{code}
type LTyClDecl l name = GenLocated l (TyClDecl l name)

-- | A type or class declaration.
data TyClDecl l name
  = ForeignType {
                tcdLName    :: GenLocated l name,
                tcdExtName  :: Maybe FastString
    }

  | -- | @type/data family T :: *->*@
    FamDecl { tcdFam :: FamilyDecl l name }

  | -- | @type@ declaration
    SynDecl { tcdLName  :: GenLocated l name       -- ^ Type constructor
            , tcdTyVars :: LHsTyVarBndrs l name    -- ^ Type variables; for an associated type
                                                   --   these include outer binders
            , tcdRhs    :: LHsType l name          -- ^ RHS of type declaration
            , tcdFVs    :: PostRn name NameSet }

  | -- | @data@ declaration
    DataDecl { tcdLName    :: GenLocated l name    -- ^ Type constructor
             , tcdTyVars   :: LHsTyVarBndrs l name -- ^ Type variables; for an assoicated type
                                                   --   these include outer binders
                                                   -- Eg  class T a where
                                                   --       type F a :: *
                                                   --       type F a = a -> a
                                                   -- Here the type decl for 'f' includes 'a' 
                                                   -- in its tcdTyVars
             , tcdDataDefn :: HsDataDefn l name
             , tcdFVs      :: PostRn name NameSet }

  | ClassDecl { tcdCtxt    :: LHsContext l name,        -- ^ Context...
                tcdLName   :: GenLocated l name,        -- ^ Name of the class
                tcdTyVars  :: LHsTyVarBndrs l name,     -- ^ Class type variables
                tcdFDs     :: [GenLocated l (FunDep name)], -- ^ Functional deps
                tcdSigs    :: [LSig l name],              -- ^ Methods' signatures
                tcdMeths   :: LHsBinds l name,            -- ^ Default methods
                tcdATs     :: [LFamilyDecl l name],       -- ^ Associated types; ie
                tcdATDefs  :: [LTyFamDefltEqn l name],    -- ^ Associated type defaults
                tcdDocs    :: [LDocDecl l],               -- ^ Haddock docs
                tcdFVs     :: PostRn name NameSet
    }

  deriving (Typeable)
deriving instance (DataId id, Data l) => Data (TyClDecl l id)

 -- This is used in TcTyClsDecls to represent
 -- strongly connected components of decls
 -- No familiy instances in here
 -- The role annotations must be grouped with their decls for the
 -- type-checker to infer roles correctly
data TyClGroup l name
  = TyClGroup { group_tyclds :: [LTyClDecl l name]
              , group_roles  :: [LRoleAnnotDecl l name] }
    deriving (Typeable)
deriving instance (DataId id, Data l) => Data (TyClGroup l id)

tyClGroupConcat :: [TyClGroup l name] -> [LTyClDecl l name]
tyClGroupConcat = concatMap group_tyclds

mkTyClGroup :: [LTyClDecl l name] -> TyClGroup l name
mkTyClGroup decls = TyClGroup { group_tyclds = decls, group_roles = [] }

type LFamilyDecl l name = GenLocated l (FamilyDecl l name)
data FamilyDecl l name = FamilyDecl
  { fdInfo    :: FamilyInfo l name          -- type or data, closed or open
  , fdLName   :: GenLocated l name          -- type constructor
  , fdTyVars  :: LHsTyVarBndrs l name       -- type variables
  , fdKindSig :: Maybe (LHsKind l name) }   -- result kind
  deriving( Typeable )
deriving instance (DataId id, Data l) => Data (FamilyDecl l id)

data FamilyInfo l name
  = DataFamily
  | OpenTypeFamily
     -- this list might be empty, if we're in an hs-boot file and the user
     -- said "type family Foo x where .."
  | ClosedTypeFamily [LTyFamInstEqn l name]
  deriving( Typeable )
deriving instance (DataId name, Data l) => Data (FamilyInfo l name)

\end{code}

------------------------------
Simple classifiers

\begin{code}
-- | @True@ <=> argument is a @data@\/@newtype@
-- declaration.
isDataDecl :: TyClDecl l name -> Bool
isDataDecl (DataDecl {}) = True
isDataDecl _other        = False

-- | type or type instance declaration
isSynDecl :: TyClDecl l name -> Bool
isSynDecl (SynDecl {})   = True
isSynDecl _other        = False

-- | type class
isClassDecl :: TyClDecl l name -> Bool
isClassDecl (ClassDecl {}) = True
isClassDecl _              = False

-- | type/data family declaration
isFamilyDecl :: TyClDecl l name -> Bool
isFamilyDecl (FamDecl {})  = True
isFamilyDecl _other        = False

-- | type family declaration
isTypeFamilyDecl :: TyClDecl l name -> Bool
isTypeFamilyDecl (FamDecl (FamilyDecl { fdInfo = info })) = case info of
  OpenTypeFamily      -> True
  ClosedTypeFamily {} -> True
  _                   -> False
isTypeFamilyDecl _ = False

-- | open type family info
isOpenTypeFamilyInfo :: FamilyInfo l name -> Bool
isOpenTypeFamilyInfo OpenTypeFamily = True
isOpenTypeFamilyInfo _              = False

-- | closed type family info
isClosedTypeFamilyInfo :: FamilyInfo l name -> Bool
isClosedTypeFamilyInfo (ClosedTypeFamily {}) = True
isClosedTypeFamilyInfo _                     = False

-- | data family declaration
isDataFamilyDecl :: TyClDecl l name -> Bool
isDataFamilyDecl (FamDecl (FamilyDecl { fdInfo = DataFamily })) = True
isDataFamilyDecl _other      = False

\end{code}

Dealing with names

\begin{code}
tyFamInstDeclName :: (OutputableBndr name, Outputable l)
                  => TyFamInstDecl l name -> name
tyFamInstDeclName = unLoc . tyFamInstDeclLName

tyFamInstDeclLName :: (OutputableBndr name, Outputable l)
                   => TyFamInstDecl l name -> GenLocated l name
tyFamInstDeclLName (TyFamInstDecl { tfid_eqn =
                     (L _ (TyFamEqn { tfe_tycon = ln })) })
  = ln

tyClDeclLName :: TyClDecl l name -> GenLocated l name
tyClDeclLName (FamDecl { tcdFam = FamilyDecl { fdLName = ln } }) = ln
tyClDeclLName decl = tcdLName decl

tcdName :: TyClDecl l name -> name
tcdName = unLoc . tyClDeclLName

tyClDeclTyVars :: (OutputableBndr name, ApiAnnotation l)
  => TyClDecl l name -> LHsTyVarBndrs l name
tyClDeclTyVars decl@(ForeignType {}) = pprPanic "tyClDeclTyVars" (ppr decl)
tyClDeclTyVars (FamDecl { tcdFam = FamilyDecl { fdTyVars = tvs } }) = tvs
tyClDeclTyVars d = tcdTyVars d
\end{code}

\begin{code}
countTyClDecls :: [TyClDecl l name] -> (Int, Int, Int, Int, Int)
        -- class, synonym decls, data, newtype, family decls
countTyClDecls decls
 = (count isClassDecl    decls,
    count isSynDecl      decls,  -- excluding...
    count isDataTy       decls,  -- ...family...
    count isNewTy        decls,  -- ...instances
    count isFamilyDecl   decls)
 where
   isDataTy DataDecl{ tcdDataDefn = HsDataDefn { dd_ND = DataType } } = True
   isDataTy _                                                       = False

   isNewTy DataDecl{ tcdDataDefn = HsDataDefn { dd_ND = NewType } } = True
   isNewTy _                                                      = False

-- | Does this declaration have a complete, user-supplied kind signature?
-- See Note [Complete user-supplied kind signatures]
hsDeclHasCusk :: TyClDecl l name -> Bool
hsDeclHasCusk (ForeignType {}) = True
hsDeclHasCusk (FamDecl { tcdFam = fam_decl }) = famDeclHasCusk fam_decl
hsDeclHasCusk (SynDecl { tcdTyVars = tyvars, tcdRhs = rhs })
  = hsTvbAllKinded tyvars && rhs_annotated rhs
  where
    rhs_annotated (L _ ty) = case ty of
      HsParTy lty  -> rhs_annotated lty
      HsKindSig {} -> True
      _            -> False
hsDeclHasCusk (DataDecl { tcdTyVars = tyvars })  = hsTvbAllKinded tyvars
hsDeclHasCusk (ClassDecl { tcdTyVars = tyvars }) = hsTvbAllKinded tyvars

-- | Does this family declaration have a complete, user-supplied kind signature?
famDeclHasCusk :: FamilyDecl l name -> Bool
famDeclHasCusk (FamilyDecl { fdInfo = ClosedTypeFamily _
                           , fdTyVars = tyvars
                           , fdKindSig = m_sig })
  = hsTvbAllKinded tyvars && isJust m_sig
famDeclHasCusk _ = True  -- all open families have CUSKs!
\end{code}

Note [Complete user-supplied kind signatures]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
We kind-check declarations differently if they have a complete, user-supplied
kind signature (CUSK). This is because we can safely generalise a CUSKed
declaration before checking all of the others, supporting polymorphic recursion.
See https://ghc.haskell.org/trac/ghc/wiki/GhcKinds/KindInference#Proposednewstrategy
and #9200 for lots of discussion of how we got here.

A declaration has a CUSK if we can know its complete kind without doing any inference,
at all. Here are the rules:

 - A class or datatype is said to have a CUSK if and only if all of its type
variables are annotated. Its result kind is, by construction, Constraint or *
respectively.

 - A type synonym has a CUSK if and only if all of its type variables and its
RHS are annotated with kinds.

 - A closed type family is said to have a CUSK if and only if all of its type
variables and its return type are annotated.

 - An open type family always has a CUSK -- unannotated type variables (and return type) default to *.

\begin{code}
instance (OutputableBndr name, ApiAnnotation l)
              => Outputable (TyClDecl l name) where

    ppr (ForeignType {tcdLName = ltycon})
        = hsep [ptext (sLit "foreign import type dotnet"), ppr ltycon]

    ppr (FamDecl { tcdFam = decl }) = ppr decl
    ppr (SynDecl { tcdLName = ltycon, tcdTyVars = tyvars, tcdRhs = rhs })
      = hang (ptext (sLit "type") <+>
              pp_vanilla_decl_head ltycon tyvars [] <+> equals)
          4 (ppr rhs)

    ppr (DataDecl { tcdLName = ltycon, tcdTyVars = tyvars, tcdDataDefn = defn })
      = pp_data_defn (pp_vanilla_decl_head ltycon tyvars) defn

    ppr (ClassDecl {tcdCtxt = context, tcdLName = lclas, tcdTyVars = tyvars,
                    tcdFDs  = fds,
                    tcdSigs = sigs, tcdMeths = methods,
                    tcdATs = ats, tcdATDefs = at_defs})
      | null sigs && isEmptyBag methods && null ats && null at_defs -- No "where" part
      = top_matter

      | otherwise       -- Laid out
      = vcat [ top_matter <+> ptext (sLit "where")
             , nest 2 $ pprDeclList (map ppr ats ++
                                     map ppr_fam_deflt_eqn at_defs ++
                                     pprLHsBindsForUser methods sigs) ]
      where
        top_matter = ptext (sLit "class")
                     <+> pp_vanilla_decl_head lclas tyvars (unLoc context)
                     <+> pprFundeps (map unLoc fds)

instance (OutputableBndr name, ApiAnnotation l)
    => Outputable (TyClGroup l name) where
  ppr (TyClGroup { group_tyclds = tyclds, group_roles = roles })
    = ppr tyclds $$
      ppr roles

instance (OutputableBndr name, ApiAnnotation l)
  => Outputable (FamilyDecl l name) where
  ppr (FamilyDecl { fdInfo = info, fdLName = ltycon,
                    fdTyVars = tyvars, fdKindSig = mb_kind})
      = vcat [ pprFlavour info <+> pp_vanilla_decl_head ltycon tyvars [] <+> pp_kind <+> pp_where
             , nest 2 $ pp_eqns ]
        where
          pp_kind = case mb_kind of
                      Nothing   -> empty
                      Just kind -> dcolon <+> ppr kind
          (pp_where, pp_eqns) = case info of
            ClosedTypeFamily eqns -> ( ptext (sLit "where")
                                     , if null eqns
                                       then ptext (sLit "..")
                                       else vcat $ map ppr_fam_inst_eqn eqns )
            _                     -> (empty, empty)

pprFlavour :: FamilyInfo l name -> SDoc
pprFlavour DataFamily            = ptext (sLit "data family")
pprFlavour OpenTypeFamily        = ptext (sLit "type family")
pprFlavour (ClosedTypeFamily {}) = ptext (sLit "type family")

instance Outputable (FamilyInfo l name) where
  ppr = pprFlavour

pp_vanilla_decl_head :: (OutputableBndr name, ApiAnnotation l)
   => GenLocated l name
   -> LHsTyVarBndrs l name
   -> HsContext l name
   -> SDoc
pp_vanilla_decl_head thing tyvars context
 = hsep [pprHsContext context, pprPrefixOcc (unLoc thing), ppr tyvars]

pp_fam_inst_lhs :: (OutputableBndr name, ApiAnnotation l)
   => GenLocated l name
   -> HsTyPats l name
   -> HsContext l name
   -> SDoc
pp_fam_inst_lhs thing (HsWB { hswb_cts = typats }) context -- explicit type patterns
   = hsep [ pprHsContext context, pprPrefixOcc (unLoc thing)
          , hsep (map (pprParendHsType.unLoc) typats)]

pprTyClDeclFlavour :: TyClDecl l a -> SDoc
pprTyClDeclFlavour (ClassDecl {})   = ptext (sLit "class")
pprTyClDeclFlavour (SynDecl {})     = ptext (sLit "type")
pprTyClDeclFlavour (ForeignType {}) = ptext (sLit "foreign type")
pprTyClDeclFlavour (FamDecl { tcdFam = FamilyDecl { fdInfo = info }})
  = pprFlavour info
pprTyClDeclFlavour (DataDecl { tcdDataDefn = HsDataDefn { dd_ND = nd } })
  = ppr nd
\end{code}

%************************************************************************
%*                                                                      *
\subsection[ConDecl]{A data-constructor declaration}
%*                                                                      *
%************************************************************************

\begin{code}

data HsDataDefn l name -- The payload of a data type defn
                       -- Used *both* for vanilla data declarations,
                       --       *and* for data family instances
  = -- | Declares a data type or newtype, giving its constructors
    -- @
    --  data/newtype T a = <constrs>
    --  data/newtype instance T [a] = <constrs>
    -- @
    HsDataDefn { dd_ND     :: NewOrData,
                 dd_ctxt   :: LHsContext l name,         -- ^ Context
                 dd_cType  :: Maybe CType,
                 dd_kindSig:: Maybe (LHsKind l name),
                     -- ^ Optional kind signature.
                     --
                     -- @(Just k)@ for a GADT-style @data@,
                     -- or @data instance@ decl, with explicit kind sig
                     --
                     -- Always @Nothing@ for H98-syntax decls

                 dd_cons   :: [LConDecl l name],
                     -- ^ Data constructors
                     --
                     -- For @data T a = T1 | T2 a@
                     --   the 'LConDecl's all have 'ResTyH98'.
                     -- For @data T a where { T1 :: T a }@
                     --   the 'LConDecls' all have 'ResTyGADT'.

                 dd_derivs :: Maybe [LHsType l name]
                     -- ^ Derivings; @Nothing@ => not specified,
                     --              @Just []@ => derive exactly what is asked
                     --
                     -- These "types" must be of form
                     -- @
                     --      forall ab. C ty1 ty2
                     -- @
                     -- Typically the foralls and ty args are empty, but they
                     -- are non-empty for the newtype-deriving case
    }
    deriving( Typeable )
deriving instance (DataId id, Data l) => Data (HsDataDefn l id)

data NewOrData
  = NewType                     -- ^ @newtype Blah ...@
  | DataType                    -- ^ @data Blah ...@
  deriving( Eq, Data, Typeable )                -- Needed because Demand derives Eq

type LConDecl l name = GenLocated l (ConDecl l name)

-- data T b = forall a. Eq a => MkT a b
--   MkT :: forall b a. Eq a => MkT a b

-- data T b where
--      MkT1 :: Int -> T Int

-- data T = Int `MkT` Int
--        | MkT2

-- data T a where
--      Int `MkT` Int :: T Int

data ConDecl l name
  = ConDecl
    { con_name      :: GenLocated l name
        -- ^ Constructor name.  This is used for the DataCon itself, and for
        -- the user-callable wrapper Id.

    , con_explicit  :: HsExplicitFlag
        -- ^ Is there an user-written forall? (cf. 'HsTypes.HsForAllTy')

    , con_qvars     :: LHsTyVarBndrs l name
        -- ^ Type variables.  Depending on 'con_res' this describes the
        -- following entities
        --
        --  - ResTyH98:  the constructor's *existential* type variables
        --  - ResTyGADT: *all* the constructor's quantified type variables
        --
        -- If con_explicit is Implicit, then con_qvars is irrelevant
        -- until after renaming.

    , con_cxt       :: LHsContext l name
        -- ^ The context.  This /does not/ include the \"stupid theta\" which
        -- lives only in the 'TyData' decl.

    , con_details   :: HsConDeclDetails l name
        -- ^ The main payload

    , con_res       :: ResType (LHsType l name)
        -- ^ Result type of the constructor

    , con_doc       :: Maybe (LHsDocString l)
        -- ^ A possible Haddock comment.

    , con_old_rec :: Bool
        -- ^ TEMPORARY field; True <=> user has employed now-deprecated syntax for
        --                             GADT-style record decl   C { blah } :: T a b
        -- Remove this when we no longer parse this stuff, and hence do not
        -- need to report decprecated use
    } deriving (Typeable)
deriving instance (DataId name, Data l) => Data (ConDecl l name)

type HsConDeclDetails l name = HsConDetails (LBangType l name)
                                            [ConDeclField l name]

hsConDeclArgTys :: HsConDeclDetails l name -> [LBangType l name]
hsConDeclArgTys (PrefixCon tys)    = tys
hsConDeclArgTys (InfixCon ty1 ty2) = [ty1,ty2]
hsConDeclArgTys (RecCon flds)      = map cd_fld_type flds

data ResType ty
   = ResTyH98           -- Constructor was declared using Haskell 98 syntax
   | ResTyGADT ty       -- Constructor was declared using GADT-style syntax,
                        --      and here is its result type
   deriving (Data, Typeable)

instance Outputable ty => Outputable (ResType ty) where
         -- Debugging only
   ppr ResTyH98       = ptext (sLit "ResTyH98")
   ppr (ResTyGADT ty) = ptext (sLit "ResTyGADT") <+> ppr ty
\end{code}


\begin{code}
pp_data_defn :: (OutputableBndr name, ApiAnnotation l)
                  => (HsContext l name -> SDoc)   -- Printing the header
                  -> HsDataDefn l name
                  -> SDoc
pp_data_defn pp_hdr (HsDataDefn { dd_ND = new_or_data, dd_ctxt = L _ context
                                , dd_kindSig = mb_sig
                                , dd_cons = condecls, dd_derivs = derivings })
  | null condecls
  = ppr new_or_data <+> pp_hdr context <+> pp_sig

  | otherwise
  = hang (ppr new_or_data <+> pp_hdr context <+> pp_sig)
       2 (pp_condecls condecls $$ pp_derivings)
  where
    pp_sig = case mb_sig of
               Nothing   -> empty
               Just kind -> dcolon <+> ppr kind
    pp_derivings = case derivings of
                     Nothing -> empty
                     Just ds -> hsep [ptext (sLit "deriving"), parens (interpp'SP ds)]

instance (OutputableBndr name, ApiAnnotation l) => Outputable (HsDataDefn l name) where
   ppr d = pp_data_defn (\_ -> ptext (sLit "Naked HsDataDefn")) d

instance Outputable NewOrData where
  ppr NewType  = ptext (sLit "newtype")
  ppr DataType = ptext (sLit "data")

pp_condecls :: (OutputableBndr name, ApiAnnotation l)
  => [LConDecl l name] -> SDoc
pp_condecls cs@(L _ ConDecl{ con_res = ResTyGADT _ } : _) -- In GADT syntax
  = hang (ptext (sLit "where")) 2 (vcat (map ppr cs))
pp_condecls cs                    -- In H98 syntax
  = equals <+> sep (punctuate (ptext (sLit " |")) (map ppr cs))

instance (OutputableBndr name, ApiAnnotation l) => Outputable (ConDecl l name) where
    ppr = pprConDecl

pprConDecl :: (OutputableBndr name, ApiAnnotation l) => ConDecl l name -> SDoc
pprConDecl (ConDecl { con_name = con, con_explicit = expl, con_qvars = tvs
                    , con_cxt = cxt, con_details = details
                    , con_res = ResTyH98, con_doc = doc })
  = sep [ppr_mbDoc doc, pprHsForAll expl tvs cxt, ppr_details details]
  where
    ppr_details (InfixCon t1 t2) = hsep [ppr t1, pprInfixOcc (unLoc con), ppr t2]
    ppr_details (PrefixCon tys)  = hsep (pprPrefixOcc (unLoc con) : map (pprParendHsType . unLoc) tys)
    ppr_details (RecCon fields)  = ppr con <+> pprConDeclFields fields

pprConDecl (ConDecl { con_name = con, con_explicit = expl, con_qvars = tvs
                    , con_cxt = cxt, con_details = PrefixCon arg_tys
                    , con_res = ResTyGADT res_ty })
  = ppr con <+> dcolon <+>
    sep [pprHsForAll expl tvs cxt, ppr (foldr mk_fun_ty res_ty arg_tys)]
  where
    mk_fun_ty a b = annNoLoc (HsFunTy a b)

pprConDecl (ConDecl { con_name = con, con_explicit = expl, con_qvars = tvs
                    , con_cxt = cxt, con_details = RecCon fields, con_res = ResTyGADT res_ty })
  = sep [ppr con <+> dcolon <+> pprHsForAll expl tvs cxt,
         pprConDeclFields fields <+> arrow <+> ppr res_ty]

pprConDecl decl@(ConDecl { con_details = InfixCon ty1 ty2, con_res = ResTyGADT {} })
  = pprConDecl (decl { con_details = PrefixCon [ty1,ty2] })
        -- In GADT syntax we don't allow infix constructors
        -- but the renamer puts them in this form (Note [Infix GADT constructors] in RnSource)
\end{code}

%************************************************************************
%*                                                                      *
                Instance declarations
%*                                                                      *
%************************************************************************

Note [Type family instance declarations in HsSyn]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
The data type TyFamEqn represents one equation of a type family instance.
It is parameterised over its tfe_pats field:

 * An ordinary type family instance declaration looks like this in source Haskell
      type instance T [a] Int = a -> a
   (or something similar for a closed family)
   It is represented by a TyFamInstEqn, with *type* in the tfe_pats field.

 * On the other hand, the *default instance* of an associated type looksl like
   this in source Haskell
      class C a where
        type T a b
        type T a b = a -> b   -- The default instance
   It is represented by a TyFamDefltEqn, with *type variables8 in the tfe_pats field.

\begin{code}
----------------- Type synonym family instances -------------
type LTyFamInstEqn  l name = GenLocated l (TyFamInstEqn  l name)
type LTyFamDefltEqn l name = GenLocated l (TyFamDefltEqn l name)

type HsTyPats l name = HsWithBndrs name [LHsType l name]
            -- ^ Type patterns (with kind and type bndrs)
            -- See Note [Family instance declaration binders]

type TyFamInstEqn  l name = TyFamEqn l name (HsTyPats l name)
type TyFamDefltEqn l name = TyFamEqn l name (LHsTyVarBndrs l name)
  -- See Note [Type family instance declarations in HsSyn]

-- | One equation in a type family instance declaration
-- See Note [Type family instance declarations in HsSyn]
data TyFamEqn l name pats
  = TyFamEqn
       { tfe_tycon :: GenLocated l name
       , tfe_pats  :: pats
       , tfe_rhs   :: LHsType l name }
  deriving( Typeable )
deriving instance (DataId name, Data pats, Data l)
  => Data (TyFamEqn l name pats)

type LTyFamInstDecl l name = GenLocated l (TyFamInstDecl l name)
data TyFamInstDecl l name
  = TyFamInstDecl
       { tfid_eqn  :: LTyFamInstEqn l name
       , tfid_fvs  :: PostRn name NameSet }
  deriving( Typeable )
deriving instance (DataId name, Data l) => Data (TyFamInstDecl l name)

----------------- Data family instances -------------

type LDataFamInstDecl l name = GenLocated l (DataFamInstDecl l name)
data DataFamInstDecl l name
  = DataFamInstDecl
       { dfid_tycon :: GenLocated l name
       , dfid_pats  :: HsTyPats l name    -- LHS
       , dfid_defn  :: HsDataDefn l name  -- RHS
       , dfid_fvs   :: PostRn name NameSet } -- Rree vars for
                                               -- dependency analysis
  deriving( Typeable )
deriving instance (DataId name, Data l) => Data (DataFamInstDecl l name)


----------------- Class instances -------------

type LClsInstDecl l name = GenLocated l (ClsInstDecl l name)
data ClsInstDecl l name
  = ClsInstDecl
      { cid_poly_ty :: LHsType l name  -- Context => Class Instance-type
                                       -- Using a polytype means that the renamer conveniently
                                       -- figures out the quantified type variables for us.
      , cid_binds         :: LHsBinds l name           -- Class methods
      , cid_sigs          :: [LSig l name]             -- User-supplied pragmatic info
      , cid_tyfam_insts   :: [LTyFamInstDecl l name]   -- Type family instances
      , cid_datafam_insts :: [LDataFamInstDecl l name] -- Data family instances
      , cid_overlap_mode :: Maybe OverlapMode
      }
  deriving (Typeable)
deriving instance (DataId id, Data l) => Data (ClsInstDecl l id)


----------------- Instances of all kinds -------------

type LInstDecl l name = GenLocated l (InstDecl l name)
data InstDecl l name  -- Both class and family instances
  = ClsInstD
      { cid_inst  :: ClsInstDecl l name }
  | DataFamInstD              -- data family instance
      { dfid_inst :: DataFamInstDecl l name }
  | TyFamInstD              -- type family instance
      { tfid_inst :: TyFamInstDecl l name }
  deriving (Typeable)
deriving instance (DataId id, Data l) => Data (InstDecl l id)
\end{code}

Note [Family instance declaration binders]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
A {Ty|Data}FamInstDecl is a data/type family instance declaration
the pats field is LHS patterns, and the tvs of the HsBSig
tvs are fv(pat_tys), *including* ones that are already in scope

   Eg   class C s t where
          type F t p :: *
        instance C w (a,b) where
          type F (a,b) x = x->a
   The tcdTyVars of the F decl are {a,b,x}, even though the F decl
   is nested inside the 'instance' decl.

   However after the renamer, the uniques will match up:
        instance C w7 (a8,b9) where
          type F (a8,b9) x10 = x10->a8
   so that we can compare the type patter in the 'instance' decl and
   in the associated 'type' decl

\begin{code}
instance (OutputableBndr name, ApiAnnotation l)
  => Outputable (TyFamInstDecl l name) where
  ppr = pprTyFamInstDecl TopLevel

pprTyFamInstDecl :: (OutputableBndr name, ApiAnnotation l)
  => TopLevelFlag -> TyFamInstDecl l name -> SDoc
pprTyFamInstDecl top_lvl (TyFamInstDecl { tfid_eqn = eqn })
   = ptext (sLit "type") <+> ppr_instance_keyword top_lvl <+> ppr_fam_inst_eqn eqn

ppr_instance_keyword :: TopLevelFlag -> SDoc
ppr_instance_keyword TopLevel    = ptext (sLit "instance")
ppr_instance_keyword NotTopLevel = empty

ppr_fam_inst_eqn :: (OutputableBndr name, ApiAnnotation l)
  => LTyFamInstEqn l name -> SDoc
ppr_fam_inst_eqn (L _ (TyFamEqn { tfe_tycon = tycon
                                , tfe_pats  = pats
                                , tfe_rhs   = rhs }))
    = pp_fam_inst_lhs tycon pats [] <+> equals <+> ppr rhs

ppr_fam_deflt_eqn :: (OutputableBndr name, ApiAnnotation l)
  => LTyFamDefltEqn l name -> SDoc
ppr_fam_deflt_eqn (L _ (TyFamEqn { tfe_tycon = tycon
                                 , tfe_pats  = tvs
                                 , tfe_rhs   = rhs }))
    = pp_vanilla_decl_head tycon tvs [] <+> equals <+> ppr rhs

instance (OutputableBndr name, ApiAnnotation l) => Outputable (DataFamInstDecl l name) where
  ppr = pprDataFamInstDecl TopLevel

pprDataFamInstDecl :: (OutputableBndr name, ApiAnnotation l)
   => TopLevelFlag -> DataFamInstDecl l name -> SDoc
pprDataFamInstDecl top_lvl (DataFamInstDecl { dfid_tycon = tycon
                                            , dfid_pats  = pats
                                            , dfid_defn  = defn })
  = pp_data_defn pp_hdr defn
  where
    pp_hdr ctxt = ppr_instance_keyword top_lvl <+> pp_fam_inst_lhs tycon pats ctxt

pprDataFamInstFlavour :: DataFamInstDecl l name -> SDoc
pprDataFamInstFlavour (DataFamInstDecl { dfid_defn = (HsDataDefn { dd_ND = nd }) })
  = ppr nd

instance (OutputableBndr name, ApiAnnotation l)
  => Outputable (ClsInstDecl l name) where
    ppr (ClsInstDecl { cid_poly_ty = inst_ty, cid_binds = binds
                     , cid_sigs = sigs, cid_tyfam_insts = ats
                     , cid_overlap_mode = mbOverlap
                     , cid_datafam_insts = adts })
      | null sigs, null ats, null adts, isEmptyBag binds  -- No "where" part
      = top_matter

      | otherwise       -- Laid out
      = vcat [ top_matter <+> ptext (sLit "where")
             , nest 2 $ pprDeclList $
               map (pprTyFamInstDecl NotTopLevel . unLoc)   ats ++
               map (pprDataFamInstDecl NotTopLevel . unLoc) adts ++
               pprLHsBindsForUser binds sigs ]
      where
        top_matter = ptext (sLit "instance") <+> ppOverlapPragma mbOverlap
                                             <+> ppr inst_ty

ppOverlapPragma :: Maybe OverlapMode -> SDoc
ppOverlapPragma mb =
  case mb of
    Nothing           -> empty
    Just NoOverlap    -> ptext (sLit "{-# NO_OVERLAP #-}")
    Just Overlappable -> ptext (sLit "{-# OVERLAPPABLE #-}")
    Just Overlapping  -> ptext (sLit "{-# OVERLAPPING #-}")
    Just Overlaps     -> ptext (sLit "{-# OVERLAPS #-}")
    Just Incoherent   -> ptext (sLit "{-# INCOHERENT #-}")




instance (OutputableBndr name, ApiAnnotation l)
  => Outputable (InstDecl l name) where
    ppr (ClsInstD     { cid_inst  = decl }) = ppr decl
    ppr (TyFamInstD   { tfid_inst = decl }) = ppr decl
    ppr (DataFamInstD { dfid_inst = decl }) = ppr decl

-- Extract the declarations of associated data types from an instance

instDeclDataFamInsts :: [LInstDecl l name] -> [DataFamInstDecl l name]
instDeclDataFamInsts inst_decls
  = concatMap do_one inst_decls
  where
    do_one (L _ (ClsInstD { cid_inst = ClsInstDecl { cid_datafam_insts = fam_insts } }))
      = map unLoc fam_insts
    do_one (L _ (DataFamInstD { dfid_inst = fam_inst }))      = [fam_inst]
    do_one (L _ (TyFamInstD {}))                              = []
\end{code}

%************************************************************************
%*                                                                      *
\subsection[DerivDecl]{A stand-alone instance deriving declaration}
%*                                                                      *
%************************************************************************

\begin{code}
type LDerivDecl l name = GenLocated l (DerivDecl l name)

data DerivDecl l name = DerivDecl { deriv_type :: LHsType l name
                                  , deriv_overlap_mode :: Maybe OverlapMode
                                  }
  deriving (Typeable)
deriving instance (DataId name, Data l) => Data (DerivDecl l name)

instance (OutputableBndr name, ApiAnnotation l)
   => Outputable (DerivDecl l name) where
    ppr (DerivDecl ty o)
        = hsep [ptext (sLit "deriving instance"), ppOverlapPragma o, ppr ty]
\end{code}

%************************************************************************
%*                                                                      *
\subsection[DefaultDecl]{A @default@ declaration}
%*                                                                      *
%************************************************************************

There can only be one default declaration per module, but it is hard
for the parser to check that; we pass them all through in the abstract
syntax, and that restriction must be checked in the front end.

\begin{code}
type LDefaultDecl l name = GenLocated l (DefaultDecl l name)

data DefaultDecl l name
  = DefaultDecl [LHsType l name]
  deriving (Typeable)
deriving instance (DataId name, Data l) => Data (DefaultDecl l name)

instance (OutputableBndr name, ApiAnnotation l)
              => Outputable (DefaultDecl l name) where

    ppr (DefaultDecl tys)
      = ptext (sLit "default") <+> parens (interpp'SP tys)
\end{code}

%************************************************************************
%*                                                                      *
\subsection{Foreign function interface declaration}
%*                                                                      *
%************************************************************************

\begin{code}

-- foreign declarations are distinguished as to whether they define or use a
-- Haskell name
--
--  * the Boolean value indicates whether the pre-standard deprecated syntax
--   has been used
--
type LForeignDecl l name = GenLocated l (ForeignDecl l name)

data ForeignDecl l name
  = ForeignImport (GenLocated l name) -- defines this name
                  (LHsType l name   ) -- sig_ty
                  (PostTc name Coercion) -- rep_ty ~ sig_ty
                  ForeignImport
  | ForeignExport (GenLocated l name) -- uses this name
                  (LHsType l name)    -- sig_ty
                  (PostTc name Coercion)  -- sig_ty ~ rep_ty
                  ForeignExport
  deriving (Typeable)
deriving instance (DataId name, Data l) => Data (ForeignDecl l name)
{-
    In both ForeignImport and ForeignExport:
        sig_ty is the type given in the Haskell code
        rep_ty is the representation for this type, i.e. with newtypes
               coerced away and type functions evaluated.
    Thus if the declaration is valid, then rep_ty will only use types
    such as Int and IO that we know how to make foreign calls with.
-}

noForeignImportCoercionYet :: PlaceHolder
noForeignImportCoercionYet = PlaceHolder

noForeignExportCoercionYet :: PlaceHolder
noForeignExportCoercionYet = PlaceHolder

-- Specification Of an imported external entity in dependence on the calling
-- convention
--
data ForeignImport = -- import of a C entity
                     --
                     --  * the two strings specifying a header file or library
                     --   may be empty, which indicates the absence of a
                     --   header or object specification (both are not used
                     --   in the case of `CWrapper' and when `CFunction'
                     --   has a dynamic target)
                     --
                     --  * the calling convention is irrelevant for code
                     --   generation in the case of `CLabel', but is needed
                     --   for pretty printing
                     --
                     --  * `Safety' is irrelevant for `CLabel' and `CWrapper'
                     --
                     CImport  CCallConv       -- ccall or stdcall
                              Safety          -- interruptible, safe or unsafe
                              (Maybe Header)  -- name of C header
                              CImportSpec     -- details of the C entity
  deriving (Data, Typeable)

-- details of an external C entity
--
data CImportSpec = CLabel    CLabelString     -- import address of a C label
                 | CFunction CCallTarget      -- static or dynamic function
                 | CWrapper                   -- wrapper to expose closures
                                              -- (former f.e.d.)
  deriving (Data, Typeable)

-- specification of an externally exported entity in dependence on the calling
-- convention
--
data ForeignExport = CExport  CExportSpec    -- contains the calling convention
  deriving (Data, Typeable)

-- pretty printing of foreign declarations
--

instance (OutputableBndr name, ApiAnnotation l)
  => Outputable (ForeignDecl l name) where
  ppr (ForeignImport n ty _ fimport) =
    hang (ptext (sLit "foreign import") <+> ppr fimport <+> ppr n)
       2 (dcolon <+> ppr ty)
  ppr (ForeignExport n ty _ fexport) =
    hang (ptext (sLit "foreign export") <+> ppr fexport <+> ppr n)
       2 (dcolon <+> ppr ty)

instance Outputable ForeignImport where
  ppr (CImport  cconv safety mHeader spec) =
    ppr cconv <+> ppr safety <+>
    char '"' <> pprCEntity spec <> char '"'
    where
      pp_hdr = case mHeader of
               Nothing -> empty
               Just (Header header) -> ftext header

      pprCEntity (CLabel lbl) =
        ptext (sLit "static") <+> pp_hdr <+> char '&' <> ppr lbl
      pprCEntity (CFunction (StaticTarget lbl _ isFun)) =
            ptext (sLit "static")
        <+> pp_hdr
        <+> (if isFun then empty else ptext (sLit "value"))
        <+> ppr lbl
      pprCEntity (CFunction (DynamicTarget)) =
        ptext (sLit "dynamic")
      pprCEntity (CWrapper) = ptext (sLit "wrapper")

instance Outputable ForeignExport where
  ppr (CExport  (CExportStatic lbl cconv)) =
    ppr cconv <+> char '"' <> ppr lbl <> char '"'
\end{code}


%************************************************************************
%*                                                                      *
\subsection{Transformation rules}
%*                                                                      *
%************************************************************************

\begin{code}
type LRuleDecl l name = GenLocated l (RuleDecl l name)

data RuleDecl l name
  = HsRule                      -- Source rule
        RuleName                -- Rule name
        Activation
        [RuleBndr l name]       -- Forall'd vars; after typechecking this includes tyvars
        (GenLocated l (HsExpr l name)) -- LHS
        (PostRn name NameSet)        -- Free-vars from the LHS
        (GenLocated l (HsExpr l name)) -- RHS
        (PostRn name NameSet)        -- Free-vars from the RHS
  deriving (Typeable)
deriving instance (DataId name, Data l) => Data (RuleDecl l name)

data RuleBndr l name
  = RuleBndr (GenLocated l name)
  | RuleBndrSig (GenLocated l name) (HsWithBndrs name (LHsType l name))
  deriving (Typeable)
deriving instance (DataId name, Data l) => Data (RuleBndr l name)

collectRuleBndrSigTys :: [RuleBndr l name] -> [HsWithBndrs name (LHsType l name)]
collectRuleBndrSigTys bndrs = [ty | RuleBndrSig _ ty <- bndrs]

instance (OutputableBndr name, ApiAnnotation l) => Outputable (RuleDecl l name) where
  ppr (HsRule name act ns lhs _fv_lhs rhs _fv_rhs)
        = sep [text "{-# RULES" <+> doubleQuotes (ftext name) <+> ppr act,
               nest 4 (pp_forall <+> pprExpr (unLoc lhs)),
               nest 4 (equals <+> pprExpr (unLoc rhs) <+> text "#-}") ]
        where
          pp_forall | null ns   = empty
                    | otherwise = forAllLit <+> fsep (map ppr ns) <> dot

instance (OutputableBndr name, ApiAnnotation l)
  => Outputable (RuleBndr l name) where
   ppr (RuleBndr name) = ppr name
   ppr (RuleBndrSig name ty) = ppr name <> dcolon <> ppr ty
\end{code}


%************************************************************************
%*                                                                      *
\subsection{Vectorisation declarations}
%*                                                                      *
%************************************************************************

A vectorisation pragma, one of

  {-# VECTORISE f = closure1 g (scalar_map g) #-}
  {-# VECTORISE SCALAR f #-}
  {-# NOVECTORISE f #-}

  {-# VECTORISE type T = ty #-}
  {-# VECTORISE SCALAR type T #-}

\begin{code}
type LVectDecl l name = GenLocated l (VectDecl l name)

data VectDecl l name
  = HsVect
      (GenLocated l name)
      (LHsExpr l name)
  | HsNoVect
      (GenLocated l name)
  | HsVectTypeIn                -- pre type-checking
      Bool                      -- 'TRUE' => SCALAR declaration
      (GenLocated l name)
      (Maybe (GenLocated l name)) -- 'Nothing' => no right-hand side
  | HsVectTypeOut               -- post type-checking
      Bool                      -- 'TRUE' => SCALAR declaration
      TyCon
      (Maybe TyCon)             -- 'Nothing' => no right-hand side
  | HsVectClassIn               -- pre type-checking
      (GenLocated l name)
  | HsVectClassOut              -- post type-checking
      Class
  | HsVectInstIn                -- pre type-checking (always SCALAR)  !!!FIXME: should be superfluous now
      (LHsType l name)
  | HsVectInstOut               -- post type-checking (always SCALAR) !!!FIXME: should be superfluous now
      ClsInst
  deriving (Typeable)
deriving instance (DataId name, Data l) => Data (VectDecl l name)

lvectDeclName :: NamedThing name => LVectDecl l name -> Name
lvectDeclName (L _ (HsVect         (L _ name) _))   = getName name
lvectDeclName (L _ (HsNoVect       (L _ name)))     = getName name
lvectDeclName (L _ (HsVectTypeIn   _ (L _ name) _)) = getName name
lvectDeclName (L _ (HsVectTypeOut  _ tycon _))      = getName tycon
lvectDeclName (L _ (HsVectClassIn  (L _ name)))     = getName name
lvectDeclName (L _ (HsVectClassOut cls))            = getName cls
lvectDeclName (L _ (HsVectInstIn   _))              = panic "HsDecls.lvectDeclName: HsVectInstIn"
lvectDeclName (L _ (HsVectInstOut  _))              = panic "HsDecls.lvectDeclName: HsVectInstOut"

lvectInstDecl :: LVectDecl l name -> Bool
lvectInstDecl (L _ (HsVectInstIn _))  = True
lvectInstDecl (L _ (HsVectInstOut _)) = True
lvectInstDecl _                       = False

instance (OutputableBndr name, ApiAnnotation l) => Outputable (VectDecl l name) where
  ppr (HsVect v rhs)
    = sep [text "{-# VECTORISE" <+> ppr v,
           nest 4 $
             pprExpr (unLoc rhs) <+> text "#-}" ]
  ppr (HsNoVect v)
    = sep [text "{-# NOVECTORISE" <+> ppr v <+> text "#-}" ]
  ppr (HsVectTypeIn False t Nothing)
    = sep [text "{-# VECTORISE type" <+> ppr t <+> text "#-}" ]
  ppr (HsVectTypeIn False t (Just t'))
    = sep [text "{-# VECTORISE type" <+> ppr t, text "=", ppr t', text "#-}" ]
  ppr (HsVectTypeIn True t Nothing)
    = sep [text "{-# VECTORISE SCALAR type" <+> ppr t <+> text "#-}" ]
  ppr (HsVectTypeIn True t (Just t'))
    = sep [text "{-# VECTORISE SCALAR type" <+> ppr t, text "=", ppr t', text "#-}" ]
  ppr (HsVectTypeOut False t Nothing)
    = sep [text "{-# VECTORISE type" <+> ppr t <+> text "#-}" ]
  ppr (HsVectTypeOut False t (Just t'))
    = sep [text "{-# VECTORISE type" <+> ppr t, text "=", ppr t', text "#-}" ]
  ppr (HsVectTypeOut True t Nothing)
    = sep [text "{-# VECTORISE SCALAR type" <+> ppr t <+> text "#-}" ]
  ppr (HsVectTypeOut True t (Just t'))
    = sep [text "{-# VECTORISE SCALAR type" <+> ppr t, text "=", ppr t', text "#-}" ]
  ppr (HsVectClassIn c)
    = sep [text "{-# VECTORISE class" <+> ppr c <+> text "#-}" ]
  ppr (HsVectClassOut c)
    = sep [text "{-# VECTORISE class" <+> ppr c <+> text "#-}" ]
  ppr (HsVectInstIn ty)
    = sep [text "{-# VECTORISE SCALAR instance" <+> ppr ty <+> text "#-}" ]
  ppr (HsVectInstOut i)
    = sep [text "{-# VECTORISE SCALAR instance" <+> ppr i <+> text "#-}" ]
\end{code}

%************************************************************************
%*                                                                      *
\subsection[DocDecl]{Document comments}
%*                                                                      *
%************************************************************************

\begin{code}

type LDocDecl l = GenLocated l (DocDecl)

data DocDecl
  = DocCommentNext HsDocString
  | DocCommentPrev HsDocString
  | DocCommentNamed String HsDocString
  | DocGroup Int HsDocString
  deriving (Data, Typeable)

-- Okay, I need to reconstruct the document comments, but for now:
instance Outputable DocDecl where
  ppr _ = text "<document comment>"

docDeclDoc :: DocDecl -> HsDocString
docDeclDoc (DocCommentNext d) = d
docDeclDoc (DocCommentPrev d) = d
docDeclDoc (DocCommentNamed _ d) = d
docDeclDoc (DocGroup _ d) = d

\end{code}

%************************************************************************
%*                                                                      *
\subsection[DeprecDecl]{Deprecations}
%*                                                                      *
%************************************************************************

We use exported entities for things to deprecate.

\begin{code}
type LWarnDecl l name = GenLocated l (WarnDecl name)

data WarnDecl name = Warning name WarningTxt
  deriving (Data, Typeable)

instance OutputableBndr name => Outputable (WarnDecl name) where
    ppr (Warning thing txt)
      = hsep [text "{-# DEPRECATED", ppr thing, doubleQuotes (ppr txt), text "#-}"]
\end{code}

%************************************************************************
%*                                                                      *
\subsection[AnnDecl]{Annotations}
%*                                                                      *
%************************************************************************

\begin{code}
type LAnnDecl l name = GenLocated l (AnnDecl l name)

data AnnDecl l name = HsAnnotation (AnnProvenance name)
                                   (GenLocated l (HsExpr l name))
  deriving (Typeable)
deriving instance (DataId name, Data l) => Data (AnnDecl l name)

instance (OutputableBndr name, ApiAnnotation l)
  => Outputable (AnnDecl l name) where
    ppr (HsAnnotation provenance expr)
      = hsep [text "{-#", pprAnnProvenance provenance, pprExpr (unLoc expr), text "#-}"]


data AnnProvenance name = ValueAnnProvenance name
                        | TypeAnnProvenance name
                        | ModuleAnnProvenance
  deriving (Data, Typeable, Functor, Foldable, Traversable)

annProvenanceName_maybe :: AnnProvenance name -> Maybe name
annProvenanceName_maybe (ValueAnnProvenance name) = Just name
annProvenanceName_maybe (TypeAnnProvenance name)  = Just name
annProvenanceName_maybe ModuleAnnProvenance       = Nothing

pprAnnProvenance :: OutputableBndr name => AnnProvenance name -> SDoc
pprAnnProvenance ModuleAnnProvenance       = ptext (sLit "ANN module")
pprAnnProvenance (ValueAnnProvenance name) = ptext (sLit "ANN") <+> ppr name
pprAnnProvenance (TypeAnnProvenance name)  = ptext (sLit "ANN type") <+> ppr name
\end{code}

%************************************************************************
%*                                                                      *
\subsection[RoleAnnot]{Role annotations}
%*                                                                      *
%************************************************************************

\begin{code}
type LRoleAnnotDecl l name = GenLocated l (RoleAnnotDecl l name)

-- See #8185 for more info about why role annotations are
-- top-level declarations
data RoleAnnotDecl l name
  = RoleAnnotDecl (GenLocated l name)         -- type constructor
                  [GenLocated l (Maybe Role)] -- optional annotations
  deriving (Data, Typeable)

instance (OutputableBndr name, Outputable l) => Outputable (RoleAnnotDecl l name) where
  ppr (RoleAnnotDecl ltycon roles)
    = ptext (sLit "type role") <+> ppr ltycon <+>
      hsep (map (pp_role . unLoc) roles)
    where
      pp_role Nothing  = underscore
      pp_role (Just r) = ppr r

roleAnnotDeclName :: RoleAnnotDecl l name -> name
roleAnnotDeclName (RoleAnnotDecl (L _ name) _) = name

\end{code}
