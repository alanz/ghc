{-
%
% (c) The University of Glasgow 2006
% (c) The GRASP/AQUA Project, Glasgow University, 1992-1998
%

ApiAnnotation: types and functions to add a companion annotation
structure to the parsed source for use in source to source conversions
via the GHC API.

See https://ghc.haskell.org/trac/ghc/wiki/GhcAstAnnotations

-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE ExistentialQuantification #-}
module ApiAnnotation (
    ApiAnnKey(..), mkApiAnnKey,
    ApiAnns,

    Value,newValue,typeValue,fromValue,

    getAnnotation,

    AnnHsModule(..),AnnLIEs(..),
    AnnImportDecl(..),

    -- * IE
    AnnIEModuleContents(..),AnnIEVar(..),

    -- * ImpExpSubSpec
    AnnImpExpAll(..),AnnImpExpList(..),

    -- * TyClDecl
    AnnClassDecl(..),AnnSynDecl(..),AnnFamDecl(..),AnnDataDecl(..),

    AnnClsInstDecl(..),
    AnnTyFamInstDecl(..),
    AnnDataFamInstDecl(..),
    AnnDerivDecl(..),
    AnnRoleAnnotDecl(..),
    AnnDefaultDecl(..),
    AnnForeignDecl(..),

    AnnDeprecations(..),
    AnnWarnings(..),
    AnnRules(..),
    AnnHsAnnotation(..),

    -- * FamilyInfo
    AnnClosedTypeFamily(..),

    -- * VectDecl
    AnnHsVect(..),
    AnnHsNoVect(..),
    AnnHsVectTypeIn(..),
    AnnHsVectClassIn(..),

    -- *
    AnnTyFamInstEqn(..),

    -- * HsBind
    AnnPatSynBind(..),

    -- *
    AnnGRHSs(..),
    AnnGRHS(..),

    -- * HsExpr
    AnnHsLet(..),AnnHsDo(..)

    ) where

import Data.Data
import Data.Maybe
import Outputable
import SrcLoc

import qualified Data.Map as Map

{-

Theory of operation.

The HsSyn AST does not capture the locations of certain keywords and
punctuation, such as 'let', 'in', 'do', etc.

These locations are required by any tools wanting to parse a haskell
file, transform the AST in some way, and then regenerate the original
layout for the unchaged parts.

Rather than pollute the AST with information irrelevant to the actual
compilation process, these locations are captured in the lexer /
parser and returned as a separate structure ApiAnns structure in the
ParsedSource.

Each AST element that needs an annotation has an entry in this Map,
which as a key comprising the SrcSpan of the original element and the
TyeRep of the stored annotation, if it were wrapped in a Just.

This allows code using the annotation to access this as follows

    processHsLet :: ApiAnns -> LHsExpr -> CustomReturnType
    processHsLet anns (L l (HsExpr localBinds expr)) = r
      where
        Just ann = getAnnotation anns l :: Maybe AnnHsLet
        ...


-}

type ApiAnns = Map.Map ApiAnnKey Value

data ApiAnnKey = AK SrcSpan TypeRep
                  deriving (Eq,Ord,Show)

mkApiAnnKey :: (Typeable a) => SrcSpan -> a -> ApiAnnKey
mkApiAnnKey l a = AK l (typeOf (Just a))

-- ---------------------------------------------------------------------
-- Based on
-- https://github.com/ndmitchell/shake/blob/master/Development/Shake/Value.hs

data Value = forall a . (Eq a, Show a, Typeable a, Outputable a) => Value a

newValue :: (Eq a, Show a, Typeable a, Outputable a) => a -> Value
newValue = Value

typeValue :: Value -> TypeRep
typeValue (Value x) = typeOf x

fromValue :: Typeable a => Value -> a
fromValue (Value x) = fromMaybe (error errMsg) $ res
  where
    res = cast x
    errMsg = "fromValue, bad cast from " ++ show (typeOf x)
                ++ " to " ++ show (typeOf res)

instance Show Value where
  show (Value a) = show a

instance Outputable Value where
  ppr (Value a) = ppr a

instance Eq Value where
  Value a == Value b = maybe False (a ==) $ cast b
  Value a /= Value b = maybe True (a /=) $ cast b


-- ---------------------------------------------------------------------

-- | Retrieve an annotation based on the SrcSpan of the annotated AST
-- element, and the known type of the annotation.
getAnnotation :: (Typeable a) => ApiAnns -> SrcSpan -> Maybe a
getAnnotation anns span = res
  where res = case  Map.lookup (AK span (typeOf res)) anns of
                       Nothing -> Nothing
                       Just d -> Just $ fromValue d

-- --------------------------------------------------------------------

-- Each annotation data type is named with an "Ann" prefix on the name
-- of the type being annotated.

-- This annotation will only be present if there is a module header
data AnnHsModule = AnnHsModule
       { ahsmodule_module_where :: Maybe (SrcSpan,SrcSpan)
       , ahsmodule_braces :: Maybe (SrcSpan,SrcSpan)
         -- ^ '{' '}' surrounding imports/decls, if present
       , ahsmodule_semi :: Maybe SrcSpan
         -- ^ ';' between imports and decls, if present
       }
            deriving (Eq,Data,Typeable,Show)

data AnnLIEs = AnnLIEs { alie_oparen, alie_cparen :: SrcSpan }
            deriving (Eq,Data,Typeable,Show)

data AnnImportDecl = AnnImportDecl
       { aimportdecl_src :: Maybe (SrcSpan,SrcSpan)
       , aimportdecl_safe,aimportdecl_qual,aimportdecl_pkg
       , aimportdecl_as,aimportdecl_hiding :: Maybe SrcSpan }
            deriving (Eq,Data,Typeable,Show)

-- IE
data AnnIEModuleContents = AnnIEModuleContents
         { aiemodulecontents_m :: SrcSpan }
            deriving (Eq,Data,Typeable,Show)

data AnnIEVar = AnnIEVar { aiemodulecontents_o :: SrcSpan }
            deriving (Eq,Data,Typeable,Show)

-- ImpExpSubSpec
data AnnImpExpAll = AnnImpExpAll
      { aimpexpall_op,aimpexpall_dd,aimpexpall_cp :: SrcSpan }
            deriving (Eq,Data,Typeable,Show)

data AnnImpExpList = AnnImpExpList
      { aimpexplist_op,aimpexplist_cp :: SrcSpan }
            deriving (Eq,Data,Typeable,Show)

-- TyClDecl
data AnnClassDecl = AnnClassDecl { aclassdecl_class :: SrcSpan }
            deriving (Eq,Data,Typeable,Show)

data AnnSynDecl = AnnSynDecl { asyndecl_type,asyndecl_eq :: SrcSpan }
            deriving (Eq,Data,Typeable,Show)

data AnnFamDecl = AnnFamDecl
       { afamdecl_td :: SrcSpan, afamdecl_mfamily :: Maybe SrcSpan }
            deriving (Eq,Data,Typeable,Show)

data AnnDataDecl = AnnDataDecl { adatadecl_dn :: SrcSpan }
            deriving (Eq,Data,Typeable,Show)

--

data AnnClsInstDecl = AnnClsInstDecl
        { aclsinstdecl_instance :: SrcSpan
        , aclsinstdecl_overlap  :: Maybe (SrcSpan,SrcSpan) }
            deriving (Eq,Data,Typeable,Show)

--

data AnnTyFamInstDecl = AnnTyFamInstDecl
         { atyfaminstdecl_type :: SrcSpan
         , atyfaminstdecl_minst :: Maybe SrcSpan
         }
            deriving (Eq,Data,Typeable,Show)

--

data AnnDataFamInstDecl = AnnDataFamInstDecl
         { adatafaminstdecl_type :: SrcSpan
         , adatafaminstdecl_minst :: Maybe SrcSpan }
            deriving (Eq,Data,Typeable,Show)

--

data AnnDerivDecl = AnnDerivDecl
         { aderivdecl_deriving,aderivdecl_inst :: SrcSpan
         , aderivdecl_overlap :: Maybe (SrcSpan,SrcSpan)
         }
            deriving (Eq,Data,Typeable,Show)

--

data AnnRoleAnnotDecl = AnnRoleAnnotDecl
         { aroleannotdecl_type,aroleannotdecl_role :: SrcSpan }
            deriving (Eq,Data,Typeable,Show)

--

data AnnDefaultDecl = AnnDefaultDecl
         { adefaultdecl_def,adefaultdecl_op,adefaultdecl_cp :: SrcSpan }
            deriving (Eq,Data,Typeable,Show)

--

data AnnForeignDecl = AnnForeignDecl { aforeigndecl_f :: SrcSpan }
            deriving (Eq,Data,Typeable,Show)

--

data AnnDeprecations = AnnDeprecations
         { adeprecations_open,adeprecations_close :: SrcSpan }
            deriving (Eq,Data,Typeable,Show)

--

data AnnWarnings = AnnWarnings
         { awarnings_open,awarnings_close :: SrcSpan }
            deriving (Eq,Data,Typeable,Show)

--

data AnnRules = AnnRules { arules_open,arules_close :: SrcSpan }
            deriving (Eq,Data,Typeable,Show)

--

data AnnHsAnnotation = AnnHsAnnotation
        { ahsannotation_open  :: SrcSpan,
          ahsannotation_mkw   :: Maybe SrcSpan,
          ahsannotation_close :: SrcSpan }
            deriving (Eq,Data,Typeable,Show)

-- FamilyInfo
data AnnClosedTypeFamily = AnnClosedTypeFamily
          { aclosedtypefamily_where :: SrcSpan
          , aclosedtypefamily_braces :: Maybe (SrcSpan,SrcSpan)
          , aclosedtypefamily_dd :: Maybe SrcSpan
          }
            deriving (Eq,Data,Typeable,Show)

-- VectDecl
data AnnHsVect = AnnHsVect { ahsvect_open, ahsvect_eq,ahsvect_close :: SrcSpan }
            deriving (Eq,Data,Typeable,Show)

data AnnHsNoVect = AnnHsNoVect { ahsnovect_open,ahsnovect_close :: SrcSpan }
            deriving (Eq,Data,Typeable,Show)

data AnnHsVectTypeIn = AnnHsVectTypeIn
       { ahsvecttypein_open,ahsvecttypein_type :: SrcSpan,
         ahsvecttypein_meq   :: Maybe SrcSpan,
         ahsvecttypein_close :: SrcSpan }
            deriving (Eq,Data,Typeable,Show)

data AnnHsVectClassIn = AnnHsVectClassIn
        { ahsvectclassin_open,ahsvectclassin_class,
          ahsvectclassin_close :: SrcSpan }
            deriving (Eq,Data,Typeable,Show)

--
data AnnTyFamInstEqn = AnnTyFamInstEqn { atyfaminsteqn_eq :: SrcSpan }
            deriving (Eq,Data,Typeable,Show)

-- HsBind

data AnnPatSynBind = AnnPatSynBind
        { apatsynbind_pat, apatsynbind_op :: SrcSpan }
            deriving (Eq,Data,Typeable,Show)
--

data AnnGRHSs = AnnGRHSs { agrhss_meq :: Maybe SrcSpan }
            deriving (Eq,Data,Typeable,Show)

data AnnGRHS = AnnGRHS { agrhs_g,agrhs_eq :: SrcSpan }
            deriving (Eq,Data,Typeable,Show)


-- HsExpr
data AnnHsLet = AnnHsLet { ahslet_let, ahslet_in ::  SrcSpan }
            deriving (Eq,Data,Typeable,Show)

data AnnHsDo =  AnnHsDo { ahsdo_do :: SrcSpan }
            deriving (Eq,Data,Typeable,Show)

-- ---------------------------------------------------------------------
instance Outputable AnnHsModule where
  ppr (AnnHsModule mw bs s) = text "AnnHsModule" <+> ppr mw <+> ppr bs <+> ppr s

instance Outputable AnnLIEs where
  ppr (AnnLIEs op cp) = text "AnnLIEs" <+> ppr op <+> ppr cp

instance Outputable AnnImportDecl where
  ppr (AnnImportDecl src asafe qual pkg aas ahiding)
    = text "AnnImportDecl" <+> ppr src <+> ppr asafe <+> ppr qual <+> ppr pkg
                           <+> ppr aas <+> ppr ahiding
--
instance Outputable AnnIEModuleContents where
  ppr (AnnIEModuleContents m) = text "AnnIEModuleContents" <+> ppr m

instance Outputable AnnIEVar where
  ppr (AnnIEVar p) = text "AnnIEVar" <+> ppr p

--
instance Outputable AnnImpExpAll where
  ppr (AnnImpExpAll op dd cp) = text "AnnImpExpAll" <+> ppr op <+> ppr dd
                                                    <+> ppr cp

instance Outputable AnnImpExpList where
  ppr (AnnImpExpList op cp) = text "AnnImpExpList" <+> ppr op <+> ppr cp

--
instance Outputable AnnClassDecl where
  ppr (AnnClassDecl c) = text "AnnClassDecl" <+> ppr c

instance Outputable AnnSynDecl where
  ppr (AnnSynDecl t e) = text "AnnSynDecl" <+> ppr t <+> ppr e

instance Outputable AnnFamDecl where
  ppr (AnnFamDecl t f) = text "AnnFamDecl" <+> ppr t <+> ppr f

instance Outputable AnnDataDecl where
  ppr (AnnDataDecl d) = text "AnnDataDecl" <+> ppr d

--
instance Outputable AnnClsInstDecl where
  ppr (AnnClsInstDecl i o) = text "AnnClsInstDecl" <+> ppr i <+> ppr o

--
instance Outputable AnnTyFamInstDecl where
  ppr (AnnTyFamInstDecl t i) = text "AnnTyFamInstDecl" <+> ppr t <+> ppr i

--
instance Outputable AnnDataFamInstDecl where
  ppr (AnnDataFamInstDecl t i) = text "AnnDataFamInstDecl" <+> ppr t <+> ppr i

--
instance Outputable AnnDerivDecl where
  ppr (AnnDerivDecl d i o) = text "AnnDerivDecl" <+> ppr d <+> ppr i <+> ppr o

--
instance Outputable AnnRoleAnnotDecl where
  ppr (AnnRoleAnnotDecl t r) = text "AnnRoleAnnotDecl" <+> ppr t <+> ppr r

--
instance Outputable AnnDefaultDecl where
  ppr (AnnDefaultDecl d op cp) = text "AnnDefaultDecl" <+> ppr d
                                                       <+> ppr op <+> ppr cp

--
instance Outputable AnnForeignDecl where
  ppr (AnnForeignDecl f) = text "AnnForeignDecl" <+> ppr f

--
instance Outputable AnnDeprecations where
  ppr (AnnDeprecations o c) = text "AnnDeprecations" <+> ppr o <+> ppr c

--
instance Outputable AnnWarnings where
  ppr (AnnWarnings o c) = text "AnnWarnings" <+> ppr o <+> ppr c

--
instance Outputable AnnRules where
  ppr (AnnRules o c) = text "AnnRules" <+> ppr o <+> ppr c

instance Outputable AnnHsAnnotation where
  ppr (AnnHsAnnotation o k c) = text "AnnHsAnnotation" <+> ppr o <+> ppr k
                                                       <+> ppr c
--
instance Outputable AnnClosedTypeFamily where
  ppr (AnnClosedTypeFamily w b d) = text "AnnClosedTypeFamily" <+> ppr w
                  <+> ppr b <+> ppr d
--
instance Outputable AnnHsVect where
  ppr (AnnHsVect o e c) = text "AnnHsVect" <+> ppr o <+> ppr e <+> ppr c

--
instance Outputable AnnHsNoVect where
  ppr (AnnHsNoVect o c) = text "AnnHsNoVect" <+> ppr o <+> ppr c

--
instance Outputable AnnHsVectTypeIn where
  ppr (AnnHsVectTypeIn o t me c) = text "AnnHsVectTypeIn" <+> ppr o <+> ppr t
                                                          <+> ppr me <+> ppr c
--
instance Outputable AnnHsVectClassIn where
  ppr (AnnHsVectClassIn o k c) = text "AnnHsVectClassIn" <+> ppr o <+> ppr k
                                                         <+> ppr c

--
instance Outputable AnnTyFamInstEqn where
  ppr (AnnTyFamInstEqn e) = text "AnnTyFamInstEqn" <+> ppr e
--
instance Outputable AnnPatSynBind where
  ppr (AnnPatSynBind p o) = text "AnnPatSynBind" <+> ppr p <+> ppr o

--
instance Outputable AnnGRHSs where
  ppr (AnnGRHSs me) = text "AnnGRHSs" <+> ppr me

instance Outputable AnnGRHS where
  ppr (AnnGRHS g e) = text "AnnGRHS" <+> ppr g <+> ppr e
--
instance Outputable AnnHsLet where
  ppr (AnnHsLet l i) = text "AnnHsLet" <+> ppr l <+> ppr i

instance Outputable AnnHsDo where
  ppr (AnnHsDo d) = text "AnnHsDo" <+> ppr d
