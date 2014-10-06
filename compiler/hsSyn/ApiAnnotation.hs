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

    AnnHsModule(..),AnnLIEs(..),AnnHsCommaList(..),
    AnnImportDecls(..),AnnImportDecl(..),
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

instance Show Key where
  show (Key a) = show a

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

data AnnHsCommaList = AnnHsCommaList { ahscommalist_comma :: SrcSpan }
            deriving (Eq,Data,Typeable,Show)

data AnnImportDecls = AnnImportDecls { aimportdecls_semi :: SrcSpan }
            deriving (Eq,Data,Typeable,Show)

data AnnImportDecl = AnnImportDecl
       { aimportdecl_src :: Maybe (SrcSpan,SrcSpan)
       , aimportdecl_safe,aimportdecl_qual,aimportdecl_pkg
       , aimportdecl_as,aimportdecl_hiding :: Maybe SrcSpan }
            deriving (Eq,Data,Typeable,Show)

data AnnHsLet = AnnHsLet { ahslet_let, ahslet_in ::  SrcSpan }
            deriving (Eq,Data,Typeable,Show)

data AnnHsDo =  AnnHsDo { ahsdo_do :: SrcSpan }
            deriving (Eq,Data,Typeable,Show)

-- ---------------------------------------------------------------------
instance Outputable AnnHsModule where
  ppr (AnnHsModule mw bs s) = text "AnnHsModule" <+> ppr mw <+> ppr bs <+> ppr s

instance Outputable AnnLIEs where
  ppr (AnnLIEs op cp) = text "AnnLIEs" <+> ppr op <+> ppr cp

instance Outputable AnnHsCommaList where
  ppr (AnnHsCommaList c) = text "AnnHsCommaList" <+> ppr c

instance Outputable AnnImportDecls where
  ppr (AnnImportDecls s) = text "AnnImportDecls" <+> ppr s

instance Outputable AnnImportDecl where
  ppr (AnnImportDecl src asafe qual pkg aas ahiding)
    = text "AnnImportDecl" <+> ppr src <+> ppr asafe <+> ppr qual <+> ppr pkg
                           <+> ppr aas <+> ppr ahiding

instance Outputable AnnHsLet where
  ppr (AnnHsLet l i) = text "AnnHsLet" <+> ppr l <+> ppr i

instance Outputable AnnHsDo where
  ppr (AnnHsDo d) = text "AnnHsDo" <+> ppr d
