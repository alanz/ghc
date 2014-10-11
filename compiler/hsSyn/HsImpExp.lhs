%
% (c) The University of Glasgow 2006
% (c) The GRASP/AQUA Project, Glasgow University, 1992-1998
%

HsImpExp: Abstract syntax: imports, exports, interfaces

\begin{code}
{-# LANGUAGE CPP #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE DeriveFunctor      #-}
{-# LANGUAGE DeriveFoldable     #-}
{-# LANGUAGE DeriveTraversable  #-}

module HsImpExp where

import Module           ( ModuleName )
import HsDoc            ( HsDocString )
import OccName          ( HasOccName(..), isTcOcc, isSymOcc )

import Outputable
import FastString
import SrcLoc

import Data.Data
#if __GLASGOW_HASKELL__ < 709
import Data.Foldable ( Foldable )
import Data.Traversable ( Traversable )
#endif
\end{code}

%************************************************************************
%*                                                                      *
\subsection{Import and export declaration lists}
%*                                                                      *
%************************************************************************

One per \tr{import} declaration in a module.
\begin{code}
type LImportDecl name = Located (ImportDecl name)

-- | A single Haskell @import@ declaration.
data ImportDecl name
  = ImportDecl {
      ideclName      :: Located ModuleName, -- ^ Module name.
      ideclPkgQual   :: Maybe FastString,   -- ^ Package qualifier.
      ideclSource    :: Bool,               -- ^ True <=> {-# SOURCE #-} import
      ideclSafe      :: Bool,               -- ^ True => safe import
      ideclQualified :: Bool,               -- ^ True => qualified
      ideclImplicit  :: Bool,               -- ^ True => implicit import (of Prelude)
      ideclAs        :: Maybe ModuleName,   -- ^ as Module
      ideclHiding    :: Maybe (Bool, HsCommaList (LIE name))
                            -- ^ (True => hiding, names)
    } deriving (Data, Typeable)

simpleImportDecl :: ModuleName -> ImportDecl name
simpleImportDecl mn = ImportDecl {
      ideclName      = noLoc mn,
      ideclPkgQual   = Nothing,
      ideclSource    = False,
      ideclSafe      = False,
      ideclImplicit  = False,
      ideclQualified = False,
      ideclAs        = Nothing,
      ideclHiding    = Nothing
    }
\end{code}

\begin{code}
instance (OutputableBndr name, HasOccName name) => Outputable (ImportDecl name) where
    ppr (ImportDecl { ideclName = mod', ideclPkgQual = pkg
                    , ideclSource = from, ideclSafe = safe
                    , ideclQualified = qual, ideclImplicit = implicit
                    , ideclAs = as, ideclHiding = spec })
      = hang (hsep [ptext (sLit "import"), ppr_imp from, pp_implicit implicit, pp_safe safe,
                    pp_qual qual, pp_pkg pkg, ppr mod', pp_as as])
             4 (pp_spec spec)
      where
        pp_implicit False = empty
        pp_implicit True = ptext (sLit ("(implicit)"))

        pp_pkg Nothing  = empty
        pp_pkg (Just p) = doubleQuotes (ftext p)

        pp_qual False   = empty
        pp_qual True    = ptext (sLit "qualified")

        pp_safe False   = empty
        pp_safe True    = ptext (sLit "safe")

        pp_as Nothing   = empty
        pp_as (Just a)  = ptext (sLit "as") <+> ppr a

        ppr_imp True  = ptext (sLit "{-# SOURCE #-}")
        ppr_imp False = empty

        pp_spec Nothing             = empty
        pp_spec (Just (False, ies)) = ppr_ies (fromCL ies)
        pp_spec (Just (True,  ies)) = ptext (sLit "hiding")
                                   <+> ppr_ies (fromCL ies)

        ppr_ies []  = ptext (sLit "()")
        ppr_ies ies = char '(' <+> interpp'SP ies <+> char ')'
\end{code}

%************************************************************************
%*                                                                      *
\subsection{Imported and exported entities}
%*                                                                      *
%************************************************************************

\begin{code}
type LIE name = Located (IE name)

-- | Imported or exported entity.
data IE name
  = IEVar               name
  | IEThingAbs          name             -- ^ Class/Type (can't tell)
  | IEThingAll          name             -- ^ Class/Type plus all methods/constructors
  | IEThingWith         name (HsCommaList (Located name))
                 -- ^ Class/Type plus some methods/constructors
  | IEModuleContents    ModuleName       -- ^ (Export Only)
  | IEGroup             Int HsDocString  -- ^ Doc section heading
  | IEDoc               HsDocString      -- ^ Some documentation
  | IEDocNamed          String           -- ^ Reference to named doc
  deriving (Eq, Data, Typeable)
\end{code}

\begin{code}
ieName :: IE name -> name
ieName (IEVar n)         = n
ieName (IEThingAbs  n)   = n
ieName (IEThingWith n _) = n
ieName (IEThingAll  n)   = n
ieName _ = panic "ieName failed pattern match!"

ieNames :: IE a -> [a]
ieNames (IEVar            n   ) = [n]
ieNames (IEThingAbs       n   ) = [n]
ieNames (IEThingAll       n   ) = [n]
ieNames (IEThingWith      n ns) = n : map unLoc (fromCL ns)
ieNames (IEModuleContents _   ) = []
ieNames (IEGroup          _ _ ) = []
ieNames (IEDoc            _   ) = []
ieNames (IEDocNamed       _   ) = []
\end{code}

\begin{code}

pprImpExp :: (HasOccName name, OutputableBndr name) => name -> SDoc
pprImpExp name = type_pref <+> pprPrefixOcc name
    where
    occ = occName name
    type_pref | isTcOcc occ && isSymOcc occ = ptext (sLit "type")
              | otherwise                   = empty

instance (HasOccName name, OutputableBndr name) => Outputable (IE name) where
    ppr (IEVar          var)    = pprPrefixOcc var
    ppr (IEThingAbs     thing)  = pprImpExp thing
    ppr (IEThingAll     thing)  = hcat [pprImpExp thing, text "(..)"]
    ppr (IEThingWith thing withs)
        = pprImpExp thing <> parens (fsep (punctuate comma (map pprImpExp $ map unLoc $ fromCL withs)))
    ppr (IEModuleContents mod')
        = ptext (sLit "module") <+> ppr mod'
    ppr (IEGroup n _)           = text ("<IEGroup: " ++ (show n) ++ ">")
    ppr (IEDoc doc)             = ppr doc
    ppr (IEDocNamed string)     = text ("<IEDocNamed: " ++ string ++ ">")
\end{code}


\begin{code}
-- | A comma separated list that can cope with extra commas in it, for
-- example in tuple sections or imports

-- AZ: Naming: in the import declarations this captures semicolons.
data HsCommaList a
  = Empty
  | Cons a (HsCommaList a)
-- AZ : currently abusing ExtraComma to capture all commas.
  | ExtraComma SrcSpan (HsCommaList a)
       -- ^ We need a SrcSpan for the annotation
  | Snoc (HsCommaList a) a
  | Two (HsCommaList a) -- Invariant: non-empty
        (HsCommaList a) -- Invariant: non-empty

  deriving (Data,Typeable,Functor,Foldable,Traversable)
deriving instance (Eq a) => Eq (HsCommaList a)

infixl 5  `appCL`
infixl 5  `snocCL`
infixr 5  `consCL`

appCL :: HsCommaList a -> HsCommaList a -> HsCommaList a

l1    `appCL` Empty = l1
Empty `appCL` l2    = l2
ExtraComma s ls `appCL` l2 = ExtraComma s (ls `appCL` l2)
Cons l       ls `appCL` l2 = Cons l       (ls `appCL` l2)
l1              `appCL` l2 = Two l1 l2

reverseCL ::  HsCommaList a -> HsCommaList a
reverseCL = toCLWithCommas . reverse . fromCLWithCommas

consCL :: a -> HsCommaList a -> HsCommaList a
consCL l ls = Cons l ls

snocCL :: HsCommaList a -> a -> HsCommaList a
snocCL ls l = Snoc ls l

nilCL :: HsCommaList a
nilCL = Empty

unitCL :: a -> HsCommaList a
unitCL v = Cons v Empty

extraCL :: SrcSpan -> HsCommaList a -> HsCommaList a
extraCL mss l = ExtraComma mss l

fromCL :: HsCommaList a -> [a]
fromCL a = go a []
  where go Empty            acc = acc
        go (Cons a b)       acc = a : go b acc
        go (ExtraComma _ a) acc = go a acc -- discarding comma
        go (Snoc a b)       acc = go a (b:acc)
        go (Two a b)        acc = go a (go b acc)

fromCLWithCommas :: HsCommaList a -> [Either SrcSpan a]
fromCLWithCommas a = go a []
  where go Empty            acc = acc
        go (Cons a b)       acc = (Right a) : go b acc
        go (ExtraComma s a) acc = (Left s)  : go a acc
        go (Snoc a b)       acc = go a ((Right b):acc)
        go (Two a b)        acc = go a (go b acc)

toCL :: [a] -> HsCommaList a
toCL [] = Empty
toCL (x:xs) = Cons x (toCL xs)

toCLWithCommas :: [Either SrcSpan a] -> HsCommaList a
toCLWithCommas [] = Empty
toCLWithCommas (Left s :xs) = ExtraComma s (toCLWithCommas xs)
toCLWithCommas (Right x:xs) = Cons x       (toCLWithCommas xs)

isNilCL :: HsCommaList a -> Bool
isNilCL Empty = True
isNilCL _ = False

firstLocCL :: HsCommaList (Located a) -> SrcSpan
firstLocCL Empty            = noSrcSpan
firstLocCL (Cons (L l _) _) = l
firstLocCL (ExtraComma _ a) = firstLocCL a
firstLocCL (Snoc b _)       = firstLocCL b
firstLocCL (Two a _)        = firstLocCL a

headCL :: HsCommaList a -> a
headCL Empty            = error "headCL Empty"
headCL (Cons a _)       = a
headCL (ExtraComma _ a) = headCL a
headCL (Snoc a _)       = headCL a
headCL (Two a _)        = headCL a
\end{code}

\begin{code}
instance (Outputable a) => Outputable (HsCommaList a) where
    ppr (Empty)           = empty
    ppr (ExtraComma _ cl) = comma <+> ppr cl
    ppr (Cons a cl)       = ppr a <> comma <+> ppr cl
    ppr (Snoc cl a)       = ppr cl <+> ppr a
    ppr (Two a b)         = ppr a <+> ppr b
\end{code}
