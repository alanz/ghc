--                                                              -*-haskell-*-
-- ---------------------------------------------------------------------------
-- (c) The University of Glasgow 1997-2003
---
-- The GHC grammar.
--
-- Author(s): Simon Marlow, Sven Panne 1997, 1998, 1999
-- ---------------------------------------------------------------------------

{
{-# LANGUAGE BangPatterns #-} -- required for versions of Happy before 1.18.6
{-# OPTIONS -Wwarn -w #-}
-- The above warning supression flag is a temporary kludge.
-- While working on this module you are encouraged to remove it and fix
-- any warnings in the module. See
--     http://ghc.haskell.org/trac/ghc/wiki/Commentary/CodingStyle#Warnings
-- for details

-- | This module provides the generated Happy parser for Haskell. It exports
-- a number of parsers which may be used in any library that uses the GHC API.
-- A common usage pattern is to initialize the parser state with a given string
-- and then parse that string:
--
-- @
--     runParser :: DynFlags -> String -> P a -> ParseResult a
--     runParser flags str parser = unP parser parseState
--     where
--       filename = "\<interactive\>"
--       location = mkRealSrcLoc (mkFastString filename) 1 1
--       buffer = stringToStringBuffer str
--       parseState = mkPState flags buffer location in
-- @
module Parser (parseModule, parseImport, parseStatement,
               parseDeclaration, parseExpression, parseTypeSignature,
               parseFullStmt, parseStmt, parseIdentifier,
               parseType, parseHeader) where


import HsSyn
import RdrHsSyn
import HscTypes         ( IsBootInterface, WarningTxt(..) )
import Lexer
import RdrName
import TcEvidence       ( emptyTcEvBinds )
import TysPrim          ( liftedTypeKindTyConName, eqPrimTyCon )
import TysWiredIn       ( unitTyCon, unitDataCon, tupleTyCon, tupleCon, nilDataCon,
                          unboxedUnitTyCon, unboxedUnitDataCon,
                          listTyCon_RDR, parrTyCon_RDR, consDataCon_RDR, eqTyCon_RDR )
import Type             ( funTyCon )
import ForeignCall
import OccName          ( varName, dataName, tcClsName, tvName )
import DataCon          ( DataCon, dataConName )
import SrcLoc
import Module
import Kind             ( Kind, liftedTypeKind, unliftedTypeKind, mkArrowKind )
import Class            ( FunDep )
import BasicTypes
import DynFlags
import OrdList
import HaddockUtils
import BooleanFormula   ( BooleanFormula, mkAnd, mkOr, mkTrue, mkVar )

import FastString
import Maybes           ( orElse )
import Outputable
import ApiAnnotation

import Control.Monad    ( unless, liftM )
import GHC.Exts
import Data.Char
import Control.Monad    ( mplus )
import Data.Maybe
import Data.Typeable
}

{-
-----------------------------------------------------------------------------
12 October 2012

Conflicts: 43 shift/reduce
           1 reduce/reduce

-----------------------------------------------------------------------------
24 February 2006

Conflicts: 33 shift/reduce
           1 reduce/reduce

The reduce/reduce conflict is weird.  It's between tyconsym and consym, and I
would think the two should never occur in the same context.

  -=chak

-----------------------------------------------------------------------------
31 December 2006

Conflicts: 34 shift/reduce
           1 reduce/reduce

The reduce/reduce conflict is weird.  It's between tyconsym and consym, and I
would think the two should never occur in the same context.

  -=chak

-----------------------------------------------------------------------------
6 December 2006

Conflicts: 32 shift/reduce
           1 reduce/reduce

The reduce/reduce conflict is weird.  It's between tyconsym and consym, and I
would think the two should never occur in the same context.

  -=chak

-----------------------------------------------------------------------------
26 July 2006

Conflicts: 37 shift/reduce
           1 reduce/reduce

The reduce/reduce conflict is weird.  It's between tyconsym and consym, and I
would think the two should never occur in the same context.

  -=chak

-----------------------------------------------------------------------------
Conflicts: 38 shift/reduce (1.25)

10 for abiguity in 'if x then y else z + 1'             [State 178]
        (shift parses as 'if x then y else (z + 1)', as per longest-parse rule)
        10 because op might be: : - ! * . `x` VARSYM CONSYM QVARSYM QCONSYM

1 for ambiguity in 'if x then y else z :: T'            [State 178]
        (shift parses as 'if x then y else (z :: T)', as per longest-parse rule)

4 for ambiguity in 'if x then y else z -< e'            [State 178]
        (shift parses as 'if x then y else (z -< T)', as per longest-parse rule)
        There are four such operators: -<, >-, -<<, >>-


2 for ambiguity in 'case v of { x :: T -> T ... } '     [States 11, 253]
        Which of these two is intended?
          case v of
            (x::T) -> T         -- Rhs is T
    or
          case v of
            (x::T -> T) -> ..   -- Rhs is ...

10 for ambiguity in 'e :: a `b` c'.  Does this mean     [States 11, 253]
        (e::a) `b` c, or
        (e :: (a `b` c))
    As well as `b` we can have !, VARSYM, QCONSYM, and CONSYM, hence 5 cases
    Same duplication between states 11 and 253 as the previous case

1 for ambiguity in 'let ?x ...'                         [State 329]
        the parser can't tell whether the ?x is the lhs of a normal binding or
        an implicit binding.  Fortunately resolving as shift gives it the only
        sensible meaning, namely the lhs of an implicit binding.

1 for ambiguity in '{-# RULES "name" [ ... #-}          [State 382]
        we don't know whether the '[' starts the activation or not: it
        might be the start of the declaration with the activation being
        empty.  --SDM 1/4/2002

1 for ambiguity in '{-# RULES "name" forall = ... #-}'  [State 474]
        since 'forall' is a valid variable name, we don't know whether
        to treat a forall on the input as the beginning of a quantifier
        or the beginning of the rule itself.  Resolving to shift means
        it's always treated as a quantifier, hence the above is disallowed.
        This saves explicitly defining a grammar for the rule lhs that
        doesn't include 'forall'.

1 for ambiguity when the source file starts with "-- | doc". We need another
  token of lookahead to determine if a top declaration or the 'module' keyword
  follows. Shift parses as if the 'module' keyword follows.

-- ---------------------------------------------------------------------------
-- Adding location info

This is done in a stylised way using the three macros below, L0, L1
and LL.  Each of these macros can be thought of as having type

   L0, L1, LL :: a -> Located a

They each add a SrcSpan to their argument.

   L0   adds 'noSrcSpan', used for empty productions
     -- This doesn't seem to work anymore -=chak

   L1   for a production with a single token on the lhs.  Grabs the SrcSpan
        from that token.

   LL   for a production with >1 token on the lhs.  Makes up a SrcSpan from
        the first and last tokens.

These suffice for the majority of cases.  However, we must be
especially careful with empty productions: LL won't work if the first
or last token on the lhs can represent an empty span.  In these cases,
we have to calculate the span using more of the tokens from the lhs, eg.

        | 'newtype' tycl_hdr '=' newconstr deriving
                { L (comb3 $1 $4 $5)
                    (mkTyData NewType (unLoc $2) $4 (unLoc $5)) }

We provide comb3 and comb4 functions which are useful in such cases.

Be careful: there's no checking that you actually got this right, the
only symptom will be that the SrcSpans of your syntax will be
incorrect.

/*
 * We must expand these macros *before* running Happy, which is why this file is
 * Parser.y.pp rather than just Parser.y - we run the C pre-processor first.
 */
#define L0   L noSrcSpan
#define L1   sL (getLoc $1)
#define LL   sL (comb2 $1 $>)

-- -----------------------------------------------------------------------------

-}

%token
 '_'            { L _ ITunderscore }            -- Haskell keywords
 'as'           { L _ ITas }
 'case'         { L _ ITcase }
 'class'        { L _ ITclass }
 'data'         { L _ ITdata }
 'default'      { L _ ITdefault }
 'deriving'     { L _ ITderiving }
 'do'           { L _ ITdo }
 'else'         { L _ ITelse }
 'hiding'       { L _ IThiding }
 'if'           { L _ ITif }
 'import'       { L _ ITimport }
 'in'           { L _ ITin }
 'infix'        { L _ ITinfix }
 'infixl'       { L _ ITinfixl }
 'infixr'       { L _ ITinfixr }
 'instance'     { L _ ITinstance }
 'let'          { L _ ITlet }
 'module'       { L _ ITmodule }
 'newtype'      { L _ ITnewtype }
 'of'           { L _ ITof }
 'qualified'    { L _ ITqualified }
 'then'         { L _ ITthen }
 'type'         { L _ ITtype }
 'where'        { L _ ITwhere }

 'forall'       { L _ ITforall }                -- GHC extension keywords
 'foreign'      { L _ ITforeign }
 'export'       { L _ ITexport }
 'label'        { L _ ITlabel }
 'dynamic'      { L _ ITdynamic }
 'safe'         { L _ ITsafe }
 'interruptible' { L _ ITinterruptible }
 'unsafe'       { L _ ITunsafe }
 'mdo'          { L _ ITmdo }
 'family'       { L _ ITfamily }
 'role'         { L _ ITrole }
 'stdcall'      { L _ ITstdcallconv }
 'ccall'        { L _ ITccallconv }
 'capi'         { L _ ITcapiconv }
 'prim'         { L _ ITprimcallconv }
 'javascript'   { L _ ITjavascriptcallconv }
 'proc'         { L _ ITproc }          -- for arrow notation extension
 'rec'          { L _ ITrec }           -- for arrow notation extension
 'group'    { L _ ITgroup }     -- for list transform extension
 'by'       { L _ ITby }        -- for list transform extension
 'using'    { L _ ITusing }     -- for list transform extension
 'pattern'      { L _ ITpattern } -- for pattern synonyms

 '{-# INLINE'             { L _ (ITinline_prag _ _) }
 '{-# SPECIALISE'         { L _ ITspec_prag }
 '{-# SPECIALISE_INLINE'  { L _ (ITspec_inline_prag _) }
 '{-# SOURCE'                                   { L _ ITsource_prag }
 '{-# RULES'                                    { L _ ITrules_prag }
 '{-# CORE'                                     { L _ ITcore_prag }              -- hdaume: annotated core
 '{-# SCC'                { L _ ITscc_prag }
 '{-# GENERATED'          { L _ ITgenerated_prag }
 '{-# DEPRECATED'         { L _ ITdeprecated_prag }
 '{-# WARNING'            { L _ ITwarning_prag }
 '{-# UNPACK'             { L _ ITunpack_prag }
 '{-# NOUNPACK'           { L _ ITnounpack_prag }
 '{-# ANN'                { L _ ITann_prag }
 '{-# VECTORISE'          { L _ ITvect_prag }
 '{-# VECTORISE_SCALAR'   { L _ ITvect_scalar_prag }
 '{-# NOVECTORISE'        { L _ ITnovect_prag }
 '{-# MINIMAL'            { L _ ITminimal_prag }
 '{-# CTYPE'              { L _ ITctype }
 '{-# OVERLAPPING'        { L _ IToverlapping_prag }
 '{-# OVERLAPPABLE'       { L _ IToverlappable_prag }
 '{-# OVERLAPS'           { L _ IToverlaps_prag }
 '{-# INCOHERENT'         { L _ ITincoherent_prag }
 '#-}'                                          { L _ ITclose_prag }

 '..'           { L _ ITdotdot }                        -- reserved symbols
 ':'            { L _ ITcolon }
 '::'           { L _ ITdcolon }
 '='            { L _ ITequal }
 '\\'           { L _ ITlam }
 'lcase'        { L _ ITlcase }
 '|'            { L _ ITvbar }
 '<-'           { L _ ITlarrow }
 '->'           { L _ ITrarrow }
 '@'            { L _ ITat }
 '~'            { L _ ITtilde }
 '~#'           { L _ ITtildehsh }
 '=>'           { L _ ITdarrow }
 '-'            { L _ ITminus }
 '!'            { L _ ITbang }
 '*'            { L _ ITstar }
 '-<'           { L _ ITlarrowtail }            -- for arrow notation
 '>-'           { L _ ITrarrowtail }            -- for arrow notation
 '-<<'          { L _ ITLarrowtail }            -- for arrow notation
 '>>-'          { L _ ITRarrowtail }            -- for arrow notation
 '.'            { L _ ITdot }

 '{'            { L _ ITocurly }                        -- special symbols
 '}'            { L _ ITccurly }
 vocurly        { L _ ITvocurly } -- virtual open curly (from layout)
 vccurly        { L _ ITvccurly } -- virtual close curly (from layout)
 '['            { L _ ITobrack }
 ']'            { L _ ITcbrack }
 '[:'           { L _ ITopabrack }
 ':]'           { L _ ITcpabrack }
 '('            { L _ IToparen }
 ')'            { L _ ITcparen }
 '(#'           { L _ IToubxparen }
 '#)'           { L _ ITcubxparen }
 '(|'           { L _ IToparenbar }
 '|)'           { L _ ITcparenbar }
 ';'            { L _ ITsemi }
 ','            { L _ ITcomma }
 '`'            { L _ ITbackquote }
 SIMPLEQUOTE    { L _ ITsimpleQuote      }     -- 'x

 VARID          { L _ (ITvarid    _) }          -- identifiers
 CONID          { L _ (ITconid    _) }
 VARSYM         { L _ (ITvarsym   _) }
 CONSYM         { L _ (ITconsym   _) }
 QVARID         { L _ (ITqvarid   _) }
 QCONID         { L _ (ITqconid   _) }
 QVARSYM        { L _ (ITqvarsym  _) }
 QCONSYM        { L _ (ITqconsym  _) }
 PREFIXQVARSYM  { L _ (ITprefixqvarsym  _) }
 PREFIXQCONSYM  { L _ (ITprefixqconsym  _) }

 IPDUPVARID     { L _ (ITdupipvarid   _) }              -- GHC extension

 CHAR           { L _ (ITchar     _) }
 STRING         { L _ (ITstring   _) }
 INTEGER        { L _ (ITinteger  _) } -- AZ TODO: capture original source text
 RATIONAL       { L _ (ITrational _) }

 PRIMCHAR       { L _ (ITprimchar   _) }
 PRIMSTRING     { L _ (ITprimstring _) }
 PRIMINTEGER    { L _ (ITprimint    _) }
 PRIMWORD       { L _ (ITprimword  _) }
 PRIMFLOAT      { L _ (ITprimfloat  _) }
 PRIMDOUBLE     { L _ (ITprimdouble _) }

 DOCNEXT        { L _ (ITdocCommentNext _) }
 DOCPREV        { L _ (ITdocCommentPrev _) }
 DOCNAMED       { L _ (ITdocCommentNamed _) }
 DOCSECTION     { L _ (ITdocSection _ _) }

-- Template Haskell
'[|'            { L _ ITopenExpQuote  }
'[p|'           { L _ ITopenPatQuote  }
'[t|'           { L _ ITopenTypQuote  }
'[d|'           { L _ ITopenDecQuote  }
'|]'            { L _ ITcloseQuote    }
'[||'           { L _ ITopenTExpQuote   }
'||]'           { L _ ITcloseTExpQuote  }
TH_ID_SPLICE    { L _ (ITidEscape _)  }     -- $x
'$('            { L _ ITparenEscape   }     -- $( exp )
TH_ID_TY_SPLICE { L _ (ITidTyEscape _)  }   -- $$x
'$$('           { L _ ITparenTyEscape   }   -- $$( exp )
TH_TY_QUOTE     { L _ ITtyQuote       }      -- ''T
TH_QUASIQUOTE   { L _ (ITquasiQuote _) }
TH_QQUASIQUOTE  { L _ (ITqQuasiQuote _) }

%monad { P } { >>= } { return }
%lexer { lexer } { L _ ITeof }
%tokentype { (Located Token) }

-- Exported parsers
%name parseModule module
%name parseImport importdecl
%name parseStatement stmt
%name parseDeclaration topdecl
%name parseExpression exp
%name parseTypeSignature sigdecl
%name parseFullStmt   stmt
%name parseStmt   maybe_stmt
%name parseIdentifier  identifier
%name parseType ctype
%partial parseHeader header
%%

-----------------------------------------------------------------------------
-- Identifiers; one of the entry points
identifier :: { Located RdrName }
        : qvar                          { $1 }
        | qcon                          { $1 }
        | qvarop                        { $1 }
        | qconop                        { $1 }
    | '(' '->' ')'      { LL $ getRdrName funTyCon }

-----------------------------------------------------------------------------
-- Module Header

-- The place for module deprecation is really too restrictive, but if it
-- was allowed at its natural place just before 'module', we get an ugly
-- s/r conflict with the second alternative. Another solution would be the
-- introduction of a new pragma DEPRECATED_MODULE, but this is not very nice,
-- either, and DEPRECATED is only expected to be used by people who really
-- know what they are doing. :-)

module :: { Located (HsModule RdrName) }
       : maybedocheader 'module' modid maybemodwarning maybeexports 'where' body
             {% fileSrcSpan >>= \ loc ->
                ams (L loc (HsModule (Just $3) $5 (fst $ snd $7)
                              (snd $ snd $7) (snd $4) $1)
                    )
                    ([mj AnnModule $2, mj AnnWhere $6] ++ fst $4 ++ fst $7) }
        | body2
                {% fileSrcSpan >>= \ loc ->
                   ams (L loc (HsModule Nothing Nothing
                               (fst $ snd $1) (snd $ snd $1) Nothing Nothing))
                       (fst $1) }

maybedocheader :: { Maybe LHsDocString }
        : moduleheader            { $1 }
        | {- empty -}             { Nothing }

missing_module_keyword :: { () }
        : {- empty -}                           {% pushCurrentContext }

maybemodwarning :: { ([MaybeAnn],Maybe WarningTxt) }
    : '{-# DEPRECATED' strings '#-}' { (mo $1:mc $1
                                                    : (fst $ unLoc $2)
                                       ,Just (DeprecatedTxt $ snd $ unLoc $2)) }
    | '{-# WARNING' strings '#-}'    { (mo $1:mc $3
                                                     : (fst $ unLoc $2)
                                       , Just (WarningTxt $ snd $ unLoc $2)) }
    |  {- empty -}                  { ([],Nothing) }

body    :: { ([MaybeAnn]
             ,(Located [LImportDecl RdrName], [LHsDecl RdrName])) }
        :  '{'            top '}'      { ([mo $1,mc $3,fst $2]
                                         , snd $2) }
        |      vocurly    top close    { ([], snd $2) }

body2   :: { ([MaybeAnn]
             ,(Located [LImportDecl RdrName], [LHsDecl RdrName])) }
        :  '{' top '}'                          { ([mo $1,mc $3
                                                   ,fst $2], snd $2) }
        |  missing_module_keyword top close     { ([],snd $2) }

top     :: { (MaybeAnn
             ,(Located [LImportDecl RdrName], [LHsDecl RdrName])) }
        : importdecls                   { (Nothing
                                          ,(L (gl $1) (reverse $ unLoc $1),[]))}
        | importdecls ';' cvtopdecls    { (mj AnnSemi $2
                                          ,(L (gl $1) (reverse $ unLoc $1),$3))}
        | cvtopdecls                    { (Nothing,(noLoc [],$1)) }

cvtopdecls :: { [LHsDecl RdrName] }
        : topdecls                              { cvTopDecls $1 }

-----------------------------------------------------------------------------
-- Module declaration & imports only

header  :: { Located (HsModule RdrName) }
        : maybedocheader 'module' modid maybemodwarning maybeexports 'where' header_body
                {% fileSrcSpan >>= \ loc ->
                   ams (L loc (HsModule (Just $3) $5 $7 [] (snd $4) $1
                          )) [mj AnnModule $2, mj AnnWhere $6] }
        | header_body2
                {% fileSrcSpan >>= \ loc ->
                   return (L loc (HsModule Nothing Nothing $1 [] Nothing
                          Nothing)) }

header_body :: { Located [LImportDecl RdrName] }
        :  '{'            importdecls           { $2 }
        |      vocurly    importdecls           { $2 }

header_body2 :: { Located [LImportDecl RdrName] }
        :  '{' importdecls                      { $2 }
        |  missing_module_keyword importdecls   { $2 }

-----------------------------------------------------------------------------
-- The Export List

maybeexports :: { Maybe (Located [LIE RdrName]) }
        :  '(' exportlist ')'       {% ajs (Just (L (comb2 $1 $3) (fromOL $2)))
                                           [mo $1,mc $3] }
        |  {- empty -}              { Nothing }

exportlist :: { OrdList (LIE RdrName) }
        : expdoclist ',' expdoclist   {% addAnnotation (oll $1) AnnComma (gl $2)
                                         >> return ($1 `appOL` $3) }
        | exportlist1                 { $1 }

exportlist1 :: { OrdList (LIE RdrName) }
        : expdoclist export expdoclist ',' exportlist1
                          {% (addAnnotation (oll ($1 `appOL` $2 `appOL` $3))
                                            AnnComma (gl $4) ) >>
                              return ($1 `appOL` $2 `appOL` $3 `appOL` $5) }
        | expdoclist export expdoclist             { $1 `appOL` $2 `appOL` $3 }
        | expdoclist                               { $1 }

expdoclist :: { OrdList (LIE RdrName) }
        : exp_doc expdoclist                           { $1 `appOL` $2 }
        | {- empty -}                                  { nilOL }

exp_doc :: { OrdList (LIE RdrName) }
        : docsection    { unitOL (L1 (case (unLoc $1) of (n, doc) -> IEGroup n doc)) }
        | docnamed      { unitOL (L1 (IEDocNamed ((fst . unLoc) $1))) }
        | docnext       { unitOL (L1 (IEDoc (unLoc $1))) }


   -- No longer allow things like [] and (,,,) to be exported
   -- They are built in syntax, always available
export  :: { OrdList (LIE RdrName) }
        : qcname_ext export_subspec   { unitOL (LL (mkModuleImpExp (unLoc $1)
                                                                   (unLoc $2))) }
        |  'module' modid             {% amsu (LL (IEModuleContents (unLoc $2)))
                                              [mj AnnModule $1] }
        |  'pattern' qcon             {% amsu (LL (IEVar (unLoc $2)))
                                              [mj AnnPattern $1] }

export_subspec :: { Located ImpExpSubSpec }
        : {- empty -}             { L0 ImpExpAbs }
        | '(' '..' ')'            {% ams (LL ImpExpAll)
                                         [mo $1,mc $3
                                         ,mj AnnDotdot $2] }
        | '(' ')'                 {% ams (LL (ImpExpList []))
                                         [mo $1,mc $2] }
        | '(' qcnames ')'         {% ams (LL (ImpExpList (reverse $2)))
                                         [mo $1,mc $3] }

qcnames :: { [Located RdrName] }     -- A reversed list
        :  qcnames ',' qcname_ext       {% (aa $3 (AnnComma, $2)) >>
                                           return ($3  : $1) }
        |  qcname_ext                   { [$1]  }

qcname_ext :: { Located RdrName }       -- Variable or data constructor
                                        -- or tagged type constructor
        :  qcname                       { $1 }
        |  'type' qcname                {% am (mkTypeImpExp (LL (unLoc $2)))
                                              (AnnType, $1) }

-- Cannot pull into qcname_ext, as qcname is also used in expression.
qcname  :: { Located RdrName }  -- Variable or data constructor
        :  qvar                         { $1 }
        |  qcon                         { $1 }

-----------------------------------------------------------------------------
-- Import Declarations

-- import decls can be *empty*, or even just a string of semicolons
-- whereas topdecls must contain at least one topdecl.

importdecls :: { Located [LImportDecl RdrName] }
        : importdecls ';' importdecl     {% (aa $3 (AnnSemi, $2)) >>
                                            return (LL ($3 : unLoc $1)) }
        | importdecls ';'                {% aa (LL (unLoc $1)) (AnnSemi,$2) }
        | importdecl                     { LL [$1] }
        | {- empty -}                    { noLoc [] }

importdecl :: { LImportDecl RdrName }
        : 'import' maybe_src maybe_safe optqualified maybe_pkg modid maybeas maybeimpspec
                {% ams (L (comb4 $1 $6 (snd $7) (snd $8)) $
                  ImportDecl { ideclName = $6, ideclPkgQual = snd $5
                             , ideclSource = snd $2, ideclSafe = snd $3
                             , ideclQualified = snd $4, ideclImplicit = False
                             , ideclAs = unLoc (snd $7)
                             , ideclHiding = unLoc (snd $8) })
                   (mj AnnImport $1 : fst $2 ++ fst $3 : fst $4 : fst $5
                                    : fst $7 : fst $8) }

maybe_src :: { ([MaybeAnn],IsBootInterface) }
        : '{-# SOURCE' '#-}'           { ([mo $1,mc $2],True) }
        | {- empty -}                  { ([],False) }

maybe_safe :: { (MaybeAnn,Bool) }
        : 'safe'                                { (mj AnnSafe $1,True) }
        | {- empty -}                           { (Nothing,False) }

maybe_pkg :: { (MaybeAnn,Maybe FastString) }
        : STRING                                { (mj AnnPackageName $1
                                                  ,Just (getSTRING $1)) }
        | {- empty -}                           { (Nothing,Nothing) }

optqualified :: { (MaybeAnn,Bool) }
        : 'qualified'                           { (mj AnnQualified $1,True)  }
        | {- empty -}                           { (Nothing,False) }

maybeas :: { (MaybeAnn,Located (Maybe ModuleName)) }
        : 'as' modid                            { (mj AnnAs $1
                                                  ,LL (Just (unLoc $2))) }
        | {- empty -}                           { (Nothing,noLoc Nothing) }

maybeimpspec :: { ([MaybeAnn],Located (Maybe (Bool, [LIE RdrName]))) }
        : impspec                  { (fst $1
                                     ,L (gl (snd $1)) (Just (unLoc (snd $1)))) }
        | {- empty -}              { ([],noLoc Nothing) }

impspec :: { ([MaybeAnn],Located (Bool, [LIE RdrName])) }
        :  '(' exportlist ')'                 { ([mo $1,mc $3]
                                                ,LL (False, fromOL $2)) }
        |  'hiding' '(' exportlist ')'        { ([mj AnnHiding $1,mo $2
                                                 ,mc $4]
                                                 ,LL (True, fromOL $3)) }

-----------------------------------------------------------------------------
-- Fixity Declarations

prec    :: { Int }
        : {- empty -}           { 9 }
        | INTEGER               {% checkPrecP (L1 (fromInteger (getINTEGER $1))) }

infix   :: { Located FixityDirection }
        : 'infix'                               { L1 InfixN  }
        | 'infixl'                              { L1 InfixL  }
        | 'infixr'                              { L1 InfixR }

ops     :: { Located (OrdList (Located RdrName)) }
        : ops ',' op              {% addAnnotation (gl $3) AnnComma (gl $2) >>
                                     return (LL (unitOL $3 `appOL` (unLoc $1)))}
        | op                      { L1 (unitOL $1) }

-----------------------------------------------------------------------------
-- Top-Level Declarations

topdecls :: { OrdList (LHsDecl RdrName) }
        : topdecls ';' topdecl        {% addAnnotation (oll $3) AnnSemi (gl $2)
                                         >> return ($1 `appOL` $3) }
        | topdecls ';'                {% addAnnotation (oll $1) AnnSemi (gl $2)
                                         >> return $1 }
        | topdecl                     { $1 }

topdecl :: { OrdList (LHsDecl RdrName) }
        : cl_decl                               { unitOL (L1 (TyClD (unLoc $1))) }
        | ty_decl                               { unitOL (L1 (TyClD (unLoc $1))) }
        | inst_decl                             { unitOL (L1 (InstD (unLoc $1))) }
        | stand_alone_deriving                  { unitOL (LL (DerivD (unLoc $1))) }
        | role_annot                            { unitOL (L1 (RoleAnnotD (unLoc $1))) }
        | 'default' '(' comma_types0 ')'    {% amsu (LL $ DefD (DefaultDecl $3))
                                                    [mj AnnDefault $1
                                                    ,mo $2,mc $4] }
        | 'foreign' fdecl                       {% amsu (LL (unLoc $2))
                                                        [mj AnnForeign $1] }
        | '{-# DEPRECATED' deprecations '#-}'   { $2 } -- ++AZ++ TODO
        | '{-# WARNING' warnings '#-}'          { $2 } -- ++AZ++ TODO
        | '{-# RULES' rules '#-}'               { $2 } -- ++AZ++ TODO
        | '{-# VECTORISE' qvar '=' exp '#-}' {% amsu (LL $ VectD (HsVect $2 $4))
                                                    [mo $1,mj AnnEqual $3
                                                    ,mc $5] }
        | '{-# NOVECTORISE' qvar '#-}'       {% amsu (LL $ VectD (HsNoVect $2))
                                                     [mo $1,mc $3] }
        | '{-# VECTORISE' 'type' gtycon '#-}'
                                {% amsu (LL $
                                    VectD (HsVectTypeIn False $3 Nothing))
                                    [mo $1,mj AnnType $2,mc $4] }

        | '{-# VECTORISE_SCALAR' 'type' gtycon '#-}'
                                {% amsu (LL $
                                    VectD (HsVectTypeIn True $3 Nothing))
                                    [mo $1,mj AnnType $2,mc $4] }

        | '{-# VECTORISE' 'type' gtycon '=' gtycon '#-}'
                                {% amsu (LL $
                                    VectD (HsVectTypeIn False $3 (Just $5)))
                                    [mo $1,mj AnnType $2,mj AnnEqual $4,mc $6] }
        | '{-# VECTORISE_SCALAR' 'type' gtycon '=' gtycon '#-}'
                                {% amsu (LL $
                                    VectD (HsVectTypeIn True $3 (Just $5)))
                                    [mo $1,mj AnnType $2,mj AnnEqual $4,mc $6] }

        | '{-# VECTORISE' 'class' gtycon '#-}'
                                         {% amsu (LL $ VectD (HsVectClassIn $3))
                                                 [mo $1,mj AnnClass $2,mc $4] }
        | annotation { unitOL $1 }
        | decl_no_th                            { unLoc $1 }

        -- Template Haskell Extension
        -- The $(..) form is one possible form of infixexp
        -- but we treat an arbitrary expression just as if
        -- it had a $(..) wrapped around it
        | infixexp                              { unitOL (LL $ mkSpliceDecl $1) }

-- Type classes
--
cl_decl :: { LTyClDecl RdrName }
        : 'class' tycl_hdr fds where_cls
                {% amms (mkClassDecl (comb4 $1 $2 $3 $4) $2 $3 (snd $ unLoc $4))
                        (mj AnnClass $1: (fst $ unLoc $4)) }

-- Type declarations (toplevel)
--
ty_decl :: { LTyClDecl RdrName }
           -- ordinary type synonyms
        : 'type' type '=' ctypedoc
                -- Note ctype, not sigtype, on the right of '='
                -- We allow an explicit for-all but we don't insert one
                -- in   type Foo a = (b,b)
                -- Instead we just say b is out of scope
                --
                -- Note the use of type for the head; this allows
                -- infix type constructors to be declared
                {% amms (mkTySynonym (comb2 $1 $4) $2 $4)
                        [mj AnnType $1,mj AnnEqual $3] }

           -- type family declarations
        | 'type' 'family' type opt_kind_sig where_type_family
                -- Note the use of type for the head; this allows
                -- infix type constructors to be declared
                {% amms (mkFamDecl (comb4 $1 $3 $4 $5) (snd $ unLoc $5) $3
                                   (unLoc $4))
                        (mj AnnType $1:mj AnnFamily $2:(fst $ unLoc $5)) }

          -- ordinary data type or newtype declaration
        | data_or_newtype capi_ctype tycl_hdr constrs deriving
                {% amms (mkTyData (comb4 $1 $3 $4 $5) (snd $ unLoc $1) $2 $3
                           Nothing [L (gl $4) (reverse (unLoc $4))]
                                   (snd $ unLoc $5))
                                   -- We need the location on tycl_hdr in case
                                   -- constrs and deriving are both empty
                        ((fst $ unLoc $1):(fst $ unLoc $5)) }

          -- ordinary GADT declaration
        | data_or_newtype capi_ctype tycl_hdr opt_kind_sig
                 gadt_constrlist
                 deriving
            {% amms (mkTyData (comb4 $1 $3 $5 $6) (snd $ unLoc $1) $2 $3
                            (unLoc $4) (snd $ unLoc $5) (snd $ unLoc $6) )
                                   -- We need the location on tycl_hdr in case
                                   -- constrs and deriving are both empty
                    ((fst $ unLoc $1):(fst $ unLoc $5)++(fst $ unLoc $6)) }

          -- data/newtype family
        | 'data' 'family' type opt_kind_sig
                {% amms (mkFamDecl (comb3 $1 $2 $4) DataFamily $3 (unLoc $4))
                        [mj AnnData $1,mj AnnFamily $2] }

inst_decl :: { LInstDecl RdrName }
        : 'instance' overlap_pragma inst_type where_inst
        {% do
            let (binds, sigs, _, ats, adts, _) = cvBindsAndSigs (snd $ unLoc $4)
            let cid = ClsInstDecl { cid_poly_ty = $3, cid_binds = binds
                                  , cid_sigs = sigs, cid_tyfam_insts = ats
                                  , cid_overlap_mode = snd $2
                                  , cid_datafam_insts = adts }
            ams (L (comb3 $1 $3 $4) (ClsInstD { cid_inst = cid }))
                ((mj AnnInstance $1 : (fst $2)) ++ (fst $ unLoc $4)) }

           -- type instance declarations
        | 'type' 'instance' ty_fam_inst_eqn
                {% amms (mkTyFamInst (comb2 $1 $3) $3)
                    [mj AnnType $1,mj AnnInstance $2] }

          -- data/newtype instance declaration
        | data_or_newtype 'instance' capi_ctype tycl_hdr constrs deriving
            {% amms (mkDataFamInst (comb4 $1 $4 $5 $6) (snd $ unLoc $1) $3 $4
                                      Nothing [L (gl $5) (reverse (unLoc $5))]
                                              (snd $ unLoc $6))
                    ((fst $ unLoc $1):mj AnnInstance $2:(fst $ unLoc $6)) }

          -- GADT instance declaration
        | data_or_newtype 'instance' capi_ctype tycl_hdr opt_kind_sig
                 gadt_constrlist
                 deriving
            {% amms (mkDataFamInst (comb4 $1 $4 $6 $7) (snd $ unLoc $1) $3 $4
                                   (unLoc $5) (snd $ unLoc $6) (snd $ unLoc $7))
                    ((fst $ unLoc $1):mj AnnInstance $2
                       :(fst $ unLoc $6)++(fst $ unLoc $7)) }

overlap_pragma :: { ([MaybeAnn],Maybe OverlapMode) }
  : '{-# OVERLAPPABLE'    '#-}' { ([mo $1,mc $2], Just Overlappable) }
  | '{-# OVERLAPPING'     '#-}' { ([mo $1,mc $2], Just Overlapping) }
  | '{-# OVERLAPS'        '#-}' { ([mo $1,mc $2], Just Overlaps) }
  | '{-# INCOHERENT'      '#-}' { ([mo $1,mc $2], Just Incoherent) }
  | {- empty -}                 { ([],Nothing) }


-- Closed type families

where_type_family :: { Located ([MaybeAnn],FamilyInfo RdrName) }
        : {- empty -}                      { noLoc ([],OpenTypeFamily) }
        | 'where' ty_fam_inst_eqn_list
               { LL (mj AnnWhere $1:(fst $ unLoc $2)
                    ,ClosedTypeFamily (reverse (snd $ unLoc $2))) }

ty_fam_inst_eqn_list :: { Located ([MaybeAnn],[LTyFamInstEqn RdrName]) }
        :     '{' ty_fam_inst_eqns '}'     { LL ([mo $1,mc $3]
                                                ,unLoc $2) }
        | vocurly ty_fam_inst_eqns close   { let L loc _ = $2 in
                                             L loc ([],unLoc $2) }
        |     '{' '..' '}'                 { LL ([mo $1,mj AnnDotdot $2
                                                 ,mc $3],[]) }
        | vocurly '..' close               { let L loc _ = $2 in
                                             L loc ([mj AnnDotdot $2],[]) }

ty_fam_inst_eqns :: { Located [LTyFamInstEqn RdrName] }
        : ty_fam_inst_eqns ';' ty_fam_inst_eqn
                                      {% addAnnotation (gl $3) AnnSemi (gl $2)
                                         >> return (LL ($3 : unLoc $1)) }
        | ty_fam_inst_eqns ';'        {% addAnnotation (gl $1) AnnSemi (gl $2)
                                         >> return (LL (unLoc $1)) }
        | ty_fam_inst_eqn             { LL [$1] }

ty_fam_inst_eqn :: { LTyFamInstEqn RdrName }
        : type '=' ctype
                -- Note the use of type for the head; this allows
                -- infix type constructors and type patterns
              {% do { eqn <- mkTyFamInstEqn $1 $3
                    ; aa (LL eqn) (AnnEqual, $2) } }

-- Associated type family declarations
--
-- * They have a different syntax than on the toplevel (no family special
--   identifier).
--
-- * They also need to be separate from instances; otherwise, data family
--   declarations without a kind signature cause parsing conflicts with empty
--   data declarations.
--
at_decl_cls :: { LHsDecl RdrName }
        :  -- data family declarations, with optional 'family' keyword
          'data' opt_family type opt_kind_sig
                {% amms (liftM mkTyClD (mkFamDecl (comb3 $1 $3 $4) DataFamily $3
                                                  (unLoc $4)))
                        (mj AnnData $1:$2) }

           -- type family declarations, with optional 'family' keyword
           -- (can't use opt_instance because you get shift/reduce errors
        | 'type' type opt_kind_sig
               {% amms (liftM mkTyClD (mkFamDecl (comb3 $1 $2 $3)
                                                  OpenTypeFamily $2 (unLoc $3)))
                       [mj AnnType $1] }
        | 'type' 'family' type opt_kind_sig
               {% amms (liftM mkTyClD (mkFamDecl (comb3 $1 $3 $4)
                                                  OpenTypeFamily $3 (unLoc $4)))
                       [mj AnnType $1,mj AnnFamily $2] }

           -- default type instances, with optional 'instance' keyword
        | 'type' ty_fam_inst_eqn
                {% amms (liftM mkInstD (mkTyFamInst (comb2 $1 $2) $2))
                        [mj AnnType $1] }
        | 'type' 'instance' ty_fam_inst_eqn
                {% amms (liftM mkInstD (mkTyFamInst (comb2 $1 $3) $3))
                        [mj AnnType $1,mj AnnInstance $2] }

opt_family   :: { [MaybeAnn] }
              : {- empty -}   { [] }
              | 'family'      { [mj AnnFamily $1] }

-- Associated type instances
--
at_decl_inst :: { LInstDecl RdrName }
           -- type instance declarations
        : 'type' ty_fam_inst_eqn
                -- Note the use of type for the head; this allows
                -- infix type constructors and type patterns
                {% amms (mkTyFamInst (comb2 $1 $2) $2)
                        [mj AnnType $1] }

        -- data/newtype instance declaration
        | data_or_newtype capi_ctype tycl_hdr constrs deriving
               {% amms (mkDataFamInst (comb4 $1 $3 $4 $5) (snd $ unLoc $1) $2 $3
                                    Nothing [L (gl $4) (reverse (unLoc $4))]
                                            (snd $ unLoc $5))
                       ((fst $ unLoc $1):(fst $ unLoc $5)) }

        -- GADT instance declaration
        | data_or_newtype capi_ctype tycl_hdr opt_kind_sig
                 gadt_constrlist
                 deriving
                {% amms (mkDataFamInst (comb4 $1 $3 $5 $6) (snd $ unLoc $1) $2
                                $3 (unLoc $4) (snd $ unLoc $5) (snd $ unLoc $6))
                        ((fst $ unLoc $1):(fst $ unLoc $5)++(fst $ unLoc $6)) }

data_or_newtype :: { Located (MaybeAnn,NewOrData) }
        : 'data'        { L1 (mj AnnData    $1,DataType) }
        | 'newtype'     { L1 (mj AnnNewtype $1,NewType) }

opt_kind_sig :: { Located (Maybe (LHsKind RdrName)) }
        :                             { noLoc Nothing }
        | '::' kind                   {% ajl (LL (Just $2)) AnnDcolon (gl $1) }

-- tycl_hdr parses the header of a class or data type decl,
-- which takes the form
--      T a b
--      Eq a => T a
--      (Eq a, Ord b) => T a b
--      T Int [a]                       -- for associated types
-- Rather a lot of inlining here, else we get reduce/reduce errors
tycl_hdr :: { Located (Maybe (LHsContext RdrName), LHsType RdrName) }
        : context '=>' type         {% return (L (comb2 $1 $2) (unLoc $1))
                                       >>= \c@(L l _) ->
                                         (addAnnotation l AnnDarrow (gl $2))
                                       >> (return (LL (Just c, $3)))
                                    }
        | type                      { L1 (Nothing, $1) }

capi_ctype :: { Maybe (Located CType) }
capi_ctype : '{-# CTYPE' STRING STRING '#-}'
                       {% ajs (Just (LL (CType (Just (Header (getSTRING $2)))
                                        (getSTRING $3))))
                              [mo $1,mj AnnHeader $2,mj AnnVal $3,mc $4] }

           | '{-# CTYPE'        STRING '#-}'
                       {% ajs (Just (LL (CType Nothing  (getSTRING $2))))
                              [mo $1,mj AnnVal $2,mc $3] }

           |           { Nothing }

-----------------------------------------------------------------------------
-- Stand-alone deriving

-- Glasgow extension: stand-alone deriving declarations
stand_alone_deriving :: { LDerivDecl RdrName }
  : 'deriving' 'instance' overlap_pragma inst_type
                         {% ams (LL (DerivDecl $4 (snd $3)))
                                (mj AnnDeriving $1:mj AnnInstance $2:(fst $3)) }

-----------------------------------------------------------------------------
-- Role annotations

role_annot :: { LRoleAnnotDecl RdrName }
role_annot : 'type' 'role' oqtycon maybe_roles
          {% amms (mkRoleAnnotDecl (comb3 $1 $3 $4) $3 (reverse (unLoc $4)))
                  [mj AnnType $1,mj AnnRole $2] }

-- Reversed!
maybe_roles :: { Located [Located (Maybe FastString)] }
maybe_roles : {- empty -}    { noLoc [] }
            | roles          { $1 }

roles :: { Located [Located (Maybe FastString)] }
roles : role             { LL [$1] }
      | roles role       { LL $ $2 : unLoc $1 }

-- read it in as a varid for better error messages
role :: { Located (Maybe FastString) }
role : VARID             { L1 $ Just $ getVARID $1 }
     | '_'               { L1 Nothing }

-- Pattern synonyms

-- Glasgow extension: pattern synonyms
pattern_synonym_decl :: { LHsDecl RdrName }
        : 'pattern' pat '=' pat
            {% do { (name, args) <- splitPatSyn $2
                  ; ams (LL . ValD $ mkPatSynBind name args $4
                                                  ImplicitBidirectional)
                        [mj AnnPattern $1,mj AnnEqual $3]
                  }}
        | 'pattern' pat '<-' pat
            {% do { (name, args) <- splitPatSyn $2
                  ; ams (LL . ValD $ mkPatSynBind name args $4 Unidirectional)
                        [mj AnnPattern $1,mj AnnLarrow $3]
                  }}
        | 'pattern' pat '<-' pat where_decls
            {% do { (name, args) <- splitPatSyn $2
                  ; mg <- toPatSynMatchGroup name (snd $ unLoc $5)
                  ; ams (LL . ValD $
                    mkPatSynBind name args $4 (ExplicitBidirectional mg))
                       (mj AnnPattern $1:mj AnnLarrow $3:(fst $ unLoc $5))
                  }}

where_decls :: { Located ([MaybeAnn]
                         , Located (OrdList (LHsDecl RdrName))) }
        : 'where' '{' decls '}'       { LL ([mj AnnWhere $1,mo $2
                                            ,mc $4],$3) }
        | 'where' vocurly decls close { L (comb2 $1 $3) ([mj AnnWhere $1]
                                          ,$3) }

vars0 :: { [Located RdrName] }
        : {- empty -}                 { [] }
        | varid vars0                 { $1 : $2 }

-----------------------------------------------------------------------------
-- Nested declarations

-- Declaration in class bodies
--
decl_cls  :: { Located (OrdList (LHsDecl RdrName)) }
decl_cls  : at_decl_cls                 { LL (unitOL $1) }
          | decl                        { $1 }

          -- A 'default' signature used with the generic-programming extension
          | 'default' infixexp '::' sigtypedoc
                    {% do { (TypeSig l ty) <- checkValSig $2 $4
                          ; ams (LL $ unitOL (LL $ SigD (GenericSig l ty)))
                                [mj AnnDefault $1,mj AnnDcolon $3] } }

          -- A 'default' signature used with the generic-programming extension
          | 'default' infixexp '::' sigtypedoc
                    {% do { (TypeSig l ty) <- checkValSig $2 $4
                          ; ams (LL $ unitOL (LL $ SigD (GenericSig l ty)))
                                [mj AnnDefault $1,mj AnnDotdot $3] } }

decls_cls :: { Located (OrdList (LHsDecl RdrName)) }    -- Reversed
          : decls_cls ';' decl_cls      {% addAnnotation (gl $3) AnnSemi (gl $2)
                                           >> return (LL ((unLoc $1) `appOL`
                                                                    unLoc $3)) }
          | decls_cls ';'               {% addAnnotation (gl $1) AnnSemi (gl $2)
                                           >> return (LL  (unLoc $1)) }
          | decl_cls                    { $1 }
          | {- empty -}                 { noLoc nilOL }

decllist_cls
        :: { Located ([MaybeAnn]
                     , OrdList (LHsDecl RdrName)) }      -- Reversed
        : '{'         decls_cls '}'     { LL ([mo $1,mc $3]
                                             ,unLoc $2) }
        |     vocurly decls_cls close   { L (gl $2) ([],unLoc $2) }

-- Class body
--
where_cls :: { Located ([MaybeAnn]
                       ,(OrdList (LHsDecl RdrName))) }    -- Reversed
                                -- No implicit parameters
                                -- May have type declarations
        : 'where' decllist_cls          { LL (mj AnnWhere $1:(fst $ unLoc $2)
                                             ,snd $ unLoc $2) }
        | {- empty -}                   { noLoc ([],nilOL) }

-- Declarations in instance bodies
--
decl_inst  :: { Located (OrdList (LHsDecl RdrName)) }
decl_inst  : at_decl_inst               { LL (unitOL (L1 (InstD (unLoc $1)))) }
           | decl                       { $1 }

decls_inst :: { Located (OrdList (LHsDecl RdrName)) }   -- Reversed
           : decls_inst ';' decl_inst   {% addAnnotation (gl $3) AnnSemi (gl $2)
                                           >> return
                                            (LL ((unLoc $1) `appOL` unLoc $3)) }
           | decls_inst ';'             {% addAnnotation (gl $1) AnnSemi (gl $2)
                                           >> return (LL (unLoc $1)) }
           | decl_inst                  { $1 }
           | {- empty -}                { noLoc nilOL }

decllist_inst
        :: { Located ([MaybeAnn]
                     , OrdList (LHsDecl RdrName)) }      -- Reversed
        : '{'         decls_inst '}'    { LL ([mo $1,mc $3],unLoc $2) }
        |     vocurly decls_inst close  { L (gl $2) ([],unLoc $2) }

-- Instance body
--
where_inst :: { Located ([MaybeAnn]
                        , OrdList (LHsDecl RdrName)) }   -- Reversed
                                -- No implicit parameters
                                -- May have type declarations
        : 'where' decllist_inst         { LL (mj AnnWhere $1:(fst $ unLoc $2)
                                             ,(snd $ unLoc $2)) }
        | {- empty -}                   { noLoc ([],nilOL) }

-- Declarations in binding groups other than classes and instances
--
decls   :: { Located (OrdList (LHsDecl RdrName)) }
        : decls ';' decl                {% addAnnotation (gl $3) AnnSemi (gl $2)
                                           >> return (
                                          let { this = unLoc $3;
                                    rest = unLoc $1;
                                    these = rest `appOL` this }
                              in rest `seq` this `seq` these `seq`
                                    LL these) }
        | decls ';'                     {% addAnnotation (gl $1) AnnSemi (gl $2)
                                           >> return (LL (unLoc $1)) }
        | decl                          { $1 }
        | {- empty -}                   { noLoc nilOL }

decllist :: { Located ([MaybeAnn],OrdList (LHsDecl RdrName)) }
        : '{'            decls '}'      { LL ([mo $1,mc $3],unLoc $2) }
        |     vocurly    decls close    { L (gl $2) ([],unLoc $2) }

-- Binding groups other than those of class and instance declarations
--
binds   ::  { Located ([MaybeAnn],HsLocalBinds RdrName) }
                                         -- May have implicit parameters
                                                -- No type declarations
        : decllist          { L1 (fst $ unLoc $1
                                  ,HsValBinds (cvBindGroup (snd $ unLoc $1))) }

        | '{'            dbinds '}'     { LL ([mo $1,mc $3]
                                             ,HsIPBinds (IPBinds (unLoc $2)
                                                         emptyTcEvBinds)) }

        |     vocurly    dbinds close   { L (getLoc $2) ([]
                                            ,HsIPBinds (IPBinds (unLoc $2)
                                                        emptyTcEvBinds)) }


wherebinds :: { Located ([MaybeAnn],HsLocalBinds RdrName) }
                                                -- May have implicit parameters
                                                -- No type declarations
        : 'where' binds                 { LL (mj AnnWhere $1 : (fst $ unLoc $2)
                                             ,snd $ unLoc $2) }
        | {- empty -}                   { noLoc ([],emptyLocalBinds) }


-----------------------------------------------------------------------------
-- Transformation Rules

rules   :: { OrdList (LHsDecl RdrName) }
        :  rules ';' rule               {% addAnnotation (gl $3) AnnSemi (gl $2)
                                           >> return ($1 `snocOL` $3) }
        |  rules ';'                    {% addAnnotation (oll $1) AnnSemi (gl $2)
                                           >> return $1 }
        |  rule                         { unitOL $1 }
        |  {- empty -}                  { nilOL }

rule    :: { LHsDecl RdrName }
        : STRING rule_activation rule_forall infixexp '=' exp
         {%ams (LL $ RuleD (HsRule (L (gl $1) (getSTRING $1))
                                  ((snd $2) `orElse` AlwaysActive)
                                  (snd $3) $4 placeHolderNames $6
                                  placeHolderNames))
               (mj AnnEqual $5 : (fst $2) ++ (fst $3)) }

-- Rules can be specified to be NeverActive, unlike inline/specialize pragmas
rule_activation :: { ([MaybeAnn],Maybe Activation) }
        : {- empty -}                           { ([],Nothing) }
        | rule_explicit_activation              { (fst $1,Just (snd $1)) }

rule_explicit_activation :: { ([MaybeAnn]
                              ,Activation) }  -- In brackets
        : '[' INTEGER ']'       { ([mo $1,mj AnnVal $2,mc $3]
                                  ,ActiveAfter  (fromInteger (getINTEGER $2))) }
        | '[' '~' INTEGER ']'   { ([mo $1,mj AnnTilde $2,mj AnnVal $3,mc $4]
                                  ,ActiveBefore (fromInteger (getINTEGER $3))) }
        | '[' '~' ']'           { ([mo $1,mj AnnTilde $2,mc $3]
                                  ,NeverActive) }

rule_forall :: { ([MaybeAnn],[LRuleBndr RdrName]) }
        : 'forall' rule_var_list '.'     { ([mj AnnForall $1,mj AnnDot $3],$2) }
        | {- empty -}                    { ([],[]) }

rule_var_list :: { [LRuleBndr RdrName] }
        : rule_var                              { [$1] }
        | rule_var rule_var_list                { $1 : $2 }

rule_var :: { LRuleBndr RdrName }
        : varid                           { LL (RuleBndr $1) }
        | '(' varid '::' ctype ')'        {% ams (LL (RuleBndrSig $2
                                                         (mkHsWithBndrs $4)))
                                                 [mo $1,mj AnnDcolon $3,mc $5] }

-----------------------------------------------------------------------------
-- Warnings and deprecations (c.f. rules)

warnings :: { OrdList (LHsDecl RdrName) }
        : warnings ';' warning         {% addAnnotation (oll $3) AnnSemi (gl $2)
                                          >> return ($1 `appOL` $3) }
        | warnings ';'                 {% addAnnotation (oll $1) AnnSemi (gl $2)
                                          >> return $1 }
        | warning                      { $1 }
        | {- empty -}                  { nilOL }

-- SUP: TEMPORARY HACK, not checking for `module Foo'
warning :: { OrdList (LHsDecl RdrName) }
        : namelist strings
                { toOL [ LL $ WarningD (Warning n (WarningTxt $ snd $ unLoc $2))
                       | n <- unLoc $1 ] }

deprecations :: { OrdList (LHsDecl RdrName) }
        : deprecations ';' deprecation
                                       {% addAnnotation (oll $3) AnnSemi (gl $2)
                                          >> return ($1 `appOL` $3) }
        | deprecations ';'             {% addAnnotation (oll $1) AnnSemi (gl $2)
                                          >> return $1 }
        | deprecation                  { $1 }
        | {- empty -}                  { nilOL }

-- SUP: TEMPORARY HACK, not checking for `module Foo'
deprecation :: { OrdList (LHsDecl RdrName) }
        : namelist strings
             { toOL [ LL $ WarningD (Warning n (DeprecatedTxt $ snd $ unLoc $2))
                    | n <- unLoc $1 ] }

strings :: { Located ([MaybeAnn],[Located FastString]) }
    : STRING { L1 ([],[L (gl $1) (getSTRING $1)]) }
    | '[' stringlist ']' { LL $ ([mo $1,mc $3],fromOL (unLoc $2)) }

stringlist :: { Located (OrdList (Located FastString)) }
    : stringlist ',' STRING {% addAnnotation (gl $3) AnnComma (gl $2) >>
                               return (LL (unLoc $1 `snocOL`
                                                  (L (gl $3) (getSTRING $3)))) }
    | STRING                { LL (unitOL (L (gl $1) (getSTRING $1))) }

-----------------------------------------------------------------------------
-- Annotations
annotation :: { LHsDecl RdrName }
    : '{-# ANN' name_var aexp '#-}'      {% ams (LL (AnnD $ HsAnnotation
                                            (ValueAnnProvenance (unLoc $2)) $3))
                                            [mo $1,mc $4] }

    | '{-# ANN' 'type' tycon aexp '#-}'  {% ams (LL (AnnD $ HsAnnotation
                                            (TypeAnnProvenance (unLoc $3)) $4))
                                            [mo $1,mj AnnType $2,mc $5] }

    | '{-# ANN' 'module' aexp '#-}'      {% ams (LL (AnnD $ HsAnnotation
                                                 ModuleAnnProvenance $3))
                                                [mo $1,mj AnnModule $2,mc $4] }


-----------------------------------------------------------------------------
-- Foreign import and export declarations

fdecl :: { LHsDecl RdrName }
fdecl : 'import' callconv safety fspec
                {% mkImport (unLoc $2) (unLoc $3) (snd $ unLoc $4) >>= \i ->
                  ams (LL i) (mj AnnImport $1 : (fst $ unLoc $4)) }
      | 'import' callconv        fspec
                {% do { d <- mkImport (unLoc $2) PlaySafe (snd $ unLoc $3);
                        ams (LL d) (mj AnnImport $1 : (fst $ unLoc $3)) } }
      | 'export' callconv fspec
                {% mkExport (unLoc $2) (snd $ unLoc $3) >>= \i ->
                   ams (LL i) (mj AnnExport $1 : (fst $ unLoc $3)) }

callconv :: { Located CCallConv }
          : 'stdcall'                   { LL StdCallConv }
          | 'ccall'                     { LL CCallConv   }
          | 'capi'                      { LL CApiConv    }
          | 'prim'                      { LL PrimCallConv}
          | 'javascript'                { LL JavaScriptCallConv }

safety :: { Located Safety }
        : 'unsafe'                      { LL PlayRisky }
        | 'safe'                        { LL PlaySafe }
        | 'interruptible'               { LL PlayInterruptible }

fspec :: { Located ([MaybeAnn]
                    ,(Located FastString, Located RdrName, LHsType RdrName)) }
       : STRING var '::' sigtypedoc     { LL ([mj AnnDcolon $3]
                                             ,(L (getLoc $1)
                                                    (getSTRING $1), $2, $4)) }
       |        var '::' sigtypedoc     { LL ([mj AnnDcolon $2]
                                             ,(noLoc nilFS, $1, $3)) }
         -- if the entity string is missing, it defaults to the empty string;
         -- the meaning of an empty entity string depends on the calling
         -- convention

-----------------------------------------------------------------------------
-- Type signatures

opt_sig :: { ([MaybeAnn],Maybe (LHsType RdrName)) }
        : {- empty -}                   { ([],Nothing) }
        | '::' sigtype                  { ([mj AnnDotdot $1],Just $2) }

opt_asig :: { ([MaybeAnn],Maybe (LHsType RdrName)) }
        : {- empty -}                   { ([],Nothing) }
        | '::' atype                    { ([mj AnnDotdot $1],Just $2) }

sigtype :: { LHsType RdrName }          -- Always a HsForAllTy,
                                        -- to tell the renamer where to generalise
        : ctype                         { L1 (mkImplicitHsForAllTy (noLoc []) $1) }
        -- Wrap an Implicit forall if there isn't one there already

sigtypedoc :: { LHsType RdrName }       -- Always a HsForAllTy
        : ctypedoc                      { L1 (mkImplicitHsForAllTy (noLoc []) $1) }
        -- Wrap an Implicit forall if there isn't one there already

sig_vars :: { Located ([Located RdrName]) }    -- Returned in reversed order
         : sig_vars ',' var             {% addAnnotation (gl $3) AnnComma (gl $2)
                                           >> return (LL ($3 : unLoc $1)) }
         | var                          { L1 [$1] }

sigtypes1 :: { (OrdList (LHsType RdrName)) }      -- Always HsForAllTys
        : sigtype                      { unitOL $1 }
        | sigtype ',' sigtypes1        {% addAnnotation (gl $1) AnnComma (gl $2)
                                          >> return ((unitOL $1) `appOL` $3) }

-----------------------------------------------------------------------------
-- Types

strict_mark :: { Located HsBang }
        : '!'                        { (L1 (HsUserBang Nothing      True)) }
        | '{-# UNPACK' '#-}'         {% ams (LL (HsUserBang (Just True)  False))
                                            [mo $1,mc $2] }
        | '{-# NOUNPACK' '#-}'       {% ams (LL (HsUserBang (Just False) True))
                                            [mo $1,mc $2] }
        | '{-# UNPACK' '#-}' '!'     {% ams (LL (HsUserBang (Just True)  True))
                                            [mo $1,mc $2] }
        | '{-# NOUNPACK' '#-}' '!'   {% ams (LL (HsUserBang (Just False) True))
                                               [mo $1,mc $2] }
        -- Although UNPACK with no '!' is illegal, we get a
        -- better error message if we parse it here

-- A ctype is a for-all type
ctype   :: { LHsType RdrName }
        : 'forall' tv_bndrs '.' ctype   {% hintExplicitForall (getLoc $1) >>
                                           ams (LL $ mkExplicitHsForAllTy $2
                                                                 (noLoc []) $4)
                                               [mj AnnForall $1,mj AnnDot $3] }
        | context '=>' ctype            {% ams (LL $ mkQualifiedHsForAllTy
                                                                         $1 $3)
                                              [mj AnnDarrow $2] }
        | ipvar '::' type               {% ams (LL (HsIParamTy (unLoc $1) $3))
                                               [mj AnnDcolon $2] }
        | type                          { $1 }

----------------------
-- Notes for 'ctypedoc'
-- It would have been nice to simplify the grammar by unifying `ctype` and
-- ctypedoc` into one production, allowing comments on types everywhere (and
-- rejecting them after parsing, where necessary).  This is however not possible
-- since it leads to ambiguity. The reason is the support for comments on record
-- fields:
--         data R = R { field :: Int -- ^ comment on the field }
-- If we allow comments on types here, it's not clear if the comment applies
-- to 'field' or to 'Int'. So we must use `ctype` to describe the type.

ctypedoc :: { LHsType RdrName }
        : 'forall' tv_bndrs '.' ctypedoc {% hintExplicitForall (getLoc $1) >>
                                            ams (LL $ mkExplicitHsForAllTy $2
                                                                  (noLoc []) $4)
                                                [mj AnnForall $1,mj AnnDot $3] }
        | context '=>' ctypedoc        {% ams (LL $ mkQualifiedHsForAllTy $1 $3)
                                              [mj AnnDarrow $2] }
        | ipvar '::' type              {% ams (LL (HsIParamTy (unLoc $1) $3))
                                              [mj AnnDcolon $2] }
        | typedoc                      { $1 }

----------------------
-- Notes for 'context'
-- We parse a context as a btype so that we don't get reduce/reduce
-- errors in ctype.  The basic problem is that
--      (Eq a, Ord a)
-- looks so much like a tuple type.  We can't tell until we find the =>

-- We have the t1 ~ t2 form both in 'context' and in type,
-- to permit an individual equational constraint without parenthesis.
-- Thus for some reason we allow    f :: a~b => blah
-- but not                          f :: ?x::Int => blah
context :: { LHsContext RdrName }
        : btype '~'      btype          {% amms (checkContext
                                             (LL $ HsEqTy $1 $3))
                                             [mj AnnTilde $2] }
        | btype                         {% checkContext $1 }

type :: { LHsType RdrName }
        : btype                         { $1 }
        | btype qtyconop type           { LL $ mkHsOpTy $1 $2 $3 }
        | btype tyvarop  type           { LL $ mkHsOpTy $1 $2 $3 }
        | btype '->'     ctype          {% ams (LL $ HsFunTy $1 $3)
                                               [mj AnnRarrow $2] }
        | btype '~'      btype          {% ams (LL $ HsEqTy $1 $3)
                                               [mj AnnTilde $2] }
                                        -- see Note [Promotion]
        | btype SIMPLEQUOTE qconop type     { LL $ mkHsOpTy $1 $3 $4 }
        | btype SIMPLEQUOTE varop  type     { LL $ mkHsOpTy $1 $3 $4 }

typedoc :: { LHsType RdrName }
        : btype                          { $1 }
        | btype docprev                  { LL $ HsDocTy $1 $2 }
        | btype qtyconop type            { LL $ mkHsOpTy $1 $2 $3 }
        | btype qtyconop type docprev    { LL $ HsDocTy (L (comb3 $1 $2 $3) (mkHsOpTy $1 $2 $3)) $4 }
        | btype tyvarop  type            { LL $ mkHsOpTy $1 $2 $3 }
        | btype tyvarop  type docprev    { LL $ HsDocTy (L (comb3 $1 $2 $3) (mkHsOpTy $1 $2 $3)) $4 }
        | btype '->'     ctypedoc        {% ams (LL $ HsFunTy $1 $3)
                                                [mj AnnRarrow $2] }
        | btype docprev '->' ctypedoc    {% ams (LL $ HsFunTy (L (comb2 $1 $2)
                                                            (HsDocTy $1 $2)) $4)
                                                [mj AnnRarrow $3] }
        | btype '~'      btype           {% ams (LL $ HsEqTy $1 $3)
                                                [mj AnnTilde $2] }
                                        -- see Note [Promotion]
        | btype SIMPLEQUOTE qconop type     { LL $ mkHsOpTy $1 $3 $4 }
        | btype SIMPLEQUOTE varop  type     { LL $ mkHsOpTy $1 $3 $4 }

btype :: { LHsType RdrName }
        : btype atype                   { LL $ HsAppTy $1 $2 }
        | atype                         { $1 }

atype :: { LHsType RdrName }
        : ntgtycon                       { L1 (HsTyVar (unLoc $1)) }      -- Not including unit tuples
        | tyvar                          { L1 (HsTyVar (unLoc $1)) }      -- (See Note [Unit tuples])
        | strict_mark atype              { LL (HsBangTy (unLoc $1) $2) }  -- Constructor sigs only
        | '{' fielddecls '}'             {% amms (checkRecordSyntax
                                                    (LL $ HsRecTy $2))
                                                        -- Constructor sigs only
                                                 [mo $1,mc $3] }
        | '(' ')'                        {% ams (LL $ HsTupleTy
                                                    HsBoxedOrConstraintTuple [])
                                                [mo $1,mc $2] }
        | '(' ctype ',' comma_types1 ')' {% ams (LL $ HsTupleTy
                                             HsBoxedOrConstraintTuple ($2 : $4))
                                                [mo $1,mj AnnComma $3,mc $5] }
        | '(#' '#)'                   {% ams (LL $ HsTupleTy HsUnboxedTuple [])
                                             [mo $1,mc $2] }
        | '(#' comma_types1 '#)'      {% ams (LL $ HsTupleTy HsUnboxedTuple $2)
                                             [mo $1,mc $3] }
        | '[' ctype ']'               {% ams (LL $ HsListTy  $2) [mo $1,mc $3] }
        | '[:' ctype ':]'             {% ams (LL $ HsPArrTy  $2) [mo $1,mc $3] }
        | '(' ctype ')'               {% ams (LL $ HsParTy   $2) [mo $1,mc $3] }
        | '(' ctype '::' kind ')'     {% ams (LL $ HsKindSig $2 $4)
                                             [mo $1,mj AnnDcolon $3,mc $5] }
        | quasiquote                  { L1 (HsQuasiQuoteTy (unLoc $1)) }
        | '$(' exp ')'                {% ams (LL $ mkHsSpliceTy $2)
                                             [mo $1,mc $3] }
        | TH_ID_SPLICE                { LL $ mkHsSpliceTy $ L1 $ HsVar $
                                        mkUnqual varName (getTH_ID_SPLICE $1) }
                                      -- see Note [Promotion] for the followings
        | SIMPLEQUOTE qcon                            { LL $ HsTyVar $ unLoc $2 }
        | SIMPLEQUOTE  '(' ctype ',' comma_types1 ')'
                                    {% ams (LL $ HsExplicitTupleTy [] ($3 : $5))
                                           [mo $2,mj AnnComma $4,mc $6] }
        | SIMPLEQUOTE  '[' comma_types0 ']'     {% ams (LL $ HsExplicitListTy
                                                            placeHolderKind $3)
                                                       [mo $2,mc $4] }
        | SIMPLEQUOTE var                       { LL $ HsTyVar $ unLoc $2 }

        | '[' ctype ',' comma_types1 ']'  {% ams (LL $ HsExplicitListTy
                                                     placeHolderKind ($2 : $4))
                                                 [mo $1, mj AnnComma $3,mc $5] }
        | INTEGER                     { LL $ HsTyLit $ HsNumTy $ getINTEGER $1 }
        | STRING                      { LL $ HsTyLit $ HsStrTy $ getSTRING  $1 }

-- An inst_type is what occurs in the head of an instance decl
--      e.g.  (Foo a, Gaz b) => Wibble a b
-- It's kept as a single type, with a MonoDictTy at the right
-- hand corner, for convenience.
inst_type :: { LHsType RdrName }
        : sigtype                       { $1 }

inst_types1 :: { [LHsType RdrName] }
        : inst_type                     { [$1] }

        | inst_type ',' inst_types1    {% addAnnotation (gl $1) AnnComma (gl $2)
                                          >> return ($1 : $3) }

comma_types0  :: { [LHsType RdrName] }
        : comma_types1                  { $1 }
        | {- empty -}                   { [] }

comma_types1    :: { [LHsType RdrName] }
        : ctype                        { [$1] }
        | ctype  ',' comma_types1      {% addAnnotation (gl $1) AnnComma (gl $2)
                                          >> return ($1 : $3) }

tv_bndrs :: { [LHsTyVarBndr RdrName] }
         : tv_bndr tv_bndrs             { $1 : $2 }
         | {- empty -}                  { [] }

tv_bndr :: { LHsTyVarBndr RdrName }
        : tyvar                         { L1 (UserTyVar (unLoc $1)) }
        | '(' tyvar '::' kind ')'       {% ams (LL (KindedTyVar (unLoc $2) $4))
                                               [mo $1,mj AnnDotdot $3
                                               ,mc $5] }

fds :: { Located [Located (FunDep RdrName)] }
        : {- empty -}                   { noLoc [] }
        | '|' fds1                      {% ams (LL (reverse (unLoc $2)))
                                                [mj AnnVbar $1] }

fds1 :: { Located [Located (FunDep RdrName)] }
        : fds1 ',' fd                  {% addAnnotation (gl $3) AnnComma (gl $2)
                                          >> return (LL ($3 : unLoc $1)) }
        | fd                           { L1 [$1] }

fd :: { Located (FunDep RdrName) }
        : varids0 '->' varids0  {% ams (L (comb3 $1 $2 $3)
                                       (reverse (unLoc $1), reverse (unLoc $3)))
                                       [mj AnnRarrow $2] }

varids0 :: { Located [RdrName] }
        : {- empty -}                   { noLoc [] }
        | varids0 tyvar                 { LL (unLoc $2 : unLoc $1) }

-----------------------------------------------------------------------------
-- Kinds

kind :: { LHsKind RdrName }
        : bkind                  { $1 }
        | bkind '->' kind        {% ams (LL $ HsFunTy $1 $3)
                                        [mj AnnRarrow $2] }

bkind :: { LHsKind RdrName }
        : akind                  { $1 }
        | bkind akind            { LL $ HsAppTy $1 $2 }

akind :: { LHsKind RdrName }
        : '*'                    { L1 $ HsTyVar (nameRdrName liftedTypeKindTyConName) }
        | '(' kind ')'           {% ams (LL $ HsParTy $2)
                                        [mo $1,mc $3] }
        | pkind                  { $1 }
        | tyvar                  { L1 $ HsTyVar (unLoc $1) }

pkind :: { LHsKind RdrName }  -- promoted type, see Note [Promotion]
        : qtycon                          { L1 $ HsTyVar $ unLoc $1 }
        | '(' ')'                   {% ams (LL $ HsTyVar $ getRdrName unitTyCon)
                                           [mo $1,mc $2] }
        | '(' kind ',' comma_kinds1 ')'   {% ams (LL $ HsTupleTy HsBoxedTuple
                                                                     ( $2 : $4))
                                                 [mo $1,mj AnnComma $3,mc $5] }
        | '[' kind ']'                    {% ams (LL $ HsListTy $2)
                                                 [mo $1,mc $3] }

comma_kinds1 :: { [LHsKind RdrName] }
        : kind                         { [$1] }
        | kind  ',' comma_kinds1       {% addAnnotation (gl $1) AnnComma (gl $2)
                                          >> return ($1 : $3) }

{- Note [Promotion]
   ~~~~~~~~~~~~~~~~

- Syntax of promoted qualified names
We write 'Nat.Zero instead of Nat.'Zero when dealing with qualified
names. Moreover ticks are only allowed in types, not in kinds, for a
few reasons:
  1. we don't need quotes since we cannot define names in kinds
  2. if one day we merge types and kinds, tick would mean look in DataName
  3. we don't have a kind namespace anyway

- Syntax of explicit kind polymorphism  (IA0_TODO: not yet implemented)
Kind abstraction is implicit. We write
> data SList (s :: k -> *) (as :: [k]) where ...
because it looks like what we do in terms
> id (x :: a) = x

- Name resolution
When the user write Zero instead of 'Zero in types, we parse it a
HsTyVar ("Zero", TcClsName) instead of HsTyVar ("Zero", DataName). We
deal with this in the renamer. If a HsTyVar ("Zero", TcClsName) is not
bounded in the type level, then we look for it in the term level (we
change its namespace to DataName, see Note [Demotion] in OccName). And
both become a HsTyVar ("Zero", DataName) after the renamer.

-}


-----------------------------------------------------------------------------
-- Datatype declarations

gadt_constrlist :: { Located ([MaybeAnn]
                          ,[Located [LConDecl RdrName]]) } -- Returned in order
        : 'where' '{'        gadt_constrs '}'   { L (comb2 $1 $3)
                                                    ([mj AnnWhere $1
                                                     ,mo $2
                                                     ,mc $4]
                                                    , unLoc $3) }
        | 'where' vocurly    gadt_constrs close  { L (comb2 $1 $3)
                                                     ([mj AnnWhere $1]
                                                     , unLoc $3) }
        | {- empty -}                            { noLoc ([],[]) }

gadt_constrs :: { Located [Located [LConDecl RdrName]] }
        : gadt_constr ';' gadt_constrs
                  {% addAnnotation (gl $1) AnnSemi (gl $2)
                     >> return (L (comb2 $1 $3) ((snd $ unLoc $1) : unLoc $3)) }
        | gadt_constr                   { L (gl $1) [snd $ unLoc $1] }
        | {- empty -}                   { noLoc [] }

-- We allow the following forms:
--      C :: Eq a => a -> T a
--      C :: forall a. Eq a => !a -> T a
--      D { x,y :: a } :: T a
--      forall a. Eq a => D { x,y :: a } :: T a

gadt_constr :: { Located ([MaybeAnn],Located [LConDecl RdrName]) }
                   -- Returns a list because of:   C,D :: ty
        : con_list '::' sigtype
                { sL (comb2 $1 $3) ([mj AnnDotdot $2]
                  ,sL (comb2 $1 $3) (map (sL (comb2 $1 $3))
                                          (mkGadtDecl (unLoc $1) $3))) }


                -- Deprecated syntax for GADT record declarations
        | oqtycon '{' fielddecls '}' '::' sigtype
                {% do { cd <- mkDeprecatedGadtRecordDecl (comb2 $1 $6) $1 $3 $6
                      ; cd' <- checkRecordSyntax cd
                      ; return (L (comb2 $1 $6)
                                   ([mo $2,mc $4,mj AnnDotdot $5]
                                   ,L (comb2 $1 $6) [cd'])) } }

constrs :: { Located [LConDecl RdrName] }
        : maybe_docnext '=' constrs1    {% ams (L (comb2 $2 $3)
                                                     (addConDocs (unLoc $3) $1))
                                               [mj AnnEqual ($2)] }

constrs1 :: { Located [LConDecl RdrName] }
        : constrs1 maybe_docnext '|' maybe_docprev constr
            {% addAnnotation (gl $5) AnnVbar (gl $3)
               >> return (LL (addConDoc $5 $2 : addConDocFirst (unLoc $1) $4)) }
        | constr                                          { L1 [$1] }

constr :: { LConDecl RdrName }
        : maybe_docnext forall context '=>' constr_stuff maybe_docprev
                {% ams (let (con,details) = unLoc $5 in
                  addConDoc (L (comb4 $2 $3 $4 $5) (mkSimpleConDecl con
                                                   (snd $ unLoc $2) $3 details))
                            ($1 `mplus` $6))
                        (mj AnnDarrow $4:(fst $ unLoc $2)) }
        | maybe_docnext forall constr_stuff maybe_docprev
                {% ams ( let (con,details) = unLoc $3 in
                  addConDoc (L (comb2 $2 $3) (mkSimpleConDecl con
                                           (snd $ unLoc $2) (noLoc []) details))
                            ($1 `mplus` $4))
                       (fst $ unLoc $2) }

forall :: { Located ([MaybeAnn],[LHsTyVarBndr RdrName]) }
        : 'forall' tv_bndrs '.'       { LL ([mj AnnForall $1,mj AnnDot $3],$2) }
        | {- empty -}                 { noLoc ([],[]) }

constr_stuff :: { Located (Located RdrName, HsConDeclDetails RdrName) }
-- We parse the constructor declaration
--      C t1 t2
-- as a btype (treating C as a type constructor) and then convert C to be
-- a data constructor.  Reason: it might continue like this:
--      C t1 t2 %: D Int
-- in which case C really would be a type constructor.  We can't resolve this
-- ambiguity till we come across the constructor oprerator :% (or not, more usually)
        : btype                         {% splitCon $1 >>= return.LL }
        | btype conop btype             {  LL ($2, InfixCon $1 $3) }

fielddecls :: { [Located [ConDeclField RdrName]] }
        : {- empty -}     { [] }
        | fielddecls1     { $1 }

fielddecls1 :: { [Located [ConDeclField RdrName]] }
        : fielddecl maybe_docnext ',' maybe_docprev fielddecls1
          {% addAnnotation (gl $1) AnnComma (gl $3)
             >> return ((L (gl $1) [ addFieldDoc f $4 | f <- unLoc $1 ])
                                    : addFieldDocs $5 $2) }
                             -- This adds the doc $4 to each field separately
        | fielddecl   { [$1] }

fielddecl :: { Located [ConDeclField RdrName] } -- A list because of   f,g :: Int
        : maybe_docnext sig_vars '::' ctype maybe_docprev
                   {% ams (L (comb2 $2 $4) [ ConDeclField fld $4 ($1 `mplus` $5)
                                           | fld <- reverse (unLoc $2) ])
                          [mj AnnDotdot $3] }
-- We allow the odd-looking 'inst_type' in a deriving clause, so that
-- we can do deriving( forall a. C [a] ) in a newtype (GHC extension).
-- The 'C [a]' part is converted to an HsPredTy by checkInstType
-- We don't allow a context, but that's sorted out by the type checker.
deriving :: { Located ([MaybeAnn]
                      ,Maybe [LHsType RdrName]) }
        : {- empty -}                     { noLoc ([],Nothing) }
        | 'deriving' qtycon               { let { L loc tv = $2 }
                                            in LL ([mj AnnDeriving $1]
                                                  ,Just [L loc (HsTyVar tv)]) }
        | 'deriving' '(' ')'              { LL ([mj AnnDeriving $1,mo $2,mc $3]
                                               ,Just []) }
        | 'deriving' '(' inst_types1 ')'  { LL ([mj AnnDeriving $1,mo $2,mc $4]
                                               ,Just $3) }
             -- Glasgow extension: allow partial
             -- applications in derivings

-----------------------------------------------------------------------------
-- Value definitions

{- Note [Declaration/signature overlap]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
There's an awkward overlap with a type signature.  Consider
        f :: Int -> Int = ...rhs...
   Then we can't tell whether it's a type signature or a value
   definition with a result signature until we see the '='.
   So we have to inline enough to postpone reductions until we know.
-}

{-
  ATTENTION: Dirty Hackery Ahead! If the second alternative of vars is var
  instead of qvar, we get another shift/reduce-conflict. Consider the
  following programs:

     { (^^) :: Int->Int ; }          Type signature; only var allowed

     { (^^) :: Int->Int = ... ; }    Value defn with result signature;
                                     qvar allowed (because of instance decls)

  We can't tell whether to reduce var to qvar until after we've read the signatures.
-}

docdecl :: { LHsDecl RdrName }
        : docdecld { L1 (DocD (unLoc $1)) }

docdecld :: { LDocDecl }
        : docnext                               { L1 (DocCommentNext (unLoc $1)) }
        | docprev                               { L1 (DocCommentPrev (unLoc $1)) }
        | docnamed                              { L1 (case (unLoc $1) of (n, doc) -> DocCommentNamed n doc) }
        | docsection                            { L1 (case (unLoc $1) of (n, doc) -> DocGroup n doc) }

decl_no_th :: { Located (OrdList (LHsDecl RdrName)) }
        : sigdecl               { $1 }

        | '!' aexp rhs          {% do { let { e = LL (SectionR (LL (HsVar bang_RDR)) $2) };
                                        pat <- checkPattern empty e;
                                        _ <- ams (LL ())
                                                (mj AnnBang $1:(fst $ unLoc $3));
                                        return $ LL $ unitOL $ LL $ ValD $
                                            PatBind pat (snd $ unLoc $3)
                                                    placeHolderType
                                                    placeHolderNames
                                                    (Nothing,[]) } }
                                -- Turn it all into an expression so that
                                -- checkPattern can check that bangs are enabled

        | infixexp opt_sig rhs  {% do { r <- checkValDef empty $1 (snd $2) $3;
                                        let { l = comb2 $1 $> };
                                        _ <- ams (LL ()) (fst $ unLoc $3);
                                        return $! (sL l (unitOL $! (sL l $ ValD r))) } }
        | pattern_synonym_decl  { LL $ unitOL $1 }
        | docdecl               { LL $ unitOL $1 }

decl    :: { Located (OrdList (LHsDecl RdrName)) }
        : decl_no_th            { $1 }

        -- Why do we only allow naked declaration splices in top-level
        -- declarations and not here? Short answer: because readFail009
        -- fails terribly with a panic in cvBindsAndSigs otherwise.
        | splice_exp            { LL $ unitOL (LL $ mkSpliceDecl $1) }

rhs     :: { Located ([MaybeAnn],GRHSs RdrName (LHsExpr RdrName)) }
        : '=' exp wherebinds    { sL (comb3 $1 $2 $3)
                                    ((mj AnnEqual $1 : (fst $ unLoc $3))
                                    ,GRHSs (unguardedRHS $2) (snd $ unLoc $3)) }
        | gdrhs wherebinds      { LL (fst $ unLoc $2
                                    ,GRHSs (reverse (unLoc $1))
                                                    (snd $ unLoc $2)) }

gdrhs :: { Located [LGRHS RdrName (LHsExpr RdrName)] }
        : gdrhs gdrh            { LL ($2 : unLoc $1) }
        | gdrh                  { L1 [$1] }

gdrh :: { LGRHS RdrName (LHsExpr RdrName) }
        : '|' guardquals '=' exp  {% ams (sL (comb2 $1 $>) $ GRHS (unLoc $2) $4)
                                         [mj AnnVbar $1,mj AnnEqual $3] }

sigdecl :: { Located (OrdList (LHsDecl RdrName)) }
        :
        -- See Note [Declaration/signature overlap] for why we need infixexp here
          infixexp '::' sigtypedoc
                        {% do s <- checkValSig $1 $3
                        ; _ <- ams (LL ()) [mj AnnDotdot $2]
                        ; return (LL $ unitOL (LL $ SigD s)) }
        | var ',' sig_vars '::' sigtypedoc
           {% ams (LL $ toOL [ LL $ SigD (TypeSig ($1:reverse (unLoc $3)) $5) ])
                  [mj AnnComma $2,mj AnnDotdot $4] }

        | infix prec ops        { LL $ toOL [ LL $ SigD (FixSig (FixitySig n (Fixity $2 (unLoc $1))))
                                            | n <- fromOL $ unLoc $3 ] }
        | '{-# INLINE' activation qvar '#-}'
                {% ams (LL $ unitOL (LL $ SigD (InlineSig $3
                                     (mkInlinePragma (getINLINE $1) (snd $2)))))
                       (mo $1:mc $4:fst $2) }

        -- AZ TODO: adjust hsSyn so all the SpecSig from a single SPECIALISE pragma is kept together
        | '{-# SPECIALISE' activation qvar '::' sigtypes1 '#-}'
             { let inl_prag = mkInlinePragma (EmptyInlineSpec, FunLike) (snd $2)
               in LL $ toOL [ LL $ SigD (SpecSig $3 t inl_prag)
                            | t <- fromOL $5] }

        | '{-# SPECIALISE_INLINE' activation qvar '::' sigtypes1 '#-}'
             { LL $ toOL [ LL $ SigD (SpecSig $3 t
                                  (mkInlinePragma (getSPEC_INLINE $1) (snd $2)))
                         | t <- fromOL $5] }

        | '{-# SPECIALISE' 'instance' inst_type '#-}'
                {% ams (LL $ unitOL (LL $ SigD (SpecInstSig $3)))
                       [mo $1,mj AnnInstance $2,mc $4] }

        -- AZ TODO: Do we need locations in the name_formula_opt?
        -- A minimal complete definition
        | '{-# MINIMAL' name_boolformula_opt '#-}'
                {% ams (LL $ unitOL (LL $ SigD (MinimalSig (snd $2))))
                       (mo $1:mc $3:fst $2) }

activation :: { ([MaybeAnn],Maybe Activation) }
        : {- empty -}                           { ([],Nothing) }
        | explicit_activation                   { (fst $1,Just (snd $1)) }

explicit_activation :: { ([MaybeAnn],Activation) }  -- In brackets
        : '[' INTEGER ']'       { ([mo $1,mj AnnVal $2,mc $3]
                                  ,ActiveAfter  (fromInteger (getINTEGER $2))) }
        | '[' '~' INTEGER ']'   { ([mo $1,mj AnnTilde $2,mj AnnVal $3
                                                 ,mc $4]
                                  ,ActiveBefore (fromInteger (getINTEGER $3))) }

-----------------------------------------------------------------------------
-- Expressions

quasiquote :: { Located (HsQuasiQuote RdrName) }
        : TH_QUASIQUOTE   { let { loc = getLoc $1
                                ; ITquasiQuote (quoter, quote, quoteSpan) = unLoc $1
                                ; quoterId = mkUnqual varName quoter }
                            in L1 (mkHsQuasiQuote quoterId (RealSrcSpan quoteSpan) quote) }
        | TH_QQUASIQUOTE  { let { loc = getLoc $1
                                ; ITqQuasiQuote (qual, quoter, quote, quoteSpan) = unLoc $1
                                ; quoterId = mkQual varName (qual, quoter) }
                            in sL (getLoc $1) (mkHsQuasiQuote quoterId (RealSrcSpan quoteSpan) quote) }

exp   :: { LHsExpr RdrName }
        : infixexp '::' sigtype {% ams (LL $ ExprWithTySig $1 $3)
                                       [mj AnnDotdot $2] }
        | infixexp '-<' exp     {% ams (LL $ HsArrApp $1 $3 placeHolderType
                                                        HsFirstOrderApp True)
                                       [mj Annlarrowtail $2] }
        | infixexp '>-' exp     {% ams (LL $ HsArrApp $3 $1 placeHolderType
                                                      HsFirstOrderApp False)
                                       [mj Annrarrowtail $2] }
        | infixexp '-<<' exp    {% ams (LL $ HsArrApp $1 $3 placeHolderType
                                                      HsHigherOrderApp True)
                                       [mj AnnLarrowtail $2] }
        | infixexp '>>-' exp    {% ams (LL $ HsArrApp $3 $1 placeHolderType
                                                      HsHigherOrderApp False)
                                       [mj AnnRarrowtail $2] }
        | infixexp              { $1 }

infixexp :: { LHsExpr RdrName }
        : exp10                       { $1 }
        | infixexp qop exp10          { LL (OpApp $1 $2 placeHolderFixity $3) }

exp10 :: { LHsExpr RdrName }
        : '\\' apat apats opt_asig '->' exp
                   {% ams (LL $ HsLam (mkMatchGroup FromSource
                            [LL $ Match ($2:$3) (snd $4) (unguardedGRHSs $6)]))
                          [mj AnnLam $1,mj AnnRarrow $5] }
        | 'let' binds 'in' exp          {% ams (LL $ HsLet (snd $ unLoc $2) $4)
                                               (mj AnnLet $1:mj AnnIn $3
                                                 :(fst $ unLoc $2)) }
        | '\\' 'lcase' altslist
            {% ams (LL $ HsLamCase placeHolderType
                                   (mkMatchGroup FromSource (snd $ unLoc $3)))
                   (mj AnnLam $1:(fst $ unLoc $3)) }
        | 'if' exp optSemi 'then' exp optSemi 'else' exp
                           {% checkDoAndIfThenElse $2 (snd $3) $5 (snd $6) $8 >>
                              ams (LL $ mkHsIf $2 $5 $8)
                                  (mj AnnIf $1:mj AnnThen $4
                                             :mj AnnElse $7
                                             :(fst $3)++(fst $6)) }
        | 'if' ifgdpats                 {% hintMultiWayIf (getLoc $1) >>
                                           ams (LL $ HsMultiIf
                                                      placeHolderType
                                                      (reverse $ unLoc $2))
                                               [mj AnnIf $1] }
        | 'case' exp 'of' altslist      {% ams (LL $ HsCase $2 (mkMatchGroup
                                                   FromSource (snd $ unLoc $4)))
                                               (mj AnnCase $1:mj AnnOf $3
                                                  :(fst $ unLoc $4)) }
        | '-' fexp                      {% ams (LL $ NegApp $2 noSyntaxExpr)
                                               [mj AnnMinus $1] }

        | 'do' stmtlist              {% ams (L (comb2 $1 $2)
                                               (mkHsDo DoExpr (snd $ unLoc $2)))
                                               (mj AnnDo $1:(fst $ unLoc $2)) }
        | 'mdo' stmtlist            {% ams (L (comb2 $1 $2)
                                              (mkHsDo MDoExpr (snd $ unLoc $2)))
                                           (mj AnnMdo $1:(fst $ unLoc $2)) }

        | scc_annot exp        {% do { on <- extension sccProfilingOn
                                     ; ams (LL $ if on
                                                  then HsSCC (snd $ unLoc $1) $2
                                                  else HsPar $2)
                                           (fst $ unLoc $1) } }

        | hpc_annot exp        {% do { on <- extension hpcEnabled
                                       ; ams (LL $ if on
                                                    then HsTickPragma
                                                             (snd $ unLoc $1) $2
                                                    else HsPar $2)
                                             (fst $ unLoc $1) } }

        | 'proc' aexp '->' exp
                       {% checkPattern empty $2 >>= \ p ->
                           checkCommand $4 >>= \ cmd ->
                           ams (LL $ HsProc p (LL $ HsCmdTop cmd placeHolderType
                                                placeHolderType []))
                                            -- TODO: is LL right here?
                               [mj AnnProc $1,mj AnnRarrow $3] }

        | '{-# CORE' STRING '#-}' exp  {% ams (LL $ HsCoreAnn (getSTRING $2) $4)
                                              [mo $1,mj AnnVal $2
                                              ,mc $3] }
                                          -- hdaume: core annotation
        | fexp                         { $1 }

optSemi :: { ([MaybeAnn],Bool) }
        : ';'         { ([mj AnnSemi $1],True) }
        | {- empty -} { ([],False) }

scc_annot :: { Located ([MaybeAnn],FastString) }
        : '{-# SCC' STRING '#-}'      {% do scc <- getSCC $2
                                            ; return $ LL
                                               ([mo $1,mj AnnVal $2
                                                ,mc $3],scc) }
        | '{-# SCC' VARID  '#-}'      { LL ([mo $1,mj AnnVal $2
                                         ,mc $3]
                                        ,(getVARID $2)) }

hpc_annot :: { Located ([MaybeAnn],(FastString,(Int,Int),(Int,Int))) }
        : '{-# GENERATED' STRING INTEGER ':' INTEGER '-' INTEGER ':' INTEGER '#-}'
                                      { LL $ ([mo $1,mj AnnVal $2
                                              ,mj AnnVal2 $3,mj AnnColon $4
                                              ,mj AnnVal3 $5,mj AnnMinus $6
                                              ,mj AnnVal4 $7,mj AnnColon2 $8
                                              ,mj AnnVal5 $9,mc $10]
                                              ,(getSTRING $2
                                               ,( fromInteger $ getINTEGER $3
                                                , fromInteger $ getINTEGER $5
                                                )
                                               ,( fromInteger $ getINTEGER $7
                                                , fromInteger $ getINTEGER $9
                                                )
                                               ))
                                         }

fexp    :: { LHsExpr RdrName }
        : fexp aexp                             { LL $ HsApp $1 $2 }
        | aexp                                  { $1 }

aexp    :: { LHsExpr RdrName }
        : qvar '@' aexp         {% ams (LL $ EAsPat $1 $3) [mj AnnAt $2] }
        | '~' aexp              {% ams (LL $ ELazyPat $2) [mj AnnTilde $1] }
        | aexp1                 { $1 }

aexp1   :: { LHsExpr RdrName }
        : aexp1 '{' fbinds '}' {% do { r <- mkRecConstrOrUpdate $1 (comb2 $2 $4)
                                                                   (snd $3)
                                     ; _ <- ams (LL ()) [mo $2,mc $4]
                                     ; checkRecordSyntax (LL r) }}
        | aexp2                { $1 }

aexp2   :: { LHsExpr RdrName }
        : ipvar                         { L1 (HsIPVar $! unLoc $1) }
        | qcname                        { L1 (HsVar   $! unLoc $1) }
        | literal                       { L1 (HsLit   $! unLoc $1) }
-- This will enable overloaded strings permanently.  Normally the renamer turns HsString
-- into HsOverLit when -foverloaded-strings is on.
--      | STRING     { sL (getLoc $1) (HsOverLit $! mkHsIsString
--                                        (getSTRING $1) placeHolderType) }
        | INTEGER    { sL (getLoc $1) (HsOverLit $! mkHsIntegral
                                          (getINTEGER $1) placeHolderType) }
        | RATIONAL   { sL (getLoc $1) (HsOverLit $! mkHsFractional
                                          (getRATIONAL $1) placeHolderType) }

        -- N.B.: sections get parsed by these next two productions.
        -- This allows you to write, e.g., '(+ 3, 4 -)', which isn't
        -- correct Haskell (you'd have to write '((+ 3), (4 -))')
        -- but the less cluttered version fell out of having texps.
        | '(' texp ')'                  {% ams (LL (HsPar $2)) [mo $1,mc $3] }
        | '(' tup_exprs ')'             {% ams (LL (ExplicitTuple $2 Boxed))
                                               [mo $1,mc $3] }

        | '(#' texp '#)'                {% ams (LL (ExplicitTuple [L (gl $2)
                                                         (Present $2)] Unboxed))
                                               [mo $1,mc $3] }
        | '(#' tup_exprs '#)'           {% ams (LL (ExplicitTuple $2 Unboxed))
                                               [mo $1,mc $3] }

        | '[' list ']'                  {% ams (LL (unLoc $2)) [mo $1,mc $3] }
        | '[:' parr ':]'                {% ams (LL (unLoc $2)) [mo $1,mc $3] }
        | '_'                           { L1 EWildPat }

        -- Template Haskell Extension
        | splice_exp            { $1 }

        | SIMPLEQUOTE  qvar     { LL $ HsBracket (VarBr True  (unLoc $2)) }
        | SIMPLEQUOTE  qcon     { LL $ HsBracket (VarBr True  (unLoc $2)) }
        | TH_TY_QUOTE tyvar     { LL $ HsBracket (VarBr False (unLoc $2)) }
        | TH_TY_QUOTE gtycon    { LL $ HsBracket (VarBr False (unLoc $2)) }
        | '[|' exp '|]'       {% ams (LL $ HsBracket (ExpBr $2)) [mo $1,mc $3] }
        | '[||' exp '||]'     {% ams (LL $ HsBracket (TExpBr $2)) [mo $1,mc $3]}
        | '[t|' ctype '|]'    {% ams (LL $ HsBracket (TypBr $2)) [mo $1,mc $3] }
        | '[p|' infixexp '|]' {% checkPattern empty $2 >>= \p ->
                                      ams (LL $ HsBracket (PatBr p))
                                          [mo $1,mc $3] }
        | '[d|' cvtopbody '|]' {% ams (LL $ HsBracket (DecBrL (snd $2)))
                                      (mo $1:mc $3:fst $2) }
        | quasiquote          { L1 (HsQuasiQuoteE (unLoc $1)) }

        -- arrow notation extension
        | '(|' aexp2 cmdargs '|)'  {% ams (LL $ HsArrForm $2
                                                           Nothing (reverse $3))
                                          [mo $1,mc $4] }

splice_exp :: { LHsExpr RdrName }
        : TH_ID_SPLICE          { L1 $ mkHsSpliceE 
                                        (L1 $ HsVar (mkUnqual varName 
                                                        (getTH_ID_SPLICE $1))) } 
        | '$(' exp ')'          {% ams (LL $ mkHsSpliceE $2) [mo $1,mc $3] }
        | TH_ID_TY_SPLICE       { L1 $ mkHsSpliceTE 
                                        (L1 $ HsVar (mkUnqual varName 
                                                     (getTH_ID_TY_SPLICE $1))) } 
        | '$$(' exp ')'         {% ams (LL $ mkHsSpliceTE $2) [mo $1,mc $3] }

cmdargs :: { [LHsCmdTop RdrName] }
        : cmdargs acmd                  { $2 : $1 }
        | {- empty -}                   { [] }

acmd    :: { LHsCmdTop RdrName }
        : aexp2                 {% checkCommand $1 >>= \ cmd ->
                                    return (L1 $ HsCmdTop cmd
                                           placeHolderType placeHolderType []) }

cvtopbody :: { ([MaybeAnn],[LHsDecl RdrName]) }
        :  '{'            cvtopdecls0 '}'               { ([mo $1,mc $3],$2) }
        |      vocurly    cvtopdecls0 close             { ([],$2) }

cvtopdecls0 :: { [LHsDecl RdrName] }
        : {- empty -}           { [] }
        | cvtopdecls            { $1 }

-----------------------------------------------------------------------------
-- Tuple expressions

-- "texp" is short for tuple expressions:
-- things that can appear unparenthesized as long as they're
-- inside parens or delimitted by commas
texp :: { LHsExpr RdrName }
        : exp                           { $1 }

        -- Note [Parsing sections]
        -- ~~~~~~~~~~~~~~~~~~~~~~~
        -- We include left and right sections here, which isn't
        -- technically right according to the Haskell standard.
        -- For example (3 +, True) isn't legal.
        -- However, we want to parse bang patterns like
        --      (!x, !y)
        -- and it's convenient to do so here as a section
        -- Then when converting expr to pattern we unravel it again
        -- Meanwhile, the renamer checks that real sections appear
        -- inside parens.
        | infixexp qop        { LL $ SectionL $1 $2 }
        | qopm infixexp       { LL $ SectionR $1 $2 }

       -- View patterns get parenthesized above
        | exp '->' texp   {% ams (LL $ EViewPat $1 $3) [mj AnnRarrow $2] }

-- Always at least one comma
tup_exprs :: { [LHsTupArg RdrName] }
           : texp commas_tup_tail 
                          {% do { addAnnotation (gl $1) AnnComma (fst $2)
                                ; return ((L (gl $1) (Present $1)) : snd $2) } }

           | commas tup_tail
                {% do { mapM_ (\ll -> addAnnotation (gl ll) AnnComma (gl ll)) $2
                      ; return
                           (let tt = if null $2
                                       then [noLoc missingTupArg]
                                       else $2
                            in map (\l -> L l missingTupArg) (fst $1) ++ tt) } }

-- Always starts with commas; always follows an expr
commas_tup_tail :: { (SrcSpan,[LHsTupArg RdrName]) }
commas_tup_tail : commas tup_tail
       {% do { mapM_ (\ll -> addAnnotation ll AnnComma ll) (tail $ fst $1)
             ; return (
         let tt = if null $2
                    then [L (last $ fst $1) missingTupArg]
                    else $2
         in (head $ fst $1
            ,(map (\l -> L l missingTupArg) (init $ fst $1)) ++ tt)) } }

-- Always follows a comma
tup_tail :: { [LHsTupArg RdrName] }
          : texp commas_tup_tail {% addAnnotation (gl $1) AnnComma (fst $2) >>
                                    return ((L (gl $1) (Present $1)) : snd $2) }
          | texp                 { [L (gl $1) (Present $1)] }
          | {- empty -}          { [] {- [noLoc missingTupArg] -} }

-----------------------------------------------------------------------------
-- List expressions

-- The rules below are little bit contorted to keep lexps left-recursive while
-- avoiding another shift/reduce-conflict.
list :: { LHsExpr RdrName }
        : texp    { L1 $ ExplicitList placeHolderType Nothing [$1] }
        | lexps   { L1 $ ExplicitList placeHolderType Nothing
                                                   (reverse (unLoc $1)) }
        | texp '..'             {% ams (LL $ ArithSeq noPostTcExpr Nothing
                                       (From $1)) [mj AnnDotdot $2] }
        | texp ',' exp '..'     {% ams (LL $ ArithSeq noPostTcExpr Nothing
                                                               (FromThen $1 $3))
                                       [mj AnnComma $2,mj AnnDotdot $4] }
        | texp '..' exp         {% ams (LL $ ArithSeq noPostTcExpr Nothing
                                       (FromTo $1 $3)) [mj AnnDotdot $2] }
        | texp ',' exp '..' exp {% ams (LL $ ArithSeq noPostTcExpr Nothing
                                       (FromThenTo $1 $3 $5))
                                       [mj AnnComma $2,mj AnnDotdot $4] }
        | texp '|' flattenedpquals
             {% checkMonadComp >>= \ ctxt ->
                ams (sL (comb2 $1 $>) $
                        mkHsComp ctxt (unLoc $3) $1)
                    [mj AnnVbar $2] }

lexps :: { Located [LHsExpr RdrName] }
        : lexps ',' texp          {% addAnnotation (gl $ head $ unLoc $1)
                                                            AnnComma (gl $2) >>
                                      return (LL (((:) $! $3) $! unLoc $1)) }
        | texp ',' texp            {% addAnnotation (gl $1) AnnComma (gl $2) >>
                                      return (LL [$3,$1]) }

-----------------------------------------------------------------------------
-- List Comprehensions

flattenedpquals :: { Located [LStmt RdrName (LHsExpr RdrName)] }
    : pquals   { case (unLoc $1) of
                    [qs] -> L1 qs
                    -- We just had one thing in our "parallel" list so
                    -- we simply return that thing directly

                    qss -> L1 [L1 $ ParStmt [ParStmtBlock qs [] noSyntaxExpr |
                                            qs <- qss]
                                            noSyntaxExpr noSyntaxExpr]
                    -- We actually found some actual parallel lists so
                    -- we wrap them into as a ParStmt
                }

pquals :: { Located [[LStmt RdrName (LHsExpr RdrName)]] }
    : squals '|' pquals     {% addAnnotation (gl $ last $ unLoc $1) AnnVbar (gl $2) >>
                               return (L (getLoc $2) (reverse (unLoc $1) : unLoc $3)) }
    | squals                { L (getLoc $1) [reverse (unLoc $1)] }

squals :: { Located [LStmt RdrName (LHsExpr RdrName)] }   -- In reverse order, because the last
                                        -- one can "grab" the earlier ones
    : squals ',' transformqual
             {% addAnnotation (gl $ last $ unLoc $1) AnnComma (gl $2) >>
                return (LL [L (getLoc $3) ((unLoc $3) (reverse (unLoc $1)))]) }
    | squals ',' qual
             {% addAnnotation (gl $ last $ unLoc $1) AnnComma (gl $2) >>
                return (LL ($3 : unLoc $1)) }
    | transformqual                       { LL [L (getLoc $1) ((unLoc $1) [])] }
    | qual                                { L1 [$1] }
--  | transformquals1 ',' '{|' pquals '|}'   { LL ($4 : unLoc $1) }
--  | '{|' pquals '|}'                       { L1 [$2] }


-- It is possible to enable bracketing (associating) qualifier lists
-- by uncommenting the lines with {| |} above. Due to a lack of
-- consensus on the syntax, this feature is not being used until we
-- get user demand.

transformqual :: { Located ([LStmt RdrName (LHsExpr RdrName)] -> Stmt RdrName (LHsExpr RdrName)) }
                        -- Function is applied to a list of stmts *in order*
    : 'then' exp               {% ams (LL $ \ss -> (mkTransformStmt ss $2))
                                      [mj AnnThen $1] }
    | 'then' exp 'by' exp      {% ams (LL $ \ss -> (mkTransformByStmt ss $2 $4))
                                      [mj AnnThen $1,mj AnnBy  $3] }
    | 'then' 'group' 'using' exp
             {% ams (LL $ \ss -> (mkGroupUsingStmt ss $4))
                    [mj AnnThen $1,mj AnnGroup $2,mj AnnUsing $3] }

    | 'then' 'group' 'by' exp 'using' exp
             {% ams (LL $ \ss -> (mkGroupByUsingStmt ss $4 $6))
                     [mj AnnThen $1,mj AnnGroup $2,mj AnnBy $3,mj AnnUsing $5] }

-- Note that 'group' is a special_id, which means that you can enable
-- TransformListComp while still using Data.List.group. However, this
-- introduces a shift/reduce conflict. Happy chooses to resolve the conflict
-- in by choosing the "group by" variant, which is what we want.

-----------------------------------------------------------------------------
-- Parallel array expressions

-- The rules below are little bit contorted; see the list case for details.
-- Note that, in contrast to lists, we only have finite arithmetic sequences.
-- Moreover, we allow explicit arrays with no element (represented by the nil
-- constructor in the list case).

parr :: { LHsExpr RdrName }
        :                      { noLoc (ExplicitPArr placeHolderType []) }
        | texp                 { L1 $ ExplicitPArr placeHolderType [$1] }
        | lexps                { L1 $ ExplicitPArr placeHolderType
                                                          (reverse (unLoc $1)) }
        | texp '..' exp        {% ams (LL $ PArrSeq noPostTcExpr (FromTo $1 $3))
                                      [mj AnnDotdot $2] }
        | texp ',' exp '..' exp
                        {% ams (LL $ PArrSeq noPostTcExpr (FromThenTo $1 $3 $5))
                               [mj AnnComma $2,mj AnnDotdot $4] }
        | texp '|' flattenedpquals
                        {% ams (LL $ mkHsComp PArrComp (unLoc $3) $1)
                               [mj AnnVbar $2] }

-- We are reusing `lexps' and `flattenedpquals' from the list case.

-----------------------------------------------------------------------------
-- Guards

guardquals :: { Located [LStmt RdrName (LHsExpr RdrName)] }
    : guardquals1           { L (getLoc $1) (reverse (unLoc $1)) }

guardquals1 :: { Located [LStmt RdrName (LHsExpr RdrName)] }
    : guardquals1 ',' qual  {% ams (LL ($3 : unLoc $1)) [mj AnnComma $2] }
    | qual                  { L1 [$1] }

-----------------------------------------------------------------------------
-- Case alternatives

altslist :: { Located ([MaybeAnn],[LMatch RdrName (LHsExpr RdrName)]) }
        : '{'            alts '}'    { LL ([mo $1,mc $3],(reverse (unLoc $2))) }

        |     vocurly    alts  close { L (getLoc $2) ([],(reverse (unLoc $2))) }
        | '{'                 '}'    { noLoc ([mo $1,mc $2],[]) }
        |     vocurly          close { noLoc ([],[]) }

alts    :: { Located [LMatch RdrName (LHsExpr RdrName)] }
        : alts1                         { L1 (unLoc $1) }
        | ';' alts                      {% ams (LL (unLoc $2))
                                               [mj AnnSemi (head $ unLoc $2)] }

alts1   :: { Located [LMatch RdrName (LHsExpr RdrName)] }
        : alts1 ';' alt           {% ams (LL ($3 : unLoc $1)) [mj AnnSemi $3] }
        | alts1 ';'               {% ams (LL (unLoc $1))
                                         [mj AnnSemi (last $ unLoc $1)] }
        | alt                     { L1 [$1] }

alt     :: { LMatch RdrName (LHsExpr RdrName) }
        : pat opt_sig alt_rhs           { LL (Match [$1] (snd $2) (unLoc $3)) }

alt_rhs :: { Located (GRHSs RdrName (LHsExpr RdrName)) }
        : ralt wherebinds             { LL (GRHSs (unLoc $1) (snd $ unLoc $2)) }

ralt :: { Located [LGRHS RdrName (LHsExpr RdrName)] }
        : '->' exp             {% ams (LL (unguardedRHS $2)) [mj AnnRarrow $1] }
        | gdpats               { L1 (reverse (unLoc $1)) }

gdpats :: { Located [LGRHS RdrName (LHsExpr RdrName)] }
        : gdpats gdpat                  { LL ($2 : unLoc $1) }
        | gdpat                         { L1 [$1] }

-- optional semi-colons between the guards of a MultiWayIf, because we use
-- layout here, but we don't need (or want) the semicolon as a separator (#7783).
gdpatssemi :: { Located [LGRHS RdrName (LHsExpr RdrName)] }
        : gdpatssemi gdpat optSemi     {% ams (sL (comb2 $1 $2) ($2 : unLoc $1))
                                              (fst $3) }
        | gdpat optSemi                 {% ams (L1 [$1]) (fst $2) }

-- layout for MultiWayIf doesn't begin with an open brace, because it's hard to
-- generate the open brace in addition to the vertical bar in the lexer, and
-- we don't need it.
ifgdpats :: { Located [LGRHS RdrName (LHsExpr RdrName)] }
         : '{' gdpatssemi '}'             {% ams (LL (unLoc $2)) [mo $1,mc $3] }
         |     gdpatssemi close           { $1 }

gdpat   :: { LGRHS RdrName (LHsExpr RdrName) }
        : '|' guardquals '->' exp
                                  {% ams (sL (comb2 $1 $>) $ GRHS (unLoc $2) $4)
                                         [mj AnnVbar $1,mj AnnRarrow $3] }

-- 'pat' recognises a pattern, including one with a bang at the top
--      e.g.  "!x" or "!(x,y)" or "C a b" etc
-- Bangs inside are parsed as infix operator applications, so that
-- we parse them right when bang-patterns are off
pat     :: { LPat RdrName }
pat     :  exp          {% checkPattern empty $1 }
        | '!' aexp      {% amms (checkPattern empty (LL (SectionR
                                                     (L1 (HsVar bang_RDR)) $2)))
                                [mj AnnBang $1] }

bindpat :: { LPat RdrName }
bindpat :  exp            {% checkPattern
                                 (text "Possibly caused by a missing 'do'?") $1 }
        | '!' aexp        {% amms (checkPattern
                                     (text "Possibly caused by a missing 'do'?")
                                     (LL (SectionR (L1 (HsVar bang_RDR)) $2)))
                                  [mj AnnBang $1] }

apat   :: { LPat RdrName }
apat    : aexp                  {% checkPattern empty $1 }
        | '!' aexp              {% amms (checkPattern empty
                                            (LL (SectionR
                                                (L1 (HsVar bang_RDR)) $2)))
                                        [mj AnnBang $1] }

apats  :: { [LPat RdrName] }
        : apat apats            { $1 : $2 }
        | {- empty -}           { [] }

-----------------------------------------------------------------------------
-- Statement sequences

stmtlist :: { Located ([MaybeAnn],[LStmt RdrName (LHsExpr RdrName)]) }
        : '{'           stmts '}'       { LL ((mo $1:mc $3:(fst $ unLoc $2))
                                             ,(snd $ unLoc $2)) }
        |     vocurly   stmts close     { L (gl $2) (fst $ unLoc $2
                                                    ,snd $ unLoc $2) }

--      do { ;; s ; s ; ; s ;; }
-- The last Stmt should be an expression, but that's hard to enforce
-- here, because we need too much lookahead if we see do { e ; }
-- So we use BodyStmts throughout, and switch the last one over
-- in ParseUtils.checkDo instead
stmts :: { Located ([MaybeAnn],[LStmt RdrName (LHsExpr RdrName)]) }
        : stmt stmts_help        { LL (fst $ unLoc $2,($1 : (snd $ unLoc $2))) }
        | ';' stmts     {% if null (snd $ unLoc $2)
                             then ams (LL ([mj AnnSemi $1],snd $ unLoc $2)) []
                             else ams (LL ([],snd $ unLoc $2)) [mj AnnSemi $1] }

        | {- empty -}            { noLoc ([],[]) }

stmts_help :: { Located ([MaybeAnn],[LStmt RdrName (LHsExpr RdrName)]) }
                                                               -- might be empty
        : ';' stmts    {% if null (snd $ unLoc $2)
                             then ams (LL ([mj AnnSemi $1],snd $ unLoc $2)) []
                             else ams (LL ([],snd $ unLoc $2)) [mj AnnSemi $1] }

        | {- empty -}                   { noLoc ([],[]) }

-- For typing stmts at the GHCi prompt, where
-- the input may consist of just comments.
maybe_stmt :: { Maybe (LStmt RdrName (LHsExpr RdrName)) }
        : stmt                          { Just $1 }
        | {- nothing -}                 { Nothing }

stmt  :: { LStmt RdrName (LHsExpr RdrName) }
        : qual                          { $1 }
        | 'rec' stmtlist                {% ams (LL $ mkRecStmt (snd $ unLoc $2))
                                               [mj AnnRec $1] }

qual  :: { LStmt RdrName (LHsExpr RdrName) }
    : bindpat '<-' exp                  {% ams (LL $ mkBindStmt $1 $3)
                                               [mj AnnLarrow $2] }
    | exp                               { L1 $ mkBodyStmt $1 }
    | 'let' binds                       {% ams (LL $ LetStmt (snd $ unLoc $2))
                                               [mj AnnLet $1] }

-----------------------------------------------------------------------------
-- Record Field Update/Construction

fbinds  :: { ([MaybeAnn],([LHsRecField RdrName (LHsExpr RdrName)], Bool)) }
        : fbinds1                       { $1 }
        | {- empty -}                   { ([],([], False)) }

fbinds1 :: { ([MaybeAnn],([LHsRecField RdrName (LHsExpr RdrName)], Bool)) }
        : fbind ',' fbinds1
                {% addAnnotation (gl $1) AnnComma (gl $2) >>
                   return (case $3 of (ma,(flds, dd)) -> (ma,($1 : flds, dd))) }
        | fbind                         { ([],([$1], False)) }
        | '..'                          { ([mj AnnDotdot $1],([],   True)) }

fbind   :: { LHsRecField RdrName (LHsExpr RdrName) }
        : qvar '=' texp {% ams  (LL $ HsRecField $1 $3                False)
                                [mj AnnEqual $2] }
                        -- RHS is a 'texp', allowing view patterns (Trac #6038)
                        -- and, incidentaly, sections.  Eg
                        -- f (R { x = show -> s }) = ...

        | qvar          { LL $ HsRecField $1 placeHolderPunRhs True }
                        -- In the punning case, use a place-holder
                        -- The renamer fills in the final value

-----------------------------------------------------------------------------
-- Implicit Parameter Bindings

dbinds  :: { Located [LIPBind RdrName] }
        : dbinds ';' dbind
                      {% addAnnotation (gl $ last $ unLoc $1) AnnSemi (gl $2) >>
                         return (let { this = $3; rest = unLoc $1 }
                              in rest `seq` this `seq` LL (this : rest)) }
        | dbinds ';'  {% addAnnotation (gl $ last $ unLoc $1) AnnSemi (gl $2) >>
                         return (LL (unLoc $1)) }
        | dbind                        { let this = $1 in this `seq` L1 [this] }
--      | {- empty -}                  { [] }

dbind   :: { LIPBind RdrName }
dbind   : ipvar '=' exp                {% ams (LL (IPBind (Left (unLoc $1)) $3))
                                              [mj AnnEqual $2] }

ipvar   :: { Located HsIPName }
        : IPDUPVARID            { L1 (HsIPName (getIPDUPVARID $1)) }

-----------------------------------------------------------------------------
-- Warnings and deprecations

name_boolformula_opt :: { ([MaybeAnn],BooleanFormula (Located RdrName)) }
        : name_boolformula          { $1 }
        | {- empty -}               { ([],mkTrue) }

name_boolformula :: { ([MaybeAnn],BooleanFormula (Located RdrName)) }
        : name_boolformula_and                      { $1 }
        | name_boolformula_and '|' name_boolformula { ((mj AnnVbar $2:fst $1)++(fst $3)
                                                      ,mkOr [snd $1,snd $3]) }

name_boolformula_and :: { ([MaybeAnn],BooleanFormula (Located RdrName)) }
        : name_boolformula_atom                             { $1 }
        | name_boolformula_atom ',' name_boolformula_and
                  { ((mj AnnComma $2:fst $1)++(fst $3), mkAnd [snd $1,snd $3]) }

name_boolformula_atom :: { ([MaybeAnn],BooleanFormula (Located RdrName)) }
        : '(' name_boolformula ')'  { ([mo $1,mc $3],snd $2) }
        | name_var                  { ([],mkVar $1) }

-- AZ TODO: warnings/deprecations are incompletely annotated
namelist :: { Located [RdrName] }
namelist : name_var              { L1 [unLoc $1] }
         | name_var ',' namelist { LL (unLoc $1 : unLoc $3) }

name_var :: { Located RdrName }
name_var : var { $1 }
         | con { $1 }

-----------------------------------------
-- Data constructors
qcon    :: { Located RdrName }
        : qconid                { $1 }
        | '(' qconsym ')'       {% ams (LL (unLoc $2)) [mo $1,mc $3] }
        | sysdcon               { L1 $ nameRdrName (dataConName (unLoc $1)) }
-- The case of '[:' ':]' is part of the production `parr'

con     :: { Located RdrName }
        : conid                 { $1 }
        | '(' consym ')'        {% ams (LL (unLoc $2)) [mo $1,mc $3] }
        | sysdcon               { L1 $ nameRdrName (dataConName (unLoc $1)) }

con_list :: { Located [Located RdrName] }
con_list : con                  { L1 [$1] }
         | con ',' con_list     {% ams (LL ($1 : unLoc $3)) [mj AnnComma $2] }

sysdcon :: { Located DataCon }  -- Wired in data constructors
        : '(' ')'               {% ams (LL unitDataCon) [mo $1,mc $2] }
        | '(' commas ')'        {% ams (LL $ tupleCon BoxedTuple (snd $2 + 1))
                                       [mo $1,mc $3] }
        | '(#' '#)'             {% ams (LL $ unboxedUnitDataCon) [mo $1,mc $2] }
        | '(#' commas '#)'      {% ams (LL $ tupleCon UnboxedTuple (snd $2 + 1))
                                       [mo $1,mc $3] }
        | '[' ']'               {% ams (LL nilDataCon) [mo $1,mc $2] }

conop :: { Located RdrName }
        : consym                { $1 }
        | '`' conid '`'         {% ams (LL (unLoc $2)) [mo $1,mc $3] }

qconop :: { Located RdrName }
        : qconsym               { $1 }
        | '`' qconid '`'        {% ams (LL (unLoc $2)) [mo $1,mc $3] }

----------------------------------------------------------------------------
-- Type constructors


-- See Note [Unit tuples] in HsTypes for the distinction
-- between gtycon and ntgtycon
gtycon :: { Located RdrName }  -- A "general" qualified tycon, including unit tuples
        : ntgtycon                     { $1 }
        | '(' ')'                      {% ams (LL $ getRdrName unitTyCon)
                                              [mo $1,mc $2] }
        | '(#' '#)'                    {% ams (LL $ getRdrName unboxedUnitTyCon)
                                              [mo $1,mc $2] }

ntgtycon :: { Located RdrName }  -- A "general" qualified tycon, excluding unit tuples
        : oqtycon               { $1 }
        | '(' commas ')'        {% ams (LL $ getRdrName (tupleTyCon BoxedTuple
                                                        (snd $2 + 1)))
                                       (mo $1:mc $3:(mcommas (fst $2))) }
        | '(#' commas '#)'      {% ams (LL $ getRdrName (tupleTyCon UnboxedTuple
                                                        (snd $2 + 1)))
                                       (mo $1:mc $3:(mcommas (fst $2))) }
        | '(' '->' ')'          {% ams (LL $ getRdrName funTyCon)
                                       [mo $1,mj AnnRarrow $2,mc $3] }
        | '[' ']'               {% ams (LL $ listTyCon_RDR) [mo $1,mc $2] }
        | '[:' ':]'             {% ams (LL $ parrTyCon_RDR) [mo $1,mc $2] }
        | '(' '~#' ')'          {% ams (LL $ getRdrName eqPrimTyCon)
                                        [mo $1,mj AnnTildehsh $2,mc $3] }

oqtycon :: { Located RdrName }  -- An "ordinary" qualified tycon;
                                -- These can appear in export lists
        : qtycon                        { $1 }
        | '(' qtyconsym ')'             {% ams (LL (unLoc $2)) [mo $1,mc $3] }
        | '(' '~' ')'                   {% ams (LL $ eqTyCon_RDR)
                                               [mo $1,mj AnnTilde $2,mc $3] }

qtyconop :: { Located RdrName } -- Qualified or unqualified
        : qtyconsym                     { $1 }
        | '`' qtycon '`'                {% ams (LL (unLoc $2)) [mo $1,mc $3] }

qtycon :: { Located RdrName }   -- Qualified or unqualified
        : QCONID                        { L1 $! mkQual tcClsName (getQCONID $1) }
        | PREFIXQCONSYM                 { L1 $! mkQual tcClsName (getPREFIXQCONSYM $1) }
        | tycon                         { $1 }

tycon   :: { Located RdrName }  -- Unqualified
        : CONID                         { L1 $! mkUnqual tcClsName (getCONID $1) }

qtyconsym :: { Located RdrName }
        : QCONSYM                       { L1 $! mkQual tcClsName (getQCONSYM $1) }
        | QVARSYM                       { L1 $! mkQual tcClsName (getQVARSYM $1) }
        | tyconsym                      { $1 }

-- Does not include "!", because that is used for strictness marks
--               or ".", because that separates the quantified type vars from the rest
tyconsym :: { Located RdrName }
        : CONSYM                        { L1 $! mkUnqual tcClsName (getCONSYM $1) }
        | VARSYM                        { L1 $! mkUnqual tcClsName (getVARSYM $1) }
        | '*'                           { L1 $! mkUnqual tcClsName (fsLit "*")    }
        | '-'                           { L1 $! mkUnqual tcClsName (fsLit "-")    }


-----------------------------------------------------------------------------
-- Operators

op      :: { Located RdrName }   -- used in infix decls
        : varop                 { $1 }
        | conop                 { $1 }

varop   :: { Located RdrName }
        : varsym                { $1 }
        | '`' varid '`'         {% ams (LL (unLoc $2)) [mo $1,mc $3] }

qop     :: { LHsExpr RdrName }   -- used in sections
        : qvarop                { L1 $ HsVar (unLoc $1) }
        | qconop                { L1 $ HsVar (unLoc $1) }

qopm    :: { LHsExpr RdrName }   -- used in sections
        : qvaropm               { L1 $ HsVar (unLoc $1) }
        | qconop                { L1 $ HsVar (unLoc $1) }

qvarop :: { Located RdrName }
        : qvarsym               { $1 }
        | '`' qvarid '`'        {% ams (LL (unLoc $2)) [mo $1,mc $3] }

qvaropm :: { Located RdrName }
        : qvarsym_no_minus      { $1 }
        | '`' qvarid '`'        {% ams (LL (unLoc $2)) [mo $1,mc $3] }

-----------------------------------------------------------------------------
-- Type variables

tyvar   :: { Located RdrName }
tyvar   : tyvarid               { $1 }

tyvarop :: { Located RdrName }
tyvarop : '`' tyvarid '`'       {% ams (LL (unLoc $2)) [mo $1,mc $3] }
        | '.'                   {% parseErrorSDoc (getLoc $1)
                                      (vcat [ptext (sLit "Illegal symbol '.' in type"),
                                             ptext (sLit "Perhaps you intended to use RankNTypes or a similar language"),
                                             ptext (sLit "extension to enable explicit-forall syntax: forall <tvs>. <type>")])
                                }

tyvarid :: { Located RdrName }
        : VARID                 { L1 $! mkUnqual tvName (getVARID $1) }
        | special_id            { L1 $! mkUnqual tvName (unLoc $1) }
        | 'unsafe'              { L1 $! mkUnqual tvName (fsLit "unsafe") }
        | 'safe'                { L1 $! mkUnqual tvName (fsLit "safe") }
        | 'interruptible'       { L1 $! mkUnqual tvName (fsLit "interruptible") }

-----------------------------------------------------------------------------
-- Variables

var     :: { Located RdrName }
        : varid                 { $1 }
        | '(' varsym ')'        {% ams (LL (unLoc $2)) [mo $1,mc $3] }

qvar    :: { Located RdrName }
        : qvarid                { $1 }
        | '(' varsym ')'        {% ams (LL (unLoc $2)) [mo $1,mc $3] }
        | '(' qvarsym1 ')'      {% ams (LL (unLoc $2)) [mo $1,mc $3] }
-- We've inlined qvarsym here so that the decision about
-- whether it's a qvar or a var can be postponed until
-- *after* we see the close paren.

qvarid :: { Located RdrName }
        : varid                 { $1 }
        | QVARID                { L1 $! mkQual varName (getQVARID $1) }
        | PREFIXQVARSYM         { L1 $! mkQual varName (getPREFIXQVARSYM $1) }

-- Note that 'role' and 'family' get lexed separately regardless of
-- the use of extensions. However, because they are listed here, this
-- is OK and they can be used as normal varids.
varid :: { Located RdrName }
        : VARID                 { L1 $! mkUnqual varName (getVARID $1) }
        | special_id            { L1 $! mkUnqual varName (unLoc $1) }
        | 'unsafe'              { L1 $! mkUnqual varName (fsLit "unsafe") }
        | 'safe'                { L1 $! mkUnqual varName (fsLit "safe") }
        | 'interruptible'       { L1 $! mkUnqual varName (fsLit "interruptible") }
        | 'forall'              { L1 $! mkUnqual varName (fsLit "forall") }
        | 'family'              { L1 $! mkUnqual varName (fsLit "family") }
        | 'role'                { L1 $! mkUnqual varName (fsLit "role") }

qvarsym :: { Located RdrName }
        : varsym                { $1 }
        | qvarsym1              { $1 }

qvarsym_no_minus :: { Located RdrName }
        : varsym_no_minus       { $1 }
        | qvarsym1              { $1 }

qvarsym1 :: { Located RdrName }
qvarsym1 : QVARSYM              { L1 $ mkQual varName (getQVARSYM $1) }

varsym :: { Located RdrName }
        : varsym_no_minus       { $1 }
        | '-'                   { L1 $ mkUnqual varName (fsLit "-") }

varsym_no_minus :: { Located RdrName } -- varsym not including '-'
        : VARSYM                { L1 $ mkUnqual varName (getVARSYM $1) }
        | special_sym           { L1 $ mkUnqual varName (unLoc $1) }


-- These special_ids are treated as keywords in various places,
-- but as ordinary ids elsewhere.   'special_id' collects all these
-- except 'unsafe', 'interruptible', 'forall', 'family', and 'role',
-- whose treatment differs depending on context
special_id :: { Located FastString }
special_id
        : 'as'                  { L1 (fsLit "as") }
        | 'qualified'           { L1 (fsLit "qualified") }
        | 'hiding'              { L1 (fsLit "hiding") }
        | 'export'              { L1 (fsLit "export") }
        | 'label'               { L1 (fsLit "label")  }
        | 'dynamic'             { L1 (fsLit "dynamic") }
        | 'stdcall'             { L1 (fsLit "stdcall") }
        | 'ccall'               { L1 (fsLit "ccall") }
        | 'capi'                { L1 (fsLit "capi") }
        | 'prim'                { L1 (fsLit "prim") }
        | 'javascript'          { L1 (fsLit "javascript") }
        | 'group'               { L1 (fsLit "group") }

special_sym :: { Located FastString }
special_sym : '!'       { L1 (fsLit "!") }
            | '.'       { L1 (fsLit ".") }
            | '*'       { L1 (fsLit "*") }

-----------------------------------------------------------------------------
-- Data constructors

qconid :: { Located RdrName }   -- Qualified or unqualified
        : conid                 { $1 }
        | QCONID                { L1 $! mkQual dataName (getQCONID $1) }
        | PREFIXQCONSYM         { L1 $! mkQual dataName (getPREFIXQCONSYM $1) }

conid   :: { Located RdrName }
        : CONID                 { L1 $ mkUnqual dataName (getCONID $1) }

qconsym :: { Located RdrName }  -- Qualified or unqualified
        : consym                { $1 }
        | QCONSYM               { L1 $ mkQual dataName (getQCONSYM $1) }

consym :: { Located RdrName }
        : CONSYM                { L1 $ mkUnqual dataName (getCONSYM $1) }

        -- ':' means only list cons
        | ':'                   { L1 $ consDataCon_RDR }


-----------------------------------------------------------------------------
-- Literals

literal :: { Located HsLit }
        : CHAR                  { L1 $ HsChar       $ getCHAR $1 }
        | STRING                { L1 $ HsString     $ getSTRING $1 }
        | PRIMINTEGER           { L1 $ HsIntPrim    $ getPRIMINTEGER $1 }
        | PRIMWORD              { L1 $ HsWordPrim    $ getPRIMWORD $1 }
        | PRIMCHAR              { L1 $ HsCharPrim   $ getPRIMCHAR $1 }
        | PRIMSTRING            { L1 $ HsStringPrim $ getPRIMSTRING $1 }
        | PRIMFLOAT             { L1 $ HsFloatPrim  $ getPRIMFLOAT $1 }
        | PRIMDOUBLE            { L1 $ HsDoublePrim $ getPRIMDOUBLE $1 }

-----------------------------------------------------------------------------
-- Layout

close :: { () }
        : vccurly               { () } -- context popped in lexer.
        | error                 {% popContext }

-----------------------------------------------------------------------------
-- Miscellaneous (mostly renamings)

modid   :: { Located ModuleName }
        : CONID                 { L1 $ mkModuleNameFS (getCONID $1) }
        | QCONID                { L1 $ let (mod,c) = getQCONID $1 in
                                  mkModuleNameFS
                                   (mkFastString
                                     (unpackFS mod ++ '.':unpackFS c))
                                }

commas :: { ([SrcSpan],Int) }   -- One or more commas
        : commas ','             { ((fst $1)++[gl $2],snd $1 + 1) }
        | ','                    { ([gl $1],1) }

-----------------------------------------------------------------------------
-- Documentation comments

docnext :: { LHsDocString }
  : DOCNEXT {% return (L1 (HsDocString (mkFastString (getDOCNEXT $1)))) }

docprev :: { LHsDocString }
  : DOCPREV {% return (L1 (HsDocString (mkFastString (getDOCPREV $1)))) }

docnamed :: { Located (String, HsDocString) }
  : DOCNAMED {%
      let string = getDOCNAMED $1
          (name, rest) = break isSpace string
      in return (L1 (name, HsDocString (mkFastString rest))) }

docsection :: { Located (Int, HsDocString) }
  : DOCSECTION {% let (n, doc) = getDOCSECTION $1 in
        return (L1 (n, HsDocString (mkFastString doc))) }

moduleheader :: { Maybe LHsDocString }
        : DOCNEXT {% let string = getDOCNEXT $1 in
                     return (Just (L1 (HsDocString (mkFastString string)))) }

maybe_docprev :: { Maybe LHsDocString }
        : docprev                       { Just $1 }
        | {- empty -}                   { Nothing }

maybe_docnext :: { Maybe LHsDocString }
        : docnext                       { Just $1 }
        | {- empty -}                   { Nothing }

{
happyError :: P a
happyError = srcParseFail

getVARID        (L _ (ITvarid    x)) = x
getCONID        (L _ (ITconid    x)) = x
getVARSYM       (L _ (ITvarsym   x)) = x
getCONSYM       (L _ (ITconsym   x)) = x
getQVARID       (L _ (ITqvarid   x)) = x
getQCONID       (L _ (ITqconid   x)) = x
getQVARSYM      (L _ (ITqvarsym  x)) = x
getQCONSYM      (L _ (ITqconsym  x)) = x
getPREFIXQVARSYM (L _ (ITprefixqvarsym  x)) = x
getPREFIXQCONSYM (L _ (ITprefixqconsym  x)) = x
getIPDUPVARID   (L _ (ITdupipvarid   x)) = x
getCHAR         (L _ (ITchar     x)) = x
getSTRING       (L _ (ITstring   x)) = x
getINTEGER      (L _ (ITinteger  x)) = x
getRATIONAL     (L _ (ITrational x)) = x
getPRIMCHAR     (L _ (ITprimchar   x)) = x
getPRIMSTRING   (L _ (ITprimstring x)) = x
getPRIMINTEGER  (L _ (ITprimint    x)) = x
getPRIMWORD     (L _ (ITprimword x)) = x
getPRIMFLOAT    (L _ (ITprimfloat  x)) = x
getPRIMDOUBLE   (L _ (ITprimdouble x)) = x
getTH_ID_SPLICE (L _ (ITidEscape x)) = x
getTH_ID_TY_SPLICE (L _ (ITidTyEscape x)) = x
getINLINE       (L _ (ITinline_prag inl conl)) = (inl,conl)
getSPEC_INLINE  (L _ (ITspec_inline_prag True))  = (Inline,  FunLike)
getSPEC_INLINE  (L _ (ITspec_inline_prag False)) = (NoInline,FunLike)

getDOCNEXT (L _ (ITdocCommentNext x)) = x
getDOCPREV (L _ (ITdocCommentPrev x)) = x
getDOCNAMED (L _ (ITdocCommentNamed x)) = x
getDOCSECTION (L _ (ITdocSection n x)) = (n, x)

getSCC :: Located Token -> P FastString
getSCC lt = do let s = getSTRING lt
                   err = "Spaces are not allowed in SCCs"
               -- We probably actually want to be more restrictive than this
               if ' ' `elem` unpackFS s
                   then failSpanMsgP (getLoc lt) (text err)
                   else return s

-- Utilities for combining source spans
comb2 :: Located a -> Located b -> SrcSpan
comb2 a b = a `seq` b `seq` combineLocs a b

comb3 :: Located a -> Located b -> Located c -> SrcSpan
comb3 a b c = a `seq` b `seq` c `seq`
    combineSrcSpans (getLoc a) (combineSrcSpans (getLoc b) (getLoc c))

comb4 :: Located a -> Located b -> Located c -> Located d -> SrcSpan
comb4 a b c d = a `seq` b `seq` c `seq` d `seq`
    (combineSrcSpans (getLoc a) $ combineSrcSpans (getLoc b) $
                combineSrcSpans (getLoc c) (getLoc d))

-- strict constructor version:
{-# INLINE sL #-}
sL :: SrcSpan -> a -> Located a
sL span a = span `seq` a `seq` L span a

-- Make a source location for the file.  We're a bit lazy here and just
-- make a point SrcSpan at line 1, column 0.  Strictly speaking we should
-- try to find the span of the whole file (ToDo).
fileSrcSpan :: P SrcSpan
fileSrcSpan = do
  l <- getSrcLoc;
  let loc = mkSrcLoc (srcLocFile l) 1 1;
  return (mkSrcSpan loc loc)

-- Hint about the MultiWayIf extension
hintMultiWayIf :: SrcSpan -> P ()
hintMultiWayIf span = do
  mwiEnabled <- liftM ((Opt_MultiWayIf `xopt`) . dflags) getPState
  unless mwiEnabled $ parseErrorSDoc span $
    text "Multi-way if-expressions need MultiWayIf turned on"

-- Hint about explicit-forall, assuming UnicodeSyntax is on
hintExplicitForall :: SrcSpan -> P ()
hintExplicitForall span = do
    forall      <- extension explicitForallEnabled
    rulePrag    <- extension inRulePrag
    unless (forall || rulePrag) $ parseErrorSDoc span $ vcat
      [ text "Illegal symbol '\x2200' in type" -- U+2200 FOR ALL
      , text "Perhaps you intended to use RankNTypes or a similar language"
      , text "extension to enable explicit-forall syntax: \x2200 <tvs>. <type>"
      ]

{-
%************************************************************************
%*                                                                      *
        Helper functions for generating annotations in the parser
%*                                                                      *
%************************************************************************
-}

type MaybeAnn = Maybe (SrcSpan -> P ())

gl = getLoc
gj x = Just (gl x)

aa :: Located a -> (Ann,Located c) -> P (Located a)
aa a@(L l _) (b,s) = addAnnotation l b (gl s) >> return a

am a (b,s) = do
  av@(L l _) <- a
  addAnnotation l b (gl s)
  return av

ams :: Located a -> [MaybeAnn] -> P (Located a)
ams a@(L l _) bs = (mapM_ (\a -> a l) $ catMaybes bs) >> return a

amms :: P (Located a) -> [MaybeAnn] -> P (Located a)
amms a bs = do
  av@(L l _) <- a
  (mapM_ (\a -> a l) $ catMaybes bs) >> return av

amsu :: Located a -> [MaybeAnn] -> P (OrdList (Located a))
amsu a@(L l _) bs = (mapM_ (\a -> a l) $ catMaybes bs) >> return (unitOL a)

mj :: Ann -> Located e -> Maybe (SrcSpan -> P ())
mj a l = Just (\s -> addAnnotation s a (gl l))

mo ll = mj AnnOpen ll
mc ll = mj AnnClose ll

mcommas :: [SrcSpan] -> [MaybeAnn]
mcommas ss = map (\s -> mj AnnComma (L s ())) ss

ajl a@(L _ (Just (L l _))) b s = addAnnotation l b s >> return a

ajs a@(Just (L l _)) bs = (mapM_ (\a -> a l) $ catMaybes bs) >> return a

-- |Get the location of the last element of a OrdList, or noLoc
oll :: OrdList (Located a) -> SrcSpan
oll l = case fromOL l of
         [] -> noSrcSpan
         xs -> getLoc (last xs)
}
