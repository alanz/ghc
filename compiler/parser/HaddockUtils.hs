
module HaddockUtils where

import HsSyn
import SrcLoc
import Outputable (panic)

import Control.Monad

-- -----------------------------------------------------------------------------
-- Adding documentation to record fields (used in parsing).

addFieldDoc :: ConDeclField a -> Maybe LHsDocString -> ConDeclField a
addFieldDoc fld doc = fld { cd_fld_doc = cd_fld_doc fld `mplus` doc }

addFieldDocs :: [Located [ConDeclField a]] -> Maybe LHsDocString
             -> [Located [ConDeclField a]]
addFieldDocs [] _ = []
-- addFieldDocs (x:xs) doc = addFieldDoc x doc : xs
addFieldDocs ((L _ []):_) _ = panic "addFieldDocs empty ConDeclField"
addFieldDocs ((L l (f:fs)):xs) doc = L l ((addFieldDoc f doc):fs) : xs

addConDoc :: LConDecl a -> Maybe LHsDocString -> LConDecl a
addConDoc decl    Nothing = decl
addConDoc (L p c) doc     = L p ( c { con_doc = con_doc c `mplus` doc } )

addConDocs :: [LConDecl a] -> Maybe LHsDocString -> [LConDecl a]
addConDocs [] _ = []
addConDocs [x] doc = [addConDoc x doc]
addConDocs (x:xs) doc = x : addConDocs xs doc

addConDocFirst :: [LConDecl a] -> Maybe LHsDocString -> [LConDecl a]
addConDocFirst [] _ = []
addConDocFirst (x:xs) doc = addConDoc x doc : xs
