
module HaddockUtils where

import HsSyn
import SrcLoc

import Control.Monad

-- -----------------------------------------------------------------------------
-- Adding documentation to record fields (used in parsing).

addFieldDoc :: ConDeclField a ptt -> Maybe LHsDocString -> ConDeclField a ptt
addFieldDoc fld doc = fld { cd_fld_doc = cd_fld_doc fld `mplus` doc }

addFieldDocs :: [ConDeclField a ptt] -> Maybe LHsDocString -> [ConDeclField a ptt]
addFieldDocs [] _ = []
addFieldDocs (x:xs) doc = addFieldDoc x doc : xs

addConDoc :: LConDecl a ptt -> Maybe LHsDocString -> LConDecl a ptt
addConDoc decl    Nothing = decl
addConDoc (L p c) doc     = L p ( c { con_doc = con_doc c `mplus` doc } )

addConDocs :: [LConDecl a ptt] -> Maybe LHsDocString -> [LConDecl a ptt]
addConDocs [] _ = []
addConDocs [x] doc = [addConDoc x doc]
addConDocs (x:xs) doc = x : addConDocs xs doc

addConDocFirst :: [LConDecl a ptt] -> Maybe LHsDocString -> [LConDecl a ptt]
addConDocFirst [] _ = []
addConDocFirst (x:xs) doc = addConDoc x doc : xs
