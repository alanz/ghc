
module HaddockUtils where

import HsSyn
import SrcLoc

import Control.Monad

-- -----------------------------------------------------------------------------
-- Adding documentation to record fields (used in parsing).

addFieldDoc :: LConDeclField x a -> Maybe LHsDocString -> LConDeclField x a
addFieldDoc (L l fld) doc
  = L l (fld { cd_fld_doc = cd_fld_doc fld `mplus` doc })

addFieldDocs :: [LConDeclField x a] -> Maybe LHsDocString -> [LConDeclField x a]
addFieldDocs [] _ = []
addFieldDocs (x:xs) doc = addFieldDoc x doc : xs


addConDoc :: LConDecl x a -> Maybe LHsDocString -> LConDecl x a
addConDoc decl    Nothing = decl
addConDoc (L p c) doc     = L p ( c { con_doc = con_doc c `mplus` doc } )

addConDocs :: [LConDecl x a] -> Maybe LHsDocString -> [LConDecl x a]
addConDocs [] _ = []
addConDocs [x] doc = [addConDoc x doc]
addConDocs (x:xs) doc = x : addConDocs xs doc

addConDocFirst :: [LConDecl x a] -> Maybe LHsDocString -> [LConDecl x a]
addConDocFirst [] _ = []
addConDocFirst (x:xs) doc = addConDoc x doc : xs
