
module HaddockUtils where

import HsSyn
import SrcLoc

import Control.Monad

-- -----------------------------------------------------------------------------
-- Adding documentation to record fields (used in parsing).

addFieldDoc :: ConDeclField l a -> Maybe (LHsDocString l) -> ConDeclField l a
addFieldDoc fld doc = fld { cd_fld_doc = cd_fld_doc fld `mplus` doc }

addFieldDocs :: [ConDeclField l a] -> Maybe (LHsDocString l)
             -> [ConDeclField l a]
addFieldDocs [] _ = []
addFieldDocs (x:xs) doc = addFieldDoc x doc : xs

addConDoc :: LConDecl l a -> Maybe (LHsDocString l) -> LConDecl l a
addConDoc decl    Nothing = decl
addConDoc (L p c) doc     = L p ( c { con_doc = con_doc c `mplus` doc } )

addConDocs :: [LConDecl l a] -> Maybe (LHsDocString l) -> [LConDecl l a]
addConDocs [] _ = []
addConDocs [x] doc = [addConDoc x doc]
addConDocs (x:xs) doc = x : addConDocs xs doc

addConDocFirst :: [LConDecl l a] -> Maybe (LHsDocString l) -> [LConDecl l a]
addConDocFirst [] _ = []
addConDocFirst (x:xs) doc = addConDoc x doc : xs
