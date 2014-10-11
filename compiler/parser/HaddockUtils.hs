
module HaddockUtils where

import HsSyn
import SrcLoc

import Control.Monad

-- -----------------------------------------------------------------------------
-- Adding documentation to record fields (used in parsing).

addFieldDoc :: ConDeclField a -> Maybe LHsDocString -> ConDeclField a
addFieldDoc fld doc = fld { cd_fld_doc = cd_fld_doc fld `mplus` doc }

addFieldDocs :: [ConDeclField a] -> Maybe LHsDocString -> [ConDeclField a]
addFieldDocs [] _ = []
addFieldDocs (x:xs) doc = addFieldDoc x doc : xs

addConDoc :: LConDecl a -> Maybe LHsDocString -> LConDecl a
addConDoc decl    Nothing = decl
addConDoc (L p c) doc     = L p ( c { con_doc = con_doc c `mplus` doc } )

addConDocs :: [LConDecl a] -> Maybe LHsDocString -> [LConDecl a]
addConDocs [] _ = []
addConDocs [x] doc = [addConDoc x doc]
addConDocs (x:xs) doc = x : addConDocs xs doc

-- | Add the docs to the last LConDecl
addConDocsCL :: HsCommaList (LConDecl a) -> Maybe LHsDocString
             -> HsCommaList (LConDecl a)
addConDocsCL Empty _ = Empty
addConDocsCL xs doc = reverseCL $ addConDocs' xsr -- This may be inefficient
  where
    xsr = reverseCL xs
    addConDocs' (Cons x xs) = Cons (addConDoc x doc) xs
    addConDocs' (ExtraComma l xs) = ExtraComma l (addConDocs' xs)
    addConDocs' _ = error "should not occur due to reverseCL"

addConDocFirstCL :: HsCommaList (LConDecl a) -> Maybe LHsDocString
                 -> HsCommaList (LConDecl a)
addConDocFirstCL Empty _ = Empty
addConDocFirstCL (Cons x xs)       doc = Cons (addConDoc x doc) xs
addConDocFirstCL (ExtraComma l xs) doc = ExtraComma l (addConDocFirstCL xs doc)
addConDocFirstCL (Snoc bs x)       doc = Snoc (addConDocFirstCL bs doc) x
addConDocFirstCL (Two as bs)       doc = Two (addConDocFirstCL as doc) bs

addConDocFirst :: [LConDecl a] -> Maybe LHsDocString -> [LConDecl a]
addConDocFirst [] _ = []
addConDocFirst (x:xs) doc = addConDoc x doc : xs
