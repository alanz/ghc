{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
module T12530 where

import Language.Haskell.TH

$([d|   -- Test the Template Haskell pretty-printing for TypeApplications
        f = id @(Maybe Int)

    |])
