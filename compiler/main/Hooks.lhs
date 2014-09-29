\section[Hooks]{Low level API hooks}

\begin{code}
module Hooks ( Hooks
             , emptyHooks
             , lookupHook
             , getHooked
               -- the hooks:
             , dsForeignsHook
             , tcForeignImportsHook
             , tcForeignExportsHook
             , hscFrontendHook
             , hscCompileOneShotHook
             , hscCompileCoreExprHook
             , ghcPrimIfaceHook
             , runPhaseHook
             , linkHook
             , runQuasiQuoteHook
             , runRnSpliceHook
             , getValueSafelyHook
             ) where

import DynFlags
import HsTypes
import Name
import PipelineMonad
import HscTypes
import HsDecls
import HsBinds
import HsExpr
import {-# SOURCE #-} DsMonad
import OrdList
import Id
import TcRnTypes
import Bag
import RdrName
import CoreSyn
import BasicTypes
import Type
-- import SrcLoc

import Data.Maybe
\end{code}

%************************************************************************
%*                                                                      *
\subsection{Hooks}
%*                                                                      *
%************************************************************************

\begin{code}

-- | Hooks can be used by GHC API clients to replace parts of
--   the compiler pipeline. If a hook is not installed, GHC
--   uses the default built-in behaviour

emptyHooks :: Hooks l
emptyHooks = Hooks Nothing Nothing Nothing Nothing Nothing Nothing
                   Nothing Nothing Nothing Nothing Nothing Nothing

data Hooks l = Hooks
  { dsForeignsHook         :: Maybe ([LForeignDecl l Id]
                           -> DsM l (ForeignStubs, OrdList (Id, CoreExpr)))

  , tcForeignImportsHook   :: Maybe ([LForeignDecl l Name]
                         -> TcM l ([Id], [LForeignDecl l Id], Bag GlobalRdrElt))

  , tcForeignExportsHook   :: Maybe ([LForeignDecl l Name]
                           -> TcM l (LHsBinds l TcId, [LForeignDecl l TcId],
                                    Bag GlobalRdrElt))

  , hscFrontendHook        :: Maybe (ModSummary -> Hsc l (TcGblEnv l))

  , hscCompileOneShotHook  :: Maybe (HscEnv l -> ModSummary -> SourceModified
                           -> IO HscStatus)

  , hscCompileCoreExprHook :: Maybe (HscEnv l -> l -> CoreExpr -> IO HValue)

  , ghcPrimIfaceHook       :: Maybe ModIface

  , runPhaseHook           :: Maybe (PhasePlus -> FilePath -> DynFlags
                           -> CompPipeline l (PhasePlus, FilePath))

  , linkHook               :: Maybe (GhcLink -> DynFlags -> Bool
                           -> HomePackageTable -> IO SuccessFlag)

  , runQuasiQuoteHook      :: Maybe (HsQuasiQuote Name
                           -> RnM l (HsQuasiQuote Name))

  , runRnSpliceHook        :: Maybe (LHsExpr l Name
                           -> RnM l (LHsExpr l Name))

  , getValueSafelyHook     :: Maybe (HscEnv l -> Name -> Type
                           -> IO (Maybe HValue))
  }

getHooked :: (Functor f, HasDynFlags f) => (Hooks l -> Maybe a) -> a -> f a
getHooked hook def = fmap (lookupHook hook def) getDynFlags

lookupHook :: (Hooks l -> Maybe a) -> a -> DynFlags -> a
lookupHook hook def _ = fromMaybe def . hook $ hooks

-- ++AZ++ temporary
hooks :: Hooks l
hooks = emptyHooks

\end{code}

