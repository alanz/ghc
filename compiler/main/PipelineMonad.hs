{-# LANGUAGE NamedFieldPuns #-}
-- | The CompPipeline monad and associated ops
--
-- Defined in separate module so that it can safely be imported from Hooks
module PipelineMonad (
    CompPipeline(..), evalP
  , PhasePlus(..)
  , PipeEnv(..), PipeState(..), PipelineOutput(..)
  , getPipeEnv, getPipeState, setDynFlags, setModLocation, setStubO 
  ) where

import MonadUtils
import Outputable
import DynFlags
import DriverPhases
import HscTypes
import Module

import Control.Monad

newtype CompPipeline l a = P { unP :: PipeEnv -> PipeState l -> IO (PipeState l, a) }

evalP :: CompPipeline l a -> PipeEnv -> PipeState l -> IO a
evalP f env st = liftM snd $ unP f env st

instance Functor (CompPipeline l) where
    fmap = liftM

instance Applicative (CompPipeline l) where
    pure = return
    (<*>) = ap

instance Monad (CompPipeline l) where
  return a = P $ \_env state -> return (state, a)
  P m >>= k = P $ \env state -> do (state',a) <- m env state
                                   unP (k a) env state'

instance MonadIO (CompPipeline l) where
    liftIO m = P $ \_env state -> do a <- m; return (state, a)

data PhasePlus = RealPhase Phase
               | HscOut HscSource ModuleName HscStatus

instance Outputable PhasePlus where
    ppr (RealPhase p) = ppr p
    ppr (HscOut {}) = text "HscOut"

-- -----------------------------------------------------------------------------
-- The pipeline uses a monad to carry around various bits of information

-- PipeEnv: invariant information passed down
data PipeEnv = PipeEnv {
       pe_isHaskellishFile :: Bool,
       stop_phase   :: Phase,       -- ^ Stop just before this phase
       src_filename :: String,      -- ^ basename of original input source
       src_basename :: String,      -- ^ basename of original input source
       src_suffix   :: String,      -- ^ its extension
       output_spec  :: PipelineOutput -- ^ says where to put the pipeline output
  }

-- PipeState: information that might change during a pipeline run
data PipeState l = PipeState {
       hsc_env   :: HscEnv l,
          -- ^ only the DynFlags change in the HscEnv.  The DynFlags change
          -- at various points, for example when we read the OPTIONS_GHC
          -- pragmas in the Cpp phase.
       maybe_loc :: Maybe ModLocation,
          -- ^ the ModLocation.  This is discovered during compilation,
          -- in the Hsc phase where we read the module header.
       maybe_stub_o :: Maybe FilePath
          -- ^ the stub object.  This is set by the Hsc phase if a stub
          -- object was created.  The stub object will be joined with
          -- the main compilation object using "ld -r" at the end.
  }

data PipelineOutput
  = Temporary
        -- ^ Output should be to a temporary file: we're going to
        -- run more compilation steps on this output later.
  | Persistent
        -- ^ We want a persistent file, i.e. a file in the current directory
        -- derived from the input filename, but with the appropriate extension.
        -- eg. in "ghc -c Foo.hs" the output goes into ./Foo.o.
  | SpecificFile
        -- ^ The output must go into the specific outputFile in DynFlags.
        -- We don't store the filename in the constructor as it changes
        -- when doing -dynamic-too.
    deriving Show

getPipeEnv :: CompPipeline l PipeEnv
getPipeEnv = P $ \env state -> return (state, env)

getPipeState :: CompPipeline l (PipeState l)
getPipeState = P $ \_env state -> return (state, state)

instance HasDynFlags (CompPipeline l) where
    getDynFlags = P $ \_env state -> return (state, hsc_dflags (hsc_env state))

setDynFlags :: DynFlags -> CompPipeline l ()
setDynFlags dflags = P $ \_env state ->
  return (state{hsc_env= (hsc_env state){ hsc_dflags = dflags }}, ())

setModLocation :: ModLocation -> CompPipeline l ()
setModLocation loc = P $ \_env state ->
  return (state{ maybe_loc = Just loc }, ())

setStubO :: FilePath -> CompPipeline l ()
setStubO stub_o = P $ \_env state ->
  return (state{ maybe_stub_o = Just stub_o }, ())
