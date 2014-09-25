module Vectorise.Monad.Local 
  ( readLEnv
  , setLEnv
  , updLEnv
  , localV
  , closedV
  , getBindName
  , inBind
  , lookupTyVarPA
  , defLocalTyVar
  , defLocalTyVarWithPA
  , localTyVars
  )
where
  
import Vectorise.Monad.Base
import Vectorise.Env

import CoreSyn
import Name
import VarEnv
import Var
import FastString

-- Local Environment ----------------------------------------------------------

-- |Project something from the local environment.
--
readLEnv :: (LocalEnv -> a) -> VM l a
readLEnv f  = VM $ \_ genv lenv -> return (Yes genv lenv (f lenv))

-- |Set the local environment.
--
setLEnv :: LocalEnv -> VM l ()
setLEnv lenv  = VM $ \_ genv _ -> return (Yes genv lenv ())

-- |Update the environment using the provided function.
--
updLEnv :: (LocalEnv -> LocalEnv) -> VM l ()
updLEnv f  = VM $ \_ genv lenv -> return (Yes genv (f lenv) ())

-- |Perform a computation in its own local environment.
-- This does not alter the environment of the current state.
--
localV :: VM l a -> VM l a
localV p 
  = do  
    { env <- readLEnv id
    ; x   <- p
    ; setLEnv env
    ; return x
    }

-- |Perform a computation in an empty local environment.
--
closedV :: VM l a -> VM l a
closedV p 
  = do
    { env <- readLEnv id
    ; setLEnv (emptyLocalEnv { local_bind_name = local_bind_name env })
    ; x   <- p
    ; setLEnv env
    ; return x
    }

-- |Get the name of the local binding currently being vectorised.
--
getBindName :: VM l FastString
getBindName = readLEnv local_bind_name

-- |Run a vectorisation computation in a local environment, 
-- with this id set as the current binding.
--
inBind :: Id -> VM l a -> VM l a
inBind id p
  = do updLEnv $ \env -> env { local_bind_name = occNameFS (getOccName id) }
       p

-- |Lookup a PA tyvars from the local environment.
--
lookupTyVarPA :: Var -> VM l (Maybe CoreExpr)
lookupTyVarPA tv 
  = readLEnv $ \env -> lookupVarEnv (local_tyvar_pa env) tv

-- |Add a tyvar to the local environment.
--
defLocalTyVar :: TyVar -> VM l ()
defLocalTyVar tv = updLEnv $ \env ->
  env { local_tyvars   = tv : local_tyvars env
      , local_tyvar_pa = local_tyvar_pa env `delVarEnv` tv
      }

-- |Add mapping between a tyvar and pa dictionary to the local environment.
--
defLocalTyVarWithPA :: TyVar -> CoreExpr -> VM l ()
defLocalTyVarWithPA tv pa = updLEnv $ \env ->
  env { local_tyvars   = tv : local_tyvars env
      , local_tyvar_pa = extendVarEnv (local_tyvar_pa env) tv pa
      }

-- |Get the set of tyvars from the local environment.
--
localTyVars :: VM l [TyVar]
localTyVars = readLEnv (reverse . local_tyvars)
