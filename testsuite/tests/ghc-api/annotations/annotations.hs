{-# LANGUAGE RankNTypes #-}

-- This program must be called with GHC's libdir as the single command line
-- argument.
module Main where

-- import Data.Generics
import Data.Data
import System.IO
import GHC
import ApiAnnotation -- AZ needs to be in GHC
import MonadUtils
import Outputable
import Bag (filterBag,isEmptyBag)
import System.Directory (removeFile)
import System.Environment( getArgs )
import qualified Data.Map as Map
import Data.Dynamic ( fromDynamic,Dynamic )

main::IO()
main = do
        [libdir] <- getArgs
        testOneFile libdir "AnnotationLet"

testOneFile libdir fileName = do
        p <- runGhc (Just libdir) $ do
                        dflags <- getSessionDynFlags
                        setSessionDynFlags dflags
                        let mn =mkModuleName fileName
                        addTarget Target { targetId = TargetModule mn
                                         , targetAllowObjCode = True
                                         , targetContents = Nothing }
                        load LoadAllTargets
                        modSum <- getModSummary mn
                        p <- parseModule modSum
                        t <- typecheckModule p
                        d <- desugarModule t
                        l <- loadModule d
                        let ts=typecheckedSource l
                            r =renamedSource l
                        -- liftIO (putStr (showSDocDebug (ppr ts)))
                        return (pm_annotations p)

        let anns = Map.fromList p
            AK l _ = fst $ head p
            annLet = (getAnnotation anns l) :: Maybe AnnHsLet

        print (anns,annLet,l)
