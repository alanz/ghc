{-# LANGUAGE RankNTypes #-}

-- This program must be called with GHC's libdir as the single command line
-- argument.
module Main where

-- import Data.Generics
import Data.Data
import Data.List
import System.IO
import GHC
import DynFlags
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

        let anns = p
            AK l _ = fst $ head $ Map.toList p
            annLet = (getAnnotation anns l) :: Maybe AnnHsLet

        putStrLn (intercalate "\n" [showAnns anns,pp annLet,pp l])

showAnns anns = "[\n" ++ (intercalate "\n"
   $ map (\(AK s k,v)
              -> ("(AK " ++ pp s ++ " " ++ show k ++",\n " ++ pp v ++ ")\n"))
   $ Map.toList anns)
    ++ "]\n"

pp a = showPpr unsafeGlobalDynFlags a

-- | Retrieve an annotation based on the SrcSpan of the annotated AST
-- element, and the known type of the annotation.
getAnnotation1 :: (Typeable a) => ApiAnns -> SrcSpan -> Maybe a
getAnnotation1 anns span = res
  where res = case  Map.lookup (AK span (typeOf res)) anns of
                       Nothing -> Nothing
                       Just d -> Just $ fromValue d
