module Lusp.Evaluate (evaluate
                     ,initialEnv
                     ,stdlibInstallDir
                     ,autoCompleteTokens) where

import Lusp.Environment (bindMetaVars
                        ,getAllVarNames)
import Lusp.Eval (eval
                 ,load)
import Lusp.LispVal (LispVal
                    ,Env)
import Lusp.LispValUtils (isVoid
                         ,isPrimitiveFunc
                         ,isIOFunc
                         ,isFunc
                         ,isPort)
import Lusp.Primitives (primitiveEnv)

import Control.Monad (forM)
import Paths_lusp_lib (getDataFileName)
import System.FilePath (takeDirectory)

-- | Evaluates a list of expressions in the given environment
evaluate :: Env
         -> [LispVal]
         -- ^ List of expressions
         -> IO [LispVal]
         -- ^ List of results with unprintable values filtered out
evaluate env vals = filterUnprintable $ forM vals (eval env)

-- | Returns an environment containing prmitives and meta information
initialEnv :: [String]
           -- ^ List of directories to look for source files in
           -> [String]
           -- ^ Command line arguments
           -> IO Env
initialEnv importPaths args = do
    env <- primitiveEnv
    e <- bindMetaVars env importPaths args
    load e "stdlib.scm"
    return e

-- | Extracts a list of tokens for autocompletion from the environment
autoCompleteTokens :: Env -> IO [String]
autoCompleteTokens = getAllVarNames

-- | Returns the directory in which stdlib.scm is installed
stdlibInstallDir :: IO FilePath
stdlibInstallDir = takeDirectory <$> getDataFileName "stdlib.scm"

-- | Filters out unprintable values
filterUnprintable :: Functor f => f [LispVal] -> f [LispVal]
filterUnprintable xs = filter predicate <$> xs
  where predicate x = not (isVoid x
                        || isPrimitiveFunc x
                        || isIOFunc x
                        || isFunc x
                        || isPort x)
