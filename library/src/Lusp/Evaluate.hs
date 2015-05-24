module Lusp.Evaluate (evaluate
                     ,initialEnv) where

import Lusp.Environment (bindMetaVars)
import Lusp.Eval (eval)
import Lusp.LispVal (LispVal
                    ,Env)
import Lusp.LispValUtils (isVoid
                         ,isPrimitiveFunc
                         ,isIOFunc
                         ,isFunc
                         ,isPort)
import Lusp.Primitives (primitiveEnv)

import Control.Monad (forM)

-- | Evaluates a list of expressions in the given environment
evaluate :: Env
         -> [LispVal]
         -- ^ List of expressions
         -> IO [LispVal]
         -- ^ List of results with unprintable values filtered out
evaluate env vals = filterUnprintable $ forM vals (eval env)

-- | Returns an environment containing prmitives and meta information
initialEnv :: String
           -- ^ Path to the source code file
           -> [String]
           -- ^ Command line arguments
           -> IO Env
initialEnv path args = primitiveEnv >>= \e -> bindMetaVars e path args

-- | Filters out unprintable values
filterUnprintable :: Functor f => f [LispVal] -> f [LispVal]
filterUnprintable xs = filter predicate <$> xs
  where predicate x = not (isVoid x
                        || isPrimitiveFunc x
                        || isIOFunc x
                        || isFunc x
                        || isPort x)
