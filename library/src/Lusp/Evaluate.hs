module Lusp.Evaluate (evaluate) where

import Lusp.Environment (bindMetaVars)
import Lusp.Eval (eval)
import Lusp.LispVal (LispVal)
import Lusp.LispValUtils (isVoid
                         ,isPrimitiveFunc
                         ,isIOFunc
                         ,isFunc
                         ,isPort)
import Lusp.Primitives (primitiveEnv)

import Control.Monad (forM)

-- | Evaluates a list of expressions
evaluate :: String
         -- ^ Path to the source code file
         -> [String]
         -- ^ command line arguments
         -> [LispVal]
         -- ^ List of expressions
         -> IO [LispVal]
         -- ^ List of results with unprintable values filtered out
evaluate path args vals = filter predicate <$> (env >>= forM vals . eval)
  where env = primitiveEnv >>= \e -> bindMetaVars e path args
        predicate x = not (isVoid x
                          || isPrimitiveFunc x
                          || isIOFunc x
                          || isFunc x
                          || isPort x)
