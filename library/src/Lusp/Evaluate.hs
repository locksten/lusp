module Lusp.Evaluate (evaluate) where

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
evaluate :: [LispVal]
         -- ^ List of expressions
         -> IO [LispVal]
         -- ^ List of results with unprintable values filtered out
evaluate vals = filter predicate <$> (forM vals . eval =<< primitiveEnv)
  where predicate x = not (isVoid x
                        || isPrimitiveFunc x
                        || isIOFunc x
                        || isFunc x
                        || isPort x)
