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

evaluate :: [LispVal] -> IO [LispVal]
evaluate vals = filter predicate <$> (forM vals . eval =<< primitiveEnv)
  where predicate x = not (isVoid x
                        || isPrimitiveFunc x
                        || isIOFunc x
                        || isFunc x
                        || isPort x)
