module Lusp.Evaluate (evaluate) where

import Lusp.Eval (eval)
import Lusp.LispVal (LispVal
                    ,isVoid)
import Lusp.Primitives (primitiveEnv)

import Control.Monad (forM)

evaluate :: [LispVal] -> IO [LispVal]
evaluate vals = filter (not . isVoid) <$> (forM vals . eval =<< primitiveEnv)
