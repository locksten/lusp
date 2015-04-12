module Lusp.Evaluate (evaluate) where

import Lusp.Eval (eval)
import Lusp.LispVal (LispVal
                    ,isVoid)
import Lusp.Primitives (primitiveEnv)

evaluate :: [LispVal] -> IO [LispVal]
evaluate vals = filter (not . isVoid) <$>
    (flip mapM vals . eval =<< primitiveEnv)
