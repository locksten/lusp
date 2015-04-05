module Lusp.Eval (eval) where

import Lusp.LispError (LispError(..))
import Lusp.LispVal (LispVal(..))
import Lusp.Numeric

import Control.Exception (throw)

eval :: LispVal -> LispVal
eval v@(String _)  = v
eval v@(Integer _) = v
eval v@(Float _)   = v
eval v@(Ratio _)   = v
eval v@(Complex _) = v
eval v@(Bool _)    = v
eval v@(Char _)    = v
eval (List [Atom "quote", v]) = v
eval (List (Atom func:args)) = apply func $ map eval args
eval badForm = throw $ BadSpecialForm "Unrecognized special form" badForm

apply :: String -> [LispVal] -> LispVal
apply func args = maybe (throw $
                  NotFunction "Unrecognized primitive function" func)
                  ($ args) $ lookup func primitives

primitives :: [(String, ([LispVal] -> LispVal))]
primitives = undefined
