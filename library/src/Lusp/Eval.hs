module Lusp.Eval (eval) where

import Lusp.LispError (LispError(BadSpecialForm
                                ,NotFunction))
import Lusp.LispVal (LispVal(..))
import qualified Lusp.Numeric as N (add
                                   ,subtract
                                   ,multiply
                                   ,divide
                                   ,modulo
                                   ,remainder
                                   ,quotient)

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
primitives = [("+", N.add)
             ,("-", N.subtract)
             ,("*", N.multiply)
             ,("/", N.divide)
             ,("modulo", N.modulo)
             ,("remainder", N.remainder)
             ,("quotient", N.quotient)]
