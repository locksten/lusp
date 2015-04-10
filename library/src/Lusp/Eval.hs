module Lusp.Eval (eval) where

import Lusp.Environment (Env)
import Lusp.LispError (LispError(BadSpecialForm
                                ,NotFunction))
import Lusp.LispVal (LispVal(List
                            ,Atom
                            ,String
                            ,Integer
                            ,Real
                            ,Ratio
                            ,Complex
                            ,Bool
                            ,Char))
import qualified Lusp.Numeric as N (add
                                   ,subtract
                                   ,multiply
                                   ,divide
                                   ,modulo
                                   ,remainder
                                   ,quotient)

import Control.Exception (throw)

eval :: Env -> LispVal -> IO LispVal
eval _   v@(String _)  = return v
eval _   v@(Integer _) = return v
eval _   v@(Real _)    = return v
eval _   v@(Ratio _)   = return v
eval _   v@(Complex _) = return v
eval _   v@(Bool _)    = return v
eval _   v@(Char _)    = return v
eval env (Atom v)      = getVar env v
eval _   (List [Atom "quote", v]) = return v
eval env (List (Atom func:args)) = apply func <$> mapM (eval env) args
eval _  badForm = throw $ BadSpecialForm "Unrecognized special form" badForm

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
