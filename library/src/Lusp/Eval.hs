module Lusp.Eval (evaluate) where

import Lusp.Environment (emptyEnv, getVar, setVar, defineVar, bindVars)
import Lusp.LispError (LispError(BadSpecialForm
                                ,NotFunction
                                ,TypeMismatch
                                ,Other))
import Lusp.LispVal (LispVal(List
                            ,Atom
                            ,String
                            ,Integer
                            ,Real
                            ,Ratio
                            ,Complex
                            ,Bool
                            ,Char)
                            ,PrimitiveFunc
                    ,isVoid
                    ,Env)
import qualified Lusp.Numeric as N (add
                                   ,subtract
                                   ,multiply
                                   ,divide
                                   ,modulo
                                   ,remainder
                                   ,quotient)

import Control.Exception (throw)
import Control.Monad (liftM)

evaluate :: [LispVal] -> IO [LispVal]
evaluate vals = filter (not . isVoid) <$>
    (flip mapM vals . eval =<< primitiveEnv)

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
eval env (List [Atom "if", predicate, consequnce, alternative]) =
        eval env predicate >>= \res ->
           case res of
             Bool True  -> eval env consequnce
             Bool False -> eval env alternative
             badType    -> throw $ TypeMismatch "bool" badType
eval env (List [Atom "if", predicate, consequnce]) =
        eval env predicate >>= \res ->
           case res of
             Bool True  -> eval env consequnce
             Bool False -> throw $ Other "False if without alternative"
             badType    -> throw $ TypeMismatch "bool" badType
eval env (List [Atom "set!", Atom var, form]) = eval env form
        >>= setVar env var
eval env (List [Atom "define", Atom var, form]) = eval env form
        >>= defineVar env var
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

primitiveEnv :: IO Env
primitiveEnv = emptyEnv >>= (flip bindVars $ map makePrimitiveFunc primitives)
     where makePrimitiveFunc (var, func) = (var, PrimitiveFunc func)
