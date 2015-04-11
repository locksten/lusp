module Lusp.Eval (evaluate) where

import Lusp.Environment (emptyEnv, getVar, setVar, defineVar, bindVars)
import Lusp.LispError (LispError(BadSpecialForm
                                ,TypeMismatch
                                ,NumArgs
                                ,Other))
import Lusp.LispVal (LispVal(List
                            ,DottedList
                            ,Atom
                            ,String
                            ,Integer
                            ,Real
                            ,Ratio
                            ,Complex
                            ,Bool
                            ,Char
                            ,PrimitiveFunc
                            ,Func)
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
eval env (List (Atom "define" : List (Atom var : params) : body)) =
     makeNormalFunc env params body >>= defineVar env var
eval env (List (Atom "define" : DottedList (Atom var : params)varargs : body)) =
     makeVarArgFunc varargs env params body >>= defineVar env var
eval env (List (Atom "lambda" : List params : body)) =
     makeNormalFunc env params body
eval env (List (Atom "lambda" : DottedList params varargs : body)) =
     makeVarArgFunc varargs env params body
eval env (List (Atom "lambda" : varargs@(Atom _) : body)) =
     makeVarArgFunc varargs env [] body
eval env (List (func : args)) = eval env func >>=
    (mapM (eval env) args >>=) . apply
eval _  badForm = throw $ BadSpecialForm "Unrecognized special form" badForm

apply :: LispVal -> [LispVal] -> IO LispVal
apply (PrimitiveFunc f) args = return (f args)
apply (Func params varargs body closure) args =
    if num params /= num args && varargs == Nothing
       then throw $ NumArgs (num params) args
       else (bindVars closure $ zip params args) >>=
           bindVarArgs varargs >>= evalBody
  where remainingArgs = drop (length params) args
        num = toInteger . length
        evalBody env = liftM last $ mapM (eval env) body
        bindVarArgs arg env = case arg of
            Just argName -> bindVars env [(argName, List $ remainingArgs)]
            Nothing -> return env
apply badType _ = throw $ TypeMismatch "function" badType

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

makeFunc :: Maybe String -> Env -> [LispVal] -> [LispVal] -> IO LispVal
makeFunc varargs env params body = return $
    Func (map show params) varargs body env

makeNormalFunc :: Env -> [LispVal] -> [LispVal] -> IO LispVal
makeNormalFunc = makeFunc Nothing

makeVarArgFunc :: LispVal -> Env -> [LispVal] -> [LispVal] -> IO LispVal
makeVarArgFunc = makeFunc . Just . show
