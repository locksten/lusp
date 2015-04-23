module Lusp.Eval (eval
                 ,apply) where

import Lusp.Environment (childEnv
                        ,getVar
                        ,setVar
                        ,defineVar
                        ,bindVars)
import Lusp.LispError (LispError(BadSpecialForm
                                ,TypeMismatch
                                ,NumArgs
                                ,StackTrace))
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
                            ,IOFunc
                            ,Func
                            ,Void)
                    ,Env)
import Lusp.LispValUtils (extractList)
import Lusp.Parser (parse)

import Control.Exception (throw
                         ,catch)
import Control.Monad (zipWithM_)
import Data.Maybe (isNothing)

eval :: Env -> LispVal -> IO LispVal
eval env val = catch (eval' env val) (care val)
  where care :: LispVal -> LispError -> IO LispVal
        care v e = throw $ StackTrace e v

eval' :: Env -> LispVal -> IO LispVal
eval' _   v@(String _)  = return v
eval' _   v@(Integer _) = return v
eval' _   v@(Real _)    = return v
eval' _   v@(Ratio _)   = return v
eval' _   v@(Complex _) = return v
eval' _   v@(Bool _)    = return v
eval' _   v@(Char _)    = return v
eval' env (Atom v)      = getVar env v
eval' _   (List [Atom "quote", v]) = return v
eval' env (List [Atom "load", String filename]) =
    parse <$> readFile filename >>= mapM_ (eval env) >> return Void
eval' env (List [Atom "if", predicate, consequnce, alternative]) =
    eval env predicate >>= \res ->
       case res of
         Bool True  -> eval env consequnce
         _          -> eval env alternative
eval' env (List [Atom "if", predicate, consequnce]) =
    eval env predicate >>= \res ->
       case res of
         Bool True  -> eval env consequnce
         _          -> return Void
eval' env (List [Atom "set!", Atom var, form]) = eval env form
    >>= setVar env var
eval' env (List [Atom "define", Atom var, form]) = eval env form
    >>= defineVar env var
eval' env (List (Atom "define" : List (Atom var : params) : body)) =
    makeNormalFunc env params body >>= defineVar env var
eval' env (List (Atom "define" : DottedList (Atom var : params)
    varargs : body)) =
        makeVarArgFunc varargs env params body >>= defineVar env var
eval' env (List (Atom "lambda" : List params : body)) =
    makeNormalFunc env params body
eval' env (List (Atom "lambda" : DottedList params varargs : body)) =
    makeVarArgFunc varargs env params body
eval' env (List (Atom "lambda" : varargs@(Atom _) : body)) =
    makeVarArgFunc varargs env [] body
eval' env (List (Atom "begin" : expressions)) =
    last <$> mapM (eval env) expressions
eval' env (List (Atom "let" : List bindings : body)) =
    bindLet env bindings >>= \e -> last <$> mapM (eval e) body
eval' env (List (func : args)) = eval env func >>=
    (mapM (eval env) args >>=) . apply
eval' _  badForm = throw $ BadSpecialForm "Unrecognized special form" badForm

bindLet :: Env -> [LispVal] -> IO Env
bindLet env bindings = childEnv env >>= \e ->
      mapM (eval e) inits >>= zipWithM_ (defineVar e) vars >> return e
  where vars = show . head <$> bindsList
        inits = last <$> bindsList
        bindsList = toList <$> bindings
        toList :: LispVal -> [LispVal]
        toList xs = if length (extractList xs) /= 2
                       then throw $ TypeMismatch "(<var> <expr>)" xs
                       else extractList xs

apply :: LispVal -> [LispVal] -> IO LispVal
apply (PrimitiveFunc f) args = return (f args)
apply (IOFunc f) args = f args
apply (Func params varargs body closure) args =
  if num params /= num args && isNothing varargs
     then throw $ NumArgs (show $ num params) args
     else childEnv closure >>= flip bindVars (zip params args) >>=
         bindVarArgs varargs >>= evalBody
  where remainingArgs = drop (length params) args
        num = toInteger . length
        evalBody env = last <$> mapM (eval env) body
        bindVarArgs arg env = case arg of
            Just argName -> bindVars env [(argName, List remainingArgs)]
            Nothing -> return env
apply badType _ = throw $ TypeMismatch "function" badType

makeFunc :: Maybe String -> Env -> [LispVal] -> [LispVal] -> IO LispVal
makeFunc varargs env params body = return $
    Func (map show params) varargs body env

makeNormalFunc :: Env -> [LispVal] -> [LispVal] -> IO LispVal
makeNormalFunc = makeFunc Nothing

makeVarArgFunc :: LispVal -> Env -> [LispVal] -> [LispVal] -> IO LispVal
makeVarArgFunc = makeFunc . Just . show
