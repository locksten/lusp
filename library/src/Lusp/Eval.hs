module Lusp.Eval (eval
                 ,apply
                 ,load) where

import Lusp.Environment (childEnv
                        ,getVar
                        ,setVar
                        ,defineVar
                        ,isBound
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
import Lusp.LispValUtils (extractList
                         ,extractStr)
import Lusp.Parser (parse)

import Control.Exception (throw
                         ,catch)
import Control.Monad (zipWithM_)
import Data.Maybe (isNothing)
import System.Directory (findFile)
import System.IO.Error (mkIOError
                       ,doesNotExistErrorType)

-- | Wraps 'Lusp.Eval.eval'' for creating a stack trace
eval :: Env -> LispVal -> IO LispVal
eval env val = catch (eval' env val) (care val)
  where care :: LispVal -> LispError -> IO LispVal
        care v e = throw $ StackTrace e v

-- | Evaluates an expression
eval' :: Env
      -- ^ Environment in which to evaluate the expression
      -> LispVal
      -- ^ Expression to evaluate
      -> IO LispVal
      -- ^ Result of the expression
eval' _   v@(String _)  = return v
eval' _   v@(Integer _) = return v
eval' _   v@(Real _)    = return v
eval' _   v@(Ratio _)   = return v
eval' _   v@(Complex _) = return v
eval' _   v@(Bool _)    = return v
eval' _   v@(Char _)    = return v
eval' env (Atom v)      = getVar env v
eval' _   (List [Atom "quote", v]) = return v
eval' env (List [Atom "quasiquote", List xs]) =
    eval env (List (Atom "append" : (enquote <$> xs)))
  where enquote x = case x of
            List [Atom "unquote", y]          -> List [Atom "list", y]
            List [Atom "unquote-splicing", y] -> y
            _                                 ->
              List [Atom "list", List [Atom "quote", x]]
eval' env (List [Atom "load", String filename]) =
    load env filename >> return Void
eval' env (List [Atom "if", predicate, consequnce, alternative]) =
    eval env predicate >>= \res ->
       case res of
         Bool False -> eval env alternative
         _          -> eval env consequnce
eval' env (List [Atom "if", predicate, consequnce]) =
    eval env predicate >>= \res ->
       case res of
         Bool False -> return Void
         _          -> eval env consequnce
eval' env (List [Atom "set!", Atom var, form]) = eval env form
    >>= setVar env var

eval' env (List [Atom "set-car!", Atom var, form]) = eval env form >>= \new ->
    getVar env var >>= \old ->
      case old of
        List (_:xs)         -> setVar env var $ List (new : xs)
        DottedList (_:xs) e -> setVar env var $ DottedList (new : xs) e
        x                   -> throw $ TypeMismatch "pair" x
eval' env (List [Atom "set-cdr!", Atom var, form]) = eval env form >>= \new ->
    getVar env var >>= \old ->
      case new of
        List n         -> setVar env var $ List (first old : n)
        DottedList n e -> setVar env var $ DottedList (first old : n) e
        n              -> setVar env var $ DottedList [first old] n
  where
      first x = case x of
                  List (y:_)         -> y
                  DottedList (y:_) _ -> y
                  y                  -> throw $ TypeMismatch "pair" y
eval' env (List (Atom "define-macro" : Atom keyword : [body])) =
    defineVar env (macroPrefix ++ keyword) body
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
eval' env (List (Atom "eval" : [expr])) = eval env expr >>= eval env
eval' env (List (Atom func : args)) = isBound env prefixed >>= \bound ->
    if bound then handleMacro else handleFunc
  where prefixed = macroPrefix ++ func
        handleFunc = eval env (Atom func) >>= (mapM (eval env) args >>=) . apply
        handleMacro = getVar env prefixed >>= \f ->
          eval env (List [Atom "apply", f, List [Atom "quote", List args]])
          >>= \again -> eval env again
eval' _  badForm = throw $ BadSpecialForm "Unrecognized special form" badForm

macroPrefix :: String
macroPrefix = "MACRO-"

-- | Evaluate let bindings
bindLet :: Env
        -- ^ Environment
        -> [LispVal]
        -- ^ List of (<var> <expr>)
        -> IO Env
        -- ^ Child environmen with the new bindings
bindLet env bindings = childEnv env >>= \e ->
      mapM (eval e) inits >>= zipWithM_ (defineVar e) vars >> return e
  where vars = show . head <$> bindsList
        inits = last <$> bindsList
        bindsList = toList <$> bindings
        toList :: LispVal -> [LispVal]
        toList xs = if length (extractList xs) /= 2
                       then throw $ TypeMismatch "(<var> <expr>)" xs
                       else extractList xs

-- | Apply the function to the arguments
apply :: LispVal
      -- ^ Function
      -> [LispVal]
      -- ^ Arguments
      -> IO LispVal
      -- ^ Result
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

load :: Env -> FilePath -> IO ()
load env filename =
    list <$> getVar env "import-paths" >>= flip findFile filename >>= \found ->
      case found of
        Nothing -> throw $
          mkIOError doesNotExistErrorType "" Nothing (Just filename)
        Just x  -> parse <$> readFile x >>= mapM_ (eval env) >> return ()
  where list = fmap extractStr . extractList
