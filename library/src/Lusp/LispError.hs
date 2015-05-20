module Lusp.LispError (LispError(..)) where

import Lusp.LispVal (LispVal)

import Control.Exception (Exception)
import Text.Parsec.Error (ParseError)

data LispError = NumArgs String [LispVal]
               | TypeMismatch String LispVal
               | ParseError ParseError
               | BadSpecialForm String LispVal
               | NotFunction String String
               | UnboundVar String String
               | DivBy0
               | Other String
               | ErrorCommand String
               | StackTrace LispError LispVal
instance Show LispError where show = showError
instance Exception LispError

showError :: LispError -> String
showError (UnboundVar message varname)  = message ++ ": " ++ varname
showError (BadSpecialForm message form) = message ++ ": " ++ show form
showError (NotFunction message func)    = message ++ ": " ++ show func
showError (NumArgs expected found)      = "Expected " ++ expected ++
    " arguments" ++ "; found values " ++ show found
showError (TypeMismatch expected found) = "Invalid type: expected " ++ expected
    ++ ", found " ++ show found
showError (ParseError parseErr) = "Parse error at " ++ show parseErr
showError (DivBy0)              = "Attempted division by 0"
showError (Other str)           = show str
showError (ErrorCommand str)    = "Error: " ++ show str
showError e@(StackTrace _ _)    = "\n > Stack Trace:\n" ++ showStackTrace e

showStackTrace :: LispError -> String
showStackTrace (StackTrace e v) = show v ++ "\n" ++ case e of
    x@(StackTrace _ _) -> showStackTrace x
    x                  -> "Exception: " ++ show x
showStackTrace _ = error "Expected StackTrace"
